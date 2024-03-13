module Language.Ribbon.Parsing.Lexer where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text

import Data.Char qualified as Char
import Data.Sequence qualified as Seq

import Data.Word (Word32)

import Data.Functor ((<&>))

import Data.Tag
import Data.Pos
import Data.Range
import Data.Attr
import Data.SyntaxError
import Data.Nil

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Dynamic
import Control.Monad.Reader.Dynamic
import Control.Monad.IO.Class (MonadIO(..))
import Control.Has

import GHC.IO qualified as IO

import Text.Pretty

import Language.Ribbon.Util
import Language.Ribbon.Lexical
import Language.Ribbon.Parsing.Monad
import Language.Ribbon.Parsing.Text
import Control.Concurrent.ParallelIO.Global (extraWorkerWhileBlocked)






lexFile :: Has m [OS, Err Doc] => FilePath -> m (ATag TokenSeq)
lexFile fp = do
    lx <- liftError @SyntaxError $ lexStreamFromFile fp
    liftError @SyntaxError $ runReaderT (evalParserT (tag doc) lx) fp


-- | Marker for @Has@ ie @Has m '[Lex]@ ~ @MonadParser LexStream m@
data Lex

type instance Has m (Lex ': effs) = (MonadParser LexStream m, Has m effs)

data LexStream
    = LexStream
    { pos :: !Pos
    , input :: Text
    }
    deriving Show

instance Pretty LexStream where
    pPrint (LexStream p i) = pPrint p <+> text (show i)

instance Nil LexStream where
    nil = LexStream Nil mempty
    isNil (LexStream _ i) = Text.null i

instance ParseInput LexStream where
    type InputElement LexStream = Char
    formatInput fp ls = case unconsInput ls of
        Left e -> formatProblem (attrInput fp ls) e
        Right (c, ls') -> UnexpectedFailure
            (inputPretty c) :@: attrInputDiff fp ls ls'
    unconsInput ls = do
        (h, input') <- maybeError DecodeEof do
            Text.uncons ls.input

        size <- byteWidth $ Char.ord h

        let offset' = size + ls.pos.offset
            (line', column') =
                if h == '\n'
                    then (ls.pos.line + 1, 1)
                    else (ls.pos.line, ls.pos.column + 1)

        pure (h, ls {
            input = input',
            pos = Pos offset' line' column'
        }) where
        byteWidth c
            | c == 0xFFFD = Left DecodeBadEncoding
            | c <= 0x7f = pure 1
            | c <= 0x7ff = pure 2
            | c <= 0xffff = pure 3
            | otherwise = pure 4
    attrInput fp ls = Attr fp (unitRange ls.pos)
    attrInputDiff fp ls ls' = Attr fp (Range ls.pos ls'.pos)

-- | Wrap a @Text@ in a new @LexStream@,
--   note that the @Text@, if decoded from another source, may use
--   @Text.lenientDecode@ as the markers for invalid characters it generates
--   are checked for by @ParseInput@ implementation
lexStreamFromText :: Text -> LexStream
lexStreamFromText = LexStream (Pos 0 1 1)

-- | Lazily decode a @ByteString@ into @Text@, and wrap it in a new @LexStream@
lexStreamFromByteString :: ByteString -> LexStream
lexStreamFromByteString = lexStreamFromText .
    Text.decodeUtf8With Text.lenientDecode

-- | Lazily pack a @String@ into @Text@, and wrap it in a new @LexStream@
lexStreamFromString :: String -> LexStream
lexStreamFromString = lexStreamFromText . Text.pack

-- | Read a lazy @ByteString@ from a @FilePath@, lazily decode it to @Text@,
--   and wrap it in a new @LexStream@
lexStreamFromFile :: (MonadError SyntaxError m, MonadIO m) => FilePath -> m LexStream
lexStreamFromFile fp =
    liftEither . fmap lexStreamFromByteString =<< liftIO do
        extraWorkerWhileBlocked do
            IO.catchAny (Right <$> ByteString.readFile fp) $
                pure . Left . SyntaxError Unrecoverable . Tag (fileAttr fp) .
                    SingleFailure . hang "could not read file due to" . shown


-- | Lex a full document
doc :: Has m '[Lex] => m TokenSeq
doc = reduceDocTokenSeq <$> do
    option () shebang
    lineSeq 0


-- | Lex a single token, including trees
token :: Has m '[Lex] => Word32 -> m Token
token ind = asum do
    treeToken ind :
        [ TVersion <$> version
        , TLiteral <$> literal
        ,    TPath <$> path
        ,  TSymbol <$> symbol
        ]

-- | Lex a tree as a token
treeToken :: Has m '[Lex] => Word32 -> m Token
treeToken ind = tree ind <&> uncurry TTree

-- | Lex a tree as a kind and a sequence
tree :: Has m '[Lex] => Word32 -> m (BlockKind, TokenSeq)
tree ind = asum
    [   (BkParen, ) <$> parenBlock ind
    ,   (BkBrace, ) <$> braceBlock ind
    , (BkBracket, ) <$> bracketBlock ind
    ,  (BkWhitespace, ) <$> indentBlock ind
    ]



-- | Lex a sequence of lines
lineSeq :: Has m '[Lex] => Word32 -> m TokenSeq
lineSeq ind = do
    fLn <- tag $ line ind
    lns <- many do
        lineEnd
        ind' <- lineStart
        guard (ind' == ind)
        tag $ line ind
    pure $ Seq.fromList (fLn : lns) <&>
        fmap (TTree BkWhitespace)

-- | Lex a single line
line :: Has m '[Lex] => Word32 -> m TokenSeq
line ind = Seq.fromList <$> many (hScanning $ tag $ token ind)


-- | Lex a tree delimited by an indentation block
indentBlock :: Has m '[Lex] => Word32 -> m TokenSeq
indentBlock ind = expecting "an indented block" do
    lineEnd
    ind' <- lineStart
    guard (ind' > ind)
    lineSeq ind'

-- | Lex a tree delimited by a parenthesis block
parenBlock :: Has m '[Lex] => Word32 -> m TokenSeq
parenBlock = expecting "parenthesis block" .
    delimited "(" ")"

-- | Lex a tree delimited by a brace block
braceBlock :: Has m '[Lex] => Word32 -> m TokenSeq
braceBlock = expecting "brace block" .
    delimited "{" "}"

-- | Lex a tree delimited by a bracket block
bracketBlock :: Has m '[Lex] => Word32 -> m TokenSeq
bracketBlock = expecting "bracket block" .
    delimited "[" "]"




-- | Lex a @Path@ sequence
path :: Has m '[Lex] => m Path
path = do
    b <- optional $ tag pathBase

    cs <- case b of
        Just base -> option Nil do
            if requiresSlash base
                then do
                    at <- attrOf $ connected base.tag $ expectSymbol "/"
                    connected at body
                else connected base.tag body
        _ -> body

    pure $ Path
        { base = b
        , components = cs
        }
    where
    body = do
        fc@(_ :@: at) <- tag pathName
        Seq.fromList . (fc :) <$> connectMany at do
            at' <- attrOf $ expectSymbol "/"
            connected at' (tag pathName)

-- | Lex a @PathBase@
pathBase :: Has m '[Lex] => m PathBase
pathBase = expecting "a path base" $ asum
    [ PbRoot <$ expectSymbol "~/"
    , PbThis <$ do
        expectSymbol "."
        expectSymbol "/"
    , PbModule <$> do
        expectSymbol "module"
        hScanning simpleName
    , PbFile <$> do
        expectSymbol "file"
        noFail (hScanning string)
    , PbUp <$> do
        fromIntegral . length <$> some do
            expectSymbol ".."
            expectSymbol "/"
    ]

-- | Lex a @PathName@
pathName :: Has m '[Lex] => m PathName
pathName = expecting "a path component" do
    liftA2 PathName
        do name
        do optional (hScanning category)



-- | Lex a @FixName@ or a @SimpleName@
name :: Has m '[Lex] => m FixName
name = expecting "a name" $ asum
    [ fixName
    , FixName . Seq.singleton . FixSimple <$> simpleName
    ]

-- | Lex a @FixName@
fixName :: Has m '[Lex] => m FixName
fixName = expecting "a fix name" do
    fn :@: at <- tag $ expect '`' >> noFail do
        components <- some do
            asum [ FixOperand <$ semSpace, FixSimple <$> simpleName ]
        expect '`'

        pure $ FixName (Seq.fromList components)

    case validateFixName fn of
        Just msg -> throwError $
            SyntaxError Unrecoverable $ SingleFailure (pPrint msg) :@: at
        _ -> pure fn

-- | Lex a @SimpleName@
simpleName :: Has m '[Lex] => m SimpleName
simpleName = expecting "a simple name" $ SimpleName <$> do
    n :@: at <- tag $ asum [ operator, identifier ]
    n <$ assertAt at (n `notElem` reservedSymbols) Recoverable
        ("symbol" <+> backticks (text n) <+> "is reserved")



-- | Lex a @Literal@
literal :: Has m '[Lex] => m Literal
literal = expecting "a literal" $ asum
    [ LFloat <$> float
    , LInt <$> int
    , LChar <$> char
    , LString <$> string
    ]

-- | Lex a semantic @Version@
version :: Has m '[Lex] => m Version
version = expecting "a version" do
    (major, minor, patch) :@: at <- tag $ liftA3 (,,)
        do decDigits
        do expect '.' >> decDigits
        do expect '.' >> decDigits

    let val = major <> "." <> minor <> "." <> patch

    maybeError (seqErr val "version" at) do
        parseVersion $ filter (/= '_') val

-- | Lex an operator, identifier, punctuation, or dot sequence
symbol :: Has m '[Lex] => m String
symbol = expecting "a symbol" $ asum
    [ operator
    , identifier
    , punctuation
    , dots
    ]




-- | Lex a tree delimited by a given pair of symbols
delimited :: Has m '[Lex] => String -> String -> Word32 -> m TokenSeq
delimited open close ind = do
    at <- attrOf $ expectSeq' open
    noFail do
        let lineInd = at.range.start.column
        ts <- asum
            [ indentBlock ind                               -- capture:
            , do                                            -- {
                st <- getParseState                         --   x
                sa <- optional $ flip recurseParser st do   -- capture
                    lineStart >>= \ind' ->                  -- { x
                        lineSeq (lineInd + ind')            --   y
                sb <- optional $ flip recurseParser st do   -- capture
                    lineSeq (lineInd - 1)                   -- { x
                                                            -- , y
                case (sa, sb) of
                    (Just (a, st'a), Just (b, st'b))
                        | Seq.length a > Seq.length b -> a <$ putParseState st'a
                        | Seq.length a < Seq.length b -> b <$ putParseState st'b
                        | otherwise -> line ind
                    (Just (a, st'a), _) -> a <$ putParseState st'a
                    (_, Just (b, st'b)) -> b <$ putParseState st'b
                    _ -> line ind
            ]
        asum [doClose at, vScanning $ doClose at]
        pure ts where
    doClose at = hScanning $ expecting
        do backticks (text close)
            <+> "to close" <+> backticks (text open)
            <+> "at" <+> pPrint at
        do expectSeq' close

-- | Expect a specific symbol
expectSymbol :: Has m '[Lex] => String -> m ()
expectSymbol s = expecting (backticks $ text s) do
    x <- symbol
    guard (x == s)

-- | Expect any one of a set of symbols
expectSymbols :: Has m '[Lex] => [String] -> m String
expectSymbols ss = asum $ ss <&> \s -> s <$ expectSymbol s


vScan :: Has m '[Lex] => m ()
vScan = option () $ nextWhile_ (`elem` ['\r','\n'])

vScanning :: Has m '[Lex] => m a -> m a
vScanning = (vScan *>)

hScan :: Has m '[Lex] => m ()
hScan = option () $ nextWhile_ (== ' ')

hScanning :: Has m '[Lex] => m a -> m a
hScanning = (hScan *>)


-- | Lex an indentation level (in spaces only)
lineStart :: Has m '[Lex] => m Word32
lineStart = expecting "an indentation level" do
    sum <$> many do
        nextMap \case
            ' ' -> Just 1
            _ -> Nothing

-- | Expect a line ending (either @\\n@ or @\\r\\n@)
lineEnd :: Has m '[Lex] => m ()
lineEnd = expecting "a line ending" $ hScanning do
    option () comment
    some_ $ expectAnySeq ["\n", "\r\n"]

-- | Expect a shebang (from @#!@ to the end of the line)
shebang :: Has m '[Lex] => m ()
shebang = expecting "a shebang" do
    expectSeq "#!"
    nextWhile_ (/= '\n')

-- | Expect a comment (from @;;@ to the end of the line)
comment :: Has m '[Lex] => m ()
comment = expecting "a comment" do
    expectSeq ";;"
    option () $ nextWhile_ (/= '\n')

-- | Lex a sequence of spaces with semantic implications,
--   ie the spaces inside a @FixName@
semSpace :: Has m '[Lex] => m ()
semSpace = expecting "a semantic space" do
    nextWhile_ (== ' ')

-- | Expect a binary digit
binDigit :: Has m '[Lex] => m Char
binDigit = expecting "binary digit" do
    nextIf (`elem` ['0','1'])

-- | Expect a decimal digit
decDigit :: Has m '[Lex] => m Char
decDigit = expecting "decimal digit" do
    nextIf Char.isDigit

-- | Expect a hexadecimal digit
hexDigit :: Has m '[Lex] => m Char
hexDigit = expecting "hexadecimal digit" do
    nextIf Char.isHexDigit

-- | Expect a sequence of binary digits,
--   allowing underscores after the first digit
binDigits :: Has m '[Lex] => m String
binDigits = expecting "binary digits" do
    liftA2 (:)
        do nextIf (`elem` ['0','1'])
        do option [] $ nextWhile (`elem` ['0','1','_'])

-- | Expect a sequence of decimal digits,
--   allowing underscores after the first digit
decDigits :: Has m '[Lex] => m String
decDigits = expecting "decimal digits" do
    liftA2 (:)
        do nextIf Char.isDigit
        do option [] $ nextWhile (Char.isDigit ||| (=='_'))

-- | Expect a sequence of hexadecimal digits,
--   allowing underscores after the first digit
hexDigits :: Has m '[Lex] => m String
hexDigits = expecting "hexadecimal digits" do
    liftA2 (:)
        do nextIf Char.isHexDigit
        do option [] $ nextWhile (Char.isHexDigit ||| (=='_'))

-- | Lex a binary integer literal
binInt :: Has m '[Lex] => m Word32
binInt = tag binDigits >>= \(digits :@: at) ->
    maybeError (seqErr digits "binary integer" at) do
        parseBinInt $ filter (/= '_') digits

-- | Lex a decimal integer literal
decInt :: Has m '[Lex] => m Word32
decInt = tag decDigits >>= \(digits :@: at) ->
    maybeError (seqErr digits "decimal integer" at) do
        parseDecInt $ filter (/= '_') digits

-- | Lex a hexadecimal integer literal
hexInt :: Has m '[Lex] => m Word32
hexInt =
    tag hexDigits >>= \(digits :@: at) ->
        maybeError (seqErr digits "hexadecimal integer" at) do
            parseHexInt $ filter (/= '_') digits

-- | Lex a character escape sequence,
--   inside a @\'@-delimited character literal
--   or a @\"@-delimited string literal
escape :: Has m '[Lex] => m Char
escape = expecting "a valid escape sequence" do
    s :@: at <- tag $ expect '\\' >> do
        ('\\' :) <$> asum
            [ pure <$> nextIf (`elem` simpleEsc)
            , hexEsc
            , uniEsc
            ]
    maybeError (seqErr s "character escape" at) do
        parseChar $ filter (/= '_') s
    where
    hexEsc = ('x':) <$> do
        expect 'x'
        noFail (replicateM 2 hexDigit)
    uniEsc = ("u{" <>) . (<> "}") <$> do
        expectSeq "u{"
        noFail do
            ds <- hexDigits
            ds <$ expect '}'
    simpleEsc = ['\\', '\'', '\"', '0', 'a', 'b', 'f', 'n', 'r', 't', 'v']

-- | Lex an integer literal
int :: Has m '[Lex] => m Word32
int = expecting "an integer literal" $ asum
    [ expectSeq "0x" >> hexInt
    , expectSeq "0b" >> binInt
    , decInt
    ]

-- | Lex a floating point literal
float :: Has m '[Lex] => m Float
float = expecting "a floating point literal" do
    (whole, frac, ex) :@: at <- tag $ liftA3 (,,)
        do decDigits
        do expect '.' >> decDigits
        do option "" do
            e <- expectAny "eE"
            sign <- option '+' (expectAny "+-")
            digits <- decDigits
            pure $ e : sign : digits

    let val = whole <> "." <> frac <> ex

    maybeError (seqErr val "float" at) do
        parseFloat $ filter (/= '_') val

-- | Lex a character literal
char :: Has m '[Lex] => m Char
char = expecting "a character literal" do
    expect '\''
    c <- asum
        [ escape
        , nextIf (`notElem` '\\' : mustEscapes)
        ]
    c <$ expect '\''

-- | Lex a string literal
string :: Has m '[Lex] => m String
string = expecting "a string literal" do
    expect '\"'
    s <- many do
        asum
            [ escape
            , nextIf (`notElem` '\"' : mustEscapes)
            ]
    s <$ noFail (expect '\"')

-- | Lex an @OverloadCategory@ (does not succeed on @OUnresolved@, use @option@)
category :: Has m '[Lex] => m OverloadCategory
category = asum
    [ ONamespace <$ expectSeq "namespace"
    , OInstance <$ expectSeq "instance"
    , OType <$ expectSeq "type"
    , OValue <$ expectSeq "value"
    ]


-- | Lex an atomic punctuation symbol like @,@
--  (not a block delimiter like @{@ or a dot sequence like @..@)
punctuation :: Has m '[Lex] => m String
punctuation = expecting "punctuation" do
    ch <- nextIf (`elem` userPunctuations)
    [ch] <$ when (ch == ';') do -- avoid consuming comment start
        negativeLookahead (nextIf_ (== ';'))

-- | Lex a sequence of dots
dots :: Has m '[Lex] => m String
dots = expecting "dot or ellipsis" do
    nextWhile (== '.')

-- | Lex an operator
operator :: Has m '[Lex] => m String
operator = expecting "an operator" do
    nextWhile isSymbolic

-- | Lex an identifier
identifier :: Has m '[Lex] => m String
identifier = expecting "an identifier" do
    liftA2 (:)
        do nextIf (Char.isAlpha ||| (=='_'))
        do option [] $ nextWhile (Char.isAlphaNum ||| (`elem` ['_', '\'']))
