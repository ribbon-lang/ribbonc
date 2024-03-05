module Language.Ribbon.Parsing.Lexer where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text

import Data.Char qualified as Char
import Data.Sequence qualified as Seq
import Data.Maybe qualified as Maybe
import Data.Word (Word32)

import Data.Functor ((<&>))

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Parser.Class

import Data.Tag
import Data.Pos
import Data.Range
import Data.Attr
import Data.SyntaxError
import Data.Nil

import Text.Pretty

import Language.Ribbon.Util

import Language.Ribbon.Lexical

import Language.Ribbon.Parsing.Text





data LexStream
    = LexStream
    { pos :: !Pos
    , input :: Text
    }
    deriving Show

instance Nil LexStream where
    nil = LexStream Nil mempty
    isNil (LexStream _ i) = Text.null i

instance ParseInput LexStream where
    type InputElement LexStream = Char
    formatInput fp ls = case unconsInput ls of
        Left e -> formatProblem (attrInput fp ls) e
        Right (c, ls') -> UnexpectedFailure
            (inputIdentity c <+> inputPretty c) :@: attrInputDiff fp ls ls'
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
lexStreamFromText = LexStream Nil

-- | Lazily decode a @ByteString@ into @Text@, and wrap it in a new @LexStream@
lexStreamFromByteString :: ByteString -> LexStream
lexStreamFromByteString = lexStreamFromText .
    Text.decodeUtf8With Text.lenientDecode

-- | Lazily pack a @String@ into @Text@, and wrap it in a new @LexStream@
lexStreamFromString :: String -> LexStream
lexStreamFromString = lexStreamFromText . Text.pack

-- | Read a lazy @ByteString@ from a @FilePath@, lazily decode it to @Text@,
--   and wrap it in a new @LexStream@
lexStreamFromFile :: FilePath -> IO LexStream
lexStreamFromFile fp =
    lexStreamFromByteString <$> ByteString.readFile fp


-- | Lex a full document
doc :: MonadParse LexStream m => m TokenSeq
doc = reduceTokenSeq <$> do
    option () shebang
    lineSeq 0


-- | Lex a single token, including trees
token :: MonadParse LexStream m => Int -> m Token
token ind = asum do
    treeToken ind :
        [ TVersion <$> version
        , TLiteral <$> literal
        ,    TPath <$> path
        ,  TSymbol <$> symbol
        ]

-- | Lex a tree as a token
treeToken :: MonadParse LexStream m => Int -> m Token
treeToken ind = tree ind <&> uncurry TTree

-- | Lex a tree as a kind and a sequence
tree :: MonadParse LexStream m => Int -> m (BlockKind, TokenSeq)
tree ind = asum
    [      (BkParen, ) <$> parenBlock ind
    ,      (BkBrace, ) <$> braceBlock ind
    ,    (BkBracket, ) <$> bracketBlock ind
    , (BkWhitespace, ) <$> indentBlock ind
    ]



-- | Lex a sequence of lines
lineSeq :: MonadParse LexStream m => Int -> m TokenSeq
lineSeq ind = do
    fLn <- tag $ line ind
    lns <- many do
        lineEnd
        lineStart >>= guard . (== ind)
        tag $ line ind
    pure $ Seq.fromList $ filter (not . null . untag) (fLn : lns) <&>
        fmap (TTree BkWhitespace)

-- | Lex a single line
line :: MonadParse LexStream m => Int -> m TokenSeq
line ind = Seq.fromList <$> many (hScanning $ tag $ token ind)


-- | Lex a tree delimited by an indentation block
indentBlock :: MonadParse LexStream m => Int -> m TokenSeq
indentBlock ind = expecting "an indented block" do
    lineEnd
    ind' <- lineStart
    guard (ind' > ind)
    lineSeq ind'

-- | Lex a tree delimited by a parenthesis block
parenBlock :: MonadParse LexStream m => Int -> m TokenSeq
parenBlock = expecting "parenthesis block" .
    delimited "(" ")"

-- | Lex a tree delimited by a brace block
braceBlock :: MonadParse LexStream m => Int -> m TokenSeq
braceBlock = expecting "brace block" .
    delimited "{" "}"

-- | Lex a tree delimited by a bracket block
bracketBlock :: MonadParse LexStream m => Int -> m TokenSeq
bracketBlock = expecting "bracket block" .
    delimited "[" "]"




-- | Lex a @Path@ sequence
path :: MonadParse LexStream m => m Path
path = do
    b <- optional $ tag pathBase

    c <- case b of
        Just base | requiresSlash base -> do
            at <- attrOf $ connected base.tag $ expectSymbol "/"
            tag (connected at pathComponent)
        _ -> tag pathComponent

    cs <- Seq.fromList <$> connectMany c.tag do
        at' <- attrOf $ expectSymbol "/"
        connected at' (tag pathComponent)

    pure $ Path
        { base = b
        , components = c Seq.:<| cs
        }

-- | Lex a @PathBase@
pathBase :: MonadParse LexStream m => m PathBase
pathBase = expecting "a path base" $ asum
    [ PbRoot <$ expectSymbol "/"
    , PbThis <$ expectSymbols [".", "/"]
    , PbModule <$> do expectSymbol "module"; hScanning simpleName
    , PbFile <$> do expectSymbol "file"; noFail (hScanning string)
    , PbUp <$> do length <$> some (expectSymbols ["..", "/"])
    ]

-- | Lex a @PathComponent@
pathComponent :: MonadParse LexStream m => m PathComponent
pathComponent = expecting "a path component" do
    c <- optional category

    when (Maybe.isJust c) hScan

    PathComponent c <$> name



-- | Lex a @FixName@ or a @SimpleName@
name :: MonadParse LexStream m => m FixName
name = expecting "a name" $ asum
    [ fixName
    , FixName . Seq.singleton . FixSimple <$> simpleName
    ]

-- | Lex a @FixName@
fixName :: MonadParse LexStream m => m FixName
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
simpleName :: MonadParse LexStream m => m SimpleName
simpleName = expecting "a simple name" $ SimpleName <$> do
    n :@: at <- tag $ asum [ operator, identifier ]
    n <$ assertAt at (n `notElem` reservedSymbols) Recoverable
        ("symbol" <+> backticks (text n) <+> "is reserved")



-- | Lex a @Literal@
literal :: MonadParse LexStream m => m Literal
literal = expecting "a literal" $ asum
    [ LFloat <$> float
    , LInt <$> int
    , LChar <$> char
    , LString <$> string
    ]

-- | Lex a semantic @Version@
version :: MonadParse LexStream m => m Version
version = expecting "a version" do
    (major, minor, patch) :@: at <- tag $ liftA3 (,,)
        do decDigits
        do expect '.' >> decDigits
        do expect '.' >> decDigits

    let val = major <> "." <> minor <> "." <> patch

    maybeError (seqErr val "version" at) do
        parseVersion $ filter (/= '_') val

-- | Lex an operator, identifier, punctuation, or dot sequence
symbol :: MonadParse LexStream m => m String
symbol = expecting "a symbol" $ asum
    [ operator
    , identifier
    , punctuation
    , dots
    ]




-- | Lex a tree delimited by a given pair of symbols
delimited :: MonadParse LexStream m => String -> String -> Int -> m TokenSeq
delimited open close ind = do
    at <- attrOf $ expectSeq' open
    noFail do
        ts <- lineSeq ind
        hScanning $ expecting
            do backticks (text close)
                <+> "to close" <+> backticks (text open)
                <+> "at" <+> pPrint at
            do expectSeq' close
        pure ts

-- | Expect a specific symbol
expectSymbol :: MonadParse LexStream m => String -> m ()
expectSymbol s = expecting (backticks $ text s) do
    x <- symbol
    guard (x == s)

-- | Expect any one of a set of symbols
expectSymbols :: MonadParse LexStream m => [String] -> m String
expectSymbols ss = asum $ ss <&> \s -> s <$ expectSymbol s



hScan :: MonadParse LexStream m => m ()
hScan = option () $ nextWhile_ (== ' ')

hScanning :: MonadParse LexStream m => m a -> m a
hScanning = (hScan *>)


-- | Lex an indentation level (in spaces or tabs, with tabs counting as 4)
--
--   TODO: should tab width be configurable?
--         should hard tabs be allowed?
--         if they are, should spaces count as indentation?
lineStart :: MonadParse LexStream m => m Int
lineStart = expecting "an indentation level" do
    sum <$> many do
        nextMap \case
            ' ' -> Just 1
            '\t' -> Just 4
            _ -> Nothing

-- | Expect a line ending (either @\\n@ or @\\r\\n@)
lineEnd :: MonadParse LexStream m => m ()
lineEnd = expecting "a line ending" $ hScanning do
    option () comment
    some_ $ expectAnySeq ["\n", "\r\n"]

-- | Expect a shebang (from @#!@ to the end of the line)
shebang :: MonadParse LexStream m => m ()
shebang = expecting "a shebang" do
    expectSeq "#!"
    nextWhile_ (/= '\n')

-- | Expect a comment (from @;;@ to the end of the line)
comment :: MonadParse LexStream m => m ()
comment = expecting "a comment" do
    expectSeq ";;"
    nextWhile_ (/= '\n')

-- | Lex a sequence of spaces with semantic implications,
--   ie the spaces inside a @FixName@
semSpace :: MonadParse LexStream m => m ()
semSpace = expecting "a semantic space" do
    nextWhile_ (== ' ')

-- | Expect a binary digit
binDigit :: MonadParse LexStream m => m Char
binDigit = expecting "binary digit" do
    nextIf (`elem` ['0','1'])

-- | Expect a decimal digit
decDigit :: MonadParse LexStream m => m Char
decDigit = expecting "decimal digit" do
    nextIf Char.isDigit

-- | Expect a hexadecimal digit
hexDigit :: MonadParse LexStream m => m Char
hexDigit = expecting "hexadecimal digit" do
    nextIf Char.isHexDigit

-- | Expect a sequence of binary digits,
--   allowing underscores after the first digit
binDigits :: MonadParse LexStream m => m String
binDigits = expecting "binary digits" do
    liftA2 (:)
        do nextIf (`elem` ['0','1'])
        do option [] $ nextWhile (`elem` ['0','1','_'])

-- | Expect a sequence of decimal digits,
--   allowing underscores after the first digit
decDigits :: MonadParse LexStream m => m String
decDigits = expecting "decimal digits" do
    liftA2 (:)
        do nextIf Char.isDigit
        do option [] $ nextWhile (Char.isDigit ||| (=='_'))

-- | Expect a sequence of hexadecimal digits,
--   allowing underscores after the first digit
hexDigits :: MonadParse LexStream m => m String
hexDigits = expecting "hexadecimal digits" do
    liftA2 (:)
        do nextIf Char.isHexDigit
        do option [] $ nextWhile (Char.isHexDigit ||| (=='_'))

-- | Lex a binary integer literal
binInt :: MonadParse LexStream m => m Word32
binInt = tag binDigits >>= \(digits :@: at) ->
    maybeError (seqErr digits "binary integer" at) do
        parseBinInt $ filter (/= '_') digits

-- | Lex a decimal integer literal
decInt :: MonadParse LexStream m => m Word32
decInt = tag decDigits >>= \(digits :@: at) ->
    maybeError (seqErr digits "decimal integer" at) do
        parseDecInt $ filter (/= '_') digits

-- | Lex a hexadecimal integer literal
hexInt :: MonadParse LexStream m => m Word32
hexInt =
    tag hexDigits >>= \(digits :@: at) ->
        maybeError (seqErr digits "hexadecimal integer" at) do
            parseHexInt $ filter (/= '_') digits

-- | Lex a character escape sequence,
--   inside a @\'@-delimited character literal
--   or a @\"@-delimited string literal
escape :: MonadParse LexStream m => m Char
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
int :: MonadParse LexStream m => m Word32
int = expecting "an integer literal" $ asum
    [ expectSeq "0x" >> hexInt
    , expectSeq "0b" >> binInt
    , decInt
    ]

-- | Lex a floating point literal
float :: MonadParse LexStream m => m Float
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
char :: MonadParse LexStream m => m Char
char = expecting "a character literal" do
    expect '\''
    c <- asum
        [ escape
        , nextIf (`notElem` '\\' : mustEscapes)
        ]
    c <$ expect '\''

-- | Lex a string literal
string :: MonadParse LexStream m => m String
string = expecting "a string literal" do
    expect '\"'
    s <- many do
        asum
            [ escape
            , nextIf (`notElem` '\"' : mustEscapes)
            ]
    s <$ noFail (expect '\"')

-- | Lex an @OverloadCategory@ (does not succeed on @OUnresolved@, use @option@)
category :: MonadParse LexStream m => m OverloadCategory
category = asum
    [ ONamespace <$ expectSeq "namespace"
    , OInstance <$ expectSeq "instance"
    , OType <$ expectSeq "type"
    , OValue <$ expectSeq "value"
    ]


-- | Lex an atomic punctuation symbol like @,@
--  (not a block delimiter like @{@ or a dot sequence like @..@)
punctuation :: MonadParse LexStream m => m String
punctuation = expecting "punctuation" do
    ch <- nextIf (`elem` userPunctuations)
    [ch] <$ when (ch == ';') do -- avoid consuming comment start
        negativeLookahead (nextIf_ (== ';'))

-- | Lex a sequence of dots
dots :: MonadParse LexStream m => m String
dots = expecting "dot or ellipsis" do
    nextWhile (== '.')

-- | Lex an operator
operator :: MonadParse LexStream m => m String
operator = expecting "an operator" do
    nextWhile isSymbolic

-- | Lex an identifier
identifier :: MonadParse LexStream m => m String
identifier = expecting "an identifier" do
    liftA2 (:)
        do nextIf (Char.isAlpha ||| (=='_'))
        do option [] $ nextWhile (Char.isAlphaNum ||| (`elem` ['_', '\'']))
