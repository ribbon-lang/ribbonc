module Language.Ribbon.Parsing.Lexer where

import Data.ByteString.Lazy (ByteString)

import Data.Text.Lazy (Text)
import Data.Char qualified as Char
import Data.Sequence qualified as Seq
import Data.Maybe qualified as Maybe
import Data.Word (Word32)

import Data.Functor ((<&>))

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class

import Data.Tag

import Text.Pretty

import Language.Ribbon.Util

import Language.Ribbon.Lexical

import Language.Ribbon.Parsing.Monad.Lexer
import Language.Ribbon.Parsing.Error
import Language.Ribbon.Parsing.Text



-- | Lex a @String@, reporting errors in the given @FilePath@,
--   and convert errors into a @Doc@
lexString :: FilePath -> String -> Either Doc TokenSeq
lexString = lexStringWith doc

-- | Lex a @ByteString@, reporting errors in the given @FilePath@,
--   and convert errors into a @Doc@
lexByteString :: FilePath -> ByteString -> Either Doc TokenSeq
lexByteString = lexByteStringWith doc

-- | Lex a @Text@, reporting errors in the given @FilePath@,
--   and convert errors into a @Doc@
lexText :: FilePath -> Text -> Either Doc TokenSeq
lexText = lexTextWith doc

-- | Read a file from the given @FilePath@ and lex it,
--   converting errors into a @Doc@
lexFile :: FilePath -> IO (Either Doc TokenSeq)
lexFile = lexFileWith doc


-- | Lex a full document
doc :: Lexer TokenSeq
doc = reduceTokenSeq <$> do
    option () shebang
    lineSeq 0


-- | Lex a single token, including trees
token :: Int -> Lexer Token
token ind = asum do
    treeToken ind :
        [ TVersion <$> version
        , TLiteral <$> literal
        ,    TPath <$> path
        ,  TSymbol <$> symbol
        ]

-- | Lex a tree as a token
treeToken :: Int -> Lexer Token
treeToken ind = tree ind <&> uncurry TTree

-- | Lex a tree as a kind and a sequence
tree :: Int -> Lexer (BlockKind, TokenSeq)
tree ind = asum
    [      (BkParen, ) <$> parenBlock ind
    ,      (BkBrace, ) <$> braceBlock ind
    ,    (BkBracket, ) <$> bracketBlock ind
    , (BkWhitespace, ) <$> indentBlock ind
    ]



-- | Lex a sequence of lines
lineSeq :: Int -> Lexer TokenSeq
lineSeq ind = do
    fLn <- tag $ line ind
    lns <- many do
        lineEnd
        lineStart >>= guard . (== ind)
        tag $ line ind
    pure $ Seq.fromList $ filter (not . null . untag) (fLn : lns) <&>
        fmap (TTree BkWhitespace)

-- | Lex a single line
line :: Int -> Lexer TokenSeq
line ind = Seq.fromList <$> many (hScanning $ tag $ token ind)


-- | Lex a tree delimited by an indentation block
indentBlock :: Int -> Lexer TokenSeq
indentBlock ind = expecting "an indented block" do
    lineEnd
    ind' <- lineStart
    guard (ind' > ind)
    lineSeq ind'

-- | Lex a tree delimited by a parenthesis block
parenBlock :: Int -> Lexer TokenSeq
parenBlock = expecting "parenthesis block" .
    delimited "(" ")"

-- | Lex a tree delimited by a brace block
braceBlock :: Int -> Lexer TokenSeq
braceBlock = expecting "brace block" .
    delimited "{" "}"

-- | Lex a tree delimited by a bracket block
bracketBlock :: Int -> Lexer TokenSeq
bracketBlock = expecting "bracket block" .
    delimited "[" "]"




-- | Lex a @Path@ sequence
path :: Lexer Path
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
pathBase :: Lexer PathBase
pathBase = expecting "a path base" $ asum
    [ PbRoot <$ expectSymbol "/"
    , PbThis <$ expectSymbols [".", "/"]
    , PbModule <$> do expectSymbol "module"; hScanning simpleName
    , PbFile <$> do expectSymbol "file"; noFail (hScanning string)
    , PbUp <$> do length <$> some (expectSymbols ["..", "/"])
    ]

-- | Lex a @PathComponent@
pathComponent :: Lexer PathComponent
pathComponent = expecting "a path component" do
    c <- optional category

    when (Maybe.isJust c) hScan

    PathComponent c <$> name



-- | Lex a @FixName@ or a @SimpleName@
name :: Lexer FixName
name = expecting "a name" $ asum
    [ fixName
    , FixName . Seq.singleton . FixSimple <$> simpleName
    ]

-- | Lex a @FixName@
fixName :: Lexer FixName
fixName = expecting "a fix name" do
    pos <- getPos
    expect '`' >> noFail do
        components <- some do
            asum [ FixOperand <$ semSpace, FixSimple <$> simpleName ]
        expect '`'

        let fn = FixName (Seq.fromList components)

        at <- getAttr pos

        case validateFixName fn of
            Just msg -> throwError $
                ParseError Unrecoverable $ SingleFailure (pPrint msg) :@: at
            _ -> pure fn

-- | Lex a @SimpleName@
simpleName :: Lexer SimpleName
simpleName = expecting "a simple name" $ SimpleName <$> do
    pos <- getPos
    n <- asum [ operator, identifier ]
    at <- getAttr pos
    n <$ assertAt at (n `notElem` reservedSymbols) Recoverable do
        "symbol" <+> backticks (text n) <+> "is reserved"



-- | Lex a @Literal@
literal :: Lexer Literal
literal = expecting "a literal" $ asum
    [ LFloat <$> float
    , LInt <$> int
    , LChar <$> char
    , LString <$> string
    ]

-- | Lex a semantic @Version@
version :: Lexer Version
version = expecting "a version" do
    pos <- getPos
    major <- decDigits
    expect '.'
    minor <- decDigits
    expect '.'
    patch <- decDigits

    let val = major <> "." <> minor <> "." <> patch
    at <- getAttr pos

    maybeError (seqErr val "version" at) do
        parseVersion $ filter (/= '_') val

-- | Lex an operator, identifier, punctuation, or dot sequence
symbol :: Lexer String
symbol = expecting "a symbol" $ asum
    [ operator
    , identifier
    , punctuation
    , dots
    ]




-- | Lex a tree delimited by a given pair of symbols
delimited :: String -> String -> Int -> Lexer TokenSeq
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
expectSymbol :: String -> Lexer ()
expectSymbol s = expecting (backticks $ text s) do
    x <- symbol
    guard (x == s)

-- | Expect any one of a set of symbols
expectSymbols :: [String] -> Lexer String
expectSymbols ss = asum $ ss <&> \s -> s <$ expectSymbol s



-- | Lex an indentation level (in spaces or tabs, with tabs counting as 4)
--
--   TODO: should tab width be configurable?
--         should hard tabs be allowed?
--         if they are, should spaces count as indentation?
lineStart :: Lexer Int
lineStart = expecting "an indentation level" do
    sum <$> many do
        nextMap \case
            ' ' -> Just 1
            '\t' -> Just 4
            _ -> Nothing

-- | Expect a line ending (either @\\n@ or @\\r\\n@)
lineEnd :: Lexer ()
lineEnd = expecting "a line ending" $ hScanning do
    option () comment
    some_ $ expectAnySeq ["\n", "\r\n"]

-- | Expect a shebang (from @#!@ to the end of the line)
shebang :: Lexer ()
shebang = expecting "a shebang" do
    expectSeq "#!"
    nextWhile_ (/= '\n')

-- | Expect a comment (from @;;@ to the end of the line)
comment :: Lexer ()
comment = expecting "a comment" do
    expectSeq ";;"
    nextWhile_ (/= '\n')

-- | Lex a sequence of spaces with semantic implications,
--   ie the spaces inside a @FixName@
semSpace :: Lexer ()
semSpace = expecting "a semantic space" do
    nextWhile_ (== ' ')

-- | Expect a binary digit
binDigit :: Lexer Char
binDigit = expecting "binary digit" do
    nextIf (`elem` ['0','1'])

-- | Expect a decimal digit
decDigit :: Lexer Char
decDigit = expecting "decimal digit" do
    nextIf Char.isDigit

-- | Expect a hexadecimal digit
hexDigit :: Lexer Char
hexDigit = expecting "hexadecimal digit" do
    nextIf Char.isHexDigit

-- | Expect a sequence of binary digits,
--   allowing underscores after the first digit
binDigits :: Lexer String
binDigits = expecting "binary digits" do
    liftA2 (:)
        do nextIf (`elem` ['0','1'])
        do option [] $ nextWhile (`elem` ['0','1','_'])

-- | Expect a sequence of decimal digits,
--   allowing underscores after the first digit
decDigits :: Lexer String
decDigits = expecting "decimal digits" do
    liftA2 (:)
        do nextIf Char.isDigit
        do option [] $ nextWhile (Char.isDigit ||| (=='_'))

-- | Expect a sequence of hexadecimal digits,
--   allowing underscores after the first digit
hexDigits :: Lexer String
hexDigits = expecting "hexadecimal digits" do
    liftA2 (:)
        do nextIf Char.isHexDigit
        do option [] $ nextWhile (Char.isHexDigit ||| (=='_'))

-- | Lex a binary integer literal
binInt :: Lexer Word32
binInt = tag binDigits >>= \(digits :@: at) ->
    maybeError (seqErr digits "binary integer" at) do
        parseBinInt $ filter (/= '_') digits

-- | Lex a decimal integer literal
decInt :: Lexer Word32
decInt = tag decDigits >>= \(digits :@: at) ->
    maybeError (seqErr digits "decimal integer" at) do
        parseDecInt $ filter (/= '_') digits

-- | Lex a hexadecimal integer literal
hexInt :: Lexer Word32
hexInt =
    tag hexDigits >>= \(digits :@: at) ->
        maybeError (seqErr digits "hexadecimal integer" at) do
            parseHexInt $ filter (/= '_') digits

-- | Lex a character escape sequence,
--   inside a @\'@-delimited character literal
--   or a @\"@-delimited string literal
escape :: Lexer Char
escape = expecting "a valid escape sequence" do
    pos <- getPos
    expect '\\'
    s <- ('\\' :) <$> asum
        [ pure <$> nextIf (`elem` simpleEsc)
        , hexEsc
        , uniEsc
        ]
    at <- getAttr pos
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
int :: Lexer Word32
int = expecting "an integer literal" $ asum
    [ expectSeq "0x" >> hexInt
    , expectSeq "0b" >> binInt
    , decInt
    ]

-- | Lex a floating point literal
float :: Lexer Float
float = expecting "a floating point literal" do
    pos <- getPos
    whole <- decDigits
    expect '.'
    frac <- decDigits
    ex <- option "" do
        e <- expectAny "eE"
        sign <- option '+' (expectAny "+-")
        digits <- decDigits
        pure $ e : sign : digits

    let val = whole <> "." <> frac <> ex
    at <- getAttr pos

    maybeError (seqErr val "float" at) do
        parseFloat $ filter (/= '_') val

-- | Lex a character literal
char :: Lexer Char
char = expecting "a character literal" do
    expect '\''
    c <- asum
        [ escape
        , nextIf (`notElem` '\\' : mustEscapes)
        ]
    c <$ expect '\''

-- | Lex a string literal
string :: Lexer String
string = expecting "a string literal" do
    expect '\"'
    s <- many do
        asum
            [ escape
            , nextIf (`notElem` '\"' : mustEscapes)
            ]
    s <$ noFail (expect '\"')

-- | Lex an @OverloadCategory@ (does not succeed on @OUnresolved@, use @option@)
category :: Lexer OverloadCategory
category = asum
    [ ONamespace <$ expectSeq "namespace"
    , OInstance <$ expectSeq "instance"
    , OType <$ expectSeq "type"
    , OValue <$ expectSeq "value"
    ]


-- | Lex an atomic punctuation symbol like @,@
--  (not a block delimiter like @{@ or a dot sequence like @..@)
punctuation :: Lexer String
punctuation = expecting "punctuation" do
    ch <- nextIf (`elem` userPunctuations)
    [ch] <$ when (ch == ';') do -- avoid consuming comment start
        negativeLookahead (nextIf_ (== ';'))

-- | Lex a sequence of dots
dots :: Lexer String
dots = expecting "dot or ellipsis" do
    nextWhile (== '.')

-- | Lex an operator
operator :: Lexer String
operator = expecting "an operator" do
    nextWhile isSymbolic

-- | Lex an identifier
identifier :: Lexer String
identifier = expecting "an identifier" do
    liftA2 (:)
        do nextIf (Char.isAlpha ||| (=='_'))
        do option [] $ nextWhile (Char.isAlphaNum ||| (`elem` ['_', '\'']))
