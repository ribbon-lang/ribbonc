module Ribbon.Syntax.Lexer where

import Ribbon.Util
import Ribbon.Syntax.Literal
import Ribbon.Syntax.Text
import Ribbon.Syntax.Token
import Ribbon.Syntax.ParserM
import Ribbon.Source

import Data.Sequence qualified as Seq
import Data.Char qualified as Char
import Control.Applicative
import Control.Monad.Except (MonadError(..))
import Data.Functor ((<&>))
import Control.Monad


-- | Perform lexical analysis on a file
lexFile :: File -> Either String [Token]
lexFile = evalLexer tokens

-- | Perform lexical analysis on a string,
--   with errors reported to be in a file with the given name
lexString :: String -> String -> Either String [Token]
lexString name text = lexFile (newFile name text)


-- | Type alias for ParserM Char
type Lexer = ParserM Char

-- Evaluate a Lexer on a given File, converting ParseErrors to Strings
evalLexer :: Lexer a -> File -> Either String a
evalLexer lx file =
    let input = Seq.fromList $ uncurry (:@:) <$>
            zip (fileText file) ((`unitAttr` file) <$> [0..])
    in case runParser (noFail $ consumesAll lx) (ParseStream input file) 0 of
        Left (ParseError (msg :@: x)) ->
            Left $ "lexical error " <> show x <> ": " <> msg
        Left _ -> undefined
        Right (a, _) -> Right a


-- | Expect a sequence of whitespace characters and return them
space :: Lexer String
space = expecting "whitespace" do
    nextWhile isWhitespace

-- | Expect a sequence of whitespace characters and discard them
space_ :: Lexer ()
space_ = expecting "whitespace" do
    nextWhile_ isWhitespace

-- | Expect a line break
lineBreak :: Lexer ()
lineBreak = expecting "line break" do
    nextIf_ (=='\n')
    many_ do nextIf_ Char.isSpace

-- | Discard any whitespace before evaluating the given Lexer
scan :: Show a => Lexer a -> Lexer a
scan lx = optional space_ >> lx


-- | Expect a single binary digit (0, 1)
binDigit :: Lexer Char
binDigit = expecting "binary digit" do
    nextIf (`elem` ['0', '1'])

-- | Expect a single decimal digit (0-9)
decDigit :: Lexer Char
decDigit = expecting "decimal digit" do
    nextIf Char.isDigit

-- | Expect a single hexadecimal digit (0-9, a-f, A-F)
hexDigit :: Lexer Char
hexDigit = expecting "hex digit" do
    nextIf Char.isHexDigit


-- | Expect a sequence of binary digits (0, 1)
binDigits :: Lexer String
binDigits = expecting "binary digits" do
    nextWhile (`elem` ['0', '1'])

-- | Expect a sequence of decimal digits (0-9)
decDigits :: Lexer String
decDigits = expecting "decimal digits" do
    nextWhile Char.isDigit

-- | Expect a sequence of hexadecimal digits (0-9, a-f, A-F)
hexDigits :: Lexer String
hexDigits = expecting "hex digits" do
    nextWhile Char.isHexDigit


-- | Expect a binary integer to be parsed from a string of binary digits.
--   Note that this does not look for a `0b` prefix
binInt :: Lexer Int
binInt = binDigits >>= \digits ->
    maybe (throwError $ "invalid int " <> digits) pure (parseBinInt digits)

-- | Expect a decimal integer to be parsed from a string of decimal digits.
decInt :: Lexer Int
decInt = decDigits >>= \digits ->
    maybe (throwError $ "invalid int " <> digits) pure (parseDecInt digits)

-- | Expect a hexadecimal integer to be parsed
--   from a string of hexadecimal digits.
--   Note that this does not look for a `0x` prefix
hexInt :: Lexer Int
hexInt = hexDigits >>= \digits -> maybe
    (throwError $ "invalid int " <> digits) pure (parseHexInt digits)


-- | Expect a single character or a character escape.
--   The provided character is not accepted unless escaped
escapable :: Char -> Lexer Char
escapable s = asum
    [ expecting "an escape sequence" (nextIf (=='\\')) >> noFail escaped
    , expecting "a character" do nextIf (/= s)] where

    escaped = expecting "a valid escape sequence to follow \\" do
        asum [single, ascii, unicode]

    single = nextMap \case
        '0' -> pure '\0'
        'a' -> pure '\a'
        'b' -> pure '\b'
        'f' -> pure '\f'
        'n' -> pure '\n'
        'r' -> pure '\r'
        't' -> pure '\t'
        'v' -> pure '\v'
        '\\' -> pure '\\'
        '\'' -> pure '\''
        '\"' -> pure '\"'
        _ -> empty

    ascii = do
        expect 'x'
        code <- replicateM 2 hexDigit
        maybe empty (pure . toEnum) (parseHexInt code)

    unicode = do
        expectSeq "u{"
        code <- hexInt
        expect '}'
        if code <= 0x10FFFF
            then pure $ toEnum code
            else throwError "invalid unicode code point"

-- | Expect an integer literal,
--   with support for binary, decimal and hexadecimal.
--   Binary and hexadecimal literals must be prefixed
--   with `0b` and `0x` respectively
int :: Lexer Int
int = asum
    [ expectSeq "0b" >> binInt
    , expectSeq "0x" >> hexInt
    , decInt
    ]

-- | Expect a floating point literal.
--   The decimal point and both fractional and whole parts are required.
--   The exponent is optional, and may be prefixed with `e` or `E`,
--   and can optionally include a sign
float :: Lexer Float
float = do
    whole <- decDigits
    expecting (fail ". to separate whole and fractional parts of float") (expect '.')
    frac <- decDigits
    expo <- optional do
        _ <- expectAny "eE"
        sign <- optional (expectAny "+-")
        (sign, ) <$> noFail do
            expecting "decimal digits to follow exponent marker" decDigits
    let assembled = whole <> "." <> frac <> maybe "" (\(sign, ex) -> "e" <> maybe "+" pure sign <> ex) expo
    maybe (throwError $ "invalid float " <> assembled) pure (parseFloat assembled)

-- | Expect a character literal
char :: Lexer Char
char = do
    _ <- expect '\''
    noFail do
        ch <- escapable '\''
        _ <- expecting "\' to close char literal" (nextIf (=='\''))
        pure ch

-- | Expect a string literal
string :: Lexer String
string = do
    _ <- expect '\"'
    noFail do
        str <- many (escapable '\"')
        _ <- expecting "\" to close string literal" (nextIf (=='\"'))
        pure str

-- | Expect a source-literal value
literal :: Lexer Literal
literal = asum
    [ LChar <$> char
    , LFloat <$> float
    , LInt <$> int
    , LString <$> string
    ]


-- | Expect any identifier, including reserved identifiers
identifier :: Lexer String
identifier = expecting "an identifier" do
    f <- nextIf isIdentifierStart
    r <- many $ nextIf isIdentifierSubsequent
    pure (f : r)

-- | Expect any operator, including reserved operators
operator :: Lexer String
operator = expecting "an operator" do
    f <- nextIf isOperatorStart
    r <- many $ nextIf isOperatorSubsequent
    pure (f : r)

-- | Expect syntactic punctuation such as @,@, @;@ etc
punctuation :: Lexer String
punctuation = expecting "punctuation" do
    (:[]) <$> nextIf isPunctuation

-- | Expect a sequence of dot characters such as @.@, @..@, @...@
dotSequence :: Lexer String
dotSequence = expecting "dot sequence" do
    nextWhile (=='.')

-- | Expect a symbol, which is
--   either an identifier, punctuation, or a dot sequence
symbol :: Lexer String
symbol = asum
    [ identifier
    , operator
    , punctuation
    , dotSequence
    ]


-- | Expect any Token, without wrapping it in a Syn
tokenData :: Lexer TokenData
tokenData = asum
    [ TLiteral <$> literal
    , asum [identifier, operator] <&> \i ->
        TSymbol (select (isReservedIdentifier i) TsKeyword TsIdentifier) i
    , TSymbol TsKeyword <$> dotSequence
    , TSymbol TsPunctuation <$> punctuation
    ]

-- | Consume any leading whitespace, then expect a Token, and wrap it in a Syn
token :: Lexer Token
token = scan (syn tokenData)

-- | Expect a sequence of @token@-lexables, and consume any trailing whitespace
tokens :: Lexer [Token]
tokens = some token << optional space_
