module Language.Ribbon.Parsing.Lexer where

import Data.ByteString.Lazy (ByteString)

import Data.Text.Lazy (Text)

import Data.Tag
import Data.Attr

import Text.Pretty (Doc, (<+>), text, Pretty(..))
import Text.Pretty qualified as P

import Language.Ribbon.Util

import Language.Ribbon.Lexical

import Language.Ribbon.Parsing.Monad.Lexer
import Language.Ribbon.Parsing.Error
import Language.Ribbon.Parsing.Text
import Data.Word (Word32)
import Control.Applicative
import Data.Char qualified as Char
import Control.Monad
import Debug.Trace (traceM)
import Data.Function ((&))
import qualified Data.Sequence as Seq
import Control.Category (Category)
import qualified Data.Maybe as Maybe
import Control.Monad.Error.Class
import Data.Functor ((<&>))
import Data.Nil



 -- FIXME fill in todo when full-doc lexing is done
lexString :: FilePath -> String -> Either Doc TokenSeq
lexString = lexStringWith todo

lexByteString :: FilePath -> ByteString -> Either Doc TokenSeq
lexByteString = lexByteStringWith todo

lexText :: FilePath -> Text -> Either Doc TokenSeq
lexText = lexTextWith todo

lexFile :: FilePath -> IO (Either Doc TokenSeq)
lexFile = lexFileWith todo


binDigit :: Lexer Char
binDigit = expecting "binary digit" do
    nextIf (`elem` ['0','1'])

decDigit :: Lexer Char
decDigit = expecting "decimal digit" do
    nextIf Char.isDigit

hexDigit :: Lexer Char
hexDigit = expecting "hexadecimal digit" do
    nextIf Char.isHexDigit

binDigits :: Lexer String
binDigits = expecting "binary digits" do
    liftA2 (:)
        do nextIf (`elem` ['0','1'])
        do option [] $ nextWhile (`elem` ['0','1','_'])

decDigits :: Lexer String
decDigits = expecting "decimal digits" do
    liftA2 (:)
        do nextIf Char.isDigit
        do option [] $ nextWhile (Char.isDigit ||| (=='_'))

hexDigits :: Lexer String
hexDigits = expecting "hexadecimal digits" do
    liftA2 (:)
        do nextIf Char.isHexDigit
        do option [] $ nextWhile (Char.isHexDigit ||| (=='_'))

seqErr :: String -> String -> Attr -> ParseError a
seqErr sub kind at = ParseError Unrecoverable $ Tag at $ SingleFailure $
    "cannot parse" <+> P.backticks (text sub)
        <+> "as a" <+> text kind


binInt :: Lexer Word32
binInt = tag binDigits >>= \(digits :@: at) ->
    maybeError (seqErr digits "binary integer" at) do
        parseBinInt $ filter (/= '_') digits

decInt :: Lexer Word32
decInt = tag decDigits >>= \(digits :@: at) ->
    maybeError (seqErr digits "decimal integer" at) do
        parseDecInt $ filter (/= '_') digits

hexInt :: Lexer Word32
hexInt =
    tag hexDigits >>= \(digits :@: at) ->
        maybeError (seqErr digits "hexadecimal integer" at) do
            parseHexInt $ filter (/= '_') digits


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


int :: Lexer Word32
int = expecting "an integer literal" $ asum
    [ expectSeq "0x" >> hexInt
    , expectSeq "0b" >> binInt
    , decInt
    ]

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

char :: Lexer Char
char = expecting "a character literal" do
    expect '\''
    c <- asum
        [ escape
        , nextIf (`notElem` '\\' : mustEscapes)
        ]
    c <$ expect '\''

string :: Lexer String
string = expecting "a string literal" do
    expect '\"'
    s <- many do
        asum
            [ escape
            , nextIf (`notElem` '\"' : mustEscapes)
            ]
    s <$ noFail (expect '\"')


literal :: Lexer Literal
literal = expecting "a literal" $ asum
    [ LFloat <$> float
    , LInt <$> int
    , LChar <$> char
    , LString <$> string
    ]


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



punctuation :: Lexer String
punctuation = expecting "punctuation" do
    pure <$> nextIf (`elem` userPunctuations)

dots :: Lexer String
dots = expecting "dot or ellipsis" do
    nextWhile (== '.')

operator :: Lexer String
operator = expecting "an operator" do
    nextWhile isSymbolic

identifier :: Lexer String
identifier = expecting "an identifier" do
    liftA2 (:)
        do nextIf (Char.isAlpha ||| (=='_'))
        do option [] $ nextWhile (Char.isAlphaNum ||| (`elem` ['_', '\'']))


symbol :: Lexer String
symbol = expecting "a symbol" $ asum
    [ operator
    , identifier
    , punctuation
    , dots
    ]

expectSymbol :: String -> Lexer ()
expectSymbol s = expecting (P.backticks $ text s) do
    x <- symbol
    guard (x == s)

expectSymbols :: [String] -> Lexer ()
expectSymbols = mapM_ expectSymbol

semSpace :: Lexer ()
semSpace = expecting "a semantic space" do
    nextWhile_ (== ' ')

simpleName :: Lexer SimpleName
simpleName = expecting "a simple name" $ SimpleName <$> do
    pos <- getPos
    n <- asum [ operator, identifier ]
    at <- getAttr pos
    n <$ assertAt at (n `notElem` reservedSymbols) do
        "symbol" <+> P.backticks (text n) <+> "is reserved"

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


name :: Lexer FixName
name = expecting "a name" $ asum
    [ fixName
    , FixName . Seq.singleton . FixSimple <$> simpleName
    ]


category :: Lexer OverloadCategory
category = asum
    [ ONamespace <$ expectSeq "namespace"
    , OInstance <$ expectSeq "instance"
    , OType <$ expectSeq "type"
    , OValue <$ expectSeq "value"
    ]

pathComponent :: Lexer PathComponent
pathComponent = expecting "a path component" do
    c <- optional category

    when (Maybe.isJust c) hScan

    PathComponent (Maybe.fromMaybe OUnresolved c) <$> name

pathBase :: Lexer PathBase
pathBase = expecting "a path base" $ asum
    [ PbRoot <$ expectSymbol "/"
    , PbThis <$ expectSymbols [".", "/"]
    , PbModule <$> do expectSymbol "module"; noFail (hScanning simpleName)
    , PbFile <$> do expectSymbol "file"; noFail (hScanning string)
    , PbUp <$> do length <$> some (expectSymbols ["..", "/"])
    ]


path :: Lexer Path
path = do
    b <- optional $ tag pathBase

    c <- case b of
        Just base | pathBaseRequiresSlash base.value -> do
            at <- attrOf $ connected base.tag $ expectSymbol "/"
            tag (connected at pathComponent)
        _ -> tag pathComponent

    cs <- Seq.fromList <$> connectMany c.tag do
        at' <- attrOf $ expectSymbol "/"
        connected at' (tag pathComponent)

    pure $ Path
        { base = Maybe.fromMaybe (PbThis :@: attrFlattenToStart c.tag) b
        , components = c Seq.:<| cs
        }


indent :: Lexer Int
indent = expecting "an indentation level" do
    sum <$> many do
        nextMap \case
            ' ' -> Just 1
            '\t' -> Just 4
            _ -> Nothing

delimited :: String -> String -> Int -> Lexer TokenSeq
delimited open close ind = do
    at <- tag $ expectSeq' open
    traceM $ "begin " <> open <> close
    noFail do
        ts <- lineSeq ind
        hScanning $ expecting
            do P.backticks (text close)
                <+> "to close" <+> P.backticks (text open)
                <+> "at" <+> pPrint at
            do expectSeq' close
        pure ts

parenBlock :: Int -> Lexer TokenSeq
parenBlock = expecting "parenthesis block" .
    delimited "(" ")"

braces :: Int -> Lexer TokenSeq
braces = expecting "brace block" .
    delimited "{" "}"

brackets :: Int -> Lexer TokenSeq
brackets = expecting "bracket block" .
    delimited "[" "]"

lineEnding :: Lexer ()
lineEnding = expecting "a line ending" do
    some_ $ expectAnySeq ["\n", "\r\n"]

indentBlock :: Int -> Lexer TokenSeq
indentBlock ind = expecting "an indented block" do
    lns <- some do
        lineEnding
        ind' <- indent
        guard (ind' > ind)
        tag $ line ind'

    pure $ Seq.fromList $ filter (not . null . untag) lns <&>
        fmap (TTree . TtBlock BkLine)

line :: Int -> Lexer TokenSeq
line ind = Seq.fromList <$> many (hScanning $ tag $ token ind)

lineSeq :: Int -> Lexer TokenSeq
lineSeq ind = do
    fLn <- tag $ line ind
    lns <- many do
        lineEnding
        ind' <- indent
        guard (ind' == ind)
        tag $ line ind
    pure $ Seq.fromList $ filter (not . null . untag) (fLn : lns) <&>
        fmap (TTree . TtBlock BkLine)

block :: Int -> Lexer (BlockKind, TokenSeq)
block ind = asum
    [ (BkParen, ) <$> parenBlock ind
    , (BkBrace, ) <$> braces ind
    , (BkBracket, ) <$> brackets ind
    , (BkIndent, ) <$> indentBlock ind
    ]

token :: Int -> Lexer Token
token ind = asum
    [ TVersion <$> version
    , TLiteral <$> literal
    , TTree . TtPath <$> path
    , TSymbol <$> symbol
    , uncurry (TTree .: TtBlock) <$> block ind
    ]
