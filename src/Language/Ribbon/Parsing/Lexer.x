{
module Language.Ribbon.Parsing.Lexer
    ( lexString
    , lexByteString
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.Nil
import Data.Tag
import Data.Attr

import Text.Pretty

import Language.Ribbon.Util
import Language.Ribbon.Syntax.Literal
import Language.Ribbon.Syntax.Token

import Language.Ribbon.Parsing.Monad.Lexer
import Language.Ribbon.Parsing.Text
}


%encoding "utf8"

$bin = [01]
$dec = [0-9]
$hex = [0-9A-Fa-f]
$alpha = [A-Za-z_]
$alphanum = [A-Za-z0-9_]
$punc = [\'\,\{\}\(\)\[\]]
$backtick = [\`]
$hWhite = $white # [\n]

@reserved = let | in | as | match | with | do | fun | handler
          | module | import | use | file | pub | namespace
          | type | effect | class | instance
          | infix | infixl | infixr | prefix | postfix
          | "=>" | ";" | "./" | "../" | ".." | "."
@operator = ($printable # $white # $alphanum # $punc # $backtick # [\.\"])+
@escape = \\ ([\\\'\"0nrt] | x $hex{2} | u \{ $hex+ \})

tokens :-
    ";;" .* \n;

    <0> $hWhite* { beginLine }
    <line> \n    { endLine }
    <line> $hWhite+;

    <line> \' (@escape | $printable # [\\\'] | $white) \' { char }
    <line> $dec+ \. $dec+ \. $dec+                        { version }
    <line> $dec+ \. $dec+ (e [\+\-]? $dec+)?              { float }
    <line> $dec+                                          { decInt }
    <line> 0x $hex+                                       { hexInt }
    <line> 0b $bin+                                       { binInt }

    <line>   \"                                     { beginString }
    <string> @escape | $printable # [\\\"] | $white { accumString }
    <string> \"                                     { endString }

    <line> $backtick ($alpha $alphanum* | @operator) $backtick { escSymbol }

    <line> @reserved         { symbol }
    <line> @operator         { symbol }
    <line> $alpha $alphanum* { symbol }
    <line> $punc             { symbol }


{
beginLine :: AlexAction (ATag Token)
beginLine _ _ = do
    setStartCode line
    next

endLine :: AlexAction (ATag Token)
endLine _ _ = do
    setStartCode 0
    setIndent 0
    next

escSymbol :: AlexAction (ATag Token)
escSymbol input@AlexInput{..} len = do
    let x = excerpt input 1 (len - 1)
    TSymbol x <@> getAttr pos

symbol :: AlexAction (ATag Token)
symbol input@AlexInput{..} len = do
    let x = excerpt input 0 len
    TSymbol x <@> getAttr pos

char :: AlexAction (ATag Token)
char input@AlexInput{..} len = do
    let x = excerpt input 1 (len - 1)
    c <- maybeFail
        ("cannot parse character literal: " <> x)
        (parseChar x)
    TLiteral (LChar c) <@> getAttr pos

version :: AlexAction (ATag Token)
version input@AlexInput{..} len = do
    let x = excerpt input 0 len
    v <- maybeFail
        ("cannot parse version literal: " <> x)
        (parseVersion x)
    TVersion v <@> getAttr pos

float :: AlexAction (ATag Token)
float input@AlexInput{..} len = do
    let x = excerpt input 0 len
    f <- maybeFail
        ("cannot parse decimal digits to valid Float: " <> x)
        (parseFloat x)
    TLiteral (LFloat f) <@> getAttr pos

decInt :: AlexAction (ATag Token)
decInt input@AlexInput{..} len = do
    let x = excerpt input 0 len
    i <- maybeFail
        ("cannot parse decimal digits to valid Int: " <> x)
        (parseDecInt x)
    TLiteral (LInt i) <@> getAttr pos

hexInt :: AlexAction (ATag Token)
hexInt input@AlexInput{..} len = do
    let x = excerpt input 2 len
    i <- maybeFail
        ("cannot parse hexadecimal digits to valid Int: " <> x)
        (parseHexInt x)
    TLiteral (LInt i) <@> getAttr pos

binInt :: AlexAction (ATag Token)
binInt input@AlexInput{..} len = do
    let x = excerpt input 2 len
    i <- maybeFail
        ("cannot parse binary digits to valid Int: " <> x)
        (parseBinInt x)
    TLiteral (LInt i) <@> getAttr pos


beginString :: AlexAction (ATag Token)
beginString AlexInput{..} _ = do
    setStartCode string
    setStringStart pos
    next

accumString :: AlexAction (ATag Token)
accumString input len = do
    let x = excerpt input 0 len
    c <- maybeFail
        ("cannot parse character in string literal: " <> x)
        (parseChar x)
    pushStringAccumulator [c]
    next

endString :: AlexAction (ATag Token)
endString _ _ = do
    setStartCode line
    pos <- getStringStart
    body <- popStringAccumulator
    TLiteral (LString body) <@> getAttr pos


next :: Lexer (ATag Token)
next = do
    input <- getInput
    startCode <- getStartCode
    case alexScan input startCode of
        AlexEOF ->
            getStartCode >>= \case
                n | n /= string -> TEof <@> getUnitAttr
                _ -> unexpectedEof input.pos
        AlexError input' ->
            if isEof input'
                then unexpectedEof input'.pos
                else unexpectedInput input'.pos
        AlexSkip input' _ -> do
            setInput input'
            next
        AlexToken input' len action -> do
            setInput input'
            action input len

loop :: Lexer (Seq (ATag Token))
loop = do
    token <- next
    case token of
        t@(T' TEof) -> return (Seq.singleton t)
        _ -> (token Seq.:<|) <$> loop


-- | Perform lexical analysis on a ByteString,
--   yielding a sequence of tagged tokens
lexByteString :: FilePath -> ByteString -> Either Doc (Seq (ATag Token))
lexByteString filePath fileContent =
    case do {
        runLexer loop
            LexerState
            { input =
                AlexInput
                { pos = Nil
                , bytes = fileContent
                , startCode = 0
                }
            , stringAccumulator = ""
            , stringStart = Nil
            , filePath  = filePath
            }
    } of
        Right (tokens, _) -> Right tokens
        Left e -> Left $ pPrint e

-- | Perform lexical analysis on a String,
--   yielding a sequence of tagged tokens
lexString :: FilePath -> String -> Either Doc (Seq (ATag Token))
lexString filePath input = lexByteString filePath (fromString input)
}
