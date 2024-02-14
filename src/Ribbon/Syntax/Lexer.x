{
module Ribbon.Syntax.Lexer
    ( lexFile
    , lexString
    , lexByteString
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Ribbon.Util
import Ribbon.Display
import Ribbon.Source
import Ribbon.Syntax.Literal
import Ribbon.Syntax.Text
import Ribbon.Syntax.Token
import Ribbon.Syntax.LexerM
}


%encoding "utf8"

$bin = [01]
$dec = [0-9]
$hex = [0-9A-Fa-f]
$alpha = [A-Za-z_]
$alphanum = [A-Za-z0-9_]
$punc = [\'\,\{\}\(\)\[\]]

@reserved = let | in | as | match | with | do | fun | handler
          | module | import | use | file | pub | namespace
          | type | effect | class | instance
          | infix | infixl | infixr | prefix | postfix
          | "=>" | ";" | "./" | "../" | ".." | "."
@operator = ($printable # $white # $alphanum # $punc # [\.\"])+
@escape = \\ ([\\\'\"0nrt] | x $hex{2} | u \{ $hex+ \})

tokens :-
    <0> $white+;
    <0> ";;" .* \n;

    <0> \' (@escape | $printable # [\\\'] | $white) \' { char }
    <0> $dec+ \. $dec+ (e [\+\-]? $dec+)?              { float }
    <0> $dec+                                          { decInt }
    <0> 0x $hex+                                       { hexInt }
    <0> 0b $bin+                                       { binInt }

    <0>      \"                                        { beginString }
    <string> @escape | $printable # [\\\"] | $white    { accumString }
    <string> \"                                        { endString }

    <0> @reserved         { reserved }
    <0> @operator         { operator }
    <0> $alpha $alphanum* { ident }
    <0> $punc             { punc }


{
reserved :: AlexAction (ATag Token)
reserved input@AlexInput{..} len = do
    let x = excerpt input 0 len
    TSymbol x <@> getAttr aiPos

operator :: AlexAction (ATag Token)
operator input@AlexInput{..} len = do
    let x = excerpt input 0 len
    TSymbol x <@> getAttr aiPos

ident :: AlexAction (ATag Token)
ident input@AlexInput{..} len = do
    let x = excerpt input 0 len
    TSymbol x <@> getAttr aiPos

punc :: AlexAction (ATag Token)
punc input@AlexInput{..} len = do
    let x = excerpt input 0 len
    TSymbol x <@> getAttr aiPos

char :: AlexAction (ATag Token)
char input@AlexInput{..} len = do
    let x = excerpt input 1 (len - 1)
    c <- maybeFail
        ("cannot parse character literal: " <> x)
        (parseChar x)
    TLiteral (LChar c) <@> getAttr aiPos

float :: AlexAction (ATag Token)
float input@AlexInput{..} len = do
    let x = excerpt input 0 len
    f <- maybeFail
        ("cannot parse decimal digits to valid Float: " <> x)
        (parseFloat x)
    TLiteral (LFloat f) <@> getAttr aiPos

decInt :: AlexAction (ATag Token)
decInt input@AlexInput{..} len = do
    let x = excerpt input 0 len
    i <- maybeFail
        ("cannot parse decimal digits to valid Int: " <> x)
        (parseDecInt x)
    TLiteral (LInt i) <@> getAttr aiPos

hexInt :: AlexAction (ATag Token)
hexInt input@AlexInput{..} len = do
    let x = excerpt input 2 len
    i <- maybeFail
        ("cannot parse hexadecimal digits to valid Int: " <> x)
        (parseHexInt x)
    TLiteral (LInt i) <@> getAttr aiPos

binInt :: AlexAction (ATag Token)
binInt input@AlexInput{..} len = do
    let x = excerpt input 2 len
    i <- maybeFail
        ("cannot parse binary digits to valid Int: " <> x)
        (parseBinInt x)
    TLiteral (LInt i) <@> getAttr aiPos


beginString :: AlexAction (ATag Token)
beginString AlexInput{..} _ = do
    setStartCode string
    setStringStart aiPos
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
    setStartCode 0
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
                0 -> TEof <@> getUnitAttr
                _ -> unexpectedEof (aiPos input)
        AlexError input' ->
            if isEof input'
                then unexpectedEof (aiPos input')
                else unexpectedInput (aiPos input')
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
        _    -> (token Seq.:<|) <$> loop

-- | Perform lexical analysis on a File, yielding a sequence of tagged tokens
lexFile :: File -> Either (Doc ()) (Seq (ATag Token))
lexFile file =
    case do {
        runLexer loop
            LexerState
            { lxInput =
                AlexInput
                { aiPos = Nil
                , aiBytes = fileContent file
                }
            , lxStartCode = 0
            , lxStringAccumulator = ""
            , lxStringStart = Nil
            , lxFile  = file
            }
    } of
        Right (tokens, _) -> Right tokens
        Left e -> Left $ pPrint e

-- | Perform lexical analysis on a ByteString,
--   yielding a sequence of tagged tokens
lexByteString :: String -> ByteString -> Either (Doc ()) (Seq (ATag Token))
lexByteString name input = lexFile (File name input)

-- | Perform lexical analysis on a String,
--   yielding a sequence of tagged tokens
lexString :: String -> String -> Either (Doc ()) (Seq (ATag Token))
lexString name input = lexByteString name (fromString input)
}
