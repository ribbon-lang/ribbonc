{
module Ribbon.Syntax.Lexer
    ( next
    , loop
    ) where

import Ribbon.Util
import Ribbon.Source
import Ribbon.Syntax.Literal
import Ribbon.Syntax.Text
import Ribbon.Syntax.Token
import Ribbon.Syntax.ParserM
}

%encoding "utf8"

$bin = [01]
$dec = [0-9]
$hex = [0-9A-Fa-f]
$alpha = [A-Za-z_]
$alphanum = [A-Za-z0-9_]
$punc = [\,\{\}\(\)\[\]]

@reserved = let | in | as | match | with | do | fun | handler
          | module | import | use | pub | namespace
          | type | effect | class | instance
          | infix | infixl | infixr | prefix | postfix
          | "=>" | ";" | ("."+)
@operator = $printable # $alphanum # $punc # [\'\"]
@escape = \\ ([\\\'\"0nrt] | x $hex{2} | u \{ $hex+ \})

tokens :-
    <0> $white+;
    <0> ";;" .* \n;

    <0> @reserved         { reserved }
    <0> @operator         { operator }
    <0> $alpha $alphanum* { ident }
    <0> $punc             { punc }

    <0> \' (@escape | $printable # [\\\'] | $white) \' { char }
    <0> $dec+ \. $dec+ (e [\+\-]? $dec+)?              { float }
    <0> $dec+                                          { decInt }
    <0> 0x $hex+                                       { hexInt }
    <0> 0b $bin+                                       { binInt }

    <0>      \"                                        { beginString }
    <string> @escape | $printable # [\\\"] | $white    { accumString }
    <string> \"                                        { endString }

{
reserved :: AlexAction Token
reserved input@AlexInput{..} len = do
    let excerpt = alexExcerpt input 0 len
    TSymbol excerpt <@> getAttr aiPos

operator :: AlexAction Token
operator input@AlexInput{..} len = do
    let excerpt = alexExcerpt input 0 len
    TSymbol excerpt <@> getAttr aiPos

ident :: AlexAction Token
ident input@AlexInput{..} len = do
    let excerpt = alexExcerpt input 0 len
    TSymbol excerpt <@> getAttr aiPos

punc :: AlexAction Token
punc input@AlexInput{..} len = do
    let excerpt = alexExcerpt input 0 len
    TSymbol excerpt <@> getAttr aiPos

char :: AlexAction Token
char input@AlexInput{..} len = do
    let excerpt = alexExcerpt input 1 (len - 1)
    c <- maybeFail
        ("cannot parse character literal: " <> excerpt)
        (parseChar excerpt)
    TLiteral (LChar c) <@> getAttr aiPos

float :: AlexAction Token
float input@AlexInput{..} len = do
    let excerpt = alexExcerpt input 0 len
    f <- maybeFail
        ("cannot parse decimal digits to valid Float: " <> excerpt)
        (parseFloat excerpt)
    TLiteral (LFloat f) <@> getAttr aiPos

decInt :: AlexAction Token
decInt input@AlexInput{..} len = do
    let excerpt = alexExcerpt input 0 len
    i <- maybeFail
        ("cannot parse decimal digits to valid Int: " <> excerpt)
        (parseDecInt excerpt)
    TLiteral (LInt i) <@> getAttr aiPos

hexInt :: AlexAction Token
hexInt input@AlexInput{..} len = do
    let excerpt = alexExcerpt input 2 len
    i <- maybeFail
        ("cannot parse hexadecimal digits to valid Int: " <> excerpt)
        (parseHexInt excerpt)
    TLiteral (LInt i) <@> getAttr aiPos

binInt :: AlexAction Token
binInt input@AlexInput{..} len = do
    let excerpt = alexExcerpt input 2 len
    i <- maybeFail
        ("cannot parse binary digits to valid Int: " <> excerpt)
        (parseBinInt excerpt)
    TLiteral (LInt i) <@> getAttr aiPos


beginString :: AlexAction Token
beginString AlexInput{..} _ = do
    setAlexStartCode string
    setAlexStringStart aiPos
    next

accumString :: AlexAction Token
accumString input len = do
    let excerpt = alexExcerpt input 0 len
    c <- maybeFail
        ("cannot parse character in string literal: " <> excerpt)
        (parseChar excerpt)
    pushAlexStringAccumulator [c]
    next

endString :: AlexAction Token
endString _ _ = do
    setAlexStartCode 0
    pos <- getAlexStringStart
    body <- popAlexStringAccumulator
    TLiteral (LString body) <@> getAttr pos


next :: Parser Token
next = do
    input <- getAlexInput
    startCode <- getAlexStartCode
    case alexScan input startCode of
        AlexEOF ->
            getAlexStartCode >>= \case
                0 -> TEof <@> getUnitAttr
                _ -> failWith . ParseUnexpectedEof $ aiPos input
        AlexError input' ->
            if alexEOF input'
                then failWith . ParseUnexpectedEof $ aiPos input'
                else failWith . ParseUnexpectedInput $ aiPos input'
        AlexSkip input' _ -> do
            setAlexInput input'
            next
        AlexToken input' len action -> do
            setAlexInput input'
            action input len

loop :: Parser [Token]
loop = do
    token <- next
    case token of
        T' TEof -> return []
        _    -> (token :) <$> loop
}
