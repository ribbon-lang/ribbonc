{
module Language.Ribbon.Parsing.Lexer
    ( lexString
    , lexByteString
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Word(Word8)

import Data.Nil
import Data.Tag
import Data.Pos
import Data.Attr

import Text.Pretty

import Language.Ribbon.Util

import Language.Ribbon.Lexical

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

    <src> $hWhite* { beginLine srcLine }
    <srcLine> \n   { endLine src }
    <srcLine> $hWhite+;

    <srcLine> \' (@escape | $printable # [\\\'] | $white) \' { char }
    <srcLine> $dec+ \. $dec+ \. $dec+                        { version }
    <srcLine> $dec+ \. $dec+ (e [\+\-]? $dec+)?              { float }
    <srcLine> $dec+                                          { decInt }
    <srcLine> 0x $hex+                                       { hexInt }
    <srcLine> 0b $bin+                                       { binInt }

    <srcLine>   \"                                     { beginString srcString }
    <srcString> @escape | $printable # [\\\"] | $white { accumString }
    <srcString> \"                                     { endString srcLine }

    <srcLine> $backtick ($alpha $alphanum* | @operator) $backtick { escSymbol }

    <srcLine> @reserved         { symbol }
    <srcLine> @operator         { symbol }
    <srcLine> $alpha $alphanum* { symbol }
    <srcLine> $punc             { symbol }


{
beginLine :: Int -> AlexAction (ATag Token)
beginLine sc _ _ = do
    setStartCode sc
    next

endLine :: Int -> AlexAction (ATag Token)
endLine sc _ _ = do
    setStartCode sc
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


beginString :: Int -> AlexAction (ATag Token)
beginString sc AlexInput{..} _ = do
    setStartCode sc
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

endString :: Int -> AlexAction (ATag Token)
endString sc _ _ = do
    setStartCode sc
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
                n | n /= srcString -> TEof <@> getUnitAttr
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
                , startCode = src
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



-- | Placeholder required by alex; there are no patterns with a left context
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar =
    -- No patterns with a left context
    undefined

-- | Get the next byte from an input stream, updating the position
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@AlexInput{..} =
    case ByteString.uncons bytes of
        Just (byte, bytes') ->
            let pos' = nextPos pos byte
                input' =
                    input
                    { pos   = pos'
                    , bytes = bytes'
                    }
            in Just (byte, input')
        _ -> Nothing
    where
        nextPos (Pos o l _ _) 0x0a =
            Pos (o + 1) (l + 1) 1 0

        nextPos (Pos o l c i) ch =
            Pos (o + 1) l (c + 1)
                if startCode == src
                -- NOTE: tab width should possibly be a compiler option
                    then i + select (ch == 0x09) 4 1
                    else i
}
