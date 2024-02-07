module Ribbon.Syntax.ParserM where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

import Data.Word (Word8)
import Data.Int (Int64)

import Data.Functor

import Control.Monad.State.Class
import Control.Monad

import Ribbon.Util
import Ribbon.Source
import Data.String (fromString)


data ParseError
    = ParseFailure
        { pfErrorPos :: !Pos
        , pfErrorMessage :: !String
        }
    | ParseUnexpectedEof !Pos
    | ParseUnexpectedInput !Pos
    deriving Show

type AlexAction a = AlexInput -> Int -> Parser a

data AlexInput
    = AlexInput
    { aiPos   :: !Pos
    , aiBytes :: !ByteString
    }
    deriving Show

data AlexState
    = AlexState
    { asInput :: !AlexInput
    , asStartCode :: !Int
    , asStringAccumulator :: !String
    , asStringStart :: !Pos
    }
    deriving Show

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar =
    -- No patterns with a left context
    undefined

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@AlexInput {..} =
    case ByteString.uncons aiBytes of
        Just (byte, aiBytes') ->
            let aiPos' = nextPos aiPos byte
                input' =
                    input
                    { aiPos   = aiPos'
                    , aiBytes = aiBytes'
                    }
            in Just (byte, input')
        _ -> Nothing
    where
        nextPos (Pos o l _) 0x0a = Pos (o + 1) (l + 1) 1
        nextPos (Pos o l c) _ = Pos (o + 1) l $ c + 1

alexEOF :: AlexInput -> Bool
alexEOF = ByteString.null . aiBytes

alexExcerpt :: AlexInput -> Int -> Int -> String
alexExcerpt AlexInput{..} offset len
    = Text.unpack
    . Text.decodeUtf8
    . ByteString.toStrict
    . ByteString.drop (fromIntegral offset)
    $ ByteString.take (fromIntegral len) aiBytes

data ParserState
    = ParserState
    { pAlexState :: !AlexState
    , pFile :: !File
    }
    deriving Show

newtype Parser a
    = Parser
    { runParser :: ParserState -> Either ParseError (a, ParserState) }

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure a = Parser $ Right . (a, )
    (<*>) = ap

instance Monad Parser where
    (Parser p) >>= f = Parser \s -> case p s of
        Right (a, s') -> runParser (f a) s'
        Left e -> Left e

instance MonadFail Parser where
    fail msg = Parser \s ->
        Left $ ParseFailure (aiPos $ asInput $ pAlexState s) msg

instance MonadState ParserState Parser where
    state = Parser . (Right .)

parseFileWith :: File -> Parser a -> Either ParseError a
parseFileWith file p = fst <$> runParser p
    ParserState
    { pAlexState =
        AlexState
        { asInput =
            AlexInput
            { aiPos = Nil
            , aiBytes = fileContent file
            }
        , asStartCode = 0
        , asStringAccumulator = ""
        , asStringStart = Nil
        }
    , pFile  = file
    }

parseByteStringWith :: String -> ByteString -> Parser a -> Either ParseError a
parseByteStringWith name input = parseFileWith (File name input)

parseStringWith :: String -> String -> Parser a -> Either ParseError a
parseStringWith name input = parseByteStringWith name (fromString input)

failWith :: ParseError -> Parser a
failWith = Parser . const . Left

getOffset :: Parser Int64
getOffset = posOffset <$> getPos

getPos :: Parser Pos
getPos = gets $ aiPos . asInput . pAlexState

getFile :: Parser File
getFile = gets pFile

getRange :: Pos -> Parser Range
getRange start = Range start <$> getPos

getUnitRange :: Parser Range
getUnitRange = getPos <&> \p -> Range p p

getAttr :: Pos -> Parser Attr
getAttr start = Attr <$> getFile <*> getRange start

getUnitAttr :: Parser Attr
getUnitAttr = Attr <$> getFile <*> getUnitRange

getAlexState :: Parser AlexState
getAlexState = gets pAlexState

setAlexState :: AlexState -> Parser ()
setAlexState as = modify \s -> s { pAlexState = as }

getAlexInput :: Parser AlexInput
getAlexInput = asInput <$> getAlexState

setAlexInput :: AlexInput -> Parser ()
setAlexInput ai = modify \s ->
    s { pAlexState = (pAlexState s) { asInput = ai } }

getAlexStartCode :: Parser Int
getAlexStartCode = asStartCode <$> getAlexState

setAlexStartCode :: Int -> Parser ()
setAlexStartCode sc = modify \s ->
    s { pAlexState = (pAlexState s) { asStartCode = sc } }

setAlexStringStart :: Pos -> Parser ()
setAlexStringStart p = modify \s ->
    s { pAlexState = (pAlexState s) { asStringStart = p } }

getAlexStringStart :: Parser Pos
getAlexStringStart = asStringStart <$> getAlexState

pushAlexStringAccumulator :: String -> Parser ()
pushAlexStringAccumulator a = modify \s ->
    s { pAlexState =
            let s' = pAlexState s
            in s' { asStringAccumulator = asStringAccumulator s' <> a }
      }

clearAlexStringAccumulator :: Parser ()
clearAlexStringAccumulator = modify \s ->
    s { pAlexState = (pAlexState s) { asStringAccumulator = "" } }

popAlexStringAccumulator :: Parser String
popAlexStringAccumulator =
    asStringAccumulator <$> getAlexState << clearAlexStringAccumulator
