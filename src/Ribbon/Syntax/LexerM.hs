module Ribbon.Syntax.LexerM where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString

import Data.Word (Word8)
import Data.Int (Int64)

import Control.Monad.State.Class
import Control.Monad

import Ribbon.Util
import Ribbon.Source
import Ribbon.Display




-- | A lexical analysis error
data LexError
    -- | A special failure with a message, such as when integers are too large
    = LexFailure !(ATag String)
    -- | An unexpected end of file
    | LexUnexpectedEof !Attr
    -- | An unexpected input
    | LexUnexpectedInput !Attr
    deriving Show

instance Pretty ann LexError where
    pPrint = (text "lexical error at" <+>) . \case
        LexFailure (msg :@: a) ->
            (pPrint a <> text ":") <+> text msg
        LexUnexpectedEof a ->
            (pPrint a <> text ":") <+> text "unexpected end of file"
        LexUnexpectedInput a ->
            (pPrint a <> text ":") <+> text "unexpected input"

-- | The type of "lexlets" for the lexer
type AlexAction a = AlexInput -> Int -> Lexer a

-- | The input stream of the lexer
data AlexInput
    -- | Construct an input stream for the lexer
    = AlexInput
    -- | The current position in the file being lexed
    { aiPos   :: !Pos
    -- | The remaining bytes to be lexed
    , aiBytes :: !ByteString
    }
    deriving Show

-- | Placeholder required by alex; there are no patterns with a left context
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar =
    -- No patterns with a left context
    undefined

-- | Get the next byte from an input stream, updating the position
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

-- | Check if an input stream is at the end of the file
isEof :: AlexInput -> Bool
isEof = ByteString.null . aiBytes

-- | Convert a subsection of the input's byte string to a string
excerpt :: AlexInput -> Int -> Int -> String
excerpt AlexInput{..} offset len
    = bytesToString
    . ByteString.drop (fromIntegral offset)
    $ ByteString.take (fromIntegral len) aiBytes

-- | The state of the lexer, contains the input stream,
--   the current start code, and the string accumulator,
--   as well as the current file being lexed
data LexerState
    = LexerState
    { lxInput :: !AlexInput
    , lxStartCode :: !Int
    , lxStringAccumulator :: !String
    , lxStringStart :: !Pos
    , lxFile :: !File
    }
    deriving Show

-- | The lexer monad
newtype Lexer a
    -- | Wrap a function into a Lexer action
    = Lexer
    -- | Unwrap a Lexer action to a function
    { runLexer :: LexerState -> Either LexError (a, LexerState) }

instance Functor Lexer where
    fmap = liftM

instance Applicative Lexer where
    pure a = Lexer $ Right . (a, )
    (<*>) = ap

instance Monad Lexer where
    (Lexer p) >>= f = Lexer \s -> case p s of
        Right (a, s') -> runLexer (f a) s'
        Left e -> Left e

instance MonadFail Lexer where
    fail msg = Lexer \s -> Left $ LexFailure
        (msg :@: Attr (lxFile s)
        (unitRange $ aiPos $ lxInput s))

instance MonadState LexerState Lexer where
    state = Lexer . (Right .)


-- | Fail lexing with a custom lexical error
failWith :: LexError -> Lexer a
failWith = Lexer . const . Left

-- | Fail lexing at a given position due to an unexpected end of file
unexpectedEof :: Pos -> Lexer a
unexpectedEof p = getFile >>=
    failWith . LexUnexpectedEof . (`Attr` unitRange p)

-- | Fail lexing at a given position due to an unexpected input
unexpectedInput :: Pos -> Lexer a
unexpectedInput p = getFile >>=
    failWith . LexUnexpectedInput . (`Attr` unitRange p)

-- | Get the current byte offset in the input stream
getOffset :: Lexer Int64
getOffset = posOffset <$> getPos

-- | Get the current position in the input stream
getPos :: Lexer Pos
getPos = gets $ aiPos . lxInput

-- | Get the current file being lexed
getFile :: Lexer File
getFile = gets lxFile

-- | Create a range from the given position to the current one
getRange :: Pos -> Lexer Range
getRange start = Range start <$> getPos

-- | Create a unary range from the current position
getUnitRange :: Lexer Range
getUnitRange = unitRange <$> getPos

-- | Create a range from the given position to the current one and build
--   an Attr from it and the file being lexed
getAttr :: Pos -> Lexer Attr
getAttr start = Attr <$> getFile <*> getRange start

-- | Create a unary range from the current position and build
--   an Attr from it and the file being lexed
getUnitAttr :: Lexer Attr
getUnitAttr = Attr <$> getFile <*> getUnitRange

-- | Get the current state
getState :: Lexer LexerState
getState = get

-- | Set the current state
setState :: LexerState -> Lexer ()
setState = put

-- | Get the current input
getInput :: Lexer AlexInput
getInput = lxInput <$> getState

-- | Set the current input
setInput :: AlexInput -> Lexer ()
setInput ai = modify \s ->
    s { lxInput = ai }

-- | Get the current start code
getStartCode :: Lexer Int
getStartCode = lxStartCode <$> getState

-- | Set the current start code
setStartCode :: Int -> Lexer ()
setStartCode sc = modify \s ->
    s { lxStartCode = sc }

-- | Set the starting Pos for the string accumulator
setStringStart :: Pos -> Lexer ()
setStringStart p = modify \s ->
    s { lxStringStart = p }

-- | Get the starting Pos for the string accumulator
getStringStart :: Lexer Pos
getStringStart = lxStringStart <$> getState

-- | Push a string to the end of the string accumulator
pushStringAccumulator :: String -> Lexer ()
pushStringAccumulator a = modify \s ->
    s { lxStringAccumulator = lxStringAccumulator s <> a }

-- | Clear the string accumulator
clearStringAccumulator :: Lexer ()
clearStringAccumulator = modify \s ->
    s { lxStringAccumulator = "" }

-- | Clear the string accumulator and return its contents
popStringAccumulator :: Lexer String
popStringAccumulator =
    lxStringAccumulator <$> getState << clearStringAccumulator
