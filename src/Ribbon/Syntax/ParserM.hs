module Ribbon.Syntax.ParserM where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.Functor ((<&>))

import Data.Set (Set)
import Data.Set qualified as Set

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Applicative

import Ribbon.Util
import Ribbon.Display
import Ribbon.Source
import Ribbon.Syntax.Token


-- | Error handling display class for ParseExcept
class Unexpected assert where
    -- | Format an unexpected item as a user-friendly string
    formatUnexpected :: assert -> String

instance Unexpected Char where
    formatUnexpected c = "unexpected character `" <> display c <> "`"

instance Unexpected TokenData where
    formatUnexpected t = "unexpected " <> formatUnexpected (tokenKind t) <> " `" <> display t <> "`"

instance Unexpected TokenKind where
    formatUnexpected = display

-- | Input type for ParserM
type ParseStream i = Seq (Syn i)

-- | ParserM error type
data ParseExcept
    = ParseError (Syn String)
    | ParseFail Int (Set String)
    deriving Show

instance Display ParseExcept where
    display = \case
        (ParseError (msg :@: x)) -> display x <> ": " <> msg
        (ParseFail x msgs) -> "unhandled failure at offset " <> display x <> ": " <> formatExpected msgs

-- | Composition class for monads wrapping ParserM
class ( Monad m, Alternative m, MonadFail m, MonadError String m
      , MonadReader (ParseStream i) m, MonadState Int m
      )
    => ParserMonad i m | m -> i where
    -- | Lift a ParserM action into the current monad
    liftP :: ParserM i a -> m a
    -- | Catch a ParseExcept in a ParserM action embedded in the current monad
    catchParseExcept :: m a -> (ParseExcept -> m a) -> m a

instance ParserMonad i (ParserM i) where
    liftP = id

    catchParseExcept (ParserM a) f = ParserM \s i ->
        case a s i of
            Left e -> runParser (f e) s i
            x -> x

-- | Parsing monad, parameterized by
--   input element type i (Such as Char)
--   and output type o (Such as Token)
newtype ParserM i o
    -- | Wrap a function into a ParserM action
    = ParserM
    -- | Unwrap a ParserM action into a function
    { runParser :: ParseStream i -> Int -> Either ParseExcept (o, Int) }
    deriving Functor

instance Applicative (ParserM i) where
    pure a = ParserM \_ i -> Right (a, i)
    ParserM f <*> ParserM a = ParserM \s i -> do
        (f', i') <- f s i
        (a', i'') <- a s i'
        pure (f' a', i'')

instance Monad (ParserM i) where
    ParserM a >>= f = ParserM \s i -> do
        (a', i') <- a s i
        runParser (f a') s i'

instance Alternative (ParserM i) where
    empty = ParserM \_ i -> Left (ParseFail i Set.empty)
    ParserM a <|> ParserM b = ParserM \s i ->
        case a s i of
            Left (ParseFail ia mas) -> case b s i of
                Left (ParseFail ib mbs) -> Left (ParseFail (min ia ib) (mas <> mbs))
                x -> x
            x -> x

instance MonadFail (ParserM i) where
    fail msg = ParserM \_ i -> Left (ParseFail i (Set.singleton msg))

instance MonadError String (ParserM i) where
    throwError msg = ParserM \s i -> Left (ParseError (msg :@: psAttr s i))
    catchError (ParserM a) f = ParserM \s i ->
        case a s i of
            Left (ParseError (msg :@: _)) -> runParser (f msg) s i
            x -> x

instance MonadReader (ParseStream i) (ParserM i) where
    ask = ParserM (curry Right)
    local f (ParserM a) = ParserM \s _ ->
        let s' = f s in a s' 0

instance MonadState Int (ParserM i) where
    get = ParserM \_ i -> Right (i, i)
    put i = ParserM \_ _ -> Right ((), i)

-- | Get the Attr for a particular offset in a ParseStream
psAttr :: ParseStream i -> Int -> Attr
psAttr ps i = case Seq.lookup i ps of
    Just (_ :@: x) -> x
    _ -> error "invalid attribute index for psAttr"

-- | Get the syntax object for a particular offset in a ParseStream
psValue :: ParseStream i -> Int -> Maybe i
psValue ps i = synData <$> Seq.lookup i ps

-- | Get the length of a ParseStream
psLength :: ParseStream i -> Int
psLength = Seq.length

-- | Throw a custom parser error in the current monad.
--   Note that you would normally use
--  `fail`, `empty`, in combination `expecting`, etc
throwParseExcept :: ParserMonad i m => ParseExcept -> m a
throwParseExcept e = liftP $ ParserM \_ _ -> Left e

-- | Catch a ParseFail in the current monad.
catchParseFail :: ParserMonad i m => m a -> (Int -> Set String -> m a) -> m a
catchParseFail m f = m `catchParseExcept` \case
    ParseFail i msgs -> f i msgs
    e -> throwParseExcept e

-- | Catch a ParseError in the current monad.
catchParseError :: ParserMonad i m => m a -> (Attr -> String -> m a) -> m a
catchParseError m f = m `catchParseExcept` \case
    ParseError (msg :@: ie) -> f ie msg
    e -> throwParseExcept e

-- | Get the stream associated with the current ParserM action
parseStream :: ParserMonad i m => m (ParseStream i)
parseStream = liftP $ ParserM (curry Right)

-- | Get the current offset in the current ParserM action
parseOffset :: ParserMonad i m => (Int -> Int) -> m Int
parseOffset f = liftP $ ParserM \_ i -> let i' = f i in Right (i', i')

-- | Run a ParserM action with an alternative for failure.
--   Note that this discards the expectations
--   of the first action, unlike `(<|>)`
recoverWith :: ParserMonad i m => m a -> m a -> m a
recoverWith m1 m2 = m2 `catchParseFail` \_ _ -> m1

-- | Trigger a parser failure at a given offset with a set of expected values.
--   Note that you would normally use `fail`, `empty`,
--   in combination with `expecting`, etc
parseFail :: ParserMonad i m => Int -> Set String -> m a
parseFail i msgs = liftP $ ParserM \_ _ -> Left (ParseFail i msgs)

-- | Trigger a parser error at a given Attr with a message.
--   Note that you would normally use `throwError`
parseError :: ParserMonad i m => Attr -> String -> m a
parseError a msg = liftP $ ParserM \_ _ -> Left (ParseError (msg :@: a))

-- | Trigger a parser failure at the current offset
--   with a set of expected values.
failMulti :: ParserMonad i m => [String] -> m a
failMulti msgs = liftP $ ParserM \_ i -> Left (ParseFail i (Set.fromList msgs))

-- | Trigger a parser failure at a given offset with a single expectation
failAt :: ParserMonad i m => Int -> String -> m a
failAt i msg = liftP $ ParserM \_ _ -> Left (ParseFail i (Set.singleton msg))

-- | Trigger a parser error at a given Attr with a message
throwErrorAt :: ParserMonad i m => Attr -> String -> m a
throwErrorAt a m = liftP $ ParserM \_ _ -> Left (ParseError (m :@: a))

-- | If the given parser fails,
--   replace its expectation set with the given message
expecting :: ParserMonad i m => String -> m a -> m a
expecting msg p = catchParseFail p \_ _ -> fail msg

-- | If the given parser fails,
--   replace its expectation set with the given one
expectingMulti :: ParserMonad i m => [String] -> m a -> m a
expectingMulti msgs m = m `catchParseFail` \_ _ -> failMulti msgs

-- | Formatting function for unexpected input, or expected input at EOF
formatInput :: Unexpected i => ParseStream i -> Int -> String
formatInput s ie = case psValue s ie of
    Just v -> formatUnexpected v
    _ -> "expected additional input"

-- | Formatting function for expected input
formatExpected :: Set String -> String
formatExpected Nil = "expected additional input"
formatExpected msgs = "expected " <> display msgs

-- | Formatting function used by `noFail` to produce an error message,
--   given either a set of expectations from the failure,
--   or a `formatInput` message
format :: (Unexpected i, Display i) => ParseStream i -> Int -> Set String -> String
format s i msgs =
    if Set.null msgs
        then formatInput s i
        else formatExpected msgs

-- | Run a parser, and if it fails, convert the failure to a parse error
noFail :: (Unexpected i, Display i, ParserMonad i m) => m a -> m a
noFail m = m `catchParseFail` \i msgs -> do
    s <- parseStream
    parseError (psAttr s i) (format s i msgs)

-- | Ensure that the parser consumes the remaining ParseStream
consumesAll :: (Unexpected i, ParserMonad i m) => m a -> m a
consumesAll m = do
    a <- m
    s <- parseStream
    i <- getOffset
    if i >= psLength s
        then pure a
        else parseError (psAttr s i) (formatInput s i)

-- | Fail with an expectation message if the condition is false
guard :: ParserMonad i m => Bool -> String -> m ()
guard p expStr = if p then pure () else fail expStr

-- | Fail with an expectation set if the condition is false
guardMulti :: ParserMonad i m => Bool -> [String] -> m ()
guardMulti p expStrs = if p then pure () else failMulti expStrs

-- | Fail at the given offset with an expectation set if the condition is false
guardMultiAt :: ParserMonad i m => Bool -> Int -> Set String -> m ()
guardMultiAt p i expStrs = if p then pure () else parseFail i expStrs

-- | Error with a message if the condition is false
assert :: ParserMonad i m => Bool -> String -> m ()
assert p expStr = if p then pure () else throwError expStr

-- | Error with a message at the given Attr if the condition is false
assertAt :: ParserMonad i m => Bool -> Attr -> String -> m ()
assertAt p a expStr = if p then pure () else throwErrorAt a expStr

-- | Get the current offset in the current parser monad
getOffset :: ParserMonad i m => m Int
getOffset = parseOffset id

-- | Set the current offset in the current parser monad
setOffset :: ParserMonad i m => Int -> m ()
setOffset i = void $ parseOffset (const i)

-- | Modify the current offset in the current parser monad
modOffset :: ParserMonad i m => (Int -> Int) -> m ()
modOffset f = void $ parseOffset f

-- | Advance the offset in the current parser monad
advance :: ParserMonad i m => m ()
advance = liftP $ ParserM \_ i -> Right ((), i + 1)

-- | Get the currently selected element in the parse stream
peek :: ParserMonad i m => m i
peek = liftP $ ParserM \s i ->
    case psValue s i of
        Just a -> Right (a, i)
        _ -> Left (ParseFail i (Set.singleton "additional input"))

-- | Get the currently selected element in the parse stream,
--   returning Nothing if the stream is empty
tryPeek :: ParserMonad i m => m (Maybe i)
tryPeek = liftP $ ParserM \s i ->
    case psValue s i of
        Just a -> Right (Just a, i)
        _ -> Right (Nothing, i)

-- | Get a location Attr for the current position in the ParseStream
attr :: ParserMonad i m => m Attr
attr = liftP $ ParserM \s i -> Right (psAttr s i, i)

-- | Get a location Attr for the current position in the ParseStream,
--   and advance the stream offset
attrNext :: ParserMonad i m => m Attr
attrNext = attr << optional advance

-- | Wraps the output of a parser in a Syn with an Attr for the range consumed
syn :: (ParserMonad i m) => m a -> m (Syn a)
syn p = do
    x1 <- attr
    a <- p
    x2 <- attr
    pure (a :@: (x1 <> x2))

-- | Execute @syn@ and discard the result, keeping only the Attr generated
attrOf :: ParserMonad i m => m a -> m Attr
attrOf = fmap synAttr . syn

-- | Get the currently selected element in the parse stream,
--   and advance the stream offset
next :: ParserMonad i m => m i
next = peek << advance

-- | Get the currently selected element's Attr in the parse stream
peekAttr :: ParserMonad i m => m (Syn i)
peekAttr = flip (:@:) <$> attr <*> peek

-- | Get the currently selected element's Attr in the parse stream,
--   and wrap it around the current element using Syn,
--   then advance the stream offset
nextAttr :: ParserMonad i m => m (Syn i)
nextAttr = flip (:@:) <$> attr <*> next

-- | Advance the stream offset if the current element satisfies the predicate,
--   returning the matched element
nextIf :: ParserMonad i m => (i -> Bool) -> m i
nextIf p = do
    a <- peek
    if p a
        then a <$ advance
        else empty

-- | Advance the stream offset if the current element satisfies the predicate,
--   returning the matched element along with its Attr
nextIfAttr :: ParserMonad i m => (Syn i -> Bool) -> m (Syn i)
nextIfAttr p = do
    t <- peekAttr
    if p t
        then t <$ advance
        else empty

-- | Advance the stream offset if the current element satisfies the predicate,
--   discarding the matched element
nextIf_ :: ParserMonad i m => (i -> Bool) -> m ()
nextIf_ p = do
    a <- peek
    if p a
        then advance
        else empty

-- | Advance the stream offset as long as
--   the current element satisfies the predicate,
--   returning the matched elements as a list
nextWhile :: ParserMonad i m => (i -> Bool) -> m [i]
nextWhile p = some (nextIf p)

-- | Advance the stream offset as long as
--   the current element satisfies the predicate,
--   discarding the matched elements
nextWhile_ :: ParserMonad i m => (i -> Bool) -> m ()
nextWhile_ p = some_ (nextIf_ p)

-- | Advance the stream offset if the current element
--   satisfies a mapping predicate, returning the mapped value
nextMap :: ParserMonad i m => (i -> Maybe a) -> m a
nextMap p = do
    a <- peek
    case p a of
        Just a' -> a' <$ advance
        _ -> empty

-- | @nextIf_ (== e)@
expect' :: (Eq i, ParserMonad i m) => i -> m ()
expect' e = nextIf_ (== e)

-- | @nextIf (`elem` es)@
expectAny' :: (Eq i, ParserMonad i m) => [i] -> m i
expectAny' es = nextIf (`elem` es)

-- | Consume an expected sequence of inputs
expectSeq' :: (Eq i, ParserMonad i m) => [i] -> m ()
expectSeq' es = do
    forM_ es \e -> do
        a <- peek
        if a == e
            then advance
            else empty

-- | Consume one of any expected sequences of inputs
expectAnySeq' :: (Eq i, ParserMonad i m) => [[i]] -> m [i]
expectAnySeq' es = do
    asum (es <&> \e -> e <$ expectSeq' e)

-- | @expecting (display e) (expect' e)@
expect :: (Display i, Eq i, ParserMonad i m) => i -> m ()
expect e = expecting (display e) (expect' e)

-- | @expectingMulti (display <$> es) (expectAny' es)@
expectAny :: (Display i, Eq i, ParserMonad i m) => [i] -> m i
expectAny es = expectingMulti (display <$> es) (expectAny' es)

-- | @expecting (display e) (expectSeq' e)@
expectSeq :: (Display [i], Eq i, ParserMonad i m) => [i] -> m ()
expectSeq e = expecting (display e) (expectSeq' e)

-- | @expectingMulti (display <$> es) (expectAnySeq' es)@
expectAnySeq :: (Display [i], Eq i, ParserMonad i m) => [[i]] -> m [i]
expectAnySeq es = expectingMulti (display <$> es) (expectAnySeq' es)

-- | Expect a list of `m a` separated by `m sep`.
--   The list must contain at least one element
listSome :: ParserMonad i m => m sep -> m a -> m [a]
listSome sep p = do
    a <- p
    as <- many (sep >> p)
    pure (a:as)

-- | Expect a list of `m a` separated by `m sep`.
--   The list may be empty
listMany :: ParserMonad i m => m sep -> m a -> m [a]
listMany sep p = recoverWith (pure []) (listSome sep p)
