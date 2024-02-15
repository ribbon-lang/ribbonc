module Ribbon.Syntax.ParserM where

import Data.Functor
import Data.Foldable

import Data.String (fromString)

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.Set (Set)
import Data.Set qualified as Set

import Data.ByteString.Lazy (ByteString)

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Ribbon.Util
import Ribbon.Source
import Ribbon.Display
import Ribbon.Syntax.Token
import Ribbon.Syntax.Literal
import Ribbon.Syntax.Lexer qualified as L
import Ribbon.Syntax.Text




-- | Input type for Parser
data ParseStream
    -- | Create a new ParseStream
    = ParseStream
    -- | The input sequence of syntax objects to parse
    { psInput :: Seq (ATag Token)
    -- | The file associated with the input sequence
    , psFile :: File
    }
    deriving Show

-- | Parser error type
data ParseExcept
    -- | Indicates an unrecoverable error during parsing,
    --   with a message ascribed an Attr that describes the error
    = ParseError (ATag (Doc ()))
    -- | Indicates a recoverable error during parsing
    --   with an offset in the ParseStream,
    --   and a set of expectations that were not met
    | ParseFail Int (Set String)
    deriving Show

instance Pretty () ParseExcept where
    pPrint = \case
        (ParseError (msg :@: x)) ->
            hang (text "error at" <+> pPrint x <+> text ":")
                msg
        (ParseFail x msgs) ->
            hang (text "unhandled failure at offset" <+> pPrint x <+> text ":")
                (formatExpected msgs)

-- | Composition class for monads wrapping Parser
class ( Monad m, Alternative m, MonadFail m, MonadError (Doc ()) m
      , MonadReader ParseStream m, MonadState Int m
      )
    => ParserMonad m where
    -- | Lift a Parser action into the current monad
    liftP :: Parser a -> m a
    unliftP :: m a -> m (Parser a)
    -- | Catch a ParseExcept in a Parser action embedded in the current monad
    catchParseExcept :: m a -> (ParseExcept -> m a) -> m a


instance ParserMonad Parser where
    liftP = id
    unliftP = pure

    catchParseExcept (Parser a) f = Parser \s i ->
        case a s i of
            Left e -> runParser (f e) s i
            x -> x

-- | Parsing monad
newtype Parser a
    -- | Wrap a function into a Parser action
    = Parser
    -- | Unwrap a Parser action into a function
    { runParser :: ParseStream -> Int -> Either ParseExcept (a, Int) }
    deriving Functor

instance Applicative Parser where
    pure a = Parser \_ i -> Right (a, i)
    Parser f <*> Parser a = Parser \s i -> do
        (f', i') <- f s i
        (a', i'') <- a s i'
        pure (f' a', i'')

instance Monad Parser where
    Parser a >>= f = Parser \s i -> do
        (a', i') <- a s i
        runParser (f a') s i'

instance Alternative Parser where
    empty = Parser \_ i -> Left (ParseFail i Set.empty)
    Parser a <|> Parser b = Parser \s i ->
        case a s i of
            Left (ParseFail ia mas) -> case b s i of
                Left (ParseFail ib mbs) ->
                    Left (ParseFail (min ia ib) (mas <> mbs))
                x -> x
            x -> x

instance MonadFail Parser where
    fail msg = Parser \_ i -> Left (ParseFail i (Set.singleton msg))

instance MonadError (Doc ()) Parser where
    throwError msg = Parser \s i -> Left (ParseError (msg :@: psAttr s i))
    catchError (Parser a) f = Parser \s i ->
        case a s i of
            Left (ParseError (msg :@: _)) -> runParser (f msg) s i
            x -> x

instance MonadReader ParseStream Parser where
    ask = Parser (curry Right)
    local f (Parser a) = Parser \s _ ->
        let s' = f s in a s' 0

instance MonadState Int Parser where
    get = Parser \_ i -> Right (i, i)
    put i = Parser \_ _ -> Right ((), i)

-- | Get the Attr for a particular offset in a ParseStream
psAttr :: ParseStream -> Int -> Attr
psAttr ps i = case Seq.lookup i (psInput ps) of
    Just (_ :@: x) -> x
    _ -> error "psAttr: invalid offset"

-- | Get the syntax object for a particular offset in a ParseStream
psValue :: ParseStream -> Int -> Maybe Token
psValue ps i = untag <$> Seq.lookup i (psInput ps)

-- | Get the length of a ParseStream
psLength :: ParseStream -> Int
psLength = Seq.length . psInput

-- | Throw a custom parser error in the current monad.
--   Note that you would normally use
--  `fail`, `empty`, in combination `expecting`, etc
throwParseExcept :: ParserMonad m => ParseExcept -> m a
throwParseExcept e = liftP $ Parser \_ _ -> Left e

-- | Catch a ParseFail in the current monad.
catchParseFail :: ParserMonad m => m a -> (Int -> Set String -> m a) -> m a
catchParseFail m f = m `catchParseExcept` \case
    ParseFail i msgs -> f i msgs
    e -> throwParseExcept e

-- | Catch a ParseError in the current monad.
catchParseError :: ParserMonad m => m a -> (Attr -> Doc () -> m a) -> m a
catchParseError m f = m `catchParseExcept` \case
    ParseError (msg :@: ie) -> f ie msg
    e -> throwParseExcept e

-- | Get the stream associated with the current Parser action
getStream :: ParserMonad m => m ParseStream
getStream = liftP $ Parser (curry Right)

-- | Get the token sequence associated with the current Parser action
getTokenSeq :: ParserMonad m => m (Seq (ATag Token))
getTokenSeq = psInput <$> getStream

-- | Get the file associated with the current Parser action
getFile :: ParserMonad m => m File
getFile = psFile <$> getStream

-- | Get the current offset in the current Parser action
modOffset :: ParserMonad m => (Int -> Int) -> m Int
modOffset f = liftP $ Parser \_ i -> let i' = f i in Right (i, i')

-- | Run a Parser action with an alternative for failure.
--   Note that this discards the expectations
--   of the first action, unlike `(<|>)`
recoverWith :: ParserMonad m => m a -> m a -> m a
recoverWith m1 m2 = m2 `catchParseFail` \_ _ -> m1

-- | Trigger a parser failure with a set of expected values
parseFail :: ParserMonad m => Set String -> m a
parseFail msgs = liftP $ Parser \_ i -> Left (ParseFail i msgs)

-- | Trigger a parser error with a message
parseError :: ParserMonad m => Doc () -> m a
parseError msg = liftP $ Parser \s i -> Left (ParseError (msg :@: psAttr s i))

-- | Trigger a parser failure at a given offset with a set of expected values.
--   Note that you would normally use `fail`, `empty`,
--   in combination with `expecting`, etc
parseFail' :: ParserMonad m => Int -> Set String -> m a
parseFail' i msgs = liftP $ Parser \_ _ -> Left (ParseFail i msgs)

-- | Trigger a parser error at a given Attr with a message.
--   Note that you would normally use `throwError`
parseError' :: ParserMonad m => Attr -> Doc () -> m a
parseError' a msg = liftP $ Parser \_ _ -> Left (ParseError (msg :@: a))

-- | Trigger a parser failure at the current offset
--   with a set of expected values.
failMulti :: ParserMonad m => [String] -> m a
failMulti msgs = liftP $ Parser \_ i -> Left (ParseFail i (Set.fromList msgs))

-- | Trigger a parser failure at the current offset
--   with a set of expected values.
failAtMulti :: ParserMonad m => Int -> [String] -> m a
failAtMulti i msgs = liftP $ Parser \_ _ ->
    Left (ParseFail i (Set.fromList msgs))

-- | Trigger a parser failure at a given offset with a single expectation
failAt :: ParserMonad m => Int -> String -> m a
failAt i msg = liftP $ Parser \_ _ -> Left (ParseFail i (Set.singleton msg))

-- | Trigger a parser error at a given Attr with a message
throwErrorAt :: ParserMonad m => Attr -> Doc () -> m a
throwErrorAt a m = liftP $ Parser \_ _ -> Left (ParseError (m :@: a))

-- | If the given parser fails,
--   replace its expectation set with the given message
expecting :: ParserMonad m => String -> m a -> m a
expecting msg p = catchParseFail p \_ _ -> fail msg

-- | If the given parser fails,
--   replace its expectation set and error location with those provided
expectingAt :: ParserMonad m => Int -> String -> m a -> m a
expectingAt msgLoc msg p = catchParseFail p \_ _ -> failAt msgLoc msg

-- | If the given parser fails, as does not provide expectations,
--   replace its expectation set with the given message
expectingDefault :: ParserMonad m => String -> m a -> m a
expectingDefault msg p = catchParseFail p \i -> \case
    Nil -> failAt i msg
    msgs -> liftP $ Parser \_ _ -> Left (ParseFail i msgs)

-- | If the given parser fails,
--   extend its expectation set with the given message
expecting' :: ParserMonad m => String -> m a -> m a
expecting' msg p = catchParseFail p \_ msgs ->
    failMulti (msg : Set.toList msgs)

-- | If the given parser fails,
--   replace its expectation set with the given one
expectingMulti :: ParserMonad m => [String] -> m a -> m a
expectingMulti msgs m = m `catchParseFail` \_ _ -> failMulti msgs

-- | If the given parser fails,
--   extend its expectation set with the given one
expectingMulti' :: ParserMonad m => [String] -> m a -> m a
expectingMulti' msgs m = m `catchParseFail` \_ msgs' ->
    failMulti (msgs <> Set.toList msgs')

-- | Formatting function for unexpected input, or expected input at EOF
formatInput :: ParseStream -> Int -> Doc ann
formatInput s ie = case psValue s ie of
    Just t -> text "unexpected token" <+> backticked t
    _ -> text "expected additional input"

-- | Formatting function for expected input
formatExpected :: Set String -> Doc ann
formatExpected Nil = text "expected additional input"
formatExpected msgs = text "expected" <+> lsep (text <$> Set.toList msgs)

-- | Formatting function used by `noFail` to produce an error message,
--   given either a set of expectations from the failure,
--   or a `formatInput` message
format :: ParseStream -> Int -> Set String -> Doc ann
format s i msgs =
    if Set.null msgs
        then formatInput s i
        else formatExpected msgs

-- | Run a parser, and if it fails, convert the failure to a parse error
noFail :: ParserMonad m => m a -> m a
noFail m = m `catchParseFail` \i msgs -> do
    s <- getStream
    parseError' (psAttr s i) (format s i msgs)

-- | Run a parser, and if it fails, convert the failure to a parse error;
--   unless the stream is at its end, in which case the failure is kept
noFailBeforeEof :: ParserMonad m => m a -> m a
noFailBeforeEof m =
    peek >>= \case
        TEof -> m
        _ -> noFail m

-- | Ensure that the parser consumes the remaining ParseStream
consumesAll :: ParserMonad m => m a -> m a
consumesAll m = do
    a <- m
    s <- getStream
    i <- getOffset
    if i >= psLength s
        then pure a
        else parseError' (psAttr s i) (formatInput s i)

-- | Fail with an expectation message if the condition is false
guardFail :: ParserMonad m => Bool -> String -> m ()
guardFail p expStr = if p then pure () else fail expStr

-- | Fail with an expectation set if the condition is false
guardMulti :: ParserMonad m => Bool -> [String] -> m ()
guardMulti p expStrs = if p then pure () else failMulti expStrs

-- | Fail at the given offset with an expectation set if the condition is false
guardMultiAt :: ParserMonad m => Bool -> Int -> Set String -> m ()
guardMultiAt p i expStrs = if p then pure () else parseFail' i expStrs

-- | Error with a message if the condition is false
assert :: ParserMonad m => Bool -> Doc () -> m ()
assert p expStr = if p then pure () else throwError expStr

-- | Error with a message at the given Attr if the condition is false
assertAt :: ParserMonad m => Bool -> Attr -> Doc () -> m ()
assertAt p a expStr = if p then pure () else throwErrorAt a expStr

-- | Get the current offset in the current parser monad
getOffset :: ParserMonad m => m Int
getOffset = modOffset id

-- | Set the current offset in the current parser monad
setOffset :: ParserMonad m => Int -> m ()
setOffset i = void $ modOffset (const i)


-- | Advance the offset in the current parser monad
advance :: ParserMonad m => m ()
advance = liftP $ Parser \_ i -> Right ((), i + 1)

-- | Get the currently selected element in the parse stream
peek :: ParserMonad m => m Token
peek = liftP $ Parser \s i ->
    case psValue s i of
        Just a -> Right (a, i)
        _ -> Left (ParseFail i (Set.singleton "additional input"))

-- | Get the currently selected element in the parse stream,
--   returning Nothing if the stream is empty
tryPeek :: ParserMonad m => m (Maybe Token)
tryPeek = liftP $ Parser \s i ->
    case psValue s i of
        Just a -> Right (Just a, i)
        _ -> Right (Nothing, i)

-- | Get a location Attr for the current position in the ParseStream
attr :: ParserMonad m => m Attr
attr = liftP $ Parser \s i -> Right (psAttr s i, i)

-- | Get a location Attr for the current position in the ParseStream,
--   and advance the stream offset
attrNext :: ParserMonad m => m Attr
attrNext = attr << optional advance

-- | Wraps the output of a parser in a Tag with an Attr for the range consumed
tag :: (ParserMonad m) => m a -> m (ATag a)
tag p = do
    i1 <- getOffset
    a <- p
    i2 <- getOffset
    (a :@:) <$> buildAttr i1 i2

buildAttr :: ParserMonad m => Int -> Int -> m Attr
buildAttr i1 i2 = do
    s <- getStream
    pure (psAttr s i1 <> psAttr s (i2 - 1))

-- | Execute @tag@ and discard the result, keeping only the Attr generated
attrOf :: ParserMonad m => m a -> m Attr
attrOf = fmap tagOf . tag

-- | Execute @tag@ and discard the result,
--   keeping only the first Pos of the Attr generated
posOf :: ParserMonad m => m a -> m Pos
posOf = fmap (rangeStart . attrRange) . attrOf

-- | Get the offset of the currently selected element in the parse stream
--   then execute the given parser, returning only the offset
offsetOf :: ParserMonad m => m a -> m Int
offsetOf = (getOffset <<)

-- | Get the currently selected element in the parse stream,
--   and advance the stream offset
next :: ParserMonad m => m Token
next = peek << advance

-- | Get the currently selected element's Attr in the parse stream
peekAttr :: ParserMonad m => m (ATag Token)
peekAttr = flip (:@:) <$> attr <*> peek

-- | Get the currently selected element's Attr in the parse stream,
--   and wrap it around the current element using Syn,
--   then advance the stream offset
nextAttr :: ParserMonad m => m (ATag Token)
nextAttr = flip (:@:) <$> attr <*> next

-- | Advance the stream offset if the current element satisfies the predicate,
--   returning the matched element
nextIf :: ParserMonad m => (Token -> Bool) -> m Token
nextIf p = do
    a <- peek
    if p a
        then a <$ advance
        else empty

-- | Advance the stream offset if the current element satisfies the predicate,
--   returning the matched element along with its Attr
nextIfAttr :: ParserMonad m => (ATag Token -> Bool) -> m (ATag Token)
nextIfAttr p = do
    t <- peekAttr
    if p t
        then t <$ advance
        else empty

-- | Advance the stream offset if the current element satisfies the predicate,
--   discarding the matched element
nextIf_ :: ParserMonad m => (Token -> Bool) -> m ()
nextIf_ p = do
    a <- peek
    if p a
        then advance
        else empty

-- | Advance the stream offset as long as
--   the current element satisfies the predicate,
--   returning the matched elements as a list
nextWhile :: ParserMonad m => (Token -> Bool) -> m [Token]
nextWhile p = some (nextIf p)

-- | Advance the stream offset as long as
--   the current element satisfies the predicate,
--   discarding the matched elements
nextWhile_ :: ParserMonad m => (Token -> Bool) -> m ()
nextWhile_ p = some_ (nextIf_ p)

-- | Advance the stream offset if the current element
--   satisfies a mapping predicate, returning the mapped value
nextMap :: ParserMonad m => (Token -> Maybe a) -> m a
nextMap p = do
    a <- peek
    case p a of
        Just a' -> a' <$ advance
        _ -> empty

-- | @nextIf_ (== e)@
expect' :: ParserMonad m => Token -> m ()
expect' e = nextIf_ (== e)

-- | @nextIf (`elem` es)@
expectAny' :: ParserMonad m => [Token] -> m Token
expectAny' es = nextIf (`elem` es)

-- | Consume an expected sequence of inputs
expectSeq' :: ParserMonad m => [Token] -> m ()
expectSeq' = traverse_ \e -> do
    a <- peek
    if a == e
        then advance
        else empty

-- | Consume one of any expected sequences of inputs
expectAnySeq' :: ParserMonad m => [[Token]] -> m [Token]
expectAnySeq' es = do
    asum (es <&> \e -> e <$ expectSeq' e)

-- | @expecting (prettyShow e) (expect' e)@
expect :: ParserMonad m => Token -> m ()
expect e = expecting (prettyShow e) (expect' e)

-- | @expectingMulti (prettyShow <$> es) (expectAny' es)@
expectAny :: ParserMonad m => [Token] -> m Token
expectAny es = expectingMulti (prettyShow <$> es) (expectAny' es)

-- | @expecting (prettyShow e) (expectSeq' e)@
expectSeq :: ParserMonad m => [Token] -> m ()
expectSeq e = expecting (prettyShow e) (expectSeq' e)

-- | @expectingMulti (prettyShow <$> es) (expectAnySeq' es)@
expectAnySeq :: ParserMonad m => [[Token]] -> m [Token]
expectAnySeq es = expectingMulti (prettyShow <$> es) (expectAnySeq' es)

-- | Expect a list of `m a` separated by `m s`.
--   The list must contain at least one element
listSome :: ParserMonad m => m s -> m a -> m [a]
listSome s p = do
    a <- p
    as <- many (s >> p)
    pure (a:as)

-- | Expect a list of `m a` separated by `m s`.
--   The list may be empty
listMany :: ParserMonad m => m s -> m a -> m [a]
listMany s p = recoverWith (pure []) (listSome s p)

-- | Use a predicate to grab a sequence of elements from the stream.
--   Fails if the sequence is empty
grabDomain :: ParserMonad m => (ATag Token -> Bool) -> m (Seq (ATag Token))
grabDomain f = do
    sq <- Seq.fromList <$> some (nextIfAttr f)
    a <- attr
    pure $ sq Seq.:|> Tag a TEof

-- | Grab a sequence of elements from the stream,
--   delimited by their indentation level relative to a given Attr.
--   Fails if this results in an empty sequence
grabWhitespaceDomain :: ParserMonad m => Attr -> m (Seq (ATag Token))
grabWhitespaceDomain = grabDomain . wsDominated

-- | A predicate-builder for @grabDomain@ that matches tokens with
--   a higher indentation level than that of the given Attr
wsDominated :: Attr -> Tag Attr a -> Bool
wsDominated l (_ :@: l') =
    let Pos _ l1 c1 = rangeStart (attrRange l)
        Pos _ l2 c2 = rangeStart (attrRange l')
    in (l1 <= l2 && c1 < c2)
    || (l1 == l2 && c1 <= c2)



-- | Perform syntactic analysis on a File, using the given Parser
parseFileWith :: Parser a -> File -> Either (Doc ()) a
parseFileWith p file = do
    toks <- L.lexFile file
    evalParser p file toks

-- | Perform syntactic analysis on a string, using the given Parser,
--   with errors reported to be in a file with the given name
parseByteStringWith :: Parser a -> String -> ByteString -> Either (Doc ()) a
parseByteStringWith p name content = parseFileWith p (File name content)

-- | Perform syntactic analysis on a string, using the given Parser,
--   with errors reported to be in a file with the given name
parseStringWith :: Parser a -> String -> String -> Either (Doc ()) a
parseStringWith p name = parseByteStringWith p name . fromString


-- | Evaluate a Parser on a given File, converting ParseErrors to Strings
evalParser :: Parser a -> File -> Seq (ATag Token) -> Either (Doc ()) a
evalParser px file toks =
    case runParser
        (noFail $ consumesAll (px << eof))
        (ParseStream toks file) 0 of
            Left (ParseError (msg :@: l)) -> Left $
                hang (text "syntax error" <+> (pPrint l <> text ":"))
                    msg
            Left _ -> undefined
            Right (a, _) -> Right a

-- | Evaluate a Parser on a sub-stream of Tokens.
--   The parser must consume all tokens (except eof)
recurseParser :: ParserMonad m => m a -> Seq (ATag Token) -> m a
recurseParser px toks = do
    p <- unliftP (noFail $ consumesAll (px << eof))
    liftP do
        f <- getFile
        case runParser p (ParseStream toks f) 0 of
            Left e -> liftP $ Parser \_ _ -> Left e
            Right (a, _) -> pure a


-- | Expect a Literal
literal :: ParserMonad m => m Literal
literal = expecting "literal" $ nextMap \case
    TLiteral lit -> Just lit
    _ -> Nothing

-- | Expect a Literal of Char
char :: ParserMonad m => m Char
char = expecting "character literal" $ nextMap \case
    TLiteral (LChar c) -> Just c
    _ -> Nothing

-- | Expect the Eof token
eof :: ParserMonad m => m ()
eof = nextMap \case
    TEof -> Just ()
    _ -> Nothing

-- | Expect a Literal of Int
int :: ParserMonad m => m Int
int = expecting "integer literal" $ nextMap \case
    TLiteral (LInt i) -> Just i
    _ -> Nothing

-- | Expect a Literal of Float
float :: ParserMonad m => m Float
float = expecting "float literal" $ nextMap \case
    TLiteral (LFloat f) -> Just f
    _ -> Nothing

-- | Expect a Literal of String
string :: ParserMonad m => m String
string = expecting "string literal" $ nextMap \case
    TLiteral (LString s) -> Just s
    _ -> Nothing

-- | Expect any symbol token
anySym :: ParserMonad m => m String
anySym = expecting "a symbol" $ nextMap \case
    TSymbol s -> Just s
    _ -> Nothing

-- | Expect a symbol token that is not reserved
unreserved :: ParserMonad m => m String
unreserved = expecting "an unreserved symbol" $ nextMap \case
    TSymbol s | not (isReserved s) -> Just s
    _ -> Nothing

-- | Expect a symbol token with a specific value
sym :: ParserMonad m => String -> m ()
sym s = expecting s $ nextMap \case
    TSymbol s' | s == s' -> Just ()
    _ -> Nothing

-- | Expect a symbol token with a specific value from a given set
symOf :: ParserMonad m => [String] -> m String
symOf ss = expectingMulti ss $ nextMap \case
    TSymbol s | s `elem` ss -> Just s
    _ -> Nothing

-- | Try and parse a symbol, returning a boolean indicating success
trySym :: ParserMonad m => String -> m Bool
trySym s = peek >>= \case
    TSymbol s' | s == s' -> True <$ advance
    _ -> pure False


-- | Expect a given Parser to be surrounded with parentheses
parens :: ParserMonad m => m a -> m a
parens px = do
    a <- attrOf (sym "(")
    noFail do
        px << expecting (") to close ( at " <> prettyShow a) do
            sym ")"

-- | Expect a given Parser to be surrounded with braces
braces :: ParserMonad m => m a -> m a
braces px = do
    a <- attrOf (sym "{")
    noFail do
        px << expecting ("} to close { at " <> prettyShow a) do
            sym "}"

-- | Expect a given Parser to be surrounded with brackets
brackets :: ParserMonad m => m a -> m a
brackets px = do
    a <- attrOf (sym "[")
    noFail do
        px << expecting ("] to close [ at " <> prettyShow a) do
            sym "]"

-- | Expect a given Parser's first input token to be directly adjacent,
--   in terms of line and column position, to the previous token consumed.
--   Note that if this is used on the first token in an input it has no effect
connected :: ParserMonad m => m a -> m a
connected p = do
    f <- getFile
    px <- unliftP p
    liftP $ Parser \s i ->
        let a = psAttr s (i - 1)
            a' = psAttr s i
        in if
        | i == 0 -> runParser px s i
        | attrConnected a a' -> runParser px s i
        | otherwise -> runParser px (ParseStream mempty f) i `catchError` do
            Left . \case
                ParseFail j Nil ->
                    (ParseFail j (Set.singleton "a connected token"))
                err -> err
