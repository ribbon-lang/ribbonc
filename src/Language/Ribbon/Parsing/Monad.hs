module Language.Ribbon.Parsing.Monad where

import Data.Foldable
import Data.Functor

import Data.Attr
import Data.Nil
import Data.Tag
import Data.Diagnostic
import Data.SyntaxError

import Control.Has
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Dynamic
import Control.Monad.Error.Dynamic
import Control.Monad.Writer.Dynamic
import Control.Monad.Reader.Dynamic.Class
import Control.Monad.State.Dynamic

import Text.Pretty

import Language.Ribbon.Util
import Language.Ribbon.Analysis.Context
import Language.Ribbon.Syntax.Ref
import Language.Ribbon.Analysis.Diagnostics
import Language.Ribbon.Lexical.Name




-- | Parsing monad
newtype ParserT i m a
    = ParserT
    ( StateT i m a )
    deriving
        ( Functor, Applicative, Monad
        , MonadIO
        , MonadTrans
        )

deriving instance MonadReader r m => MonadReader r (ParserT i m)
deriving instance MonadError e m => MonadError e (ParserT i m)
deriving instance MonadWriter w m => MonadWriter w (ParserT i m)

instance ( MonadError SyntaxError m, MonadFile m
         , ParseInput i
         )
    => MonadPlus (ParserT i m)
instance ( MonadError SyntaxError m, MonadFile m
         , ParseInput i
         )
    => Alternative (ParserT i m) where
        empty = ParserT $ StateT \ts -> do
            fp <- getFilePath
            throwError $
                SyntaxError Recoverable $ formatInput fp ts
        ParserT (StateT a) <|> ParserT (StateT b) = ParserT $ StateT \ts ->
            a ts `catchError` \case
                SyntaxError Recoverable fa ->
                    b ts `catchError` \case
                        SyntaxError Recoverable fb ->
                            throwError $ SyntaxError Recoverable (fa <> fb)
                        y -> throwError y
                x -> throwError x

instance (ParseInput i, MonadError SyntaxError m, MonadFile m)
    => MonadFail (ParserT i m) where
        fail msg = do
            fp <- getFilePath
            ts <- getParseState
            throwError $ SyntaxError Unrecoverable $
                SingleFailure (text msg) :@: attrInput fp ts

instance MonadState s m => MonadState s (ParserT i m) where
    state = ParserT . lift . state

instance (ParseInput i, MonadError SyntaxError m, MonadFile m)
    => MonadParser i (ParserT i m) where
        parseState f = ParserT $ StateT (pure . f)



-- | Unwrap a @ParserT i m a@ to a function @i -> m (a, i)@
runParserT ::
    ParserT i m a -> i -> m (a, i)
runParserT (ParserT m) = runStateT m


-- | @flip runParserT@
runParserT' ::
    i -> ParserT i m a -> m (a, i)
runParserT' = flip runParserT

-- | Evaluate a @Parser@ on a given @ParseInput@,
--   discarding the final input after ensuring it @isNil@
--   (ie, has been consumed completely)
evalParserT :: (ParseInput i, MonadError SyntaxError m, MonadFile m) =>
    ParserT i m a -> i -> m a
evalParserT px toks =
    fst <$> runParserT (consumesAll px) toks

-- | @flip evalParserT@
evalParserT' :: (ParseInput i, MonadError SyntaxError m, MonadFile m) =>
    i -> ParserT i m a -> m a
evalParserT' = flip evalParserT






-- | Marker for @Has@, ie @Has m '[Parses input]@ ~ @MonadParser input m@
data Parses a

type instance Has m (Parses i ': effs) = (MonadParser i m, Has m (With '[ParseInput i] ': effs))


-- | The class of types that can be used as input for a @ParserT@
class (Eq (InputElement s), Pretty (InputElement s), Nil s)
    => ParseInput s where
        -- | The elements of a @ParseInput@, ie @Token@ for @TokenSeq@
        type InputElement s
        -- | Create a tagged @SyntaxFail@ for the given input and @FilePath@,
        --   either an eof or an unexpected character
        formatInput :: FilePath -> s -> ATag SyntaxFail
        -- | Get the next element of the input, or a @DecodeProblem@
        unconsInput :: s -> Either DecodeProblem (InputElement s, s)
        -- | Get the @Attr@ for the current input element,
        --   or for the input itself at eof
        attrInput :: FilePath -> s -> Attr
        -- | Get the @Attr@ for the range between two @ParseInput@s
        attrInputDiff :: FilePath -> s -> s -> Attr


-- | @MonadError SyntaxError@ + @MonadFail@
type MonadSyntaxError m = (MonadError SyntaxError m, MonadFail m)

type instance Has m (SyntaxError ': effs) = (MonadSyntaxError m, Has m effs)

type MonadParserBase x m
    = ( MonadPlus m
      , MonadSyntaxError m, MonadFile m
      , ParseInput x
      )

-- | The class of monads that can be used as a parser,
--   see @ParserT@ for the base transformer, see @ParseInput@ for valid inputs
class MonadParserBase x m => MonadParser x m | m -> x where
        -- | Read/Write/Modify the @ParseInput@
        --   associated with the current parser action
        parseState :: (x -> (a, x)) -> m a

instance MonadParser x m => MonadParser x (StateT s m) where
    parseState = lift . parseState

instance (Monoid w, MonadParser x m) => MonadParser x (WriterT w m) where
    parseState = lift . parseState

instance MonadParser x m => MonadParser x (ReaderT r m) where
    parseState = lift . parseState





-- | Take the current @ParseInput@ state, and replace it with @Nil@
takeParseState :: MonadParser x m => m x
takeParseState = parseState (, Nil)

-- | Get a copy of the current @ParseInput@ state
getParseState :: MonadParser x m => m x
getParseState = parseState \ts -> (ts, ts)

-- | Replace the current @ParseInput@ state with the given one
putParseState :: MonadParser x m => x -> m ()
putParseState ts = parseState $ const ((), ts)

-- | Modify the current @ParseInput@ state with a map function
modifyParseState :: MonadParser x m => (x -> x) -> m ()
modifyParseState f = parseState \ts -> ((), f ts)

-- | Discard the current @ParseInput@ state
discardParseState :: MonadParser x m => m ()
discardParseState = putParseState Nil



-- | Evaluate a parser on a new input.
--   The parser must consume all of the new input, or fail
recurseParserAll :: MonadParser x m => m a -> x -> m a
recurseParserAll px toks = do
    ts <- parseState (, toks)
    a <- catchError @SyntaxError (consumesAll px) \e -> do
        putParseState ts
        throwError e
    a <$ putParseState ts

-- | Evaluate a parser on a new input.
recurseParser :: MonadParser x m => m a -> x -> m (a, x)
recurseParser px toks = do
    ts <- parseState (, toks)
    a <- catchError @SyntaxError px \e -> do
        putParseState ts
        throwError e
    ts' <- parseState (, ts)
    pure (a, ts')


-- | Run a parser, and if it fails, make the @SyntaxError@ unrecoverable
noFail :: MonadParser x m => m a -> m a
noFail m = m `catchError` \(SyntaxError _ f) ->
    throwError $ SyntaxError Unrecoverable f

-- | Run a parser, and if it fails, make the @SyntaxError@ unrecoverable
--   when the given condition is @True@
noFailIf :: MonadParser x m => Bool -> m a -> m a
noFailIf p m = if p then noFail m else m

-- | Ensure that the given parser action consumes the remaining input
consumesAll :: MonadParser x m => m a -> m a
consumesAll m = do
    a <- m
    ts <- getParseState
    if isNil ts
        then pure a
        else do
            fp <- getFilePath
            throwError $ SyntaxError Recoverable $ formatInput fp ts


-- | Trigger an unrecoverable @SyntaxError@
--   with a message at the current location
parseError :: MonadParser x m => Recoverability -> Doc -> m a
parseError r msg = attr >>= \at -> parseErrorAt at r msg

-- | Trigger an unrecoverable @SyntaxError@
--   with a message at the given @Attr@
parseErrorAt :: MonadParser x m => Attr -> Recoverability -> Doc -> m a
parseErrorAt x r msg =
    throwError $ SyntaxError r $ SingleFailure msg :@: x


-- | Trigger an unrecoverable @SyntaxError@
--   with a message at the current location,
--   if the given condition is false
assert :: MonadParser x m => Bool -> Recoverability -> Doc -> m ()
assert p r expStr = unless p (parseError r expStr)

-- | Trigger an unrecoverable @SyntaxError@
--   with a message at the given @Attr@,
--   if the condition is false
assertAt :: MonadParser x m => Attr -> Bool -> Recoverability -> Doc -> m ()
assertAt x p r expStr = unless p (parseErrorAt x r expStr)




-- | Advance the @ParseInput@ in the current parser action
advance :: MonadParser x m => m ()
advance = do
    ps <- getParseState
    case unconsInput ps of
        Left e -> do
            fp <- getFilePath
            throwError $ SyntaxError Recoverable $
                formatProblem (attrInput fp ps) e
        Right (_, ps') -> putParseState ps'


-- | Get the currently selected @InputElement@ in the parser action
peek :: MonadParser x m => m (InputElement x)
peek = do
    ps <- getParseState
    case unconsInput ps of
        Left e -> do
            fp <- getFilePath
            throwError $ SyntaxError Recoverable $
                formatProblem (attrInput fp ps) e
        Right (t, _) -> pure t

-- | Get the currently selected @InputElement@ in the parser action,
--   returning @Nothing@ if the input is @Nil@
tryPeek :: MonadParser x m => m (Maybe (InputElement x))
tryPeek = do
    ps <- getParseState
    pure $ case unconsInput ps of
        Left _ -> Nothing
        Right (t, _) -> Just t

-- | Get a location @Attr@ for the selected position in the parser action
attr :: MonadParser x m => m Attr
attr = liftA2 attrInput getFilePath getParseState



-- | Wraps the output of a parser in an @ATag@ for the range consumed
tag :: MonadParser x m => m a -> m (ATag a)
tag p = do
    ps <- getParseState
    a <- p
    fp <- getFilePath
    ps' <- getParseState
    pure $ a :@: attrInputDiff fp ps ps'

-- | Execute @tag@ and discard the result, keeping only the @Attr@ generated
attrOf :: MonadParser x m => m a -> m Attr
attrOf = fmap tagOf . tag


-- | Get the currently selected @InputElement@ in the parser action,
--   and advance the stream offset
next :: MonadParser x m => m (InputElement x)
next = peek << advance

-- | Get the currently selected @InputElement@'s @Attr@ in the parser action
peekAttr :: MonadParser x m => m (ATag (InputElement x))
peekAttr = peek <*@*> attr

-- | Get the currently selected @InputElement@'s @Attr@ in the parser action,
--   and wrap it around the current @InputElement@ using @Tag@,
--   then advance the stream offset
nextAttr :: MonadParser x m => m (ATag (InputElement x))
nextAttr = tag next

-- | Advance the parser stream offset,
--   if the current @InputElement@ satisfies the predicate;
--   returning the matched @InputElement@
nextIf :: MonadParser x m => (InputElement x -> Bool) -> m (InputElement x)
nextIf p = do
    a <- peek
    if p a
        then a <$ advance
        else empty

-- | Advance the parser stream offset,
--   if the current @InputElement@ satisfies the predicate;
--   returning the matched @InputElement@ along with its @Attr@
nextIfAttr :: MonadParser x m =>
    (ATag (InputElement x) -> Bool) -> m (ATag (InputElement x))
nextIfAttr p = do
    t <- peekAttr
    if p t
        then t <$ advance
        else empty

-- | Advance the parser stream offset,
--   if the current @InputElement@ satisfies the predicate;
--   discarding the matched @InputElement@
nextIf_ :: MonadParser x m => (InputElement x -> Bool) -> m ()
nextIf_ p = do
    a <- peek
    if p a
        then advance
        else empty

-- | Advance the parser stream offset,
--   as long as the current @InputElement@ satisfies the predicate;
--   returning the matched @InputElement@s as a list
nextWhile :: MonadParser x m => (InputElement x -> Bool) -> m [InputElement x]
nextWhile p = some (nextIf p)

-- | Advance the parser stream offset,
--   as long as the current @InputElement@ satisfies the predicate;
--   returning the matched @InputElement@s as a list
nextWhileAttr :: MonadParser x m =>
    (ATag (InputElement x) -> Bool) -> m [ATag (InputElement x)]
nextWhileAttr p = some (nextIfAttr p)

-- | Advance the parser stream offset,
--   as long as the current @InputElement@ satisfies the predicate;
--   discarding the matched @InputElement@s
nextWhile_ :: MonadParser x m => (InputElement x -> Bool) -> m ()
nextWhile_ p = some_ (nextIf_ p)

-- | Advance the parser stream offset,
--   if the current @InputElement@ satisfies a mapping predicate;
--   returning the mapped value
nextMap :: MonadParser x m => (InputElement x -> Maybe a) -> m a
nextMap p = do
    x <- peek
    case p x of
        Just a -> a <$ advance
        _ -> empty


-- | Wrap the @SyntaxError@s of a given parser
--   in an explanation of the expectation that was had of it
expecting :: MonadParser x m => Doc -> m a -> m a
expecting msg px = px `catchError` \(SyntaxError r f) -> do
    fp <- getFilePath
    ts <- getParseState
    throwError $ SyntaxError r $ ExpectationFailure [msg] f :@: attrInput fp ts

-- | Wrap the @SyntaxError@s of a given parser
--   in an explanation of the expectations that were had of it
expectingMulti :: MonadParser x m => [Doc] -> m a -> m a
expectingMulti = expecting . lsep


-- | Wrap the @SyntaxError@s of a given parser
--   in an explanation of the expectation that was had of it
expectingAt :: MonadParser x m => Attr -> Doc -> m a -> m a
expectingAt at msg px = px `catchError` \(SyntaxError r f) ->
    throwError $ SyntaxError r $ ExpectationFailure [msg] f :@: at

-- | Wrap the @SyntaxError@s of a given parser
--   in an explanation of the expectations that were had of it
expectingMultiAt :: MonadParser x m => Attr -> [Doc] -> m a -> m a
expectingMultiAt at = expectingAt at . lsep


-- | @nextIf_ (== e)@
expect' :: MonadParser x m => InputElement x -> m ()
expect' e = nextIf_ (== e)

-- | @nextIf (`elem` es)@
expectAny' :: MonadParser x m => [InputElement x] -> m (InputElement x)
expectAny' es = nextIf (`elem` es)

-- | Consume an expected sequence of inputs
expectSeq' :: MonadParser x m => [InputElement x] -> m ()
expectSeq' = traverse_ \e -> do
    a <- peek
    if a == e
        then advance
        else empty

-- | Consume one of any expected sequences of inputs
expectAnySeq' :: MonadParser x m => [[InputElement x]] -> m [InputElement x]
expectAnySeq' es = do
    asum (es <&> \e -> e <$ expectSeq' e)


-- | @expecting (pPrint e) (expect' e)@
expect :: MonadParser x m => InputElement x -> m ()
expect e = expecting (pPrint e) (expect' e)

-- | @expectingMulti (pPrint <$> es) (expectAny' es)@
expectAny :: MonadParser x m => [InputElement x] -> m (InputElement x)
expectAny es = expectingMulti (pPrint <$> es) (expectAny' es)

-- | @expecting (pPrint e) (expectSeq' e)@
expectSeq :: MonadParser x m => [InputElement x] -> m ()
expectSeq e = expecting (pPrint e) (expectSeq' e)

-- | @expectingMulti (pPrint <$> es) (expectAnySeq' es)@
expectAnySeq :: MonadParser x m => [[InputElement x]] -> m [InputElement x]
expectAnySeq es = expectingMulti (pPrint <$> es) (expectAnySeq' es)

-- | expect the end of the input
eof :: MonadParser x m => m ()
eof = expecting "end of input" do
    ps <- getParseState
    unless (isNil ps) do
        fp <- getFilePath
        throwError $ SyntaxError Recoverable $
            formatInput fp ps

-- | Run a parser without consuming input
lookahead :: MonadParser x m => m a -> m a
lookahead p = do
    ps <- getParseState
    a <- p
    a <$ putParseState ps

-- | Run a parser without consuming input,
--   and fail if it succeeds
negativeLookahead :: MonadParser x m => m a -> m ()
negativeLookahead p = do
    ps <- getParseState
    ok <- catchError @SyntaxError (False <$ p) \_ -> True <$ putParseState ps
    unless ok do
        fp <- getFilePath
        throwError $ SyntaxError Recoverable $ formatInput fp ps


-- | Ensure the output of a parser comes from a range connected to the given one
connected :: MonadParser x m => Attr -> m a -> m a
connected at lx = do
    x :@: at' <- tag lx
    x <$ assertAt at' (attrConnected at at') Recoverable do
        "expected an element connected to the previous"

-- | Ensure each successive output of a parser comes
--   from a range connected to the previous one, starting with the given one.
--   Equivalent to `some` (ie one or more results is required)
connectSome :: MonadParser x m => Attr -> m a -> m [a]
connectSome at lx = do
    a :@: at' <- tag (connected at lx)
    (a :) <$> connectMany at' lx

-- | Ensure each successive output of a parser comes
--   from a range connected to the previous one, starting with the given one.
--   Equivalent to `many` (ie zero or more results are allowed)
connectMany :: MonadParser x m => Attr -> m a -> m [a]
connectMany at lx = option [] (connectSome at lx)

-- | @catchError@ for @Recoverable@ failures only
catchRecoverable :: MonadParser x m => m a -> (ATag SyntaxFail -> m a) -> m a
catchRecoverable px h = px `catchError` \case
    SyntaxError Recoverable f -> h f
    e -> throwError e



liftedSyntaxErrorRef :: Has m
    [ Parses x, FixName, Diag, Ref, With '[ParseInput x] ] =>
        DiagnosticBinderKind -> Maybe String ->
            WriterT [Diagnostic] m () -> m ()
liftedSyntaxErrorRef b name m =
    liftingSyntaxErrorRef b name m discardParseState pure


liftedSyntaxErrorRefName :: Has m
    [ Parses x, FixName, Diag, Ref, With '[ParseInput x] ] =>
        DiagnosticBinderKind ->
            WriterT [Diagnostic] m () -> m ()
liftedSyntaxErrorRefName b m =
    liftingSyntaxErrorRefName b m discardParseState pure


liftSyntaxErrorT :: MonadDiagnostics m =>
    DiagnosticBinder -> ErrorT SyntaxError m () -> m ()
liftSyntaxErrorT b m =
    runErrorT m >>= liftEitherHandler (reportFull . diagnosticFromSyntaxError b)

liftSyntaxError :: MonadDiagnostics m =>
    DiagnosticBinder -> SyntaxError -> m ()
liftSyntaxError b =
    reportFull . diagnosticFromSyntaxError b

liftSyntaxErrorUndef :: MonadDiagnostics m =>
    DiagnosticBinder -> SyntaxError -> m a
liftSyntaxErrorUndef b err =
    error "used undefined value from liftSyntaxErrorUndef"
        <$ reportFull (diagnosticFromSyntaxError b err)

liftingSyntaxError :: (MonadError SyntaxError m, MonadWriter [Diagnostic] m) =>
    DiagnosticBinder -> WriterT [Diagnostic] m t -> m b -> (t -> m b) -> m b
liftingSyntaxError b m failure success = do
    (x, diag) <- runWriterT do
        m `catchError` liftSyntaxErrorUndef b

    reportAll diag

    if any isError diag
        then failure
        else success x

liftedSyntaxError :: Has m [ Parses x, Diag, Ref, With '[ParseInput x] ] =>
    DiagnosticBinder -> WriterT [Diagnostic] m () -> m ()
liftedSyntaxError b m = liftingSyntaxError b m discardParseState pure


liftSyntaxErrorRef :: Has m [Ref, Diag] =>
    DiagnosticBinderKind -> Maybe String -> SyntaxError -> m ()
liftSyntaxErrorRef kind name e = do
    ref <- getRef
    liftSyntaxError
        DiagnosticBinder
            { kind = kind
            , ref = ref
            , name = name
            }
        e


liftingSyntaxErrorRef :: Has m [Ref, Err SyntaxError, Diag] =>
    DiagnosticBinderKind -> Maybe String ->
        WriterT [Diagnostic] m t -> m b -> (t -> m b) -> m b
liftingSyntaxErrorRef kind name m failure success = do
    currentLocation <- getRef
    (x, diag) <- runWriterT do
        m `catchError` liftSyntaxErrorUndef
            (DiagnosticBinder kind currentLocation name)

    reportAll diag

    if any isError diag
        then failure
        else success x


liftSyntaxErrorRefName :: Has m [Ref, FixName, Diag] =>
    DiagnosticBinderKind -> SyntaxError -> m ()
liftSyntaxErrorRefName kind e = do
    name <- getFixName
    liftSyntaxErrorRef kind (Just $ prettyShow name) e

liftingSyntaxErrorRefName :: Has m [Ref, FixName, Err SyntaxError, Diag] =>
    DiagnosticBinderKind ->
        WriterT [Diagnostic] m t -> m b -> (t -> m b) -> m b
liftingSyntaxErrorRefName kind m failure success = do
    name <- getFixName
    currentLocation <- getRef
    (x, diag) <- runWriterT do
        m `catchError` liftSyntaxErrorUndef
            (DiagnosticBinder kind currentLocation (Just $ prettyShow name))

    reportAll diag

    if any isError diag
        then failure
        else success x
