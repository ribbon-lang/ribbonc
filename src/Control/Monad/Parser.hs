module Control.Monad.Parser where

import Data.Tag
import Data.SyntaxError

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Strict
import Control.Monad.Writer.Class

import Control.Monad.Parser.Class

import Control.Monad.File
import Control.Monad.Diagnostics

import Text.Pretty




-- | A shortcut alias for a basic parser with a @FileT@ and @Either SyntaxError@
type Parser i = ParserT i (FileT (Either SyntaxError))

-- | Parsing monad
newtype ParserT i m a
    = ParserT
    ( StateT i m a )
    deriving
        ( Functor, Applicative, Monad
        , MonadFile
        , MonadIO
        , MonadTrans
        )

deriving instance MonadReader r m => MonadReader r (ParserT i m)
deriving instance MonadError e m => MonadError e (ParserT i m)
deriving instance MonadWriter w m => MonadWriter w (ParserT i m)
deriving instance MonadDiagnostics m => MonadDiagnostics (ParserT i m)

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
    get = ParserT (lift get)
    put = ParserT . lift . put
    state = ParserT . lift . state

instance (ParseInput i, MonadError SyntaxError m, MonadFile m)
    => MonadParse i (ParserT i m) where
        parseState f = ParserT $ StateT (pure . f)



-- | Unwrap a @ParserT i m a@ to a function @i -> m (a, i)@
runParserT ::
    ParserT i m a -> i -> m (a, i)
runParserT (ParserT m) = runStateT m

-- | Evaluate a @Parser@ on a given @ParseInput@,
--   discarding the final input after ensuring it @isNil@
--   (ie, has been consumed completely)
evalParserT :: (ParseInput i, MonadSyntaxError m, MonadFile m) =>
    ParserT i m a -> i -> m a
evalParserT px toks =
    fst <$> runParserT (consumesAll px) toks




-- | Run a parser on a given @FilePath@ and @ParseInput@
runParser :: Parser i a -> FilePath -> i -> Either SyntaxError (a, i)
runParser px fp toks = runFileT (runParserT px toks) fp

-- | Evaluate a parser on a given @FilePath@ and @ParseInput@,
--   discarding the final input after ensuring it @isNil@
evalParser :: ParseInput i =>
    Parser i a -> FilePath -> i -> Either SyntaxError a
evalParser px fp toks = fst <$> runParser (consumesAll px) fp toks
