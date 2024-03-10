module Control.Monad.Parser
    ( module X
    , ParserT, runParserT, evalParserT
    ) where

import Data.Tag
import Data.SyntaxError

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Dynamic
import Control.Monad.Error.Dynamic.Class
import Control.Monad.Reader.Dynamic.Class
import Control.Monad.State.Dynamic
import Control.Monad.Writer.Dynamic.Class

import Control.Monad.Parser.Class as X

import Control.Monad.File
import Control.Monad.Diagnostics.Class

import Text.Pretty



-- | Parsing monad
newtype ParserT i m a
    = ParserT
    ( StateT i m a )
    deriving
        ( Functor, Applicative, Monad
        , MonadFile
        , MonadDiagnostics
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

-- | Evaluate a @Parser@ on a given @ParseInput@,
--   discarding the final input after ensuring it @isNil@
--   (ie, has been consumed completely)
evalParserT :: (ParseInput i, MonadError SyntaxError m, MonadFile m) =>
    ParserT i m a -> i -> m a
evalParserT px toks =
    fst <$> runParserT (consumesAll px) toks

