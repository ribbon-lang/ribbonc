module Control.Monad.File.Class where

import Control.Monad.State.Strict qualified as Strict
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.Writer.Strict qualified as Strict
import Control.Monad.Writer.Lazy qualified as Lazy
import Control.Monad.Reader
import Control.Monad.Except
import Control.Has




type instance Has m (FilePath ': effs) = (MonadFile m, Has m effs)



class Monad m => MonadFile m where
    withFilePath :: FilePath -> m a -> m a
    getFilePath :: m FilePath


instance MonadFile m => MonadFile (Strict.StateT s m) where
    withFilePath fp (Strict.StateT m) = Strict.StateT (withFilePath fp . m)
    getFilePath = lift getFilePath

instance MonadFile m => MonadFile (Lazy.StateT s m) where
    withFilePath fp (Lazy.StateT m) = Lazy.StateT (withFilePath fp . m)
    getFilePath = lift getFilePath

instance (Monoid w, MonadFile m) => MonadFile (Strict.WriterT w m) where
    withFilePath fp (Strict.WriterT m) = Strict.WriterT (withFilePath fp m)
    getFilePath = lift getFilePath

instance (Monoid w, MonadFile m) => MonadFile (Lazy.WriterT w m) where
    withFilePath fp (Lazy.WriterT m) = Lazy.WriterT (withFilePath fp m)
    getFilePath = lift getFilePath

instance MonadFile m => MonadFile (ReaderT r m) where
    withFilePath fp (ReaderT m) = ReaderT (withFilePath fp . m)
    getFilePath = lift getFilePath

instance MonadFile m => MonadFile (ExceptT e m) where
    withFilePath fp (ExceptT m) = ExceptT (withFilePath fp m)
    getFilePath = lift getFilePath
