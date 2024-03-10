module Control.Monad.File.Class where

import Control.Monad.Trans.Dynamic
import Control.Monad.State.Dynamic
import Control.Monad.Writer.Dynamic
import Control.Monad.Reader.Dynamic
import Control.Monad.Error.Dynamic
import Control.Has




type instance Has m (FilePath ': effs) = (MonadFile m, Has m effs)



class Monad m => MonadFile m where
    withFilePath :: FilePath -> m a -> m a
    getFilePath :: m FilePath


instance MonadFile m => MonadFile (StateT s m) where
    withFilePath fp (StateT m) = StateT (withFilePath fp . m)
    getFilePath = lift getFilePath


instance (Monoid w, MonadFile m) => MonadFile (WriterT w m) where
    withFilePath fp (WriterT m) = WriterT (withFilePath fp m)
    getFilePath = lift getFilePath

instance MonadFile m => MonadFile (ReaderT r m) where
    withFilePath fp (ReaderT m) = ReaderT (withFilePath fp . m)
    getFilePath = lift getFilePath

instance MonadFile m => MonadFile (ErrorT e m) where
    withFilePath fp (ErrorT m) = ErrorT (withFilePath fp m)
    getFilePath = lift getFilePath
