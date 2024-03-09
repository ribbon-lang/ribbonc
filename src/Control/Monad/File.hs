module Control.Monad.File
    ( module X
    , FileT, runFileT
    ) where

import Control.Applicative
import Control.Monad.Trans

import Control.Monad.Reader
import Control.Monad.File.Class as X
import Control.Monad.Writer.Class
import Control.Monad.Parser.Class
import Control.Monad.Diagnostics.Class
import Control.Monad.State.Class
import Control.Monad.Context.Class
import Control.Monad.Builder.Class
import Control.Monad.Error.Class


newtype FileT m a
    = FileT
    ( ReaderT FilePath m a )
    deriving
    ( Functor, Applicative, Monad, Alternative, MonadPlus
    , MonadFail, MonadIO, MonadTrans
    , MonadDiagnostics
    )

deriving instance MonadState s m => MonadState s (FileT m)
deriving instance MonadWriter w m => MonadWriter w (FileT m)
deriving instance MonadError e m => MonadError e (FileT m)
deriving instance MonadParser i m => MonadParser i (FileT m)
deriving instance MonadContext c m => MonadContext c (FileT m)
deriving instance MonadBuilder s m => MonadBuilder s (FileT m)

instance MonadReader r m => MonadReader r (FileT m) where
    ask = lift ask
    local f (FileT (ReaderT m)) = FileT (ReaderT (local f . m))
    reader = lift . reader

instance Monad m => MonadFile (FileT m) where
    withFilePath f (FileT m) = FileT $ ReaderT \_ -> runReaderT m f
    getFilePath = FileT (ReaderT pure)


runFileT :: FileT m a -> FilePath -> m a
runFileT (FileT m) = runReaderT m
