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
import Control.Monad.State.Class
import Control.Monad.Except


newtype FileT m a
    = FileT
    ( ReaderT FilePath m a )
    deriving
    ( Functor, Applicative, Monad, Alternative, MonadPlus
    , MonadFail, MonadIO, MonadTrans
    )

deriving instance MonadState s m => MonadState s (FileT m)
deriving instance MonadWriter w m => MonadWriter w (FileT m)
deriving instance MonadError e m => MonadError e (FileT m)
deriving instance MonadParser i m => MonadParser i (FileT m)

instance MonadReader r m => MonadReader r (FileT m) where
    ask = lift ask
    local f (FileT (ReaderT m)) = FileT (ReaderT (local f . m))
    reader = lift . reader


runFileT :: FileT m a -> FilePath -> m a
runFileT (FileT m) = runReaderT m

instance Monad m => MonadFile (FileT m) where
    withFilePath f (FileT m) = FileT $ ReaderT \_ -> runReaderT m f
    getFilePath = FileT (ReaderT pure)
