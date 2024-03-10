module Control.Monad.Context
    ( module X
    , ContextT
    , runContextT
    ) where


import Control.Applicative
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Reader
import Control.Monad.Error.Class

import Control.Monad.File.Class
import Control.Monad.Parser.Class
import Control.Monad.Diagnostics.Class
import Control.Monad.Builder.Class
import Control.Monad.Context.Class as X



-- | Transformer for building things like module definitions
--   this is essentially a @ReaderT@, see @MonadContext@ for differences
newtype ContextT c m a
    = ContextT
    ( ReaderT c m a )
    deriving
        ( Functor, Applicative, Monad, Alternative, MonadPlus
        , MonadTrans, MonadFail, MonadIO
        , MonadDiagnostics, MonadFile
        )

deriving instance MonadWriter w m => MonadWriter w (ContextT c m)
deriving instance MonadState s m => MonadState s (ContextT c m)
deriving instance MonadError e m => MonadError e (ContextT c m)
deriving instance MonadParser i m => MonadParser i (ContextT c m)
deriving instance MonadBuilder s m => MonadBuilder s (ContextT c m)

instance MonadReader r m => MonadReader r (ContextT c m) where
    ask = lift ask
    local f (ContextT (ReaderT m)) = ContextT (ReaderT (local f . m))

instance Monad m => MonadContext c (ContextT c m) where
    getContext = ContextT ask
    localContext f (ContextT m) = ContextT (local f m)

instance {-# OVERLAPPABLE #-} MonadContext c2 m => MonadContext c2 (ContextT c1 m) where
    getContext = lift getContext
    localContext f (ContextT m) = ContextT (localContext f m)


runContextT :: ContextT c m a -> c -> m a
runContextT (ContextT m) = runReaderT m
