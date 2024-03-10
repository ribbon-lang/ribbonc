module Control.Monad.Context.Class where

import Control.Monad.State.Strict qualified as Strict
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.Writer.Strict qualified as Strict
import Control.Monad.Writer.Lazy qualified as Lazy
import Control.Monad.Reader
import Control.Monad.Except
import Control.Has




-- | Marker for @Has@, ie @Has m '[Ctx c]@ ~ @MonadContext c m@
data Ctx a

type instance Has m (Ctx c ': effs) = (MonadContext c m, Has m effs)



-- | Transformer class for building things like definitions;
--   this is functionally identical to @MonadReader@,
--   without the functional dependency
class Monad m => MonadContext c m where
    -- | Read the @c@ of a @MonadContext c m@
    getContext :: m c
    -- | Modify the @c@ of a @MonadContext c m@ for some @m a@
    localContext :: (c -> c) -> m a -> m a

instance MonadContext c m => MonadContext c (Strict.StateT s m) where
    getContext = lift getContext
    localContext f (Strict.StateT m) = Strict.StateT (localContext f . m)

instance MonadContext c m => MonadContext c (Lazy.StateT s m) where
    getContext = lift getContext
    localContext f (Lazy.StateT m) = Lazy.StateT (localContext f . m)

instance (Monoid w, MonadContext c m)
    => MonadContext c (Strict.WriterT w m) where
        getContext = lift getContext
        localContext f (Strict.WriterT m) = Strict.WriterT (localContext f m)

instance (Monoid w, MonadContext c m)
    => MonadContext c (Lazy.WriterT w m) where
        getContext = lift getContext
        localContext f (Lazy.WriterT m) = Lazy.WriterT (localContext f m)

instance MonadContext c m => MonadContext c (ReaderT r m) where
    getContext = lift getContext
    localContext f (ReaderT m) = ReaderT (localContext f . m)

instance MonadContext c m => MonadContext c (ExceptT e m) where
    getContext = lift getContext
    localContext f (ExceptT m) = ExceptT (localContext f m)


-- | Access the @c@ of a @MonadContext c m@ using a function
getsContext :: MonadContext c m => (c -> a) -> m a
getsContext f = f <$> getContext

-- | Overwrite the @c@ of a @MonadContext c m@ with a new value
useContext :: MonadContext c m => c -> m a -> m a
useContext = localContext . const
