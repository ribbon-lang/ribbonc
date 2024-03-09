module Control.Monad.Builder.Class where

import Control.Monad.State.Strict qualified as Strict
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.Writer.Strict qualified as Strict
import Control.Monad.Writer.Lazy qualified as Lazy
import Control.Monad.Reader
import Control.Monad.Except
import Control.Has




-- | Marker for @Has@, ie @Has m [Bldr s]@ ~ @MonadBuilder s m@
data Bldr a

type instance Has m (Bldr s ': effs) = (MonadBuilder s m, Has m effs)



-- | Transformer class for building things like definitions;
--   this is functionally identical to @MonadState@,
--   without the functional dependency
class Monad m => MonadBuilder s m where
    -- | Read/Write/Modify the @s@ of a @MonadBuilder@
    builderState :: (s -> (a, s)) -> m a

instance MonadBuilder s m => MonadBuilder s (Strict.StateT x m) where
    builderState = lift . builderState

instance MonadBuilder s m => MonadBuilder s (Lazy.StateT x m) where
    builderState = lift . builderState

instance (Monoid w, MonadBuilder s m)
    => MonadBuilder s (Strict.WriterT w m) where
        builderState = lift . builderState

instance (Monoid w, MonadBuilder s m)
    => MonadBuilder s (Lazy.WriterT w m) where
        builderState = lift . builderState

instance MonadBuilder s m => MonadBuilder s (ReaderT r m) where
    builderState = lift . builderState

instance MonadBuilder s m => MonadBuilder s (ExceptT e m) where
    builderState = lift . builderState


-- | Modify the @s@ of a @MonadBuilder s m@
builderModify :: MonadBuilder s m => (s -> s) -> m ()
builderModify f = builderState \s -> ((), f s)

-- | Read the @s@ of a @MonadBuilder s m@
builderGet :: MonadBuilder s m => m s
builderGet = builderState \s -> (s, s)

-- | Write the @s@ of a @MonadBuilder s m@
builderPut :: MonadBuilder s m => s -> m ()
builderPut s = builderState $ const ((), s)
