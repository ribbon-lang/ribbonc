module Control.Monad.Builder
    ( module X
    , BuilderT
    , runBuilderT, execBuilderT
    ) where


import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Writer.Class
import Control.Monad.Reader
import Control.Monad.Except

import Control.Monad.File.Class
import Control.Monad.Parser.Class
import Control.Monad.Diagnostics.Class
import Control.Monad.Context.Class
import Control.Monad.Builder.Class as X



-- | Transformer for building things like namespaces
--   this is essentially a @StateT@, see @MonadBuilder@ for differences
newtype BuilderT s m a
    = BuilderT
    ( StateT s m a )
    deriving
        ( Functor, Applicative, Monad, Alternative, MonadPlus
        , MonadTrans, MonadFail, MonadIO
        , MonadDiagnostics, MonadFile
        )

deriving instance MonadWriter w m => MonadWriter w (BuilderT x m)
deriving instance MonadReader s m => MonadReader s (BuilderT x m)
deriving instance  MonadError e m =>  MonadError e (BuilderT x m)
deriving instance MonadParser i m => MonadParser i (BuilderT x m)
deriving instance MonadContext c m => MonadContext c (BuilderT x m)

instance MonadState s m => MonadState s (BuilderT x m) where
    get = lift get
    put = lift . put

instance Monad m => MonadBuilder s (BuilderT s m) where
    builderState f = BuilderT $ state f

instance {-# OVERLAPPABLE #-} MonadBuilder s m
    => MonadBuilder s (BuilderT x m) where
        builderState = lift . builderState

runBuilderT ::
    BuilderT s m a -> s -> m (a, s)
runBuilderT (BuilderT m) = runStateT m

execBuilderT ::
    Monad m => BuilderT s m a -> s -> m s
execBuilderT m = fmap snd . runBuilderT m
