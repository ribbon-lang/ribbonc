module Control.Monad.Diagnostics
    ( module X
    , Diagnostics, runDiagnostics
    , DiagnosticsT, runDiagnosticsT
    , liftDiagnostics
    ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Strict
import Control.Monad.File.Class
import Control.Monad.Parser.Class
import Control.Monad.Error.Class

import Control.Monad.Diagnostics.Class as X

import Data.Diagnostic




-- | @DiagnosticsT@ over the @Identity@
type Diagnostics = DiagnosticsT Identity

-- | A monad transformer that can report @Diagnostic@s
newtype DiagnosticsT m a
    = DiagnosticsT
    ( WriterT [Diagnostic] m a )
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFail, MonadTrans, MonadFile)

deriving instance MonadState s m => MonadState s (DiagnosticsT m)
deriving instance MonadReader r m => MonadReader r (DiagnosticsT m)
deriving instance MonadError e m => MonadError e (DiagnosticsT m)
deriving instance MonadParser i m => MonadParser i (DiagnosticsT m)

instance Monad m => MonadDiagnostics (DiagnosticsT m) where
    reportFull d = DiagnosticsT (tell [d])


-- | Run a @DiagnosticsT@ computation,
--   returning the result and a list of @Diagnostic@s
runDiagnosticsT :: DiagnosticsT m a -> m (a, [Diagnostic])
runDiagnosticsT (DiagnosticsT a) = runWriterT a

-- | Run a @Diagnostics@ computation,
--   returning the result and a list of @Diagnostic@s
runDiagnostics :: Diagnostics a -> (a, [Diagnostic])
runDiagnostics = runIdentity . runDiagnosticsT


liftDiagnostics :: (MonadError [Diagnostic] m) =>
    DiagnosticsT m a -> m a
liftDiagnostics m = do
    (a, ds) <- runDiagnosticsT m
    a <$ unless (null ds) (throwError ds)
