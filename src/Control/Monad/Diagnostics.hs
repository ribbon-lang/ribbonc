module Control.Monad.Diagnostics where

import Control.Monad.Identity
import Control.Monad.State.Class
import Control.Monad.State.Strict qualified as Strict
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Writer.Lazy qualified as Lazy
import Control.Monad.File
import Control.Monad.Error.Class

import Data.Diagnostic

import Text.Pretty
import Control.Monad.Except




-- | A monad that can report @Diagnostic@s
class Monad m => MonadDiagnostics m where
    -- | Report a @Diagnostic@
    reportFull :: Diagnostic -> m ()

instance MonadDiagnostics m => MonadDiagnostics (Strict.StateT s m) where
    reportFull = lift . reportFull

instance MonadDiagnostics m => MonadDiagnostics (Lazy.StateT s m) where
    reportFull = lift . reportFull

instance MonadDiagnostics m => MonadDiagnostics (ReaderT r m) where
    reportFull = lift . reportFull

instance (Monoid w, MonadDiagnostics m)
    => MonadDiagnostics (WriterT w m) where
        reportFull = lift . reportFull

instance (Monoid w, MonadDiagnostics m)
    => MonadDiagnostics (Lazy.WriterT w m) where
        reportFull = lift . reportFull

instance MonadDiagnostics m => MonadDiagnostics (ExceptT e m) where
    reportFull = lift . reportFull

instance MonadDiagnostics m => MonadDiagnostics (FileT m) where
    reportFull = lift . reportFull




-- | @DiagnosticsT@ over the @Identity@
type Diagnostics = DiagnosticsT Identity

-- | A monad transformer that can report @Diagnostic@s
newtype DiagnosticsT m a
    = DiagnosticsT
    ( WriterT [Diagnostic] m a )
    deriving (Functor, Applicative, Monad, MonadFail, MonadTrans, MonadFile)

deriving instance MonadState s m => MonadState s (DiagnosticsT m)
deriving instance MonadReader r m => MonadReader r (DiagnosticsT m)
deriving instance MonadError e m => MonadError e (DiagnosticsT m)

instance Monad m => MonadDiagnostics (DiagnosticsT m) where
    reportFull d = DiagnosticsT (tell [d])


-- | Run a @DiagnosticsT@ computation,
--   returning the result and a list of @Diagnostic@s
runDiagnosticT :: DiagnosticsT m a -> m (a, [Diagnostic])
runDiagnosticT (DiagnosticsT a) = runWriterT a

-- | Run a @Diagnostics@ computation,
--   returning the result and a list of @Diagnostic@s
runDiagnostic :: Diagnostics a -> (a, [Diagnostic])
runDiagnostic = runIdentity . runDiagnosticT



-- | Report a new @Diagnostic@ using the given @DiagnosticKind@ and @Doc@,
--   with help @Doc@s
reportH :: MonadDiagnostics m => DiagnosticKind -> Doc -> [Doc] -> m ()
reportH k d h = reportFull (Diagnostic k d h)

-- | Report a new @Error@ @Diagnostic@ using the given @Doc@,
--   with help @Doc@s
reportErrorH :: MonadDiagnostics m => Doc -> [Doc] -> m ()
reportErrorH = reportH Error

-- | Report a new @Warning@ @Diagnostic@ using the given @Doc@,
--   with help @Doc@s
reportWarningH :: MonadDiagnostics m => Doc -> [Doc] -> m ()
reportWarningH = reportH Warning

-- | Report a new @Diagnostic@ using the given @DiagnosticKind@ and @Doc@
report :: MonadDiagnostics m => DiagnosticKind -> Doc -> m ()
report k d = reportH k d []

-- | Report a new @Error@ @Diagnostic@ using the given @Doc@
reportError :: MonadDiagnostics m => Doc -> m ()
reportError = report Error

-- | Report a new @Warning@ @Diagnostic@ using the given @Doc@
reportWarning :: MonadDiagnostics m => Doc -> m ()
reportWarning = report Warning
