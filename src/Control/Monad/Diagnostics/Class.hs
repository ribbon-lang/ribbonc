module Control.Monad.Diagnostics.Class where

import Control.Monad.State.Strict qualified as Strict
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.Writer.Strict qualified as Strict
import Control.Monad.Writer.Lazy qualified as Lazy
import Control.Monad.Reader
import Control.Monad.Except

import Data.Diagnostic

import Text.Pretty




-- | A monad that can report @Diagnostic@s
class Monad m => MonadDiagnostics m where
    -- | Report a @Diagnostic@
    reportFull :: Diagnostic -> m ()

instance MonadDiagnostics m => MonadDiagnostics (Strict.StateT s m) where
    reportFull = lift . reportFull

instance MonadDiagnostics m => MonadDiagnostics (Lazy.StateT s m) where
    reportFull = lift . reportFull

instance (Monoid w, MonadDiagnostics m)
    => MonadDiagnostics (Strict.WriterT w m) where
        reportFull = lift . reportFull

instance (Monoid w, MonadDiagnostics m)
    => MonadDiagnostics (Lazy.WriterT w m) where
        reportFull = lift . reportFull

instance MonadDiagnostics m => MonadDiagnostics (ReaderT r m) where
    reportFull = lift . reportFull

instance MonadDiagnostics m => MonadDiagnostics (ExceptT e m) where
    reportFull = lift . reportFull




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
