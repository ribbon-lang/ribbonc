module Control.Monad.Diagnostics.Class where
import Data.Diagnostic

import Control.Monad.State.Strict qualified as Strict
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.Writer.Strict qualified as Strict
import Control.Monad.Writer.Lazy qualified as Lazy
import Control.Monad.Reader
import Control.Monad.Except
import Control.Has

import Text.Pretty

import Language.Ribbon.Util




-- | Marker for @Has@, ie @Has m [Diag]@ ~ @MonadDiagnostics m@
data Diag

type instance Has m (Diag ': effs) = (MonadDiagnostics m, Has m effs)


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




-- | Report a new @Diagnostic@ using the given @DiagnosticKind@ and
--   a @Doc@ created from the given item,
--   with help @Doc@s
reportH :: (Pretty a, MonadDiagnostics m) => DiagnosticKind -> a -> [Doc] -> m ()
reportH k a h = reportFull (Diagnostic k (pPrint a) h)

-- | Report a new @Error@ @Diagnostic@ using
--   a @Doc@ created from the given item,
--   with help @Doc@s
reportErrorH :: (Pretty a, MonadDiagnostics m) => a -> [Doc] -> m ()
reportErrorH = reportH Error

-- | Report a new @Warning@ @Diagnostic@ using
--   a @Doc@ created from the given item,
--   with help @Doc@s
reportWarningH :: (Pretty a, MonadDiagnostics m) => a -> [Doc] -> m ()
reportWarningH = reportH Warning

-- | Report a new @Diagnostic@ using the given @DiagnosticKind@ and
--   a @Doc@ created from the given item
report :: (Pretty a, MonadDiagnostics m) => DiagnosticKind -> a -> m ()
report k a = reportH k a []

-- | Report a new @Error@ @Diagnostic@ using
--   a @Doc@ created from the given item
reportError :: (Pretty a, MonadDiagnostics m) => a -> m ()
reportError = report Error

-- | Report a new @Warning@ @Diagnostic@ using
--   a @Doc@ created from the given item
reportWarning :: (Pretty a, MonadDiagnostics m) => a -> m ()
reportWarning = report Warning


-- | Run an @ExceptT e m ()@ computation,
--   using @Pretty@ to convert @e@ to @Error@ @Diagnostic@s in @m@
errorToDiagnostic :: (MonadDiagnostics m, Pretty e) => ExceptT e m ()  -> m ()
errorToDiagnostic m = runExceptT m >>= liftEitherHandler reportError
