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
import Data.Attr




-- | Marker for @Has@, ie @Has m '[Diag]@ ~ @MonadDiagnostics m@
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
reportH :: (Pretty a, MonadDiagnostics m) => Attr -> DiagnosticKind -> a -> [Doc] -> m ()
reportH at k a h = reportFull (Diagnostic at k (pPrint a) h)

-- | Report a new @Error@ @Diagnostic@ using
--   a @Doc@ created from the given item,
--   with help @Doc@s
reportErrorH :: (Pretty a, MonadDiagnostics m) => Attr -> DiagnosticBinder -> a -> [Doc] -> m ()
reportErrorH at = reportH at . Error

-- | Report a new @Warning@ @Diagnostic@ using
--   a @Doc@ created from the given item,
--   with help @Doc@s
reportWarningH :: (Pretty a, MonadDiagnostics m) => Attr -> a -> [Doc] -> m ()
reportWarningH at = reportH at Warning

-- | Report a new @Diagnostic@ using the given @DiagnosticKind@ and
--   a @Doc@ created from the given item
report :: (Pretty a, MonadDiagnostics m) => Attr -> DiagnosticKind -> a -> m ()
report at k a = reportH at k a []

-- | Report a new @Error@ @Diagnostic@ using
--   a @Doc@ created from the given item
reportError :: (Pretty a, MonadDiagnostics m) => Attr -> DiagnosticBinder -> a -> m ()
reportError at = report at . Error

-- | Report a new @Warning@ @Diagnostic@ using
--   a @Doc@ created from the given item
reportWarning :: (Pretty a, MonadDiagnostics m) => Attr -> a -> m ()
reportWarning at = report at Warning


-- | Run an @ExceptT e m ()@ computation,
--   using @Pretty@ to convert @e@ to @Error@ @Diagnostic@s in @m@
errorToDiagnostic :: (MonadDiagnostics m, Pretty e) =>
    Attr -> DiagnosticBinder -> ExceptT e m () -> m ()
errorToDiagnostic at b m =
    runExceptT m >>= liftEitherHandler (reportFull . diagnosticFromError at b)

-- | Run an @ExceptT e m a@ computation,
--   using @Pretty@ to convert @e@ to @Error@ @Diagnostic@s in @m@,
--   and a provided function to extract an @Attr@ from @e@ for the @Diagnostic@
errorExtractToDiagnostic :: (MonadDiagnostics m, Pretty e) =>
    (e -> Attr) -> DiagnosticBinder -> ExceptT e m () -> m ()
errorExtractToDiagnostic at b m =
    runExceptT m >>= liftEitherHandler do
        reportFull . \e -> diagnosticFromError (at e) b e
