module Control.Monad.Diagnostics.Class where
import Data.Diagnostic

import Control.Monad.Trans.Dynamic
import Control.Monad.State.Dynamic
import Control.Monad.Writer.Dynamic
import Control.Monad.Reader.Dynamic
import Control.Monad.Error.Dynamic
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

instance MonadDiagnostics m => MonadDiagnostics (StateT s m) where
    reportFull = lift . reportFull

instance (Monoid w, MonadDiagnostics m)
    => MonadDiagnostics (WriterT w m) where
        reportFull = lift . reportFull

instance MonadDiagnostics m => MonadDiagnostics (ReaderT r m) where
    reportFull = lift . reportFull

instance MonadDiagnostics m => MonadDiagnostics (ErrorT e m) where
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


-- | Run an @ErrorT e m ()@ computation,
--   using @Pretty@ to convert @e@ to @Error@ @Diagnostic@s in @m@
errorToDiagnostic :: forall e m. (MonadDiagnostics m, Pretty e) =>
    Attr -> DiagnosticBinder -> ErrorT e m () -> m ()
errorToDiagnostic at b m =
    runErrorT m >>= liftEitherHandler (reportFull . diagnosticFromError at b)

-- | Run an @ErrorT e m a@ computation,
--   using @Pretty@ to convert @e@ to @Error@ @Diagnostic@s in @m@,
--   and a provided function to extract an @Attr@ from @e@ for the @Diagnostic@
errorExtractToDiagnostic :: forall e m. (MonadDiagnostics m, Pretty e) =>
    (e -> Attr) -> DiagnosticBinder -> ErrorT e m () -> m ()
errorExtractToDiagnostic at b m =
    runErrorT m >>= liftEitherHandler do
        reportFull . \e -> diagnosticFromError (at e) b e
