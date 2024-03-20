module Language.Ribbon.Analysis.Diagnostics
    ( module X
    , Diag
    , MonadDiagnostics
    , reportAll
    , reportFull
    , reportH
    , reportErrorH
    , reportWarningH
    , report
    , reportError
    , reportWarning
    , errorToDiagnostic
    , errorExtractToDiagnostic
    , diagAssertH, diagAssert
    ) where


import Data.Diagnostic
import Data.Attr

import Control.Has
import Control.Monad.Writer.Dynamic as X
import Control.Monad.Error.Dynamic

import Text.Pretty

import Language.Ribbon.Util
import Control.Monad




-- | Marker for @Has@, ie @Has m '[Diag]@ ~ @MonadDiagnostics m@
data Diag

type instance Has m (Diag ': effs) = (MonadDiagnostics m, Has m effs)


-- | @MonadWriter [Diagnostic] m@
type MonadDiagnostics = MonadWriter [Diagnostic]

-- | @tell@ specialized to @[Diagnostic]@
reportAll :: MonadDiagnostics m => [Diagnostic] -> m ()
reportAll = tell

-- | Report a @Diagnostic@
reportFull :: MonadDiagnostics m => Diagnostic -> m ()
reportFull d = tell [d]

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


-- | Throw a @Diagnostic@ @Error@ if the given condition is not met
diagAssertH :: (MonadDiagnostics m, Pretty a) =>
    Bool -> Attr -> DiagnosticBinder -> a -> [Doc] -> m ()
diagAssertH b at k a h = unless b do reportErrorH at k a h

-- | Throw a @Diagnostic@ @Error@ if the given condition is not met
diagAssert :: (MonadDiagnostics m, Pretty a) =>
    Bool -> Attr -> DiagnosticBinder -> a -> m ()
diagAssert b at k a = diagAssertH b at k a []

