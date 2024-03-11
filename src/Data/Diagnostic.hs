module Data.Diagnostic where

import Data.Attr

import Text.Pretty

import Language.Ribbon.Syntax.Ref (Ref)
import Language.Ribbon.Lexical.Name (FixName)
import Language.Ribbon.Util




-- | The kind of a @Diagnostic@, either an @Error@ or a @Warning@
data DiagnosticKind
    = Error !DiagnosticBinder
    | Warning
    deriving (Eq, Ord, Show)

instance Pretty DiagnosticKind where
    pPrint = \case
        Error binder -> pPrint binder <+> "error"
        Warning -> "warning"

-- | A deduplication identity for @Diagnostic@
data DiagnosticBinder
    = DiagnosticBinder
    { kind :: !DiagnosticBinderKind
    ,  ref :: !Ref
    , name :: !(Maybe FixName)
    }
    deriving (Eq, Ord, Show)

instance Pretty DiagnosticBinder where
    pPrint DiagnosticBinder{..} =
        pPrint kind <+> qualBackticks (maybeMEmpty $ pPrint <$> name)

data DiagnosticBinderKind
    = BadDefinition
    | ConflictingDefinition
    | Unresolved
    | TypeError
    deriving (Eq, Ord, Show)

instance Pretty DiagnosticBinderKind where
    pPrint = \case
        BadDefinition -> "bad definition"
        ConflictingDefinition -> "conflicting definitions"
        Unresolved -> "unresolved reference"
        TypeError -> "type"

-- | A compiler message about a problem in the source;
--   containing a @DiagnosticKind@, a @Doc@ describing the problem,
--   and an optional list of @Doc@s with suggestions for how to fix it
data Diagnostic
    = Diagnostic
    {   at :: !Attr
    , kind :: !DiagnosticKind
    ,  doc :: !Doc
    , help :: ![Doc]
    }
    deriving Show

instance Pretty Diagnostic where
    pPrint Diagnostic{..} =
        hang (pPrint kind <+> "at" <+> (pPrint at <> ":")) $ vcat' $
            doc : help

-- | Create a new @Diagnostic@ using the given @DiagnosticKind@ and
--   @DiagnosticBinder@, along with a @Doc@ created from the given value
diagnosticFromDoc :: Pretty a =>
    Attr -> DiagnosticKind -> a -> Diagnostic
diagnosticFromDoc at k a = Diagnostic at k (pPrint a) []

-- | Create a new @Error@ @Diagnostic@ using the given @DiagnosticBinder@ and
--   a @Doc@ created from the given value
diagnosticFromError :: Pretty a =>
    Attr -> DiagnosticBinder -> a -> Diagnostic
diagnosticFromError at = diagnosticFromDoc at . Error

-- | Create a new @Warning@ @Diagnostic@ using the given @DiagnosticBinder and
--   a @Doc@ created from the given value
diagnosticFromWarning :: Pretty a =>
    Attr -> a -> Diagnostic
diagnosticFromWarning at = diagnosticFromDoc at Warning

-- | Convert an @Either e@ to an @Either Diagnostic@
eitherDiagnostic :: Pretty e =>
    Attr -> DiagnosticBinder -> Either e a -> Either Diagnostic a
eitherDiagnostic at b = \case
    Left e -> Left $ diagnosticFromError at b e
    Right a -> Right a

-- | Convert an @Either e@ to an @Either Diagnostic@,
--   using a provided function to extract an @Attr@ for the @Diagnostic@
eitherExtractDiagnostic :: Pretty e =>
    (e -> Attr) -> DiagnosticBinder -> Either e a -> Either Diagnostic a
eitherExtractDiagnostic at b = \case
    Left e -> Left $ diagnosticFromError (at e) b e
    Right a -> Right a
