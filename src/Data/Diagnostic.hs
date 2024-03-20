module Data.Diagnostic where

import Data.Attr
import Data.SyntaxError
import Data.Tag


import Text.Pretty

import Language.Ribbon.Syntax.Ref
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

isErrorKind :: DiagnosticKind -> Bool
isErrorKind = \case
    Error _ -> True
    Warning -> False

isWarningKind :: DiagnosticKind -> Bool
isWarningKind = \case
    Error _ -> False
    Warning -> True

-- | A deduplication identity for @Diagnostic@
data DiagnosticBinder
    = DiagnosticBinder
    { kind :: !DiagnosticBinderKind
    ,  ref :: !Ref
    , name :: !(Maybe String)
    }
    deriving (Eq, Ord, Show)

instance Pretty DiagnosticBinder where
    pPrint DiagnosticBinder{..} =
        pPrint kind <+> pPrint ref <+> qualBackticks (maybeMEmpty $ text <$> name)

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
    , doc :: !Doc
    , help :: ![Doc]
    }
    deriving Show

instance Pretty Diagnostic where
    pPrint Diagnostic{..} =
        hang (pPrint kind <+> "at" <+> (pPrint at <> ":")) $ vcat' $
            doc : help

isError :: Diagnostic -> Bool
isError = isErrorKind . (.kind)

isWarning :: Diagnostic -> Bool
isWarning = isWarningKind . (.kind)

-- | Create a new @Diagnostic@ using the given @DiagnosticKind@ and
--   @DiagnosticBinder@, along with a @Doc@ created from the given value
diagnosticFromDoc :: Pretty a =>
    Attr -> DiagnosticKind -> a -> Diagnostic
diagnosticFromDoc at k a = Diagnostic at k (pPrint a) []

-- | Create a new @Diagnostic@ using the given @DiagnosticKind@ and
--   @DiagnosticBinder@, along with a @Doc@ created from the given value
diagnosticFromDocH :: Pretty a =>
    Attr -> DiagnosticKind -> a -> [Doc] -> Diagnostic
diagnosticFromDocH at k a = Diagnostic at k (pPrint a)


-- | Create a new @Error@ @Diagnostic@ using the given @DiagnosticBinder@ and
--   a @Doc@ created from the given value
diagnosticFromError :: Pretty a =>
    Attr -> DiagnosticBinder -> a -> Diagnostic
diagnosticFromError at = diagnosticFromDoc at . Error

-- | Create a new @Error@ @Diagnostic@ using the given @DiagnosticBinder@ and
--   a @Doc@ created from the given value
diagnosticFromErrorH :: Pretty a =>
    Attr -> DiagnosticBinder -> a -> [Doc] -> Diagnostic
diagnosticFromErrorH at b = diagnosticFromDocH at (Error b)

-- | Create a new @Warning@ @Diagnostic@ using the given @DiagnosticBinder and
--   a @Doc@ created from the given value
diagnosticFromWarning :: Pretty a =>
    Attr -> a -> Diagnostic
diagnosticFromWarning at = diagnosticFromDoc at Warning

-- | Create a new @Warning@ @Diagnostic@ using the given @DiagnosticBinder and
--   a @Doc@ created from the given value
diagnosticFromWarningH :: Pretty a =>
    Attr -> a -> [Doc] -> Diagnostic
diagnosticFromWarningH at = diagnosticFromDocH at Warning


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

diagnosticFromSyntaxError :: DiagnosticBinder -> SyntaxError -> Diagnostic
diagnosticFromSyntaxError b (SyntaxError _ (f :@: at)) =
    Diagnostic
    { at
    , kind = Error b
    , doc = formatFailure [at] f
    , help = []
    }
