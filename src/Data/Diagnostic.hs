module Data.Diagnostic where

import Text.Pretty



-- | The kind of a @Diagnostic@, either an @Error@ or a @Warning@
data DiagnosticKind
    = Error
    | Warning
    deriving (Eq, Ord, Enum, Bounded, Show)

instance Pretty DiagnosticKind where
    pPrint = \case
        Error -> "error"
        Warning -> "warning"

-- | A compiler message about a problem in the source;
--   containing a @DiagnosticKind@, a @Doc@ describing the problem,
--   and an optional list of @Doc@s with suggestions for how to fix it
data Diagnostic
    = Diagnostic
    { kind :: !DiagnosticKind
    , doc :: !Doc
    , help :: ![Doc]
    }
    deriving Show

instance Pretty Diagnostic where
    pPrint Diagnostic{..} = vcat'
        [ pPrint kind <+> doc
        , indent (vcat' help)
        ]

-- | Create a new @Diagnostic@ using the given @DiagnosticKind@ and
--   a @Doc@ created from the given value
diagnosticFromDoc :: Pretty a => DiagnosticKind -> a -> Diagnostic
diagnosticFromDoc k a = Diagnostic k (pPrint a) []

-- | Create a new @Error@ @Diagnostic@ using and
--   a @Doc@ created from the given value
diagnosticFromError :: Pretty a => a -> Diagnostic
diagnosticFromError = diagnosticFromDoc Error

-- | Create a new @Warning@ @Diagnostic@ using and
--   a @Doc@ created from the given value
diagnosticFromWarning :: Pretty a => a -> Diagnostic
diagnosticFromWarning = diagnosticFromDoc Warning

-- | Convert an @Either e@ to an @Either Diagnostic@
eitherDiagnostic :: Pretty e => Either e b -> Either Diagnostic b
eitherDiagnostic = \case
    Left a -> Left $ diagnosticFromError a
    Right b -> Right b
