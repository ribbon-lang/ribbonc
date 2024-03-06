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
