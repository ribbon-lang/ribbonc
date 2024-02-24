module Language.Ribbon.Syntax.Visibility where

import Text.Pretty

-- | A visibility level for a binding
data Visibility
    = Public
    | Private
    deriving (Show, Eq, Ord)

instance Pretty Visibility where
    pPrintPrec lvl _ = \case
        Public -> "pub"
        Private -> if lvl > PrettyNormal then "private" else mempty
