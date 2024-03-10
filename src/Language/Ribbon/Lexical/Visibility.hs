module Language.Ribbon.Lexical.Visibility where

import Text.Pretty




-- | A binding with a visibility level
data Visible a
    = Visible
    { visibility :: !Visibility
    ,      value :: !a
    }
    deriving (Eq, Ord, Functor, Foldable, Traversable, Show)

instance Pretty a => Pretty (Visible a) where
    pPrintPrec lvl _ Visible{..} =
        pPrintPrec lvl 0 visibility <+> pPrintPrec lvl 0 value

-- | A visibility level for a binding
data Visibility
    = Public
    | Private
    deriving (Show, Eq, Ord)

instance Pretty Visibility where
    pPrintPrec lvl _ = \case
        Public -> "pub"
        Private -> if lvl > PrettyNormal then "private" else mempty
