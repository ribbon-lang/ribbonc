module Language.Ribbon.Syntax.Namespace where

import Data.Map.Lazy (Map)
-- import Data.Map.Lazy qualified as Map

import Data.Set (Set)
-- import Data.Set qualified as Set

import Data.Attr

import Text.Pretty

import Language.Ribbon.Syntax.Path

-- | A map of overloaded definitions
type DefBindings d = Map Name (Set (ATag d))

-- | A map of overloaded definitions,
--   as well as a map of aliased names and a set of blobs
data Namespace d a
    = Namespace
    { defs :: !(DefBindings d)
    , aliases :: !(Map Name [ATag a])
    , blobs :: !(Set (ATag a))
    }
    deriving (Eq, Ord, Show)

instance (Pretty d, Pretty a) => Pretty (Namespace d a) where
    pPrintPrec lvl _ (Namespace ds as bs) =
        pPrintPrec lvl 0 ds <+> pPrintPrec lvl 0 as <+> pPrintPrec lvl 0 bs
