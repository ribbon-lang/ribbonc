module Language.Ribbon.Syntax.Group
    ( ResolvedBlobs
    , UnresolvedImports(..)
    , Group(..)
    ) where

import Data.Map (Map)
import Data.Set (Set)

import Data.Attr

import Text.Pretty

import Language.Ribbon.Syntax.Ref
import Language.Ribbon.Syntax.Name
import Language.Ribbon.Syntax.Path
import Language.Ribbon.Syntax.Binding


type GroupMap a = Map FixName (GroupSet a)

type GroupSet a = Set (ATag a)

-- | A group of imports that has been resolved to a set of references
--   to continue lookup traversal through
newtype ResolvedBlobs
    = ResolvedBlobs
    { inner :: GroupSet Ref }
    deriving (Eq, Ord, Show, Pretty)

-- | An unresolved group of imports
data UnresolvedImports
    = UnresolvedImports
    { aliases :: !(GroupMap (Binding Path))
    , blobs :: !(GroupSet Path)
    }
    deriving (Eq, Ord, Show)

instance Pretty UnresolvedImports where
    pPrintPrec lvl _ (UnresolvedImports as bs) =
        vcat
            [ hang "aliases" (pPrintPrec lvl 0 as)
            , hang "blobs" (pPrintPrec lvl 0 bs)
            ]

-- | A map of overloaded bindings
newtype Group
    = Group
    { defs :: GroupMap (Binding Ref) }
    deriving (Eq, Ord, Show, Pretty)
