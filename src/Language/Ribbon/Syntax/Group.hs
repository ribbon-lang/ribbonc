module Language.Ribbon.Syntax.Group
    ( ResolvedBlobs
    , UnresolvedImports(..)
    , Group(..)
    ) where

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Attr

import Text.Pretty

import Language.Ribbon.Lexical

import Language.Ribbon.Syntax.Ref
import Data.Functor

-- | A group of imports that has been resolved to a set of references
--   to continue lookup traversal through
newtype ResolvedBlobs
    = ResolvedBlobs
    { inner :: Set (ATag Ref) }
    deriving (Eq, Ord, Show)

instance Pretty ResolvedBlobs where
    pPrintPrec lvl _ (ResolvedBlobs bs) =
        hang "blobs" $ vcat' do
            pPrintPrec lvl 0 <$> Set.toList bs

-- | An unresolved group of imports
data UnresolvedImports
    = UnresolvedImports
    { aliases :: ![(UnresolvedName, ATag Path)]
    , blobs :: !(Set (ATag Path))
    }
    deriving (Eq, Ord, Show)

instance Pretty UnresolvedImports where
    pPrintPrec lvl _ (UnresolvedImports as bs) =
        hang "imports" $ vcat'
            [ hang "aliases" $ vcat' do
                pPrintPrec lvl 0 <$> as
            , hang "blobs" $ vcat' do
                pPrintPrec lvl 0 <$> Set.toList bs
            ]

-- | A map of overloaded bindings
newtype Group
    = Group
    { defs :: [(GroupName, ATag Ref)] }
    deriving (Eq, Ord, Show)

instance Pretty Group where
    pPrintPrec lvl _ (Group ds) =
        hang "defs" $ vcat' do
            ds <&> \(n, r) ->
                spaceWith "="
                    do pPrintPrec lvl 0 n
                    do pPrintPrec lvl 0 r
