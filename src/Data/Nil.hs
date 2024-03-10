module Data.Nil where

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Map (Map)
import Data.Map qualified as Map

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.Maybe qualified as Maybe




-- | An extension class allowing the Nil pattern to function
class Nil m where
    -- | Check if a value is nil
    isNil :: m -> Bool

    -- | The nil value for a type
    nil :: m

    default nil :: Monoid m => m
    nil = mempty

instance Nil () where
    isNil = const True

instance Nil [a] where
    isNil = null

instance Nil (Seq a) where
    isNil = Seq.null

instance Ord a => Nil (Set a) where
    isNil = Set.null

instance Ord a => Nil (Map a b) where
    isNil = Map.null

instance Semigroup a => Nil (Maybe a) where
    isNil = Maybe.isNothing

instance (Nil a, Nil b) => Nil (a, b) where
    isNil (a, b) = isNil a && isNil b
    nil = (nil, nil)

instance (Nil a, Nil b, Nil c) => Nil (a, b, c) where
    isNil (a, b, c) = isNil a && isNil b && isNil c
    nil = (nil, nil, nil)

instance (Nil a, Nil b, Nil c, Nil d) => Nil (a, b, c, d) where
    isNil (a, b, c, d) = isNil a && isNil b && isNil c && isNil d
    nil = (nil, nil, nil, nil)

instance {-# OVERLAPPABLE #-} (Eq a, Num a) => Nil a where
    isNil = (== 0)
    nil = 0

-- | Pattern alias for types with a Nil instance
pattern Nil :: Nil m => m
pattern Nil <- (isNil -> True) where
    Nil = nil

{-# COMPLETE (Seq.:|>), Nil #-}
{-# COMPLETE (Seq.:<|), Nil #-}
{-# COMPLETE (:), Nil #-}
