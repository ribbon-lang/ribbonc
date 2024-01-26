module Ribbon.Util where

import Control.Applicative
import Data.Maybe qualified as Maybe

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Map (Map)
import Data.Map qualified as Map

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq


-- | Marks something not yet implemented
todo :: a
todo = error "TODO"


-- | Branch on a boolean, selecting a or b for true or false, respectively
select :: Bool -> a -> a -> a
select True a _ = a
select False _ b = b


-- | equivalent of Applicative.some (one or more) with unit return value
some_ :: Alternative f => f a -> f ()
some_ a = some_v where
    many_v = some_v <|> pure ()
    some_v = a *> many_v

-- | equivalent of Applicative.many (zero or more) with unit return value
many_ :: Alternative f => f a -> f ()
many_ a = many_v where
    many_v = some_v <|> pure ()
    some_v = a *> many_v


-- | The reverse of (.)
compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g a = g (f a)


-- | The reverse of (>>)
(<<) :: Monad m => m b -> m a -> m b
(<<) ma mb = do a <- ma; a <$ mb

-- | An extension class for monoids allowing the Nil pattern to function
class Monoid m => Nil m where
    isNil :: m -> Bool

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


-- | Pattern alias for types with a Nil instance
pattern Nil :: Nil m => m
pattern Nil <- (isNil -> True) where
    Nil = mempty
