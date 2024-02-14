module Ribbon.Util where

import Control.Applicative
import Control.Monad.Except
import Data.Maybe qualified as Maybe

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Map (Map)
import Data.Map qualified as Map

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

import Ribbon.Display


-- | Marks something not yet implemented
todo :: a
todo = error "TODO"


-- | Drop elements from the end of a list
dropTail :: Int -> [a] -> [a]
dropTail n = reverse . drop n . reverse


-- | ByteString -> String
bytesToString :: ByteString -> String
bytesToString
    = Text.unpack
    . Text.decodeUtf8
    . ByteString.toStrict


-- | Branch on a boolean, selecting a or b for true or false, respectively
select :: Bool -> a -> a -> a
select True a _ = a
select False _ b = b

-- | Branch on a boolean, selecting a or b for true or false, respectively
selecting :: a -> a -> Bool -> a
selecting a b p = select p a b


-- | equivalent of Applicative.some (one or more) with unit return value
some_ :: Alternative f => f a -> f ()
some_ a = some_v where
    many_v = option () some_v
    some_v = a *> many_v

-- | equivalent of Applicative.many (zero or more) with unit return value
many_ :: Alternative f => f a -> f ()
many_ a = many_v where
    many_v = option () some_v
    some_v = a *> many_v


-- | equivalent of Applicative.some (one or more) with base value
someWith :: Alternative f => [a] -> f a -> f [a]
someWith base a = some_v where
    many_v = option base some_v
    some_v = liftA2 (:) a many_v

-- | equivalent of Applicative.many (zero or more) with base value
manyWith :: Alternative f => [a] -> f a -> f [a]
manyWith base a = many_v where
    many_v = option base some_v
    some_v = liftA2 (:) a many_v

-- | @optional@, with a default
option :: Alternative f => a -> f a -> f a
option a fa = fa <|> pure a


-- | The reverse of (.)
compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g a = g (f a)

-- | Split a list into multiple sub-lists
--   at each element that satisfies a predicate
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p = go where
    go [] = []
    go xs = case break p xs of
        (a, _ : b) -> a : go b
        _ -> [xs]

-- | Split a list into multiple sub-lists
--   at each element that is equal to a given value
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = splitWith . (==)

-- | Compositional @&&@
(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&&) f g a = f a && g a
infixl 8 &&&

-- | Compositional @||@
(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(|||) f g a = f a || g a
infixl 8 |||

-- | Compositional @not@
not'd :: (a -> Bool) -> (a -> Bool)
not'd f a = not (f a)

-- | The reverse of (>>)
(<<) :: Monad m => m b -> m a -> m b
(<<) ma mb = do a <- ma; a <$ mb

-- | Maybe -> Monad with monadic failure case
liftMaybe :: Monad m => m a -> Maybe a -> m a
liftMaybe failed = Maybe.maybe failed pure

-- | Maybe -> Monad with MonadFail string case
maybeFail :: MonadFail m => String -> Maybe a -> m a
maybeFail msg = liftMaybe (fail msg)

-- | Maybe -> Monad with MonadError error case
maybeError :: MonadError e m => e -> Maybe a -> m a
maybeError e = liftMaybe (throwError e)

-- | Maybe -> Alternative with empty case
maybeEmpty :: Alternative m => Maybe a -> m a
maybeEmpty = Maybe.maybe empty pure

-- | Maybe -> Monoid with mempty case
maybeMEmpty :: Monoid a => Maybe a -> a
maybeMEmpty = Maybe.fromMaybe mempty

-- | Lift a 4 argument function into an Applicative
liftA4 :: Applicative m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
liftA4 f ma mb mc md = liftA3 f ma mb mc <*> md

-- | An extension class for monoids allowing the Nil pattern to function
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


-- | Pattern alias for types with a Nil instance
pattern Nil :: Nil m => m
pattern Nil <- (isNil -> True) where
    Nil = nil




-- | A semantic version specifier
data Version
    = Version
    { versionMajor :: !Word
    , versionMinor :: !Word
    , versionPatch :: !Word
    }
    deriving (Show, Eq, Ord)

instance Pretty ann Version where
    pPrintPrec _ _ (Version major minor patch) = do
        shown major <> text "." <> shown minor <> text "." <> shown patch

instance Nil Version where
    nil = Version 0 0 0
    isNil = (== Nil)
