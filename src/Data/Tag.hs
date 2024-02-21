module Data.Tag where

import Data.Bifunctor

import Text.Pretty


-- | Wrapper for objects, with attribute
data Tag t a
    -- | Attaches an attribute to an object
    = Tag { tag :: !t, value :: !a }
    deriving (Functor, Foldable, Traversable)

-- | Infix pattern alias for Tag
pattern (:@:) :: a -> t -> Tag t a
pattern a :@: t = Tag t a
infixl 9 :@:
{-# COMPLETE (:@:) #-}

-- | Pattern alias for Tag without attribute
pattern T' :: a -> Tag t a
pattern T' a <- a :@: _
{-# COMPLETE T' #-}

instance {-# OVERLAPPABLE #-} (Show t, Show a) => Show (Tag t a) where
    show (a :@: t) = render $
        parens (shown a) <> "@" <> shown t

instance {-# OVERLAPPABLE #-} (Pretty t, Pretty a) => Pretty (Tag t a) where
    pPrintPrec l p (a :@: t) =
        if l >= PrettyRich
            then parens (pPrintPrec l 0 a) <> "@" <> brackets (pPrintPrec l 0 t)
            else pPrintPrec l p a

instance Bifunctor Tag where
    bimap f g (a :@: t) = g a :@: f t

instance Eq a => Eq (Tag t a) where
    a == b = untag a == untag b

instance Ord a => Ord (Tag t a) where
    compare a b = compare (untag a) (untag b)-- | @(a :\@:) <$> t@

-- | Lift @:\@:@ over a functor, ie @fmap (a :\@:) t@
(<@>) :: Functor f => a -> f t -> f (Tag t a)
a <@> t = (a :@:) <$> t
infixl 5 <@>

-- | Compositional @untag@
untagged :: (a -> b) -> (Tag t a -> b)
untagged f (a :@: _) = f a

-- | Get the object inside Tag
untag :: Tag t a -> a
untag (a :@: _) = a

-- | Get the attribute of a Tag
tagOf :: Tag t a -> t
tagOf (_ :@: t) = t

-- | Convert a Tag to a pair
tagSplit :: Tag t a -> (a, t)
tagSplit (a :@: t) = (a, t)

-- | Tag mapping with attribute pass-through
--
--   i.e. @_ f x = f x :\@: (tagOf x)@
tagApp1 :: (Tag t a -> b) -> (Tag t a -> Tag t b)
tagApp1 f x = f x :@: tagOf x

-- | Tag mapping with concatenation of attribute
--
--   i.e. @_ t f x = f x :\@: (t <> tagOf x)@
tagApp1With :: Semigroup t =>
    t -> (Tag t a -> b) -> Tag t a -> Tag t b
tagApp1With t f x = f x :@: (t <> tagOf x)


-- | Tag t application with attribute concatenation
--
--   i.e. @_ f x y = f x y :\@: (tagOf x <> tagOf y)@
tagApp2 :: Semigroup t =>
    (Tag t a -> Tag t b -> c) -> Tag t a -> Tag t b -> Tag t c
tagApp2 f x y = f x y :@: (tagOf x <> tagOf y)

-- | Tag t application with concatenation of attribute
--
--   i.e. @_ t f x y = f x y :\@: (t <> tagOf x <> tagOf y)@
tagApp2With ::
    Semigroup t =>
        t -> (Tag t a -> Tag t b -> c) -> Tag t a -> Tag t b -> Tag t c
tagApp2With t f x y = f x y :@: (t <> tagOf x <> tagOf y)


-- | Tag t application with attribute concatenation
--
--   i.e. @_ f x y z = f x y z :\@: (tagOf x <> tagOf y <> tagOf z)@
tagApp3 :: Semigroup t =>
    (Tag t a -> Tag t b -> Tag t c -> d)
        -> Tag t a -> Tag t b -> Tag t c -> Tag t d
tagApp3 f x y z = f x y z :@: (tagOf x <> tagOf y <> tagOf z)

-- | Tag t application with concatenation of attribute
--
--   i.e. @_ t f x y z = f x y z :\@:
--   (t <> tagOf x <> tagOf y <> tagOf z)@
tagApp3With :: Semigroup t =>
    t -> (Tag t a -> Tag t b -> Tag t c -> d)
        -> Tag t a -> Tag t b -> Tag t c -> Tag t d
tagApp3With t f x y z = f x y z :@: (t <> tagOf x <> tagOf y <> tagOf z)


-- | Tag t application with attribute concatenation
--
--   i.e. @_ f x y z = f x y z :\@: (tagOf x <> tagOf y <> tagOf z)@
tagApp4 :: Semigroup t =>
    (Tag t a -> Tag t b -> Tag t c -> d)
        -> Tag t a -> Tag t b -> Tag t c -> Tag t d
tagApp4 f x y z = f x y z :@: (tagOf x <> tagOf y <> tagOf z)

-- | Tag t application with concatenation of attribute
--
--   i.e. @_ t f x y z w = f x y z w :\@:
--   (t <> tagOf x <> tagOf y <> tagOf z <> tagOf w)@
tagApp4With :: Semigroup t =>
    t -> (Tag t a -> Tag t b -> Tag t c -> Tag t d -> e)
        -> Tag t a -> Tag t b -> Tag t c -> Tag t d -> Tag t e
tagApp4With t f x y z w = f x y z w :@:
    (t <> tagOf x <> tagOf y <> tagOf z <> tagOf w)

-- | Apply a new attribute to a Tag
reTag :: t' -> Tag t a -> Tag t' a
reTag x a = untag a :@: x

-- | Apply a new attribute to a Tag, using the attribute of another Tag
--
--   i.e. @_ a b = untag b :\@: tagOf a@
reTagFrom :: Tag t a -> Tag t' b -> Tag t b
reTagFrom a b = untag b :@: tagOf a

-- | Create a new Tag using a fresh object
--   and the attribute of an existing Tag
--
-- i.e. @_ a b = b :\@: tagOf a@
takeTag :: Tag t a -> b -> Tag t b
takeTag a b = b :@: tagOf a

-- | Map the attribute of a Tag
mapTag :: (t -> t') -> Tag t a -> Tag t' a
mapTag f (a :@: x) = a :@: f x

-- | Construct a recursive Tag from an object,
--   assigning both new Tag objects the same attribute
--
--  i.e. @_ t f a = f (a :\@: t) :\@: t@
tagCon1 :: t -> (Tag t a -> b) -> a -> Tag t b
tagCon1 t f a = f (a :@: t) :@: t

-- | Construct a recursive Tag from two objects,
--   assigning all new Tag objects the same attribute
--
--  i.e. @_ t f a b = f (a :\@: t) (b :\@: t) :\@: t@
tagCon2 :: t -> (Tag t a -> Tag t b -> c) -> a -> b -> Tag t c
tagCon2 t f a b = f (a :@: t) (b :@: t) :@: t
