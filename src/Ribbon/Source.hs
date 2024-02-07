module Ribbon.Source where

import Data.Int (Int64)

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString

import Data.Bifunctor
import Control.Exception

import Ribbon.Util
import Ribbon.Display (Display(..))
import Ribbon.Display qualified as Display


-- | Wrapper for objects, with attribute
data Tag t a
    -- | Attaches an attribute to an object
    = Tag t a
    deriving (Show, Functor, Foldable, Traversable)

-- | Infix pattern alias for Tag
pattern (:@:) :: a -> t -> Tag t a
pattern a :@: t = Tag t a
infixl 9 :@:
{-# COMPLETE (:@:) #-}

-- | Pattern alias for Tag without attribute
pattern T' :: a -> Tag t a
pattern T' a <- a :@: _
{-# COMPLETE T' #-}


-- | Codepoint-indexed position in a source file,
--   as well as its line and column numbers
data Pos
    = Pos
    { posOffset :: !Int64
    , posLine :: !Int64
    , posColumn :: !Int64
    }


-- | Range indicating the origin of a span of characters
data Range
    = Range
    { rangeStart :: !Pos
    , rangeEnd :: !Pos
    }
    deriving (Eq, Ord)


-- | Source attribution
data Attr
    -- | Source attribution for a range of characters in a file
    = Attr
    -- | File containing the range of an Attr
    { attrFile :: !File
    -- | Offset, Line and Column Range for an Attr
    , attrRange :: !Range
    }
    deriving (Eq, Ord)


-- | Source file
data File
    -- | Source file with name, text, and line and column database
    = File
    -- | Name or path of a File
    { fileName :: !String
    -- | Lazy ByteString of a File's textual content
    , fileContent :: !ByteString
    }
    deriving Show



instance Bifunctor Tag where
    bimap f g (a :@: t) = g a :@: f t

instance Eq a => Eq (Tag t a) where
    a == b = untag a == untag b

instance Ord a => Ord (Tag t a) where
    compare a b = compare (untag a) (untag b)

instance Display a => Display (Tag t a) where
    display = display . untag



instance Show Pos where
    show (Pos o l c)
         = show l <> ":" <> show c
        <> Display.parens ("@" <> show o)

instance Display Pos where
    display (Pos _ l c) = show l <> ":" <> show c

instance Eq Pos where
    a == b = posOffset a == posOffset b

instance Ord Pos where
    compare a b = compare (posOffset a) (posOffset b)

instance Nil Pos where
    isNil = (== Nil)
    nil = Pos 0 1 1


instance Show Range where
    show (Range s e) = show s <> " to " <> show e

instance Display Range where
    display (Range s e) = display s <> " to " <> display e

instance Semigroup Range where
    a <> b = Range
        (min (rangeStart a) (rangeStart b))
        (max (rangeEnd a) (rangeEnd b))

instance Monoid Range where
    mempty = Range Nil Nil

instance Nil Range where
    isNil = (== mempty)



instance Show Attr where
    show (Attr r f) = display f <> ":" <> show r

instance Display Attr where
    display (Attr r f) = display f <> ":" <> display r

instance Semigroup Attr where
    a <> b = assert (attrFile a == attrFile b) $
        Attr (attrFile a) (attrRange a <> attrRange b)

instance Monoid Attr where
    mempty = Attr (File "" ByteString.empty) mempty

instance Nil Attr where
    isNil = (== mempty)


instance Display File where
    display = fileName

instance Eq File where
    a == b = fileName a == fileName b

instance Ord File where
    compare a b = compare (fileName a) (fileName b)



loadFile :: String -> IO File
loadFile name = File name <$> ByteString.readFile name



-- | @(a :\@:) <$> t@
(<@>) :: Functor f => a -> f t -> f (Tag t a)
a <@> t = (a :@:) <$> t
infixl 4 <@>

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
tagApp1With
    :: Semigroup t
    => t
    -> (Tag t a -> b)
    -> Tag t a -> Tag t b
tagApp1With t f x = f x :@: (t <> tagOf x)


-- | Tag t application with attribute concatenation
--
--   i.e. @_ f x y = f x y :\@: (tagOf x <> tagOf y)@
tagApp2
    :: Semigroup t
    => (Tag t a -> Tag t b -> c)
    -> Tag t a -> Tag t b -> Tag t c
tagApp2 f x y = f x y :@: (tagOf x <> tagOf y)

-- | Tag t application with concatenation of attribute
--
--   i.e. @_ t f x y = f x y :\@: (t <> tagOf x <> tagOf y)@
tagApp2With
    :: Semigroup t
    => t
    -> (Tag t a -> Tag t b -> c)
    -> Tag t a -> Tag t b -> Tag t c
tagApp2With t f x y = f x y :@: (t <> tagOf x <> tagOf y)


-- | Tag t application with attribute concatenation
--
--   i.e. @_ f x y z = f x y z :\@: (tagOf x <> tagOf y <> tagOf z)@
tagApp3
    :: Semigroup t
    => (Tag t a -> Tag t b -> Tag t c -> d)
    -> Tag t a -> Tag t b -> Tag t c -> Tag t d
tagApp3 f x y z = f x y z :@: (tagOf x <> tagOf y <> tagOf z)

-- | Tag t application with concatenation of attribute
--
--   i.e. @_ t f x y z = f x y z :\@:
--   (t <> tagOf x <> tagOf y <> tagOf z)@
tagApp3With
    :: Semigroup t
    => t
    -> (Tag t a -> Tag t b -> Tag t c -> d)
    -> Tag t a -> Tag t b -> Tag t c -> Tag t d
tagApp3With t f x y z = f x y z :@: (t <> tagOf x <> tagOf y <> tagOf z)


-- | Tag t application with attribute concatenation
--
--   i.e. @_ f x y z = f x y z :\@: (tagOf x <> tagOf y <> tagOf z)@
tagApp4
    :: Semigroup t
    => (Tag t a -> Tag t b -> Tag t c -> d)
    -> Tag t a -> Tag t b -> Tag t c -> Tag t d
tagApp4 f x y z = f x y z :@: (tagOf x <> tagOf y <> tagOf z)

-- | Tag t application with concatenation of attribute
--
--   i.e. @_ t f x y z w = f x y z w :\@:
--   (t <> tagOf x <> tagOf y <> tagOf z <> tagOf w)@
tagApp4With
    :: Semigroup t
    => t
    -> (Tag t a -> Tag t b -> Tag t c -> Tag t d -> e)
    -> Tag t a -> Tag t b -> Tag t c -> Tag t d -> Tag t e
tagApp4With t f x y z w = f x y z w :@:
    (t <> tagOf x <> tagOf y <> tagOf z <> tagOf w)



-- | Apply a new attribute to a Tag
reTag :: t -> Tag t a -> Tag t a
reTag x a = untag a :@: x

-- | Apply a new attribute to a Tag, using the attribute of another Tag
--
--   i.e. @_ a b = untag b :\@: tagOf a@
reTagFrom :: Tag t a -> Tag t b -> Tag t b
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
