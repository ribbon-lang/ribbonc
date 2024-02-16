module Ribbon.Source where

import Data.Int (Int64)

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString

import Data.Bifunctor

import Control.Exception

import Ribbon.Display
import Ribbon.Util




-- | Wrapper for objects, with attribute
data Tag t a
    -- | Attaches an attribute to an object
    = Tag !t !a
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

-- | Type alias for @Tag Attr@
type ATag = Tag Attr

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
infixl 4 <@>

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




-- | Codepoint-indexed position in a source file,
--   as well as its line and column numbers
data Pos
    = Pos
    { offset :: !Int64
    , line :: !Int64
    , column :: !Int64
    }

instance Show Pos where
    show = prettyShow

instance Pretty Pos where
    pPrintPrec lvl _ (Pos o l c) =
        let s = pPrint l <> ":" <> pPrint c
        in if lvl > PrettyNormal
            then s <> parens (pPrint o)
            else s

instance Eq Pos where
    a == b = a.offset == b.offset

instance Ord Pos where
    compare a b = compare a.offset b.offset

instance Nil Pos where
    isNil = (== Nil)
    nil = Pos 0 1 1

-- | Determine if two @Pos@ are adjacent in terms of line and column
posConnected :: Pos -> Pos -> Bool
posConnected a b = a.line == b.line && a.column == b.column


-- | Range indicating the origin of a span of characters
data Range
    = Range
    { start :: !Pos
    , end :: !Pos
    }
    deriving (Eq, Ord)

instance Show Range where
    show = prettyShow

instance Pretty Range where
    pPrintPrec lvl _ (Range s e) =
        let a = pPrintPrec lvl 0 s
            b = pPrintPrec lvl 0 e
        in if s == e
            then a
            else if s.line == e.line
                then pPrint s <> "-" <> pPrint e.column
                else pPrint s <> " to " <> pPrint b

instance Semigroup Range where
    a <> b = Range
        (min a.start b.start)
        (max a.end b.end)

instance Monoid Range where
    mempty = Range Nil Nil

instance Nil Range where
    isNil = (== mempty)

-- | Create a @Range@ from a single @Pos@
unitRange :: Pos -> Range
unitRange p = Range p p

-- | Determine if two @Range@s are adjacent in terms of line and column
rangeConnected :: Range -> Range -> Bool
rangeConnected a b
     = posConnected a.end b.start
    || posConnected b.end a.start


-- | Source attribution
data Attr
    -- | Source attribution for a range of characters in a file
    = Attr
    -- | FilePath of file containing the range of an Attr
    { file :: !FilePath
    -- | Offset, Line and Column Range for an Attr
    , range :: !Range
    }
    deriving (Eq, Ord)

instance Show Attr where
    show (Attr f r) = f <> show r

instance Pretty Attr where
    pPrintPrec lvl prec (Attr f r) =
        text f <> ":" <> pPrintPrec lvl prec r

instance Semigroup Attr where
    a <> b = assert (a.file == b.file) $
        Attr a.file (a.range <> b.range)

instance Monoid Attr where
    mempty = Attr Nil mempty

instance Nil Attr where
    isNil = (== mempty)

-- | Determine if two @Attr@s are adjacent in terms of line and column
attrConnected :: Attr -> Attr -> Bool
attrConnected a b =
    a.file == b.file && rangeConnected a.range b.range



-- | Source file
data File
    -- | Source file with name, text, and line and column database
    = File
    -- | The full path of a File
    { path :: !FilePath
    -- | The name of a File as it is referenced in a module
    , name :: !FilePath
    -- | Lazy ByteString of a File's textual content
    , content :: !ByteString
    }
    deriving Show

instance Pretty File where
    pPrintPrec lvl _ = if lvl == PrettyVerbose
        then \(File path name content) -> "{" <> do
            (shown path <+> "\\" <+> text name) <> ":" $+$ do
                vcat' . fmap (indent . text) . lines . bytesToString $ content
        $+$ "}"
        else text . (.name)

instance Eq File where
    a == b = a.name == b.name

instance Ord File where
    compare a b = compare a.name b.name

instance Nil File where
    isNil = (== Nil)
    nil = File "" "" mempty

-- | Load a text file into a File object
loadFile :: FilePath -> FilePath -> IO File
loadFile fullPath name = File fullPath name <$> ByteString.readFile fullPath
