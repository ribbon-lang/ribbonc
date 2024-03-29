module Data.Attr where

import Data.Nil
import Data.Tag
import Data.Range

import Control.Exception qualified as Ex

import Text.Pretty




-- | Type alias for @Tag Attr@
type ATag = Tag Attr

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
    show = prettyShowLevel PrettyVerbose

instance Pretty Attr where
    pPrintPrec lvl prec (Attr f r) = brackets if
        | isNil f, isNil r -> "anon"
        | isNil f -> "input" <> ":" <> pPrintPrec lvl prec r
        | isNil r -> text f
        | otherwise -> text f <> ":" <> pPrintPrec lvl prec r

instance Semigroup Attr where
    a <> Nil = a
    Nil <> b = b
    a <> b = Ex.assert (a.file == b.file || isNil a.file || isNil b.file) $
        Attr a.file (a.range <> b.range)

instance Monoid Attr where
    mempty = Attr Nil Nil

instance Nil Attr where
    isNil = isNil . (.range)


attrSub :: Attr -> Attr -> Attr
attrSub = curry \case
    (a, Nil) -> a
    (Nil, b) -> b
    (a, b) -> Ex.assert (a.file == b.file || isNil a.file || isNil b.file) $
        Attr a.file (rangeSub a.range b.range)

-- | Determine if two @Attr@s are adjacent in terms of line and column
attrConnected :: Attr -> Attr -> Bool
attrConnected a b =
    a.file == b.file && rangeConnected a.range b.range


-- | Get a new @Attr@ that represents only the start of the given @Attr@
attrFlattenToStart :: Attr -> Attr
attrFlattenToStart a = a { range = rangeFlattenToStart a.range}

-- | Get a new @Attr@ that represents only the end of the given @Attr@
attrFlattenToEnd :: Attr -> Attr
attrFlattenToEnd a = a { range = rangeFlattenToEnd a.range}


-- | Fold a structure containing attributed items into a single @Attr@,
--   using a given map function to extract the @Attr@s from elements
attrFoldBy :: Foldable t => (a -> Attr) -> Attr -> t a -> Attr
attrFoldBy f = foldr ((<>) . f)


-- | Fold a structure containing @ATag@ged items into a single @Attr@
attrFold :: Foldable t => Attr -> t (ATag a) -> Attr
attrFold = attrFoldBy tagOf


-- | Fold a nested structure containing @ATag@ged items into a single @Attr@,
--   using a given map function to extract the @Attr@s from elements
attrFoldBy' :: (Foldable t1, Foldable t2) =>
    (a -> Attr) -> Attr -> t1 (t2 a) -> Attr
attrFoldBy' f = foldr (flip $ attrFoldBy f)

-- | Fold a nested structure containing @ATag@ged items into a single @Attr@
attrFold' :: (Foldable t1, Foldable t2) => Attr -> t1 (t2 (ATag a)) -> Attr
attrFold' = attrFoldBy' tagOf


-- | Create a new @Attr@ with a @Nil@ @Range@, for the given @FilePath@
fileAttr :: FilePath -> Attr
fileAttr f = Attr f mempty
