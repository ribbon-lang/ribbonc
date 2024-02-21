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
    pPrintPrec lvl prec (Attr f r) = brackets do
        text f <> ":" <> pPrintPrec lvl prec r

instance Semigroup Attr where
    a <> b = Ex.assert (a.file == b.file) $
        Attr a.file (a.range <> b.range)

instance Monoid Attr where
    mempty = Attr Nil mempty

instance Nil Attr where
    isNil = (== mempty)

-- | Determine if two @Attr@s are adjacent in terms of line and column
attrConnected :: Attr -> Attr -> Bool
attrConnected a b =
    a.file == b.file && rangeConnected a.range b.range
