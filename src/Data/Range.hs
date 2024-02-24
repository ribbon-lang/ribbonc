module Data.Range where

import Data.Pos
import Data.Nil

import Text.Pretty

-- | @Pos@ Range indicating the positional origin of an object
data Range
    = Range
    -- | @Pos@ the @Range@ starts at
    { start :: !Pos
    -- | @Pos@ the @Range@ ends at
    , end :: !Pos
    }
    deriving (Eq, Ord)

instance Show Range where
    show = prettyShowLevel PrettyVerbose

instance Pretty Range where
    pPrintPrec lvl _ (Range s e) =
        let a = pPrintPrec lvl 0 s
            b = pPrintPrec lvl 0 e
        in if s == e
            then a
            else if s.line == e.line
                then a <> "-" <> pPrint e.column
                else a <+> "to" <+> b

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

-- | Get a new @Range@ that represents only the start of the given @Range@
rangeFlattenToStart :: Range -> Range
rangeFlattenToStart = unitRange . (.start)

-- | Get a new @Range@ that represents only the end of the given @Range@
rangeFlattenToEnd :: Range -> Range
rangeFlattenToEnd = unitRange . (.end)
