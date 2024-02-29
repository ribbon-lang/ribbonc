module Language.Ribbon.Syntax.Scheme where

import Data.Nil
import Data.Attr

import Text.Pretty

import Language.Ribbon.Syntax.Name
import Language.Ribbon.Syntax.Kind




-- | A binding from a name to a kind, used for quantification
data TypeBinder
    = TypeBinder
    -- | The name of a bound type
    { name :: !(ATag SimpleName)
    -- | The kind of a bound type
    , kind :: !(ATag Kind)
    }
    deriving (Eq, Ord, Show)

instance Pretty TypeBinder where
    pPrintPrec lvl _ (n `Of` k) = pPrintPrec lvl 0 n <+> ":" <+> pPrintPrec lvl 0 k

-- | Infix-convenient alias for `TypeBinder`
pattern Of :: ATag SimpleName -> ATag Kind -> TypeBinder
pattern Of n k = TypeBinder n k
{-# COMPLETE Of #-}
infix 9 `Of`


-- | A list of @TypeBinder@s quantifying a type or definition
newtype Quantifier
    = Quantifier
    { params :: [ATag TypeBinder] }
    deriving (Eq, Ord, Semigroup, Monoid, Nil, Show)

instance Pretty Quantifier where
    pPrintPrec lvl _ (Quantifier xs)
        | Nil <- xs = Nil
        | otherwise = lsep (pPrintPrec lvl 0 <$> xs)


-- | A list of type constraints qualifying a type or definition
newtype Qualifier t
    = Qualifier
    { constraints :: [ATag t] }
    deriving (Eq, Ord, Semigroup, Monoid, Nil, Show)

instance Pretty t => Pretty (Qualifier t) where
    pPrintPrec lvl _ (Qualifier xs) = qualH "where" do
        lsep (pPrintPrec lvl 0 <$> xs)
