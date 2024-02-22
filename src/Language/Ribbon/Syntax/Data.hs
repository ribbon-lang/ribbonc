module Language.Ribbon.Syntax.Data where

import Data.Attr

import Text.Pretty




-- | A binding between a @Label@ and some type @t@
data FieldType t
    = FieldType
    { label :: !(Label t)
    , ty :: !(ATag t)
    }
    deriving (Eq, Ord, Show)

instance Pretty t => Pretty (FieldType t) where
    pPrintPrec lvl _ (FieldType a b) =
        pPrintPrec lvl 0 a <+> ":" <+> pPrintPrec lvl 0 b

pattern (:::) :: Label t -> ATag t -> FieldType t
pattern a ::: b = FieldType a b

-- | A pair of types @t@ binding a layout and a name in a @Field@
data Label t
    = Label
    { offset :: !(ATag t)
    , name :: !(ATag t)
    }
    deriving (Eq, Ord, Show)

pattern (:\\:) :: ATag t -> ATag t -> Label t
pattern a :\\: b = Label a b

instance Pretty t => Pretty (Label t) where
    pPrintPrec lvl _ (Label a b) =
        pPrintPrec lvl 0 a <+> "\\" <+> pPrintPrec lvl 0 b
