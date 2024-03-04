module Language.Ribbon.Syntax.Data where

import Data.Attr

import Text.Pretty

import Language.Ribbon.Syntax.Type




-- | A binding between a @Label@ and some type @t@
data FieldType t
    = FieldType
    { label :: !Label
    , ty :: !(ATag t)
    }
    deriving (Eq, Ord, Show)

instance Pretty t => Pretty (FieldType t) where
    pPrintPrec lvl _ (FieldType a b) =
        pPrintPrec lvl 0 a <+> ":" <+> pPrintPrec lvl 0 b

pattern (:::) :: Label -> ATag t -> FieldType t
pattern a ::: b = FieldType a b


-- | A pair of types @t@ binding a layout and a name in a @Field@
data Label
    = Label
    { offset :: !(ATag Type)
    , name :: !(ATag Type)
    }
    deriving (Eq, Ord, Show)

pattern (:\\:) :: ATag Type -> ATag Type -> Label
pattern a :\\: b = Label a b

instance Pretty Label where
    pPrintPrec lvl _ (Label a b) =
        pPrintPrec lvl 0 a <+> "\\" <+> pPrintPrec lvl 0 b
