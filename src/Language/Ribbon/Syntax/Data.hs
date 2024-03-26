module Language.Ribbon.Syntax.Data where

import Data.Attr

import Text.Pretty




-- | A binding between a @Label@ and some type @t@
data Field t
    = Field
    { label :: !(Label t)
    , ty :: !(ATag t)
    }
    deriving (Eq, Ord, Show)

instance Pretty t => Pretty (Field t) where
    pPrintPrec lvl _ (Field a b) =
        pPrintPrec lvl 0 a <+> ":" <+> pPrintPrec lvl 0 b

pattern (:::) :: Label t -> ATag t -> Field t
pattern a ::: b = Field a b


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
