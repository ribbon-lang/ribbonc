module Language.Ribbon.Syntax.Data where

import Data.Attr

import Text.Pretty

import Language.Ribbon.Syntax.Type




-- | A binding between a @Label@ and some type @t@
data Field t
    = Field
    { label :: !Label
    , ty :: !(ATag t)
    }
    deriving (Eq, Ord, Show)

instance Pretty t => Pretty (Field t) where
    pPrintPrec lvl _ (Field a b) =
        pPrintPrec lvl 0 a <+> ":" <+> pPrintPrec lvl 0 b

pattern (:::) :: Label -> ATag t -> Field t
pattern a ::: b = Field a b


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
