module Language.Ribbon.Syntax.Kind where

import Data.Attr

import Text.Pretty




-- | A kind expression ast.
--   Gives the type of a @Type@
data Kind
    -- | A kind created by kind inference, not yet resolved
    = KVar !KindVar
    -- | Nullary constructor such as @type@, @effect@, @constraint@, etc
    | KConstant !String
    -- | Type constructor, such as @type -> type@, @effect -> type@, etc
    | KArrow
    { input :: !(ATag Kind)
    , output :: !(ATag Kind)
    }
    deriving (Eq, Ord, Show)

instance Pretty Kind where
    pPrintPrec lvl prec = \case
        KVar v -> "''" <> shown v
        KConstant c -> text c
        KArrow a b -> maybeParens (prec > 0) do
            pPrintPrec lvl 1 a <+> "->" <+> pPrintPrec lvl 0 b

-- | A kind variable created by kind inference
type KindVar = Int

-- | Infix alias for `KArrow`
pattern (:~>:) :: ATag Kind -> ATag Kind -> Kind
pattern (:~>:) a b = KArrow a b
infixr 9 :~>:

-- | The kind of types
pattern KType :: Kind
pattern KType = KConstant "Type"

-- | The kind of type-level numbers
pattern KInt :: Kind
pattern KInt = KConstant "Int"

-- | The kind of type-level strings
pattern KString :: Kind
pattern KString = KConstant "String"

-- | The kind of effects
pattern KEffect :: Kind
pattern KEffect = KConstant "Effect"

-- | The kind of type constraints
pattern KConstraint :: Kind
pattern KConstraint = KConstant "Constraint"

-- | The kind of data rows
pattern KData :: Kind
pattern KData = KConstant "Data"

-- | The kind of effect rows
pattern KEffects :: Kind
pattern KEffects = KConstant "Effects"
