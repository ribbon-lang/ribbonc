module Language.Ribbon.Syntax.Kind where

import Data.Attr

import Text.Pretty




-- | A kind expression ast.
--   Gives the type of a @Type@
data Kind
    -- | A kind created by kind inference, not yet resolved
    = KVar !String
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
        KVar v -> "''" <> text v
        KConstant c -> text c
        KArrow a b -> maybeParens (prec > 0) do
            pPrintPrec lvl 1 a <+> "->" <+> pPrintPrec lvl 0 b


-- | Infix alias for `KArrow`
pattern (:~>:) :: ATag Kind -> ATag Kind -> Kind
pattern (:~>:) a b = KArrow a b
infixr 9 :~>:

-- | The kind of types
pattern KType :: Kind
pattern KType = KConstant "type"

-- | The kind of type-level numbers
pattern KNum :: Kind
pattern KNum = KConstant "num"

-- | The kind of type-level strings
pattern KStr :: Kind
pattern KStr = KConstant "str"

-- | The kind of effects
pattern KEffect :: Kind
pattern KEffect = KConstant "effect"

-- | The kind of type constraints
pattern KConstraint :: Kind
pattern KConstraint = KConstant "constraint"

-- | The kind of data rows
pattern KData :: Kind
pattern KData = KConstant "data"

-- | The kind of effect rows
pattern KEffects :: Kind
pattern KEffects = KConstant "effects"
