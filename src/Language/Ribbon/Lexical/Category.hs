module Language.Ribbon.Lexical.Category where

import Language.Ribbon.Lexical.Fixity

import Text.Pretty




-- | A class for types that can have a @Category@
class HasCategory a where
    -- | Get the @Category@ of a value
    getCategory :: a -> Category

-- | An exact category of a @Binding@, @Ref@, or definition;
--   used for disambiguation during definition lookup
data Category
    -- | A namespace item
    = Namespace
    -- | An effect type def item
    | Effect
    -- | A class type def item
    | Class
    -- | An instance def item
    | Instance
    -- | A struct type def item
    | Struct
    -- | A union type def item
    | Union
    -- | A type alias
    | Alias
    -- | A value definition
    | Value
    -- | A value declaration
    | Decl
    -- | An effect case
    | Case
    -- | A struct field projection
    | Projection
    -- | A union field injection
    | Injection
    deriving (Eq, Ord, Show, Enum, Bounded)

instance HasCategory Category where
    getCategory = id

instance Pretty Category where
    pPrint = \case
        Namespace -> "namespace"
        Effect -> "effect"
        Class -> "class"
        Instance -> "instance"
        Struct -> "struct"
        Union -> "union"
        Alias -> "type"
        Value -> "value"
        Decl -> "decl"
        Case -> "case"
        Projection -> "projection"
        Injection -> "injection"


-- | Wrapper for an item with a @Category@
data Categorical a
    = Categorical
    { category :: !Category
    ,    value :: !a
    }
    deriving (Eq, Ord, Functor, Foldable, Traversable, Show)

instance Pretty a => Pretty (Categorical a) where
    pPrint (Categorical c v) = pPrint c <+> pPrint v

instance HasCategory (Categorical a) where
    getCategory = (.category)

instance HasFixity a => HasFixity (Categorical a) where
    getFixity = getFixity . (.value)
