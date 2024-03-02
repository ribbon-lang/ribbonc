module Language.Ribbon.Lexical.Category where

import Text.Pretty



-- | A class for types that can have a category,
--  in the context of an overloaded name
class CatOverloaded e where
    -- | Get the @OverloadCategory@ of a value
    overloadedCategory :: e -> OverloadCategory


-- | An exact category of a @Binding@, @Ref@, or definition;
--   used for disambiguation during definition lookup
data ExactCategory
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
    -- | An associated type
    | Associate
    -- | A struct field projection
    | Projection
    -- | A union field injection
    | Injection
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty ExactCategory where
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
        Associate -> "associate"
        Projection -> "projection"
        Injection -> "injection"

instance CatOverloaded ExactCategory where
    overloadedCategory = \case
        Namespace -> ONamespace
        Instance -> OInstance
        Value -> OValue
        Decl -> OValue
        Projection -> OValue
        Injection -> OValue
        _ -> OType


-- | The generic category of a @Binding@,
--   used for determining overload compatibility
data OverloadCategory
    -- | A namespace-like item
    = ONamespace
    -- | An instance-like item
    | OInstance
    -- | A type-like item
    | OType
    -- | A value-like item
    | OValue
    -- | An unresolved item
    | OUnresolved
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty OverloadCategory where
    pPrintPrec lvl _ = \case
        ONamespace -> "namespace"
        OInstance -> "instance"
        OType -> "type"
        OValue -> "value"
        OUnresolved -> if lvl > PrettyNormal then "unresolved" else mempty

instance CatOverloaded OverloadCategory where
    overloadedCategory = id
