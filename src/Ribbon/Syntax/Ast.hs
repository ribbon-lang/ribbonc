module Ribbon.Syntax.Ast where

import Data.Map (Map)
-- import Data.Map qualified as Map

import Ribbon.Source
import Ribbon.Syntax.Literal


-- | Name is just a string, for now
--   Later this may be interned, include namespacing, etc
type Name = String

-- | The type of a type
type Kind = Syn KindData

-- | The type of a type
data KindData
    -- | A constant kind, like 'Type' or 'Effect'
    = KConstant Name
    -- | A type constructor kind, like @Type :--> Type@ or @Effect :--> Type@
    | KArrow Kind Kind
    deriving (Eq, Ord, Show)


-- | Binds information about terms, effects, and rows
type Type = Syn TypeData

-- | Binds information about terms, effects, and rows
data TypeData
    -- | Type variable, either bound or created by inference
    = TVar TypeVar
    -- | A free type variable, not bound by a scheme or substitution.
    --   These are replaced during kind inference
    | TFree (Maybe Name)
    -- | Type constructor, such as @:->@ or @Int@
    | TConstructor TypeConstructor
    -- | Type application, such as @Int :-> Int@ or @Maybe Int@
    | TApp Type Type
    -- | Monoidal unordered map of Type-kinded types
    | TDataRow DataRow
    -- | Monoidal unordered set of Effect-kinded types
    | TEffectRow EffectRow
    deriving (Eq, Ord, Show)

-- | Type binders with their kind
--   Bound variables are named, inference-created "meta variables" are integers
type TypeVar = Syn TypeVarData

-- | Type binders with their kind
--   Bound variables are named, inference-created "meta variables" are integers
data TypeVarData
    -- | A named type variable, bound by a scheme
    = TvBound TypeBinder
    -- | An inference-created type variable, bound by a substitution
    | TvMeta Int Kind
    deriving (Eq, Ord, Show)


-- | A type binder from a type scheme
type TypeBinder = Syn TypeBinderData

-- | A type binder from a type scheme
type TypeBinderData = (Name, Kind)


-- | A unique name associated with a Kind
data TypeConstructor = Tc Name Kind
    deriving (Eq, Ord, Show)


-- | Monoidal unordered map of Type-kinded types
type DataRow = Map (Syn Name) Type

-- | Monoidal unordered set of Effect-kinded types
type EffectRow = [Type]



-- | Rank-1 polymorphic value with a set
--   of bound type variables and a qualifier
type Scheme a = Syn (SchemeData a)

-- | Rank-1 polymorphic value with a set
--   of bound type variables and a qualifier
data SchemeData a = Forall [TypeBinder] (Qualified a)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


-- | A set of Constraints and a value
data Qualified a = Qualified [Constraint] a
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


-- | Type-relational equations
type Constraint = Syn ConstraintData

-- | Type-relational equations
data ConstraintData
    = CEqual EqualityConstraint
    | CRow RowConstraint
    deriving (Eq, Ord, Show)

-- | Constrains two types to be equivalent during constraint solving
type EqualityConstraint = (Type, Type)

-- | Constrains a particular relation between multiple
--   row-kinded types during constraint solving
data RowConstraint
    -- | The first row is a subset of the second row
    = CrSubRow SubRowConstraint
    -- | The first row when concatenated with the second row,
    --   yields the third row; sub rows are not necessarily disjoint
    | CrConcatRow ConcatRowConstraint
    deriving (Eq, Ord, Show)

-- | Constrains the first row to be a subset of the second row
type SubRowConstraint = (Type, Type)

-- | Constrains the concatenation of the first and second rows
--   to be the third row; sub rows are not necessarily disjoint
type ConcatRowConstraint = (Type, Type, Type)


-- | Terms / values
type Expr = Syn ExprData

-- | Terms / values
data ExprData
    -- | Binds terms from the environment.
    --   This includes user-defined values and functions,
    --   but also ephemeral built-ins such as Unit
    = EVar Name
    -- | A constant, literal value
    | ELit Literal
    -- | Functional abstraction ie lambda
    | EFunction Case
    -- | Applies a function to an argument
    | EApp Expr Expr
    -- | Prefix notation for function application
    | EPrefix (Int, Name) Expr
    -- | Infix notation for function application
    | EInfix (Int, Name) Expr Expr
    -- | Postfix notation for function application
    | EPostfix (Int, Name) Expr
    -- | Performs pattern matching on a scrutinee
    | EMatch Expr [Case]

    -- | Constructs a new product
    | EProductConstructor [(Name, Expr)]
    -- | Concatenates two products
    | EProductConcat Expr Expr
    -- | Narrows a product to a smaller product type
    | EProductRestrict Expr Expr
    -- | Projects a field from a product
    | EProductProject Expr Name

    -- | Constructs a new sum
    | ESumConstructor Name Expr
    -- | Inserts a sum value into a larger sum type
    | ESumExtend Name Expr Expr

    -- | Annotates a term with a type
    | EAnn Expr Type
    deriving (Eq, Ord, Show)




-- | A scrutinizer, match expression pair in a
--   function definition or pattern match
type Case = (Patt, Expr)


-- | Patterns for matching values
type Patt = Syn PattData

-- | Patterns for matching values
data PattData
    -- | Binds an incoming value to a name
    = PVar Name
    -- | Matches a particular value, not all expressions are allowed
    | PValue Expr
    -- | Matches any value
    | PWildcard
    -- | Matches a value with a sub-pattern, and binds the value to a name
    | PAs Patt Name
    -- | Matches a product value
    | PProductConstructor [(Name, Patt)] ProductRestPattern
    -- | Matches a sum value
    | PSumConstructor Name Patt
    deriving (Eq, Ord, Show)


type ProductRestPattern = Syn ProductRestPatternData

-- | Pattern for matching the rest of a product not mentioned in the constructor
data ProductRestPatternData
    -- | Matches the unnamed fields of any product
    = PrWildcard
    -- | Matches the unnamed fields of any product,
    --   and binds the narrowed product to a name
    | PrAs Name
    -- | Disallows matching any unnamed fields
    | PrNone
    deriving (Eq, Ord, Show)
