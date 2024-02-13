module Ribbon.Syntax.Ast where

import Data.Sequence (Seq)
-- import Data.Map qualified as Map


import Ribbon.Util
import Ribbon.Source
import Ribbon.Display
import Ribbon.Syntax.Literal
import Ribbon.Syntax.Token


-- | An unqualified name
newtype Name
    -- | Construct a Name from a String
    = Name String
    deriving (Eq, Ord, Show)

instance Pretty ann Name where
    pPrint (Name s) = text s


-- | A binding with kind, fixity and precedence
data KindSpec p
    -- | A type binding
    = TypeSpec !(Spec p)
    -- | An effect binding
    | EffectSpec !(Spec p)
    -- | A value binding
    | ValueSpec !(Spec p)
    -- | A namespace binding
    | NamespaceSpec !p
    deriving (Eq, Ord, Show)

instance Pretty ann p => Pretty ann (KindSpec p) where
    pPrint = \case
        TypeSpec s -> text "type" <+> pPrint s
        EffectSpec s -> text "effect" <+> pPrint s
        ValueSpec s -> text "value" <+> pPrint s
        NamespaceSpec p -> text "namespace" <+> pPrint p

-- | A binding with fixity and precedence
data Spec p
    -- | Construct a Spec from a fixity, optional precedence, and the value
    = Spec
    -- | The fixity associated with the binding
    { specFixity :: !Fixity
    -- | The precedence associated with the binding, if one was specified
    , specPrec :: !(Maybe Prec)
    -- | The binding itself
    , specValue :: !p
    }
    deriving (Eq, Ord, Show)

instance Pretty ann p => Pretty ann (Spec p) where
    pPrint (Spec f p v)
        = pPrint f <+> maybeMEmpty (shown <$> p) <+> pPrint v

-- | A name with a local path prefix
data LocalPath
    -- | Construct a LocalPath from a LocalPathBase and a list of Name
    = LocalPath
    -- | The starting point of the local path
    { lpBase :: !LocalPathBase
    -- | The subsequent components of the local path
    , lpComponents :: ![Name]
    }
    deriving (Eq, Ord, Show)

instance Pretty ann LocalPath where
    pPrint (LocalPath b ns)
        = pPrint b <> text "." <> hcat (punctuate (text ".") (pPrint <$> ns))

-- | The base of a local path
data LocalPathBase
    -- | Start at the root of the active module
    = LpRoot
    -- | Start at the namespace @n@ above the current namespace
    | LpUp !Int
    -- | Start at the given module
    | LpModule !Name
    -- | Start in the given file
    | LpFile !String
    deriving (Eq, Ord, Show)

instance Pretty ann LocalPathBase where
    pPrint = \case
        LpRoot -> text "/"
        LpUp i -> hcat $ replicate i (text "../")
        LpModule n -> text "module" <+> pPrint n
        LpFile s -> text "file" <+> doubleQuotes (text s)

-- | A name with an absolute path prefix
newtype AbsPath
    -- | Construct an AbsPath from a list of Name
    = AbsPath [Name]
    deriving (Eq, Ord, Show)

instance Pretty ann AbsPath where
    pPrint (AbsPath ns) = hcat $ punctuate (text ".") (pPrint <$> ns)

-- | A fixity specifier
data Fixity
    -- | Left associative infix operator
    = InfixL
    -- | Right associative infix operator
    | InfixR
    -- | Non-associative infix operator
    | Infix
    -- | Prefix operator
    | Prefix
    -- | Postfix operator
    | Postfix
    -- | Atomic symbol
    | Atomic
    deriving (Eq, Ord, Show)

instance Pretty ann Fixity where
    pPrint = \case
        InfixL -> text "infixl"
        InfixR -> text "infixr"
        Infix -> text "infix"
        Prefix -> text "prefix"
        Postfix -> text "postfix"
        Atomic -> text "atom"


-- | Prototypical definitions, such as types, values and effects,
--   before they are fully parsed
data ProtoDef
    -- | A type definition that has not been fully parsed
    = ProtoType
        -- | The name of the type
        { ptName :: !(ATag (Spec Name))
        -- | The body of the type definition
        , ptBody :: !(Seq (ATag Token))
        }
    -- | An effect definition that has not been fully parsed
    | ProtoEffect
        -- | The name of the effect
        { peName :: !(ATag (Spec Name))
        -- | The body of the effect definition
        , peBody :: ![ATag ProtoEffectCase]
        }
    -- | A value definition that has not been fully parsed
    | ProtoValue
        -- | The name of the value
        { pvName :: !(ATag (Spec Name))
        -- | The type designating head of the value definition
        , pvHead :: !(Seq (ATag Token))
        -- | The body of the value definition
        , pvBody :: !(Seq (ATag Token))
        }
    -- | A namespace definition who's members have not been fully parsed
    | ProtoNamespace
        -- | The name of the namespace
        { pnName :: !(ATag Name)
        -- | The members of the namespace
        , pnDefs :: ![ATag ProtoDef]
        }
    deriving (Eq, Ord, Show)

instance Pretty ann ProtoDef where
    pPrint = \case
        ProtoType n b ->
            hang (text "type" <+> pPrint n <+> text "=")
                (pPrint b)
        ProtoEffect n b ->
            hang (text "effect" <+> pPrint n <+> text "=")
                (pPrint b)
        ProtoValue n h b ->
            hang (hang (pPrint n <> text ":")
                    (pPrint h))
                (text "=" <+> pPrint b)
        ProtoNamespace n ds ->
            hang (text "namespace" <+> pPrint n <+> text "=")
                (vcat' (pPrint <$> ds))

-- | A specific case in a prototypical effect definition
data ProtoEffectCase
    -- | Construct a prototypical effect case from a name and tokens
    = ProtoEffectCase
    -- | The name of the case
    { pcName :: !(ATag (Spec Name))
    -- | The body of the case
    , pcBody :: !(Seq (ATag Token))
    }
    deriving (Eq, Ord, Show)

instance Pretty ann ProtoEffectCase where
    pPrint (ProtoEffectCase n b)
        = hang (pPrint n <+> text ":")
            (pPrint b)

-- | Kind of a definition
data DefKind
    -- | A type definition
    = DkType
    -- | An effect definition
    | DkEffect
    -- | A value definition
    | DkValue
    -- | A namespace definition
    | DkNamespace
    deriving (Eq, Ord, Show)

instance Pretty ann DefKind where
    pPrint = \case
        DkType -> text "type"
        DkEffect -> text "effect"
        DkValue -> text "value"
        DkNamespace -> text "namespace"

-- | The type of a type
data Kind
    -- | A constant kind, like 'Type' or 'Effect'
    = KConstant !Name
    -- | A type constructor kind, like @Type :--> Type@ or @Effect :--> Type@
    | KArrow !(ATag Kind) !(ATag Kind)
    deriving (Eq, Ord, Show)


-- | Binds information about terms, effects, and rows
data Type
    -- | Type variable, either bound or created by inference
    = TVar !TypeVar
    -- | A free type variable, not bound by a scheme, substitution, or env.
    --   These are replaced during kind inference
    | TFree !(Maybe Name)
    -- | Type constructor, such as @:->@ or @Int@
    | TConstructor !(Spec AbsPath)
    -- | Type-level constants, such as @Int@, @String@
    | TConstant !TypeConstant
    -- | Type application, such as @Int :-> Int@ or @Maybe Int@
    | TApp !(ATag Type) !(ATag Type)
    -- | Monoidal unordered map of Type-kinded types
    | TDataRow !DataRow
    -- | Monoidal unordered set of Effect-kinded types
    | TEffectRow !EffectRow
    deriving (Eq, Ord, Show)

-- | Type-level constants, such as @Int@, @String@
data TypeConstant
    = TcInt !Int
    | TcString !String
    deriving (Eq, Ord, Show)


-- | Type binders with their kind
--   Bound variables are named, inference-created "meta variables" are integers
data TypeVar
    -- | A named type variable, bound by a scheme
    = TvBound !TypeBinder
    -- | An inference-created type variable, bound by a substitution
    | TvMeta !Int !(ATag Kind)
    deriving (Eq, Ord, Show)


-- | A type binder from a type scheme
type TypeBinder = (ATag Name, ATag Kind)


-- | Monoidal set of Fields
type DataRow = [Field (ATag Type)]

-- | Field in a DataRow
type Field = (,) Label

-- | Label for a field in a DataRow
type Label = (ATag Type, ATag Type)

-- | Monoidal unordered set of Effect-kinded types
type EffectRow = [ATag Type]



-- | Rank-1 polymorphic value with a set
--   of bound type variables and a qualifier
type Scheme a = ATag (SchemeData a)

-- | Rank-1 polymorphic value with a set
--   of bound type variables and a qualifier
data SchemeData a = Forall ![TypeBinder] !(Qualified a)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


-- | A set of Constraints and a value
data Qualified a = Qualified ![Constraint] !a
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


-- | Type-relational equations
type Constraint = ATag ConstraintData

-- | Type-relational equations
data ConstraintData
    = CEqual !EqualityConstraint
    | CRow !RowConstraint
    deriving (Eq, Ord, Show)

-- | Constrains two types to be equivalent during constraint solving
type EqualityConstraint = (Type, Type)

-- | Constrains a particular relation between multiple
--   row-kinded types during constraint solving
data RowConstraint
    -- | The first row is a subset of the second row
    = CrSubRow !SubRowConstraint
    -- | The first row when concatenated with the second row,
    --   yields the third row; sub rows are not necessarily disjoint
    | CrConcatRow !ConcatRowConstraint
    deriving (Eq, Ord, Show)

-- | Constrains the first row to be a subset of the second row
type SubRowConstraint = (Type, Type)

-- | Constrains the concatenation of the first and second rows
--   to be the third row; sub rows are not necessarily disjoint
type ConcatRowConstraint = (Type, Type, Type)


-- | Terms / values
data Expr
    -- | Binds terms from the local environment
    = EVar !Name
    -- | Binds terms from the global environment
    | EGlobal !(Spec AbsPath)
    -- | A constant, literal value
    | ELit !Literal
    -- | Functional abstraction ie lambda
    | EFunction !(ATag Patt) !(ATag Expr)
    -- | Applies a function to an argument
    | EApp !(ATag Expr) !(ATag Expr)
    -- | Let bindings
    | ELet !(ATag Patt) !(ATag Expr) !(ATag Expr)
    -- | Performs pattern matching on a scrutinee
    | EMatch !(ATag Expr) ![Case]
    -- | Sequencing of effectful expressions
    | ESequence !(ATag Expr) !(ATag Expr)
    -- | A compound expression that has not been completely parsed
    | ECompound ![ATag Expr]

    -- | Constructs a new product
    | EProductConstructor ![Field (ATag Expr)]
    -- | Concatenates two products
    | EProductConcat !(ATag Expr) !(ATag Expr)
    -- | Narrows a product to a smaller product type
    | EProductRestrict !(ATag Expr) !(ATag Expr)
    -- | Projects a field from a product
    | EProductProject !(ATag Expr) !(ATag Name)

    -- | Constructs a new sum
    | ESumConstructor !(ATag (Spec AbsPath)) !(ATag Expr)
    -- | Inserts a sum value into a larger sum type
    | ESumExtend !(ATag Name) !(ATag Expr) !(ATag Expr)

    -- | Annotates a term with a type
    | EAnn !(ATag Expr) !(ATag Type)
    deriving (Eq, Ord, Show)




-- | A scrutinizer, match expression pair in a
--   function definition or pattern match
type Case = (ATag Patt, ATag Expr)


-- | Patterns for matching values
data Patt
    -- | Matches any value
    = PWildcard
    -- | Binds an incoming value to a name
    | PVar !Name
    -- | Matches a particular value, not all expressions are allowed
    | PValue !(ATag Expr)
    -- | Matches a value with a sub-pattern, and binds the value to a name
    | PAs !(ATag Patt) !(ATag Name)
    -- | Matches a product value
    | PProductConstructor ![Field (ATag Patt)] !(ATag ProductRestPattern)
    -- | Matches a sum value
    | PSumConstructor !(ATag Name) !(ATag Patt)
    deriving (Eq, Ord, Show)


-- | Pattern for matching the rest of a product not mentioned in the constructor
data ProductRestPattern
    -- | Disallows matching any unnamed fields
    = PrNone
    -- | Matches the unnamed fields of any product
    | PrWildcard
    -- | Matches the unnamed fields of any product,
    --   and binds the narrowed product to a name
    | PrAs !Name
    deriving (Eq, Ord, Show)
