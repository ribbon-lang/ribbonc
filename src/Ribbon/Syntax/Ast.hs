module Ribbon.Syntax.Ast where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Functor


import Ribbon.Util
import Ribbon.Source
import Ribbon.Display
import Ribbon.Syntax.Literal
import Ribbon.Syntax.Token
import Ribbon.Syntax.Text




-- Type aliases ----------------------------------------------------------------


-- | Rank-1 polymorphic value with a set
--   of bound type variables and a qualifier
type Scheme a = (Quantifier (Qualified a))

-- | A scrutinizer, match expression pair in a
--   function definition or pattern match
type Case = (ATag Patt, ATag Expr)

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

-- | Constrains two types to be equivalent during constraint solving
type EqualityConstraint = (Type, Type)

-- | Constrains the first row to be a subset of the second row
type SubRowConstraint = (Type, Type)

-- | Constrains the concatenation of the first and second rows
--   to be the third row; sub rows are not necessarily disjoint
type ConcatRowConstraint = (Type, Type, Type)





-- Specification types ---------------------------------------------------------


-- | The base of a local path
data LocalPathBase
    -- | Start at the root of the active module
    = LpRoot
    -- | Start at the namespace a number of levels above the current namespace
    | LpUp !Int
    -- | Start at the import module with the given name
    | LpModule !Name
    -- | Start in the given file
    | LpFile !String
    -- | Start at the current namespace
    | LpHere
    deriving (Eq, Ord, Show)

instance Pretty ann LocalPathBase where
    pPrint = \case
        LpRoot -> text "/"
        LpUp i -> hcat $ replicate i (text "../")
        LpModule n -> text "module" <+> pPrint n
        LpFile s -> text "file" <+> doubleQuotes (text s)
        LpHere -> text "./"

localPathBaseNeedsSlash :: LocalPathBase -> Bool
localPathBaseNeedsSlash = \case
    LpModule _ -> True
    LpFile _ -> True
    _ -> False



-- | The base of an absolute path
data AbsPathBase
    -- | Start at the root of the active module
    = ApRoot
    -- | Start at the import module with the given name
    | ApModule !Name
    -- | Start in the given file
    | ApFile !String
    deriving (Eq, Ord, Show)

instance Pretty ann AbsPathBase where
    pPrint = \case
        ApRoot -> text "/"
        ApModule n -> text "module" <+> pPrint n
        ApFile s -> text "file" <+> doubleQuotes (text s)



-- | A fixity specifier for imports
data UseFixity
    -- | Infix operator of unspecified associativity
    = UseInfix
    -- | Prefix operator
    | UsePrefix
    -- | Postfix operator
    | UsePostfix
    -- | Atomic symbol
    | UseAtomic
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty ann UseFixity where
    pPrint = \case
        UseInfix -> text "infix"
        UsePrefix -> text "prefix"
        UsePostfix -> text "postfix"
        UseAtomic -> text "atom"



-- | A fixity specifier for definitions
data DefFixity
    -- | Left associative infix operator
    = DefInfixL
    -- | Right associative infix operator
    | DefInfixR
    -- | Non-associative infix operator
    | DefInfix
    -- | Prefix operator
    | DefPrefix
    -- | Postfix operator
    | DefPostfix
    -- | Atomic symbol
    | DefAtomic
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty ann DefFixity where
    pPrint = \case
        DefInfixL -> text "infixl"
        DefInfixR -> text "infixr"
        DefInfix -> text "infix"
        DefPrefix -> text "prefix"
        DefPostfix -> text "postfix"
        DefAtomic -> text "atom"

-- | Kind of a usage
data UseKind
    -- | A type usage
    = UseType
    -- | An effect usage
    | UseEffect
    -- | A value usage
    | UseValue
    -- | A namespace usage
    | UseNamespace
    deriving (Eq, Ord, Show)

instance Pretty ann UseKind where
    pPrint = \case
        UseType -> text "type"
        UseEffect -> text "effect"
        UseValue -> text "value"
        UseNamespace -> text "namespace"



-- | Visibility of a definition
data Visibility
    -- | A public definition
    = Public
    -- | A private definition
    | Private
    deriving (Eq, Ord, Show)

instance Pretty ann Visibility where
    pPrint = \case
        Public -> text "pub"
        Private -> mempty



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
    -- | A set of lexical imports
    | DkUse
    deriving (Eq, Ord, Show)

instance Pretty ann DefKind where
    pPrint = \case
        DkType -> text "type"
        DkEffect -> text "effect"
        DkValue -> text "value"
        DkNamespace -> text "namespace"
        DkUse -> text "use"




-- Names and paths -------------------------------------------------------------


-- | An unqualified name
newtype Name
    -- | Construct a Name from a String
    = Name String
    deriving (Eq, Ord, Show)

instance Pretty ann Name where
    pPrint (Name s) = text s

printNameEscaped :: Name -> Doc ann
printNameEscaped (Name s)
    | any isOperator s = backticks (text s)
    | otherwise = text s

printTagNameEscaped :: PrettyLevel -> ATag Name -> Doc ann
printTagNameEscaped lvl (n :@: a) =
    if lvl >= PrettyRich
        then printNameEscaped n <> text "@" <> brackets (pPrintPrec lvl 0 a)
        else printNameEscaped n



-- | A name to bind a definition to
data DefName
    -- | Construct a DefName from a DefFixity, a precedence, and a Name
    = DefName
    -- | The fixity associated with the binding
    { dnFixity :: !DefFixity
    -- | The precedence associated with the binding, if one was specified
    , dnPrec :: !(Maybe Prec)
    -- | The name of the binding
    , dnName :: !Name
    }
    deriving (Eq, Ord, Show)

instance Pretty ann DefName where
    pPrint (DefName f p n) =
        pPrint f <+> maybePPrint p <+> pPrint n



-- | A name for a lexical import
data UseName
    -- | Construct a UseName from an optional DefKind,
    --   an optional UseFixity, and a Name
    = UseName
    { unKind :: !(Maybe UseKind)
    , unFixity :: !(Maybe UseFixity)
    , unName :: !Name
    }
    deriving (Eq, Ord, Show)

instance Pretty ann UseName where
    pPrint (UseName k f n) =
        maybePPrint k <+> maybePPrint f <+> pPrint n



-- | An absolute reference to a definition
data AbsRef
    = AbsRef
    { anPath :: !AbsPath
    , anFixity :: !UseFixity
    , anName :: !Name
    }
    deriving (Eq, Ord, Show)

instance Pretty ann AbsRef where
    pPrint (AbsRef p f n) =
        pPrint p <> text "/" <> (pPrint f <+> pPrint n)



-- | A name with a local path prefix
data LocalPath
    -- | Construct a LocalPath from a LocalPathBase and a list of Name
    = LocalPath
    -- | The starting point of the local path
    { lpBase :: !(ATag LocalPathBase)
    -- | The subsequent components of the local path
    , lpComponents :: ![ATag Name]
    }
    deriving (Eq, Ord, Show)

instance Pretty ann LocalPath where
    pPrintPrec lvl _ (LocalPath b ns) =
        let pNs = hcat (punctuate (text "/") (printTagNameEscaped lvl <$> ns))
        in if not (null ns) && localPathBaseNeedsSlash (untag b)
            then pPrint b <> text "/" <> pNs
            else pPrint b <> pNs

localPathNeedsSlash :: LocalPath -> Bool
localPathNeedsSlash (LocalPath b ns) =
    not (null ns) || localPathBaseNeedsSlash (untag b)



-- | A name with an absolute path prefix
data AbsPath
    -- | Construct an AbsPath from a list of Name
    = AbsPath
    -- | The base of the absolute path
    { apBase :: !(ATag AbsPathBase)
    -- | The components of the absolute path
    , apComponents :: [ATag Name]
    }
    deriving (Eq, Ord, Show)

instance Pretty ann AbsPath where
    pPrintPrec lvl _ (AbsPath b ns) =
        let pNs = hcat (punctuate (text "/") (printTagNameEscaped lvl <$> ns))
        in case (untag b, ns) of
            (_, []) -> pPrintPrec lvl 0 b
            (ApRoot, _) -> pPrintPrec lvl 0 b <> pNs
            _ -> pPrintPrec lvl 0 b <> text "/" <> pNs





-- proto module data -----------------------------------------------------------


-- | A module head, containing information about the active module,
--   such as its name, version, dependencies, source directories, and
--   user-defined meta-data
data ModuleProtoHead
    -- | Construct a ModuleProtoHead from a name, a version,
    --   a list of meta-data, a list of source directories,
    --   and a list of dependencies
    = ModuleProtoHead
    -- | The name of the active module
    { mhName :: !(ATag String)
    -- | The version of the active module
    , mhVersion :: !(ATag Version)
    -- | User-defined meta-data for the active module
    , mhMeta :: ![(ATag String, ATag String)]
    -- | The source directories of the active module
    , mhSources :: ![ATag String]
    -- | The dependencies of the active module
    , mhDependencies :: ![ATag ModuleProtoDependency]
    }
    deriving (Eq, Ord, Show)

instance Pretty ann ModuleProtoHead where
    pPrint (ModuleProtoHead n v m s ds) =
        hang (text "module" <+> pPrint n <+> text "=") do
            vcat' (meta <> rest)
        where
        meta = m <&> \(k, mv) ->
            hang (text (untag k) <+> text "=") do
                pPrint mv
        rest =
            [ hang (text "version" <+> text "=") do
                doubleQuoted v
            , hang (text "sources" <+> text "=") do
                vcat' $ punctuate (text ",") (pPrint <$> s)
            , hang (text "dependencies" <+> text "=") do
                vcat' $ punctuate (text ",") (pPrint <$> ds)
            ]

emptyModuleProtoHead :: ATag String -> ModuleProtoHead
emptyModuleProtoHead name =
    ModuleProtoHead name (Version 0 0 0 :@: Nil) [] [] []



-- | An unresolved dependency declaration of the active module
data ModuleProtoDependency
    -- | Construct a ModuleProtoDependency from a name and an optional alias
    = ModuleProtoDependency
    -- | The name and version of the dependency
    { mdNameVer :: !(ATag (String, Version))
    -- | An optional alias for the dependency
    , mdAlias :: !(Maybe (ATag Name))
    }
    deriving (Eq, Ord, Show)

instance Pretty ann ModuleProtoDependency where
    pPrint (ModuleProtoDependency (T' (n, v)) a) =
        hang (doubleQuotes (text n <> text "@" <> pPrint v)) do
            maybeMEmpty ((text "as" <+>) . pPrint <$> a)



-- | Prototypical definitions, such as types, values and effects,
--   before they are fully parsed
data ProtoDef
    -- | A type definition that has not been fully parsed
    = ProtoType
        -- | The visibility of the type
        { ptVisibility :: !Visibility
        -- | The name of the type
        , ptName :: !(ATag DefName)
        -- | The body of the type definition
        , ptBody :: !(Seq (ATag Token))
        }
    -- | An effect definition that has not been fully parsed
    | ProtoEffect
        -- | The visibility of the effect
        { peVisibility :: !Visibility
        -- | The name of the effect
        , peName :: !(ATag DefName)
        -- | The body of the effect definition
        , peBody :: ![ATag ProtoEffectCase]
        }
    -- | A value definition that has not been fully parsed
    | ProtoValue
        -- | The visibility of the value
        { pvVisibility :: !Visibility
        -- | The name of the value
        , pvName :: !(ATag DefName)
        -- | The type designating head of the value definition
        , pvHead :: !(Seq (ATag Token))
        -- | The body of the value definition
        , pvBody :: !(Seq (ATag Token))
        }
    -- | A namespace definition who's members have not been fully parsed
    | ProtoNamespace
        -- | The visibility of the namespace
        { pnVisibility :: !Visibility
        -- | The name of the namespace
        , pnName :: !(ATag Name)
        -- | The members of the namespace
        , pnDefs :: ![ATag ProtoDef]
        }
    -- | A set of lexical imports
    | ProtoUse
    { puVisibility :: !Visibility
    , puBody :: !(ATag Use)
    }
    deriving (Eq, Ord, Show)

instance Pretty ann ProtoDef where
    pPrint = \case
        ProtoType v n b ->
            hang (pPrint v <+> text "type" <+> pPrint n <+> text "=")
                (pPrint b)
        ProtoEffect v n b ->
            hang (pPrint v <+> text "effect" <+> pPrint n <+> text "=")
                (pPrint b)
        ProtoValue v n h b ->
            let x = pPrint v <+> text "value" <+> pPrint n
                t = hang (x <+> text ":") (pPrint h)
            in case (Seq.null h, Seq.null b) of
                (True, True) -> x
                (False, True) -> t
                (True, False) -> hang (x <+> text "=") (pPrint b)
                (False, False) -> hang t (text "=" <+> pPrint b)
        ProtoNamespace v n ds ->
            hang (pPrint v <+> text "namespace" <+> pPrint n <+> text "=")
                (vcat' (pPrint <$> ds))
        ProtoUse v u -> pPrint v <+> text "use" <+> pPrint u

instance Pretty ann [ATag ProtoDef] where
    pPrint ds = brackets $ vcat' (pPrint <$> ds)



-- | A specific case in a prototypical effect definition
data ProtoEffectCase
    -- | Construct a prototypical effect case from a name and tokens
    = ProtoEffectCase
    -- | The name of the case
    { pcName :: !(ATag DefName)
    -- | The body of the case
    , pcBody :: !(Seq (ATag Token))
    }
    deriving (Eq, Ord, Show)

instance Pretty ann ProtoEffectCase where
    pPrint (ProtoEffectCase n b)
        = hang (pPrint n <+> text ":")
            (pPrint b)



-- | A set of lexical imports
data Use
    -- | Construct a Use from an optional base, an optional tree,
    --   and an optional alias.
    --   Note that at least one of the former two must be present
    = Use
    -- | The base of the local path to import from
    { useBase :: !(Maybe (ATag LocalPath))
    -- | What to actually import
    , useTree :: !(Maybe (ATag UseTree))
    -- | An optional alias for the import
    , useAlias :: !(Maybe (ATag DefName))
    }
    deriving (Eq, Ord, Show)

instance Pretty ann Use where
    pPrint (Use b t a)
        = (if maybe False
                (localPathNeedsSlash . untag)
                (t >>= const b)
            then maybePPrint b <> text "/" <> maybePPrint t
            else maybePPrint b <> maybePPrint t)
        <+> maybeMEmpty ((text "as" <+>) . pPrint <$> a)



-- | Body of @Use@, indicating what to import
data UseTree
    -- | A set of bindings
    = UseBranch ![ATag Use]
    -- | A path to a specific binding
    | UseLeaf !UseName
    -- | A wildcard indicating all bindings at the given path
    | UseAll
    deriving (Eq, Ord, Show)

instance Pretty ann UseTree where
    pPrint = \case
        UseBranch ts -> braces $ lsep (pPrint <$> ts)
        UseLeaf n -> pPrint n
        UseAll -> text ".."




-- Parsed ast types ------------------------------------------------------------


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
    | TConstructor !AbsRef
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



-- | A set of Constraints and a value
data Qualified a = Qualified ![ATag Constraint] !a
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)



-- | A set of binders and a value
data Quantifier a = Forall ![ATag TypeBinder] !a
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)



-- | Type-relational equations
data Constraint
    = CEqual !EqualityConstraint
    | CRow !RowConstraint
    deriving (Eq, Ord, Show)



-- | Constrains a particular relation between multiple
--   row-kinded types during constraint solving
data RowConstraint
    -- | The first row is a subset of the second row
    = CrSubRow !SubRowConstraint
    -- | The first row when concatenated with the second row,
    --   yields the third row; sub rows are not necessarily disjoint
    | CrConcatRow !ConcatRowConstraint
    deriving (Eq, Ord, Show)



-- | Terms / values
data Expr
    -- | Binds terms from the local environment
    = EVar !Name
    -- | Binds terms from the global environment
    | EGlobal !AbsRef
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
    | ESumConstructor !(ATag AbsRef) !(ATag Expr)
    -- | Inserts a sum value into a larger sum type
    | ESumExtend !(ATag AbsRef) !(ATag Expr) !(ATag Expr)

    -- | Annotates a term with a type
    | EAnn !(ATag Expr) !(ATag Type)
    deriving (Eq, Ord, Show)



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
    | PSumConstructor !(ATag AbsRef) !(ATag Patt)
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
