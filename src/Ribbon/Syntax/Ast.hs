module Ribbon.Syntax.Ast where

import Data.Functor
import Data.Function

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map


import Ribbon.Util
import Ribbon.Source
import Ribbon.Display
import Ribbon.Syntax.Literal
import Ribbon.Syntax.Token
import Ribbon.Syntax.Text
import Data.Set (Set)
import Data.Foldable (toList)




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

-- | A complete module definition
type Module = ModuleTree Def AliasImports

-- | A module that has been converted to a tree, but is not yet fully parsed
type MidModule = ModuleTree MidDef MidImports

type AliasNamespace = Namespace Def AliasImports

type MidNamespace = Namespace MidDef MidImports

-- | A complete definition
type Def = DefF
    (Quantifier (ATag TypeDef))
    (Scheme (EffectCaseTable (ATag Type)))
    (Scheme (ATag Type, ATag Expr))
    AliasImports

-- | A definition in the process of being converted to a tree
type MidDef = DefF
    (Seq (ATag Token))
    (EffectCaseTable (Seq (ATag Token)))
    (Seq (ATag Token), Seq (ATag Token))
    MidImports





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
    | LpFile !FilePath
    -- | Start at the current namespace
    | LpHere
    deriving (Eq, Ord, Show)

instance Pretty LocalPathBase where
    pPrint = \case
        LpRoot -> "/"
        LpUp i -> hcat $ replicate i "../"
        LpModule n -> "module" <+> pPrint n
        LpFile s -> "file" <+> doubleQuotes (text s)
        LpHere -> "./"

localPathBaseNeedsSlash :: LocalPathBase -> Bool
localPathBaseNeedsSlash = \case
    LpModule _ -> True
    LpFile _ -> True
    _ -> False


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

instance Pretty UseFixity where
    pPrint = \case
        UseInfix -> "infix"
        UsePrefix -> "prefix"
        UsePostfix -> "postfix"
        UseAtomic -> "atom"



-- | A fixity specifier for overloading definitions in tree
data TreeFixity
    -- | Infix operator of unspecified associativity
    = TreeInfix
    -- | Prefix operator or atomic symbol
    | TreePrefixAtomic
    -- | Postfix operator
    | TreePostfix
    deriving (Eq, Ord, Show, Enum, Bounded)



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

instance Pretty DefFixity where
    pPrint = \case
        DefInfixL -> "infixl"
        DefInfixR -> "infixr"
        DefInfix -> "infix"
        DefPrefix -> "prefix"
        DefPostfix -> "postfix"
        DefAtomic -> "atom"

defFixityToTree :: DefFixity -> TreeFixity
defFixityToTree = \case
    DefInfixL -> TreeInfix
    DefInfixR -> TreeInfix
    DefInfix -> TreeInfix
    DefPrefix -> TreePrefixAtomic
    DefPostfix -> TreePostfix
    DefAtomic -> TreePrefixAtomic



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

instance Pretty UseKind where
    pPrint = \case
        UseType -> "type"
        UseEffect -> "effect"
        UseValue -> "value"
        UseNamespace -> "namespace"



-- | Visibility of a definition
data Visibility
    -- | A public definition
    = Public
    -- | A private definition
    | Private
    deriving (Eq, Ord, Show)

instance Pretty Visibility where
    pPrint = \case
        Public -> "pub"
        Private -> mempty



-- | Kind of a prototypical definition
data ProtoKind
    -- | A type definition
    = PkType
    -- | An effect definition
    | PkEffect
    -- | A value definition
    | PkValue
    -- | A namespace definition
    | PkNamespace
    -- | A set of lexical imports
    | PkUse
    deriving (Eq, Ord, Show)

instance Pretty ProtoKind where
    pPrint = \case
        PkType -> "type"
        PkEffect -> "effect"
        PkValue -> "value"
        PkNamespace -> "namespace"
        PkUse -> "use"



data TreeDefKind
    = TkType !TreeFixity
    | TkEffect !TreeFixity
    | TkValue !TreeFixity
    | TkNamespace
    deriving (Eq, Ord, Show)



data TreeUse
    = TreeUseType !DefFixity !Prec
    | TreeUseValue !DefFixity !Prec
    | TreeUseEffect !DefFixity !Prec
    | TreeUseNamespace
    deriving (Eq, Ord, Show)

instance Pretty TreeUse where
    pPrint = \case
        TreeUseType f p -> "type" <+> pPrint f <+> pPrint p
        TreeUseEffect f p -> "effect" <+> pPrint f <+> pPrint p
        TreeUseValue f p -> "value" <+> pPrint f <+> pPrint p
        TreeUseNamespace -> "namespace"





-- Names and paths -------------------------------------------------------------


-- | An unqualified name
newtype Name
    -- | Construct a Name from a String
    = Name {unwrapName :: String}
    deriving (Eq, Ord, Show)

instance Pretty Name where
    pPrint (Name s) = text s

printNameEscaped :: Name -> Doc
printNameEscaped (Name s)
    | any isOperator s = backticks (text s)
    | otherwise = text s

printTagNameEscaped :: PrettyLevel -> ATag Name -> Doc
printTagNameEscaped lvl (n :@: a) =
    if lvl >= PrettyRich
        then printNameEscaped n <> "@" <> brackets (pPrintPrec lvl 0 a)
        else printNameEscaped n



-- | A name to bind a definition to
data DefName
    -- | Construct a DefName from a DefFixity, a precedence, and a Name
    = DefName
    -- | The fixity associated with the binding
    { fixity :: !DefFixity
    -- | The precedence associated with the binding, if one was specified
    , prec :: !(Maybe Prec)
    -- | The name of the binding
    , name :: !Name
    }
    deriving (Eq, Ord, Show)

instance Pretty DefName where
    pPrint (DefName f p n) =
        pPrint f <+> maybePPrint p <+> pPrint n



-- | A name for a lexical import
data UseName
    -- | Construct a UseName from an optional UseKind,
    --   an optional UseFixity, and a Name
    = UseName
    { kind :: !(Maybe UseKind)
    , fixity :: !(Maybe UseFixity)
    , name :: !Name
    }
    deriving (Eq, Ord, Show)

instance Pretty UseName where
    pPrint (UseName k f n) =
        maybePPrint k <+> maybePPrint f <+> pPrint n



-- | An absolute reference to a definition
data AbsRef
    = AbsRef
    { path :: !AbsPath
    , fixity :: !UseFixity
    }
    deriving (Eq, Ord, Show)

instance Pretty AbsRef where
    pPrint (AbsRef (t Seq.:|> p) f) =
        hcat (punctuate "/" (pPrint <$> toList t))
            <> "/" <> (pPrint f <+> pPrint p)
    pPrint _ = error "AbsRef: empty AbsPath"


data LocalRef
    = LocalRef
    { path :: !LocalPath
    , fixity :: !(Maybe UseFixity)
    }
    deriving (Eq, Ord, Show)

instance Pretty LocalRef where
    pPrint (LocalRef (LocalPath b (t Seq.:|> p)) (Just f)) =
        let bd = pPrint b
            td = hcat (punctuate "/" (pPrint <$> toList t)) <> "/"
            pd = pPrint f <+> pPrint p
        in if localPathBaseNeedsSlash (untag b)
            then bd <> "/" <> td <> pd
            else bd <> td <> pd
    pPrint (LocalRef p Nothing) = pPrint p
    pPrint _ = error "LocalRef: headless LocalPath with specified fixity"



-- | A name with a local path prefix
data LocalPath
    -- | Construct a LocalPath from a LocalPathBase and a list of Name
    = LocalPath
    -- | The starting point of the local path
    { base :: !(ATag LocalPathBase)
    -- | The subsequent components of the local path
    , components :: !AbsPath
    }
    deriving (Eq, Ord, Show)

instance Pretty LocalPath where
    pPrintPrec lvl _ (LocalPath b ns) =
        let pNs = pPrintPrec lvl 0 ns
        in if not (null ns) && localPathBaseNeedsSlash (untag b)
            then pPrint b <> "/" <> pNs
            else pPrint b <> pNs

localPathNeedsSlash :: LocalPath -> Bool
localPathNeedsSlash (LocalPath b ns) =
    not (null ns) || localPathBaseNeedsSlash (untag b)



-- | A name with an absolute path prefix
type AbsPath = Seq (ATag Name)

instance Pretty AbsPath where
    pPrintPrec lvl _ ns =
        hcat (punctuate "/" (printTagNameEscaped lvl <$> toList ns))




-- module tree data ------------------------------------------------------------


data ModuleTree a u
    = ModuleTree
    { head :: !ModuleTreeHead
    , imports :: !(Map Name (ATag Module))
    , rootNamespace :: !(Namespace a u)
    }
    deriving Show

instance (Pretty a, Pretty u) => Pretty (ModuleTree a u) where
    pPrintPrec lvl _ (ModuleTree h ms ds) =
        vcatDouble
            [ pPrintPrec lvl 0 h
            , vcat' $ Map.toList ms <&> \(k, m :@: _) ->
                "import" <+> backticks do
                        pPrint m.head.name <> "@" <> pPrint m.head.version
                <+> "as" <+> pPrint k
            , pPrintPrec lvl 0 ds
            ]



data ModuleTreeHead
    = ModuleTreeHead
    { name :: !(ATag String)
    , version :: !(ATag Version)
    , meta :: ![(ATag String, ATag String)]
    }
    deriving Show

instance Pretty ModuleTreeHead where
    pPrint (ModuleTreeHead n v m) =
        hang ("module" <+> pPrint n <+> "=") do
            vcat' (rest : meta)
        where
        meta = m <&> \(k, mv) ->
            hang (text (untag k) <+> "=") do
                pPrint mv
        rest = hang ("version" <+> "=") do
                doubleQuoted v



data DefF t e v u
    -- | A type definition
    = DefType !Visibility !DefFixity !Prec !t
    -- | An effect definition
    | DefEffect !Visibility !DefFixity !Prec !e
    -- | A value definition
    | DefValue !Visibility !DefFixity !Prec !v
    -- | A namespace definition
    | DefNamespace !Visibility !(Namespace (DefF t e v u) u)
    deriving Show

instance Eq (DefF t e v u) where
    (==) = (==) `on` treeDefKind

instance Ord (DefF t e v u) where
    compare = compare `on` treeDefKind

treeDefKind :: DefF t e v u -> TreeDefKind
treeDefKind = \case
    DefType _ f _ _ -> TkType (defFixityToTree f)
    DefEffect _ f _ _ -> TkEffect (defFixityToTree f)
    DefValue _ f _ _ -> TkValue (defFixityToTree f)
    DefNamespace{} -> TkNamespace

type EffectCaseTable t = Map Name (Set (ATag (EffectCase t)))

data EffectCase t
    = EffectCase
    { fixity :: !DefFixity
    , prec :: !Prec
    , body :: !t
    }
    deriving Show

instance Eq (EffectCase t) where
    (==) = (==) `on` (.fixity)

instance Ord (EffectCase t) where
    compare = compare `on` (.fixity)


-- | A namespace, generic over the Def type
data Namespace d u
    = Namespace
    { localDefs :: Map Name (Set (ATag d))
    , imports :: u
    }
    deriving (Eq, Ord, Show)

instance Nil u => Nil (Namespace d u) where
    isNil = isNil . (.localDefs) &&& isNil . (.imports)
    nil = Namespace Nil Nil

instance (Pretty d, Pretty u) => Pretty (Namespace d u) where
    pPrintPrec lvl _ (Namespace l i) =
        vcatDouble
            [ pPrintPrec lvl 0 i
            , vcat' $ Map.toList l <&> \(k, v) ->
                hang (pPrint k <+> "=") do
                    vcat' (pPrint <$> toList v)
            ]

data AliasImports
    = AliasImports
    { aliasDefs :: Map Name (Set (ATag Alias))
    , aliasBlobs :: [ATag (Visibility, AbsPath)]
    }
    deriving (Eq, Ord, Show)

-- | A resolved lexical import
data Alias
    = Alias
    { visibility :: !Visibility
    , origin :: !AbsRef
    , kind :: !UseKind
    , fixity :: !DefFixity
    , prec :: !Prec
    , name :: !Name
    }
    deriving (Eq, Ord, Show)

data MidImports
    = MidImports
    { importDefs :: Map Name [ATag (Visibility, MidUse)]
    , importBlobs :: [ATag (Visibility, LocalPath)]
    }
    deriving (Eq, Ord, Show)

instance Nil MidImports where
    isNil = isNil . (.importDefs) &&& isNil . (.importBlobs)
    nil = MidImports Nil Nil

-- | A preprocessed but unresolved lexical import
data MidUse
    = MuNamespace
        !(ATag LocalPath)
    | MuType
        !(Maybe (DefFixity, Prec)) !(ATag LocalRef)
    | MuEffect
        !(Maybe (DefFixity, Prec)) !(ATag LocalRef)
    | MuValue
        !(Maybe (DefFixity, Prec)) !(ATag LocalRef)
    | MuUnresolved
        !(Maybe (DefFixity, Prec)) !(ATag LocalRef)
    deriving (Eq, Ord, Show)




-- proto module data -----------------------------------------------------------


data ProtoModule = ProtoModule
    { head :: ATag ProtoModuleHead
    , root :: [ATag ProtoDef]
    , files :: Map FilePath ProtoFile
    }
    deriving Show

instance Eq ProtoModule where
    (==) = (==) `on` (.head)

instance Ord ProtoModule where
    compare = compare `on` (.head)

instance Pretty ProtoModule where
    pPrintPrec lvl _ (ProtoModule h ds fs) =
        vcatDouble
            $ pPrintPrec lvl 0 h
            : vcat' (pPrintPrec lvl 0 <$> ds)
            : (pPrintPrec lvl 0 <$> Map.elems fs)



-- | A source file
data ProtoFile
    -- | Construct a ProtoFile from a FilePath
    --   and a list of definitions
    = ProtoFile
    -- | The complete FilePath for the file
    { path :: !FilePath
    -- | The definitions in the file
    , defs :: ![ATag ProtoDef]
    }
    deriving Show

instance Eq ProtoFile where
    (==) = (==) `on` (.path)

instance Ord ProtoFile where
    compare = compare `on` (.path)

instance Pretty ProtoFile where
    pPrintPrec lvl _ (ProtoFile f ds) =
        hang ("file" <+> pPrintPrec lvl 0 f <+> "=")
            if null ds
                then "{EMPTY FILE}"
                else vcat' (pPrintPrec lvl 0 <$> ds)



-- | A module head, containing information about the active module,
--   such as its name, version, dependencies, source directories, and
--   user-defined meta-data
data ProtoModuleHead
    -- | Construct a ProtoModuleHead from a name, a version,
    --   a list of meta-data, a list of source directories,
    --   and a list of dependencies
    = ProtoModuleHead
    -- | The name of the active module
    { name :: !(ATag String)
    -- | The version of the active module
    , version :: !(ATag Version)
    -- | User-defined meta-data for the active module
    , meta :: ![(ATag String, ATag String)]
    -- | The source directories of the active module
    , sources :: ![ATag FilePath]
    -- | The dependencies of the active module
    , dependencies :: ![ATag ProtoModuleDependency]
    }
    deriving Show

instance Eq ProtoModuleHead where
    (==) = (==) `on` (.name)

instance Ord ProtoModuleHead where
    compare = compare `on` (.name)

instance Pretty ProtoModuleHead where
    pPrint (ProtoModuleHead n v m s ds) =
        hang ("module" <+> pPrint n <+> "=") do
            vcat' (meta <> rest)
        where
        meta = m <&> \(k, mv) ->
            hang (text (untag k) <+> "=") do
                pPrint mv
        rest =
            [ hang ("version" <+> "=") do
                doubleQuoted v
            , hang ("sources" <+> "=") do
                vcat' $ punctuate "," (pPrint <$> s)
            , hang ("dependencies" <+> "=") do
                vcat' $ punctuate "," (pPrint <$> ds)
            ]

emptyProtoModuleHead :: ATag String -> ProtoModuleHead
emptyProtoModuleHead name =
    ProtoModuleHead name (Version 0 0 0 :@: Nil) [] [] []



-- | An unresolved dependency declaration of the active module
data ProtoModuleDependency
    -- | Construct a ProtoModuleDependency from a name and an optional alias
    = ProtoModuleDependency
    -- | The name and version of the dependency
    { nameVer :: !(ATag (String, Version))
    -- | An optional alias for the dependency
    , alias :: !(Maybe (ATag Name))
    }
    deriving Show

instance Eq ProtoModuleDependency where
    (==) = (==) `on` (.nameVer)

instance Ord ProtoModuleDependency where
    compare = compare `on` (.nameVer)

instance Pretty ProtoModuleDependency where
    pPrint (ProtoModuleDependency (T' (n, v)) a) =
        hang (doubleQuotes (text n <> "@" <> pPrint v)) do
            maybeMEmpty (("as" <+>) . pPrint <$> a)



-- | Prototypical definitions, such as types, values and effects,
--   before they are fully parsed
data ProtoDef
    -- | A type definition that has not been fully parsed
    = ProtoType
        !Visibility !(ATag DefName) !(Seq (ATag Token))
    -- | An effect definition that has not been fully parsed
    | ProtoEffect
        !Visibility !(ATag DefName) ![ATag ProtoEffectCase]
    -- | A value definition that has not been fully parsed
    | ProtoValue
        !Visibility !(ATag DefName) !(Seq (ATag Token)) !(Seq (ATag Token))
    -- | A namespace definition who's members have not been fully parsed
    | ProtoNamespace
        !Visibility  !(ATag Name)  ![ATag ProtoDef]
    -- | A set of lexical imports
    | ProtoUse
        !Visibility  !(ATag Use)
    deriving Show

instance Eq ProtoDef where
    (==) = curry \case
        (ProtoType v1 n1 _, ProtoType v2 n2 _) -> v1 == v2 && n1 == n2
        (ProtoEffect v1 n1 _, ProtoEffect v2 n2 _) -> v1 == v2 && n1 == n2
        (ProtoValue v1 n1 _ _, ProtoValue v2 n2 _ _) -> v1 == v2 && n1 == n2
        (ProtoNamespace v1 n1 _, ProtoNamespace v2 n2 _) -> v1 == v2 && n1 == n2
        (ProtoUse v1 u1, ProtoUse v2 u2) -> v1 == v2 && u1 == u2
        _ -> False

instance Pretty ProtoDef where
    pPrint = \case
        ProtoType v n b ->
            hang (pPrint v <+> "type" <+> pPrint n <+> "=")
                (pPrint b)
        ProtoEffect v n b ->
            hang (pPrint v <+> "effect" <+> pPrint n <+> "=")
                (pPrint b)
        ProtoValue v n h b ->
            let x = pPrint v <+> "value" <+> pPrint n
                t = hang (x <+> ":") (pPrint h)
            in case (Seq.null h, Seq.null b) of
                (True, True) -> x
                (False, True) -> t
                (True, False) -> hang (x <+> "=") (pPrint b)
                (False, False) -> hang t ("=" <+> pPrint b)
        ProtoNamespace v n ds ->
            hang (pPrint v <+> "namespace" <+> pPrint n <+> "=")
                (vcat' (pPrint <$> ds))
        ProtoUse v u ->
            pPrint v <+> "use" <+> pPrint u

instance Pretty [ATag ProtoDef] where
    pPrint ds = brackets $ vcat' (pPrint <$> ds)



-- | A specific case in a prototypical effect definition
data ProtoEffectCase
    -- | Construct a prototypical effect case from a name and tokens
    = ProtoEffectCase
    -- | The name of the case
    { name :: !(ATag DefName)
    -- | The body of the case
    , body :: !(Seq (ATag Token))
    }
    deriving Show

instance Eq ProtoEffectCase where
    (==) = (==) `on` (.name)

instance Ord ProtoEffectCase where
    compare = compare `on` (.name)

instance Pretty ProtoEffectCase where
    pPrint (ProtoEffectCase n b)
        = hang (pPrint n <+> ":")
            (pPrint b)


-- | A set of lexical imports
data Use
    -- | Construct a Use from an optional base, an optional tree,
    --   and an optional alias.
    --   Note that at least one of the former two must be present
    = Use
    -- | The base of the local path to import from
    { base :: !(Maybe (ATag LocalPath))
    -- | What to actually import
    , tree :: !(Maybe (ATag UseTree))
    -- | An optional alias for the import
    , alias :: !(Maybe (ATag DefName))
    }
    deriving (Eq, Ord, Show)

instance Pretty Use where
    pPrint (Use b t a)
        = (if maybe False
                (localPathNeedsSlash . untag)
                (t >>= const b)
            then maybePPrint b <> "/" <> maybePPrint t
            else maybePPrint b <> maybePPrint t)
        <+> maybeMEmpty (("as" <+>) . pPrint <$> a)



-- | Body of @Use@, indicating what to import
data UseTree
    -- | A set of bindings
    = UseBranch ![ATag Use]
    -- | A path to a specific binding
    | UseLeaf !UseName
    -- | A wildcard indicating all bindings at the given path
    | UseAll
    deriving (Eq, Ord, Show)

instance Pretty UseTree where
    pPrint = \case
        UseBranch ts -> braces $ lsep (pPrint <$> ts)
        UseLeaf n -> pPrint n
        UseAll -> ".."




-- Parsed ast types ------------------------------------------------------------

-- | User type definition
data TypeDef
    = TdAlias !(ATag Type)
    | TdProduct ![Field (ATag Type)]
    | TdSum ![Field (ATag Type)]
    deriving (Eq, Ord, Show)

instance Pretty TypeDef where
    pPrintPrec lvl _ = \case
        TdAlias t -> pPrintPrec lvl 0 t
        TdProduct ts -> parens $ sep $ punctuate "," (pPrintPrec lvl 0 <$> ts)
        TdSum ts -> braces $ sep $ punctuate "|" (pPrintPrec lvl 0 <$> ts)



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
