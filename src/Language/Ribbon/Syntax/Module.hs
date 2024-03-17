module Language.Ribbon.Syntax.Module where

import Data.Functor
import qualified Data.Foldable as Fold

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Data.List qualified as List
import Data.Char qualified as Char

import Data.Tag
import Data.Attr
import Data.Nil

import Control.Monad

import Text.Pretty

import Language.Ribbon.Util
import Language.Ribbon.Lexical
import Language.Ribbon.Syntax.Ref
import Language.Ribbon.Syntax.Data
import Language.Ribbon.Syntax.Scheme
import Language.Ribbon.Syntax.Type
import Language.Ribbon.Syntax.Value
import qualified Data.Maybe as Maybe




-- | An analysis context component, storing completed module's information.
--   Provides a lookup table from @(name string, version)@ pairs,
--   to their respective references
data ModuleContext
    = ModuleContext
    { modules :: !(Map ModuleId FinalModule)
    , moduleLookup :: !(Map (String, Version) ModuleId)
    }
    deriving Show

getModule ::
    ModuleId -> ModuleContext -> FinalModule
getModule mid
    = Maybe.fromJust
        (error $ "module " <> show mid.value <> " not found in context")
    . tryGetModule mid


tryGetModule ::
    ModuleId -> ModuleContext -> Maybe FinalModule
tryGetModule mid ctx = Map.lookup mid ctx.modules

lookupModule ::
    ATag (String, Version) -> ModuleContext -> Either (Doc, [Doc]) ModuleId
lookupModule nameVer ctx = do
    let nv@(name, ver) = untag nameVer
    case Map.lookup nv ctx.moduleLookup of
        Just tree -> Right tree
        _ -> Left
            ( "module" <+> text name
                <+> "with version" <+> pPrint ver
                <+> "not found in context"
            , nameFuzzySearchOn fst name (Map.keys ctx.moduleLookup)
                <&> \(sn, sv) ->
                    "similar match:" <+> text sn
                        <+> "with version" <+> pPrint sv
            )


nameFuzzySearch :: String -> [String] -> [String]
nameFuzzySearch = nameFuzzySearchOn id

nameFuzzySearchOn :: (a -> String) -> String -> [a] -> [a]
nameFuzzySearchOn f name ss =
    let subs = List.permutations $ List.take 7 $ lower name
    in List.sortOn (length . f) $ removeDuplicates $ foldMap (match0 subs) ss
    where
    lower = fmap Char.toLower
    match0 subs s = foldMap (`match` s) subs
    match sub a = [a | List.isSubsequenceOf sub (lower $ f a)]
    removeDuplicates [] = []
    removeDuplicates (x:xs) =
        x : removeDuplicates (List.filter ((/= f x) . f) xs)



-- | A module that has been fully parsed and analyzed
type FinalModule = Module () FinalDefs

-- | A module that is being analyzed
type AnalysisModule = Module AnalysisModuleHeader AnalysisDefs

-- | A module that is undergoing top level name resolution
type ResolverModule = Module AnalysisModuleHeader ResolverDefs

-- | A module that has been partially parsed
type ParserModule = Module AnalysisModuleHeader ParserDefs

-- | Definitions for a module that has been fully parsed and analyzed
type FinalDefs = DefSet Type Value ResolvedImports

-- | Definitions for a module that is being analyzed
type AnalysisDefs = DefSet Type Value ResolvedImports

-- | Definitions for a module that is undergoing top level name resolution
type ResolverDefs = DefSet TokenSeq TokenSeq WipImports

-- | Definitions for a module that has been partially parsed
type ParserDefs = DefSet TokenSeq TokenSeq UnresolvedImports


-- | High level module structure, storing the header, metadata, and definitions
data Module h d
    = Module
    { moduleId :: !ModuleId
    ,   header :: !h
    ,     meta :: !MetaData
    ,   defSet :: !d
    }
    deriving Show

instance (Pretty h, Pretty d)
    => Pretty (Module h d) where
        pPrintPrec lvl _ Module{..} =
            vcat'
                [ hang "moduleId" $ pPrintPrec lvl 0 moduleId
                , hang "header" $ pPrintPrec lvl 0 header
                , hang "meta" $ pPrintPrec lvl 0 meta
                , hang "defSet" $ pPrintPrec lvl 0 defSet
                ]

getGroup ::
    ItemId -> Module h (DefSet t v i) -> Maybe Group
getGroup ii = fmap untag . Map.lookup ii . (.defSet.groups)

getQuantifier ::
    ItemId -> Module h (DefSet t v i) -> Maybe Quantifier
getQuantifier ii = fmap untag . Map.lookup ii . (.defSet.quantifiers)

getQualifier ::
    ItemId -> Module h (DefSet t v i) -> Maybe (Qualifier t)
getQualifier ii = fmap untag . Map.lookup ii . (.defSet.qualifiers)

getField ::
    ItemId -> Module h (DefSet t v i) -> Maybe (Field t)
getField ii = fmap untag . Map.lookup ii . (.defSet.fields)

getType ::
    ItemId -> Module h (DefSet t v i) -> Maybe t
getType ii = fmap untag . Map.lookup ii . (.defSet.types)

getValue ::
    ItemId -> Module h (DefSet t v i) -> Maybe v
getValue ii = fmap untag . Map.lookup ii . (.defSet.values)

getImport ::
    ItemId -> Module h (DefSet t v i) -> Maybe i
getImport ii = fmap untag . Map.lookup ii . (.defSet.imports)

getParent ::
    ItemId -> Module h (DefSet t v i) -> Maybe ItemId
getParent ii = Map.lookup ii . (.defSet.parents)

-- | Parametric type storing all the definitions of a module,
--   with their form depending on compilation phase.
--   A given @Ref@ may be bound to multiple rows of the module,
--   for example a value definition may have an entry in
--   @values@, @quantifiers@, @qualifiers@, and @types@, while
--   types with fields are stored in @groups@, @quantifiers@, and @qualifiers@.
--   The root namespace is always stored in @Ref Namespace modId 0@
data DefSet t v i
    = DefSet
    {      groups :: !(Map ItemId (ATag Group))
    , quantifiers :: !(Map ItemId (ATag Quantifier))
    ,  qualifiers :: !(Map ItemId (ATag (Qualifier t)))
    ,      fields :: !(Map ItemId (ATag (Field t)))
    ,       types :: !(Map ItemId (ATag t))
    ,      values :: !(Map ItemId (ATag v))
    ,     imports :: !(Map ItemId (ATag i))
    ,     parents :: !(Map ItemId ItemId)
    }
    deriving Show

instance (Pretty t, Pretty v, Pretty i)
    => Pretty (DefSet t v i) where
        pPrintPrec lvl _ DefSet{..} =
            vcat'
                [ hang "groups" $ printMap groups
                , hang "quantifiers" $ printMap quantifiers
                , hang "qualifiers" $ printMap qualifiers
                , hang "fields" $ printMap fields
                , hang "types" $ printMap types
                , hang "values" $ printMap values
                , hang "imports" $ printMap imports
                , hang "parents" $ printMap parents
                ]
            where
            printMap :: Pretty a => Map ItemId a -> Doc
            printMap = compose Map.toList do
                vcat' . fmap \(k, v) ->
                    hang (pPrint k <+> "=") do
                        pPrintPrec lvl 0 v

instance Semigroup (DefSet t v i) where
    DefSet g1 q1 c1 f1 t1 v1 i1 p1 <> DefSet g2 q2 c2 f2 t2 v2 i2 p2 =
        DefSet
            (g1 <> g2)
            (q1 <> q2)
            (c1 <> c2)
            (f1 <> f2)
            (t1 <> t2)
            (v1 <> v2)
            (i1 <> i2)
            (p1 <> p2)

instance Monoid (DefSet t v i) where
    mempty = DefSet mempty mempty mempty mempty mempty mempty mempty mempty

instance Nil (DefSet t v i) where
    isNil DefSet{..}
         = isNil groups
        && isNil quantifiers
        && isNil qualifiers
        && isNil fields
        && isNil types
        && isNil values
        && isNil imports
        && isNil parents

parserDefsToResolverDefs :: ParserDefs -> ResolverDefs
parserDefsToResolverDefs ds =
    ds { imports = fmap unresolvedImportsToWipImports <$> ds.imports }


data AnalysisModuleHeader
    = AnalysisModuleHeader
    {        files :: !(Map FilePath ItemId)
    , dependencies :: ![(ATag SimpleName, ModuleId)]
    }
    deriving Show

instance Pretty AnalysisModuleHeader where
    pPrintPrec lvl _ AnalysisModuleHeader{..} =
        vcat'
            [ hang "files" $ pPrintPrec lvl 0 files
            , hang "dependencies" $ pPrintPrec lvl 0 dependencies
            ]


-- | A map from arbitrary keys to arbitrary lists of values
type MetaData = Map (ATag SimpleName) (ATag String)

-- | A map from locally-appropriate names to module strings and versions
type RawDependencies = [(ATag (String, Version), Maybe (ATag SimpleName))]




-- | Pair of @Visible GroupName@ and @ATag Ref@,
--   making up an entry in the @Group@'s associative array
data GroupBinding
    = GroupBinding
    { name :: GroupName
    , ref :: Ref
    }
    deriving (Eq, Ord, Show)

instance Pretty GroupBinding where
    pPrintPrec lvl _ GroupBinding{..} =
        spaceWith "="
            do pPrintPrec lvl 0 name
            do pPrintPrec lvl 0 ref


-- | A set of overloaded bindings
type Group = [Visible GroupBinding]


mergeGroups :: Group -> Group -> Group
mergeGroups lbs = (lbs <>) . filter \ib ->
    Maybe.isNothing $ getRef (specificNameFromGroupName ib.value.name) lbs

filterGroupByVis :: Group -> Group
filterGroupByVis = filter \def -> def.visibility == Public

-- | Lookup @Ref@s matching a generic @PathName@ in a @Group@
searchRef :: PathName -> Group -> Group
searchRef n = filter \def ->
    let n' = def.value.name.value.value.value
        c = def.value.name.category
    in n' == n.name && sameCategory n.category c
    where
        sameCategory = \case
            Just oc -> (oc ==) . overloadedCategory
            _ -> const True

-- | Lookup the @Ref@ associated with a @SpecificName@ in a @Group@
getRef :: SpecificName -> Group -> Maybe (ATag (Visible Ref))
getRef sn = lookupWith \def ->
    let n = def.value.name.value.value.value
        c = def.value.name.category
    in guard (n == sn.value.value && c == sn.category) $> do
        def.value.ref <$ def <$ def.value.name.value.value

-- | Insert a new @Ref@ into a @Group@, bound to a @Visible GroupName@
insertRef :: Visible GroupBinding -> Group -> Either (ATag Doc) Group
insertRef b g =
    let sn = specificNameFromGroupName b.value.name
    in case getRef sn g of
        Just ex -> Left $
            ("name" <+> pPrint b.value.name <+> "already exists")
                :@: ex.tag
        _ -> Right $ b : g


data Resolution
    = NoResolution
    | BranchResolution ![Resolution]
    | FileResolution !FilePath !ItemId
    | ModuleResolution !SimpleName !ModuleId
    | BindingResolution !(Visible GroupBinding)
    deriving (Eq, Ord, Show)

instance Pretty Resolution where
    pPrintPrec lvl _ = \case
        NoResolution -> "NoResolution"
        BranchResolution rs -> hang "BranchResolution" $ vcat' $ pPrintPrec lvl 0 <$> rs
        FileResolution fp _ -> "file" <+> doubleQuotes (text fp)
        ModuleResolution sn _ -> "module" <+> pPrintPrec lvl 0 sn
        BindingResolution b -> pPrintPrec lvl 0 b

isResolution :: Resolution -> Bool
isResolution = \case
    NoResolution -> False
    _ -> True

resolutionFromPathStack :: PathStack -> Resolution
resolutionFromPathStack = \case
    PsBase k r n _ -> case k of
        PsFileBase -> FileResolution n r.itemId
        PsModuleBase -> ModuleResolution (SimpleName n) r.moduleId
    PsCons _ ref name _ -> BindingResolution $
        Visible Public $ GroupBinding name ref

resolutionName :: Resolution -> String
resolutionName = \case
    NoResolution -> "NoResolution"
    BranchResolution rs -> resolutionName (head rs)
    FileResolution p _ -> p
    ModuleResolution n _ -> n.value
    BindingResolution b -> prettyShow b.value.name.value.value.value

data ResolvedBlob
    = ResolvedBlob
    { pathStack :: !PathStack
    ,    hiding :: ![ATag PathName]
    }
    deriving (Eq, Ord, Show)

instance Pretty ResolvedBlob where
    pPrintPrec lvl _ ResolvedBlob{..} =
        vcat'
            [ hang "pathStack" (pPrintPrec lvl 0 pathStack)
            , hang "hiding" (pPrintPrec lvl 0 hiding)
            ]

-- | A group of imports that has been resolved to a set of references
--   to continue lookup traversal through
type ResolvedImports
    = [Visible ResolvedBlob]

filterResolvedImports ::
    PathName -> ResolvedImports -> ResolvedImports
filterResolvedImports n = filter $
    not . any (pathNameMatch n . untag) . (.value.hiding) where
    pathNameMatch (PathName n1 k1) (PathName n2 k2) =
        n1 == n2 && (Maybe.isNothing k2 || k1 == k2)

data UnresolvedAlias
    = UnresolvedAlias
    { name :: !UnresolvedName
    , path :: !(ATag Path)
    }
    deriving (Eq, Ord, Show)

instance Pretty UnresolvedAlias where
    pPrintPrec lvl _ UnresolvedAlias{..} =
        spaceWith "="
            do pPrintPrec lvl 0 name
            do pPrintPrec lvl 0 path

data UnresolvedBlob
    = UnresolvedBlob
    {   path :: !(ATag Path)
    , hiding :: ![ATag PathName]
    }
    deriving (Eq, Ord, Show)

instance Pretty UnresolvedBlob where
    pPrintPrec lvl _ UnresolvedBlob{..} =
        spaceWith "hiding"
            do pPrintPrec lvl 0 path
            do pPrintPrec lvl 0 hiding

-- | A partially resolved group of imports
data WipImports
    = WipImports
    { aliases :: ![Visible UnresolvedAlias]
    ,   blobs :: ![Visible (Either UnresolvedBlob ResolvedBlob)]
    }
    deriving (Eq, Ord, Show)

lookupWipAlias ::
    FixName -> WipImports -> Maybe (Visible UnresolvedAlias)
lookupWipAlias n wi = Fold.find ((== n) . (.value.name.value.value)) wi.aliases

insertResolvedBlob :: Visible ResolvedBlob -> WipImports -> WipImports
insertResolvedBlob b wi =
    WipImports { blobs = fmap Right b : wi.blobs, aliases = wi.aliases }

-- | An unresolved group of imports
data UnresolvedImports
    = UnresolvedImports
    { aliases :: ![Visible UnresolvedAlias]
    ,   blobs :: ![Visible UnresolvedBlob]
    }
    deriving (Eq, Ord, Show)

instance Nil UnresolvedImports where
    nil = UnresolvedImports [] []
    isNil (UnresolvedImports a b) = isNil a && isNil b

instance Pretty UnresolvedImports where
    pPrintPrec lvl _ (UnresolvedImports as bs) =
        vcat'
            [ hang "aliases" $ vcat' do
                pPrintPrec lvl 0 <$> as
            , hang "blobs" $ vcat' do
                pPrintPrec lvl 0 <$> bs
            ]

unresolvedImportsToWipImports :: UnresolvedImports -> WipImports
unresolvedImportsToWipImports ui =
    WipImports { aliases = ui.aliases, blobs = fmap (fmap Left) ui.blobs }

lookupUnresolvedAlias ::
    FixName -> UnresolvedImports -> Maybe (Visible UnresolvedAlias)
lookupUnresolvedAlias n ui =
    Fold.find ((== n) . (.value.name.value.value)) ui.aliases

insertUnresolvedAlias ::
    Visible UnresolvedAlias -> UnresolvedImports ->
        Either (ATag Doc) UnresolvedImports
insertUnresolvedAlias ua ui =
    case lookupUnresolvedAlias ua.value.name.value.value ui of
        Just ex -> Left $
            ("alias" <+> pPrint ua.value.name.value.value <+> "already exists")
                :@: ex.value.name.value.tag
        _ -> Right $
            UnresolvedImports { aliases = ua : ui.aliases, blobs = ui.blobs }

insertUnresolvedBlob ::
    Visible UnresolvedBlob -> UnresolvedImports -> UnresolvedImports
insertUnresolvedBlob b ui =
    UnresolvedImports { blobs = b : ui.blobs, aliases = ui.aliases }
