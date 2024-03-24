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
type FinalModule = Module MetaData MonoType Value

-- | A module that is being analyzed
type AnalysisModule = Module AnalysisModuleHeader UserType Value

-- | A module that is undergoing top level name resolution
type ResolverModule = Module ResolverModuleHeader UserType TokenSeq

-- | A module that has been partially parsed
type ParserModule = Module ResolverModuleHeader UserType TokenSeq

-- | Definitions for a module that has been fully parsed and analyzed
type FinalDefs = DefSet UserType Value

-- | Definitions for a module that is being analyzed
type AnalysisDefs = DefSet UserType Value

-- | Definitions for a module that is undergoing top level name resolution
type ResolverDefs = DefSet UserType TokenSeq

-- | Definitions for a module that has been partially parsed
type ParserDefs = DefSet UserType TokenSeq


-- | High level module structure, storing the header, metadata, and definitions
data Module h t v
    = Module
    { header :: !h
    , defSet :: !(DefSet t v)
    }
    deriving Show

instance (Pretty h, Pretty t, Pretty v)
    => Pretty (Module h t v) where
        pPrintPrec lvl _ Module{..} =
            vcat'
                [ hang "header" $ pPrintPrec lvl 0 header
                , hang "defSet" $ pPrintPrec lvl 0 defSet
                ]

getGroup ::
    ItemId -> Module h t v -> Maybe (ATag Group)
getGroup ii = Map.lookup ii . (.defSet.groups)

getQuantifier ::
    ItemId -> Module h t v -> Maybe (ATag Quantifier)
getQuantifier ii = Map.lookup ii . (.defSet.quantifiers)

getQualifier ::
    ItemId -> Module h t v -> Maybe (ATag (Qualifier t))
getQualifier ii = Map.lookup ii . (.defSet.qualifiers)

getField ::
    ItemId -> Module h t v -> Maybe (ATag (Field t))
getField ii = Map.lookup ii . (.defSet.fields)

getType ::
    ItemId -> Module h t v -> Maybe (ATag t)
getType ii = Map.lookup ii . (.defSet.types)

getValue ::
    ItemId -> Module h t v -> Maybe (ATag v)
getValue ii = Map.lookup ii . (.defSet.values)

-- getImport ::
--     ItemId -> Module h t v -> Maybe (ATag i)
-- getImport ii = Map.lookup ii . (.defSet.imports)

getParent ::
    ItemId -> Module h t v -> Maybe ItemId
getParent ii = Map.lookup ii . (.defSet.parents)

setGroup ::
    ItemId -> ATag Group ->
        Module h t v -> Module h t v
setGroup ii g m =
    m { defSet = m.defSet { groups = Map.insert ii g m.defSet.groups } }

setQuantifier ::
    ItemId -> ATag Quantifier ->
        Module h t v -> Module h t v
setQuantifier ii q m =
    m { defSet =
        m.defSet { quantifiers =
            Map.insert ii q m.defSet.quantifiers } }

setQualifier ::
    ItemId -> ATag (Qualifier t) ->
        Module h t v -> Module h t v
setQualifier ii q m =
    m { defSet = m.defSet { qualifiers = Map.insert ii q m.defSet.qualifiers } }

setField ::
    ItemId -> ATag (Field t) ->
        Module h t v -> Module h t v
setField ii f m =
    m { defSet = m.defSet { fields = Map.insert ii f m.defSet.fields } }

setType ::
    ItemId -> ATag t ->
        Module h t v -> Module h t v
setType ii t m =
    m { defSet = m.defSet { types = Map.insert ii t m.defSet.types } }

setValue ::
    ItemId -> ATag v ->
        Module h t v -> Module h t v
setValue ii v m =
    m { defSet = m.defSet { values = Map.insert ii v m.defSet.values } }

-- setImport ::
--     ItemId -> ATag i ->
--         Module h (DefSet v i) -> Module h (DefSet v i)
-- setImport ii i m =
--     m { defSet = m.defSet { imports = Map.insert ii i m.defSet.imports } }

setParent ::
    ItemId -> ItemId ->
        Module h t v -> Module h t v
setParent ii p m =
    m { defSet = m.defSet { parents = Map.insert ii p m.defSet.parents } }


isCategory :: ItemId -> Category -> Module h t v -> Bool
isCategory ii category m = m.defSet.categories Map.!? ii == Just category


-- | Parametric type storing all the definitions of a module,
--   with their form depending on compilation phase.
--   A given @ItemId@ may be bound to multiple rows of the @DefSet@,
--   for example a value definition may have an entry in
--   @values@, @quantifiers@, @qualifiers@, and @types@, while
--   types with fields are stored in @groups@, @quantifiers@, and @qualifiers@.
--   All items except for roots have an entry in @parents@,
--   and all items have an entry in @categories@.
--   The root namespace of a module is always
--   in the column corresponding to @ItemId 0@
data DefSet t v
    = DefSet
    {     parents :: !(Map ItemId ItemId)
    ,  categories :: !(Map ItemId Category)

    ,      groups :: !(Map ItemId (ATag Group))
    , quantifiers :: !(Map ItemId (ATag Quantifier))
    ,  qualifiers :: !(Map ItemId (ATag (Qualifier t)))
    ,      fields :: !(Map ItemId (ATag (Field t)))
    ,       types :: !(Map ItemId (ATag t))
    ,      values :: !(Map ItemId (ATag v))
    }
    deriving Show

instance (Pretty t, Pretty v) => Pretty (DefSet t v) where
    pPrintPrec lvl _ DefSet{..} =
        vcat'
            [ hang "parents" $ printMap parents
            , hang "categories" $ printMap categories

            , hang "groups" $ printMap groups
            , hang "quantifiers" $ printMap quantifiers
            , hang "qualifiers" $ printMap qualifiers
            , hang "fields" $ printMap fields
            , hang "types" $ printMap types
            , hang "values" $ printMap values
            ]
        where
        printMap :: Pretty a => Map ItemId a -> Doc
        printMap = compose Map.toList do
            vcat' . fmap \(k, v) ->
                hang (pPrint k <+> "=") do
                    pPrintPrec lvl 0 v

instance Semigroup (DefSet t v) where
    DefSet p1 x1 g1 q1 c1 f1 t1 v1 <> DefSet p2 x2 g2 q2 c2 f2 t2 v2 =
        DefSet
            (p1 <> p2)
            (x1 <> x2)
            (g1 <> g2)
            (q1 <> q2)
            (c1 <> c2)
            (f1 <> f2)
            (t1 <> t2)
            (v1 <> v2)

instance Monoid (DefSet t v) where
    mempty = DefSet
        mempty mempty mempty mempty mempty mempty mempty mempty

instance Nil (DefSet t v) where
    isNil DefSet{..}
         = isNil parents
        && isNil categories
        && isNil groups
        && isNil quantifiers
        && isNil qualifiers
        && isNil fields
        && isNil types
        && isNil values

-- parserDefsToResolverDefs :: ParserDefs -> ResolverDefs
-- parserDefsToResolverDefs ds =
--     ds { imports = fmap unresolvedImportsToWipImports <$> ds.imports }

data ResolverModuleHeader
    = ResolverModuleHeader
    { unresolved :: !UnresolvedImportMap
    ,   analysis :: !AnalysisModuleHeader
    }
    deriving Show

instance Pretty ResolverModuleHeader where
    pPrintPrec lvl _ ResolverModuleHeader{..} =
        vcat'
            [ hang "unresolved" $ pPrintPrec lvl 0 unresolved
            , hang "analysis" $ pPrintPrec lvl 0 analysis
            ]

data AnalysisModuleHeader
    = AnalysisModuleHeader
    {        files :: !(Map FilePath ItemId)
    , dependencies :: !(Map SimpleName (ATag ModuleId))
    ,     metaData :: !MetaData
    ,     moduleId :: !ModuleId
    }
    deriving Show

instance Pretty AnalysisModuleHeader where
    pPrintPrec lvl _ AnalysisModuleHeader{..} =
        vcat'
            [ hang "files" $ pPrintPrec lvl 0 files
            , hang "dependencies" $ pPrintPrec lvl 0 dependencies
            , hang "metaData" $ pPrintPrec lvl 0 metaData
            , hang "moduleId" $ pPrintPrec lvl 0 moduleId
            ]


-- | A map from arbitrary keys to arbitrary values, both strings;
--   for module metadata such as package author etc
type MetaData = Map (ATag SimpleName) (ATag String)

-- | A map from locally-appropriate names to module strings and versions
type RawDependencies = [(ATag (String, Version), Maybe (ATag SimpleName))]




-- | Pair of @QualifiedName@ and @ATag Ref@,
--   making up an entry in the @Group@'s associative array
data GroupBinding
    = GroupBinding
    { name :: !QualifiedName
    ,  ref :: !Ref
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
    Maybe.isNothing $ groupGetRef ib.value.name.value.value lbs

filterGroupByVis :: Group -> Group
filterGroupByVis = filter \def -> def.visibility == Public

-- | Lookup @Ref@s matching a generic @FixName@ in a @Group@
groupSearchRef :: FixName -> Group -> Group
groupSearchRef n = filter $ (n ==) . (.value.name.value.value)

-- | Lookup the @Ref@ associated with a @FixName@ in a @Group@
groupGetRef :: FixName -> Group -> Maybe (ATag (Visible Ref))
groupGetRef fn = lookupWith \def ->
    let n = def.value.name.value.value
    in guard (n == fn) $> do
        def.value.ref <$ def <$ def.value.name.value

-- | Insert a new @Ref@ into a @Group@, bound to a @Visible GroupName@
groupInsertRef :: Visible GroupBinding -> Group -> Either (ATag Doc) Group
groupInsertRef b g =
    let sn = b.value.name.value.value
    in case groupGetRef sn g of
        Just ex -> Left $
            ("name" <+> pPrint b.value.name <+> "already exists")
                :@: ex.tag
        _ -> Right $ b : g


data ResolvedBlob
    = ResolvedBlob
    { pathStack :: !PathStack
    ,    hiding :: ![ATag FixName]
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
    FixName -> ResolvedImports -> ResolvedImports
filterResolvedImports n = filter $
    not . any ((== n) . untag) . (.value.hiding)

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
    , hiding :: ![ATag FixName]
    }
    deriving (Eq, Ord, Show)

instance Pretty UnresolvedBlob where
    pPrintPrec lvl _ UnresolvedBlob{..} =
        spaceWith "hiding"
            do pPrintPrec lvl 0 path
            do pPrintPrec lvl 0 hiding


-- | A map from @ItemId@ to @UnresolvedImports@, with their source attributions
type UnresolvedImportMap
    = Map ItemId (ATag UnresolvedImports)

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
