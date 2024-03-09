module Language.Ribbon.Syntax.Module where

import Data.Map.Strict (Map)

import Data.Functor

import Data.Tag
import Data.Attr

import Text.Pretty

import Language.Ribbon.Lexical

import Language.Ribbon.Syntax.Ref
import Language.Ribbon.Syntax.Data
import Language.Ribbon.Syntax.Scheme
import Language.Ribbon.Syntax.Type
import Language.Ribbon.Syntax.Value
import qualified Data.Foldable as Fold
import Data.Nil
import Language.Ribbon.Util
import Control.Monad




-- | An analysis context component, storing completed module's information.
--   Provides a lookup table from @(name string, version)@ pairs,
--   to their respective references
data ModuleContext
    = ModuleContext
    { modules :: !(Map ModuleId FinalModule)
    , moduleLookup :: !(Map (String, Version) Ref)
    }
    deriving Show


-- | A module that has been fully parsed and analyzed
type FinalModule = Module () FinalDefs

-- | A module that is being analyzed
type AnalysisModule = Module AnalysisModuleHeader AnalysisDefs

-- | A module that has been partially parsed
type ParserModule = Module ParserModuleHeader ParserDefs

-- | Definitions for a module that has been fully parsed and analyzed
type FinalDefs = DefSet Type Value ResolvedBlobs

-- | Definitions for a module that is being analyzed
type AnalysisDefs = DefSet Type Value ResolvedBlobs

-- | Definitions for a module that has been partially parsed
type ParserDefs = DefSet TokenSeq TokenSeq UnresolvedImports


-- | High level module structure, storing the header, metadata, and definitions
data Module h d
    = Module
    { header :: !h
    ,   meta :: !MetaData
    , defSet :: !d
    }
    deriving Show

instance (Pretty h, Pretty d)
    => Pretty (Module h d) where
        pPrintPrec lvl _ Module{..} =
            vcat'
                [ hang "header" $ pPrintPrec lvl 0 header
                , hang "meta" $ pPrintPrec lvl 0 meta
                , hang "defSet" $ pPrintPrec lvl 0 defSet
                ]

-- | Parametric type storing all the definitions of a module,
--   with their form depending on compilation phase.
--   A given @Ref@ may be bound to multiple rows of the module,
--   for example a value definition may have an entry in
--   @values@, @quantifiers@, @qualifiers@, and @types@, while
--   types with fields are stored in @groups@, @quantifiers@, and @qualifiers@.
--   The root namespace is always stored in @Ref Namespace modId 0@
data DefSet t v i
    = DefSet
    {      groups :: !(Map ItemId (Def Group))
    , quantifiers :: !(Map ItemId (Def Quantifier))
    ,  qualifiers :: !(Map ItemId (Def (Qualifier t)))
    ,      fields :: !(Map ItemId (Def (FieldType t)))
    ,       types :: !(Map ItemId (Def t))
    ,      values :: !(Map ItemId (Def v))
    ,     imports :: !(Map ItemId (Def i))
    }
    deriving Show

instance (Pretty t, Pretty v, Pretty i)
    => Pretty (DefSet t v i) where
        pPrintPrec lvl _ DefSet{..} =
            vcat'
                [ hang "groups" $ pPrintPrec lvl 0 groups
                , hang "quantifiers" $ pPrintPrec lvl 0 quantifiers
                , hang "qualifiers" $ pPrintPrec lvl 0 qualifiers
                , hang "fields" $ pPrintPrec lvl 0 fields
                , hang "types" $ pPrintPrec lvl 0 types
                , hang "values" $ pPrintPrec lvl 0 values
                , hang "imports" $ pPrintPrec lvl 0 imports
                ]


data AnalysisModuleHeader
    = AnalysisModuleHeader
    {        files :: !(Map FilePath Ref)
    , dependencies :: !(Map SimpleName Ref)
    }
    deriving Show


-- | A map from arbitrary keys to arbitrary lists of values
type MetaData = Map (ATag SimpleName) (ATag String)

-- | A map from locally-appropriate names to module strings and versions
type RawDependencies = [(ATag String, ATag Version, Maybe (ATag SimpleName))]


-- | Raw output from a parser of the head section of a module
data ParserModuleHeader
    = ParserModuleHeader
    {      sources :: ![ATag FilePath]
    , dependencies :: !RawDependencies
    }
    deriving Show

instance Pretty ParserModuleHeader where
    pPrintPrec lvl _ ParserModuleHeader{..} =
        vcat' [ hang "sources" $ pPrintPrec lvl 0 sources
              , hang "dependencies" $ pPrintPrec lvl 0 dependencies
              ]


-- | Pair of @Visible GroupName@ and @ATag Ref@,
--   making up an entry in the @Group@'s associative array
data GroupBinding
    = GroupBinding
    { name :: Visible GroupName
    , ref :: Ref
    }
    deriving (Eq, Ord, Show)

instance Pretty GroupBinding where
    pPrintPrec lvl _ GroupBinding{..} =
        spaceWith "="
            do pPrintPrec lvl 0 name
            do pPrintPrec lvl 0 ref

-- | A map of overloaded bindings
newtype Group
    = Group
    { defs :: [GroupBinding] }
    deriving (Eq, Ord, Show)

instance Pretty Group where
    pPrintPrec lvl _ (Group ds) =
        hang "defs" $ vcat' do
            pPrintPrec lvl 0 <$> ds

instance Nil Group where
    nil = Group []
    isNil = isNil . (.defs)

-- | Lookup @Ref@s matching a generic @PathName@ in a @Group@
searchRef :: PathName -> Group -> [GroupBinding]
searchRef n = compose (.defs) $ filter \def ->
    let n' = def.name.value.value.name.value
        c = def.name.value.category
    in n' == n.name && sameCategory n.category c
    where
        sameCategory = \case
            Just oc -> (oc ==) . overloadedCategory
            _ -> const True

-- | Look up the @Ref@ associated with a @SpecificName@ in a @Group@
getRef :: SpecificName -> Group -> Maybe (ATag (Visible Ref))
getRef sn = compose (.defs) $ lookupWith \def ->
    let n = def.name.value.value.name.value
        c = def.name.value.category
    in guard (n == sn.value && c == sn.category) $>
        (def.ref <$ def.name <$ def.name.value.value.name)

-- | Insert a new @Ref@ into a @Group@, bound to a @Visible GroupName@
insertRef :: Visible GroupName -> Ref -> Group -> Either (ATag Doc) Group
insertRef n r g =
    let sn = n.value.value.name.value <$ n.value
    in case getRef sn g of
        Just ex -> Left $
            ("name" <+> pPrint n <+> "already exists")
                :@: ex.tag
        _ -> Right $ g { defs = GroupBinding n r : g.defs }



-- | A definition of some item in a module, with a parent.
--   the parent is either the namespace or type the item was defined in,
--   or the module if it is the root namespace
data Def v
    = Def
    { parent :: !ItemId
    , inner :: !(ATag v)
    }
    deriving (Eq, Ord, Show)

instance Pretty v => Pretty (Def v) where
    pPrintPrec lvl _ Def{..} =
        spaceWith "::"
            do pPrintPrec lvl 0 parent
            do pPrintPrec lvl 0 inner


-- | A group of imports that has been resolved to a set of references
--   to continue lookup traversal through
newtype ResolvedBlobs
    = ResolvedBlobs
    { inner :: Map (ATag Ref) [ATag FixName] }
    deriving (Eq, Ord, Show)

instance Pretty ResolvedBlobs where
    pPrintPrec lvl _ (ResolvedBlobs bs) =
        hang "blobs" do
            pPrintPrec lvl 0 bs


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

-- | An unresolved group of imports
data UnresolvedImports
    = UnresolvedImports
    { aliases :: ![(Visible UnresolvedName, ATag Path)]
    ,   blobs :: ![UnresolvedBlob]
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

lookupAlias ::
    FixName -> UnresolvedImports -> Maybe (Visible UnresolvedName, ATag Path)
lookupAlias n ui = Fold.find ((== n) . (.value.name.value) . fst) ui.aliases

insertAlias ::
    ATag Path -> Visible UnresolvedName -> UnresolvedImports ->
        Either (ATag Doc) UnresolvedImports
insertAlias p n ui =
    case lookupAlias n.value.name.value ui of
        Just (ex, _) -> Left $
            ("alias" <+> pPrint n.value.name <+> "already exists")
                :@: ex.value.name.tag
        _ -> Right $ ui { aliases = (n, p) : ui.aliases }

insertBlob :: ATag Path -> [ATag PathName] -> UnresolvedImports -> UnresolvedImports
insertBlob p h ui = ui { blobs = UnresolvedBlob p h : ui.blobs }
