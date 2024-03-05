module Language.Ribbon.Syntax.Module where

import Data.Map.Strict (Map)

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Functor

import Data.Attr

import Text.Pretty

import Language.Ribbon.Lexical

import Language.Ribbon.Syntax.Ref
import Language.Ribbon.Syntax.Data
import Language.Ribbon.Syntax.Scheme
import Language.Ribbon.Syntax.Type
import Language.Ribbon.Syntax.Value




-- | An analysis context component, storing completed module's information.
--   Provides a lookup table from @(name string, version)@ pairs,
--   to their respective references
data ModuleContext
    = ModuleContext
    { modules :: !(RefMap FinalModule)
    , moduleLookup :: !(Map (String, Version) Ref)
    }
    deriving Show


-- | A module that has been fully parsed
type FinalModule
    = Module
        ()
        Type
        Value
        ResolvedBlobs

-- | A module that is being analyzed
type AnalysisModule
    = Module
        AnalysisModuleHeader
        Type
        Value
        ResolvedBlobs

-- | A module that has been partially parsed,
--   and is awaiting name resolution or parsing
type RawModule
    = Module
        RawModuleHeader
        TokenSeq
        TokenSeq
        UnresolvedImports


-- | Parametric type storing all the elements of a module,
--   with their form depending on compilation phase.
--   A given @Ref@ may be bound to multiple rows of the module,
--   for example a value definition may have an entry in
--   @values@, @quantifiers@, @qualifiers@, and @types@, while
--   types with fields are stored in @groups@, @quantifiers@, and @qualifiers@.
--   The root namespace is always stored in @Ref Namespace modId 0@
data Module h t v i
    = Module
    {      header :: h
    ,        meta :: !MetaData

    ,      groups :: !(RefMap (Def Group))
    , quantifiers :: !(RefMap (Def Quantifier))
    ,  qualifiers :: !(RefMap (Def (Qualifier t)))
    ,      fields :: !(RefMap (Def (FieldType t)))
    ,       types :: !(RefMap (Def t))
    ,      values :: !(RefMap (Def v))
    ,     imports :: !(RefMap (Def i))
    }
    deriving Show

instance (Pretty h, Pretty t , Pretty v, Pretty i)
    => Pretty (Module h t v i) where
        pPrintPrec lvl _ Module{..} =
            vcat'
                [ hang "header" $ pPrintPrec lvl 0 header
                , hang "meta" $ pPrintPrec lvl 0 meta
                , hang "groups" $ pPrintPrec lvl 0 groups
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
data RawModuleHeader
    = RawModuleHeader
    { sources :: ![ATag FilePath]
    , dependencies :: !RawDependencies
    }
    deriving Show

instance Pretty RawModuleHeader where
    pPrintPrec lvl _ RawModuleHeader{..} =
        vcat' [ hang "sources" $ pPrintPrec lvl 0 sources
              , hang "dependencies" $ pPrintPrec lvl 0 dependencies
              ]


-- | A map of overloaded bindings
newtype Group
    = Group
    { defs :: [(GroupName, ATag Ref)] }
    deriving (Eq, Ord, Show)

instance Pretty Group where
    pPrintPrec lvl _ (Group ds) =
        hang "defs" $ vcat' do
            ds <&> \(n, r) ->
                spaceWith "="
                    do pPrintPrec lvl 0 n
                    do pPrintPrec lvl 0 r


-- | A definition of some item in a module, with a parent.
--   the parent is either the namespace or type the item was defined in,
--   or the module if it is the root namespace
data Def v
    = Def
    { parent :: !Ref
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
    { inner :: Set (ATag Ref) }
    deriving (Eq, Ord, Show)

instance Pretty ResolvedBlobs where
    pPrintPrec lvl _ (ResolvedBlobs bs) =
        hang "blobs" $ vcat' do
            pPrintPrec lvl 0 <$> Set.toList bs


-- | An unresolved group of imports
data UnresolvedImports
    = UnresolvedImports
    { aliases :: ![(UnresolvedName, ATag Path)]
    , blobs :: !(Set (ATag Path))
    }
    deriving (Eq, Ord, Show)

instance Pretty UnresolvedImports where
    pPrintPrec lvl _ (UnresolvedImports as bs) =
        vcat'
            [ hang "aliases" $ vcat' do
                pPrintPrec lvl 0 <$> as
            , hang "blobs" $ vcat' do
                pPrintPrec lvl 0 <$> Set.toList bs
            ]
