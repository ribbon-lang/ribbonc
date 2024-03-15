module Language.Ribbon.Analysis.Builder
    ( module X
    , DefsState

    , MonadDefs
    , defsState
    , defsGet
    , defsPut
    , defsModify
    , freshItemId
    , withFreshItemId
    , bindGroup
    , bindQuantifier
    , bindQualifier
    , bindField
    , bindType
    , bindValue
    , bindImports
    , bindParent, bindChildHere
    , withParent

    , MonadGroup
    , groupState
    , groupGet
    , groupPut
    , groupModify
    , insertRef
    , insertNew

    , MonadImports
    , importsState
    , importsGet
    , importsPut
    , importsModify
    , insertAlias
    , insertBlob

    , MonadResolverSet
    , resolverSetState
    , getResolverSet
    , getsResolverSet
    , putResolverSet
    , modifyResolverSet

    , MonadParserModule
    , parserModuleState
    , getParserModule
    , getsParserModule
    , putParserModule
    , modifyParserModule
    , lookupFileId
    , lookupDependencyId
    , lookupDependencyIdAttr
    , lookupDependency
    )where

import Data.Bifunctor

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)

import Data.Tag
import Data.Attr
import Data.Diagnostic

import Control.Has
import Control.Monad.State.Dynamic as X

import Text.Pretty

import Language.Ribbon.Util

import Language.Ribbon.Lexical
import Language.Ribbon.Syntax.Ref
import Language.Ribbon.Syntax.Scheme
import Language.Ribbon.Syntax.Data
import Language.Ribbon.Syntax.Module qualified as M

import Language.Ribbon.Analysis.Diagnostics
import Language.Ribbon.Analysis.Context
import Control.Monad (guard)




-- | State object for @MonadDefs@
type DefsState = (ItemId, M.ParserDefs)

-- | @MonadState ParserDefs@
type MonadDefs = MonadState DefsState

type instance Has m (M.ParserDefs ': effs) = (MonadDefs m, Has m effs)

-- | @state@ specialized to @ParserDefs@
defsState :: MonadDefs m =>
    (DefsState -> (a, DefsState)) -> m a
defsState = state

-- | @get@ specialized to @ParserDefs@
defsGet :: MonadDefs m => m DefsState
defsGet = get

-- | @put@ specialized to @ParserDefs@
defsPut :: MonadDefs m => DefsState -> m ()
defsPut = put

-- | @modify@ specialized to @ParserDefs@
defsModify :: MonadDefs m => (DefsState -> DefsState) -> m ()
defsModify = modify


-- | Get a fresh @ItemId@
freshItemId :: MonadDefs m => m ItemId
freshItemId = defsState \(i, d) -> (i, (i + 1, d))

-- | Get a fresh @ItemId@, then call the given function to bind a @Def@ to it
withFreshItemId :: MonadDefs m =>
    (ItemId -> x -> m ()) -> x -> m ItemId
withFreshItemId f def = do
    ii <- freshItemId
    ii <$ f ii def


-- | Insert a @Group@ by @ItemId@
bindGroup :: MonadDefs m => ItemId -> ATag M.Group -> m ()
bindGroup eid def = defsModify $ second \defs ->
    defs { M.groups = Map.insert eid def defs.groups }

-- | Insert a definition quantifier by @ItemId@
bindQuantifier :: MonadDefs m => ItemId -> ATag Quantifier -> m ()
bindQuantifier eid def = defsModify $ second \defs ->
    defs { M.quantifiers = Map.insert eid def defs.quantifiers }

-- | Insert a definition qualifier by @ItemId@
bindQualifier :: MonadDefs m => ItemId -> ATag (Qualifier TokenSeq) -> m ()
bindQualifier eid def = defsModify $ second \defs ->
    defs { M.qualifiers = Map.insert eid def defs.qualifiers }

-- | Insert a field definition by @ItemId@
bindField :: MonadDefs m => ItemId -> ATag (Field TokenSeq) -> m ()
bindField eid def = defsModify $ second \defs ->
    defs { M.fields = Map.insert eid def defs.fields }

-- | Insert a type definition by @ItemId@
bindType :: MonadDefs m => ItemId -> ATag TokenSeq -> m ()
bindType eid def = defsModify $ second \defs ->
    defs { M.types = Map.insert eid def defs.types }

-- | Insert a value definition by @ItemId@
bindValue :: MonadDefs m => ItemId -> ATag TokenSeq -> m ()
bindValue eid def = defsModify $ second \defs ->
    defs { M.values = Map.insert eid def defs.values }

-- | Insert an @UnresolvedImports@ definition by @ItemId@
bindImports :: MonadDefs m => ItemId -> ATag M.UnresolvedImports -> m ()
bindImports eid def = defsModify $ second \defs ->
    defs { M.imports = Map.insert eid def defs.imports }

-- | Insert a parent @ItemId@ for a given @ItemId@
bindParent :: MonadDefs m => ItemId -> ItemId -> m ()
bindParent child parent = defsModify $ second \defs ->
    defs { M.parents = Map.insert child parent defs.parents }

bindChildHere :: Has m [ItemId, M.ParserDefs] =>
    ItemId -> m ()
bindChildHere eid = do
    ci <- getItemId
    bindParent eid ci

withParent :: Has m [ItemId, M.ParserDefs] => (ItemId -> a -> m ()) -> ItemId -> a -> m ()
withParent f eid a = f eid a >> bindChildHere eid

-- | @MonadState Group@
type MonadGroup = MonadState M.Group

type instance Has m (M.Group ': effs) = (MonadGroup m, Has m effs)

-- | @state@ specialized to @Group@
groupState :: MonadGroup m =>
    (M.Group -> (a, M.Group)) -> m a
groupState = state

-- | @get@ specialized to @Group@
groupGet :: MonadGroup m => m M.Group
groupGet = get

-- | @put@ specialized to @Group@
groupPut :: MonadGroup m => M.Group -> m ()
groupPut = put

-- | @modify@ specialized to @Group@
groupModify :: MonadGroup m =>
    (M.Group -> M.Group) -> m ()
groupModify = modify


-- | Insert an unresolved definition by @GroupName@ and @Ref@
insertRef :: Has m [ Ref, M.Group, Diag ] =>
    Visible GroupName -> Ref -> m ()
insertRef n r =
    do groupState \g ->
        case M.insertRef n r g of
            Left e -> (Just e, g)
            Right g' -> (Nothing, g')
    >>= whenJust \(err :@: at) ->
        reportErrorRefH
            n.value.value.name.tag
            ConflictingDefinition
            (Just $ prettyShow n.value.value.name.value)
            err
            ["it was first defined here:" <+> pPrint at]


-- | Insert a new @Ref@ into a @Group@, bound to a @Visible GroupName@,
--   where the @Ref@ is generated from an action producing an @ItemId@
insertNew :: Has m [ Ref, M.Group, Diag ] =>
    Visible GroupName -> m ItemId -> m ItemId
insertNew n m = do
    mi <- getModuleId
    ii <- m
    ii <$ insertRef n (Ref mi ii)



-- | @MonadState UnresolvedImports@
type MonadImports = MonadState M.UnresolvedImports

type instance Has m (M.UnresolvedImports ': effs) = (MonadImports m, Has m effs)

-- | @state@ specialized to @UnresolvedImports@
importsState :: MonadImports m =>
    (M.UnresolvedImports -> (a, M.UnresolvedImports)) -> m a
importsState = state

-- | @get@ specialized to @UnresolvedImports@
importsGet :: MonadImports m => m M.UnresolvedImports
importsGet = get

-- | @put@ specialized to @UnresolvedImports@
importsPut :: MonadImports m => M.UnresolvedImports -> m ()
importsPut = put

-- | @modify@ specialized to @UnresolvedImports@
importsModify :: MonadImports m =>
    (M.UnresolvedImports -> M.UnresolvedImports) -> m ()
importsModify = modify


-- | Insert an unresolved import by @UnresolvedName@ and @Path@;
--   Creates an error @Diagnostic@ if the name is already bound to an import
insertAlias :: Has m [Ref, M.UnresolvedImports, Diag] =>
    Visible UnresolvedName -> ATag Path -> m ()
insertAlias n p =
    do importsState \i ->
        case M.insertAlias n p i of
            Left e -> (Just e, i)
            Right i' -> (Nothing, i')
    >>= whenJust \(err :@: at) ->
        reportErrorRefH
            n.value.name.tag
            ConflictingDefinition
            (Just $ prettyShow n.value.name.value)
            err
            ["it was first defined here:" <+> pPrint at]


-- | Insert an unresolved blob import by @Path@;
--   This cannot fail, as blobs are allowed to be duplicated
insertBlob :: MonadImports m =>
    Visible (ATag Path) -> [ATag PathName] -> m ()
insertBlob p h = importsModify (M.insertBlob p h)




-- | Map of @Ref@s to sets of @Path@s, used for circular import detection
--   during name resolution
type ResolverSet = Map Ref (Set Path)

-- | @MonadState ResolverSet@
type MonadResolverSet = MonadState ResolverSet

type instance Has m (ResolverSet ': effs) = (MonadResolverSet m, Has m effs)

-- | @state@ specialized to @ResolverSet@
resolverSetState :: MonadResolverSet m =>
    (ResolverSet -> (a, ResolverSet)) -> m a
resolverSetState = state

-- | @get@ specialized to @ResolverSet@
getResolverSet :: MonadResolverSet m => m ResolverSet
getResolverSet = get

-- | @gets@ specialized to @ResolverSet@
getsResolverSet :: MonadResolverSet m => (ResolverSet -> a) -> m a
getsResolverSet = gets

-- | @put@ specialized to @ResolverSet@
putResolverSet :: MonadResolverSet m => ResolverSet -> m ()
putResolverSet = put

-- | @modify@ specialized to @ResolverSet@
modifyResolverSet :: MonadResolverSet m =>
    (ResolverSet -> ResolverSet) -> m ()
modifyResolverSet = modify





-- | @MonadState ParserModule@
type MonadParserModule = MonadState M.ParserModule

type instance Has m (M.ParserModule ': effs) = (MonadParserModule m, Has m effs)

-- | @state@ specialized to @ParserModule@
parserModuleState :: MonadParserModule m =>
    (M.ParserModule -> (a, M.ParserModule)) -> m a
parserModuleState = state

-- | @get@ specialized to @ParserModule@
getParserModule :: MonadParserModule m => m M.ParserModule
getParserModule = get

-- | @gets@ specialized to @ParserModule@
getsParserModule :: MonadParserModule m => (M.ParserModule -> a) -> m a
getsParserModule = gets

-- | @put@ specialized to @ParserModule@
putParserModule :: MonadParserModule m => M.ParserModule -> m ()
putParserModule = put

-- | @modify@ specialized to @ParserModule@
modifyParserModule :: MonadParserModule m =>
    (M.ParserModule -> M.ParserModule) -> m ()
modifyParserModule = modify

-- | Lookup a file id in the @ParserModule@
lookupFileId :: MonadParserModule m => FilePath -> m (Maybe ItemId)
lookupFileId fp = getsParserModule $ Map.lookup fp . (.header.files)

-- | Lookup a dependency id in the @ParserModule@
lookupDependencyId :: MonadParserModule m =>
    SimpleName -> m (Maybe ModuleId)
lookupDependencyId n = getsParserModule $
    compose (.header.dependencies) $ lookupWith \(an, mi) ->
        mi <$ guard (n == an.value)

-- | Lookup a dependency id in the @ParserModule@
lookupDependencyIdAttr :: MonadParserModule m =>
    SimpleName -> m (Maybe (ATag ModuleId))
lookupDependencyIdAttr n = getsParserModule $
    compose (.header.dependencies) $ lookupWith \(an, mi) ->
        mi <$ an <$ guard (n == an.value)


-- | Lookup a dependency in the @ParserModule@
lookupDependency :: Has m [M.ParserModule, M.ModuleContext] =>
    SimpleName -> m (Maybe M.FinalModule)
lookupDependency n = do
    lookupDependencyId n >>= \case
        Just dr -> getsModCtx $ Map.lookup dr . (.modules)
        _ -> pure Nothing
