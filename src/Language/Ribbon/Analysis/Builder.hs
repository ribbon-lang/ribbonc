module Language.Ribbon.Analysis.Builder
    ( module X
    , DefsState

    , MonadDefs
    , freshDefsState
    , defsStateExtract
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
    , bindCategory
    , bindCategory'
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

    , MonadResolverModule
    , parserModuleState
    , getResolverModule
    , getsResolverModule
    , putResolverModule
    , modifyResolverModule
    , lookupFileId
    , lookupDependencyId
    , lookupDependencyIdAttr
    , lookupDependency
    , resolverModuleId
    )where


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
import Language.Ribbon.Syntax.Type (UserType)
import Data.Nil




-- | State object for @MonadDefs@
type DefsState = (ItemId, M.ParserDefs, M.UnresolvedImportMap)

freshDefsState :: ItemId -> DefsState
freshDefsState i = (i + 1, Nil, Nil)

defsStateExtract :: DefsState ->
    (M.ParserDefs, M.UnresolvedImportMap)
defsStateExtract (_, d, u) = (d, u)

stateId :: (ItemId -> (a, ItemId)) -> DefsState -> (a, DefsState)
stateId f (i, d, u) = let (a, i') = f i in (a, (i', d, u))

stateDefs :: (M.ParserDefs -> (a, M.ParserDefs)) -> DefsState -> (a, DefsState)
stateDefs f (i, d, u) = let (a, d') = f d in (a, (i, d', u))

onDefs :: (M.ParserDefs -> M.ParserDefs) -> DefsState -> DefsState
onDefs f = snd . stateDefs \d -> ((), f d)

stateImports ::
    (M.UnresolvedImportMap ->
        (a, M.UnresolvedImportMap)) ->
        DefsState -> (a, DefsState)
stateImports f (i, d, u) = let (a, u') = f u in (a, (i, d, u'))

onImports ::
    (M.UnresolvedImportMap ->
        M.UnresolvedImportMap) ->
        DefsState -> DefsState
onImports f = snd . stateImports \u -> ((), f u)

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
freshItemId = defsState $ stateId \i -> (i, i + 1)

-- | Get a fresh @ItemId@, then call the given function to bind a @Def@ to it
withFreshItemId :: MonadDefs m =>
    (ItemId -> x -> m ()) -> x -> m ItemId
withFreshItemId f def = do
    ii <- freshItemId
    ii <$ f ii def


-- | Insert a @Group@ by @ItemId@
bindGroup :: MonadDefs m => ItemId -> ATag M.Group -> m ()
bindGroup eid def = defsModify $ onDefs \defs ->
    defs { M.groups = Map.insert eid def defs.groups }

-- | Insert a definition quantifier by @ItemId@
bindQuantifier :: MonadDefs m => ItemId -> ATag Quantifier -> m ()
bindQuantifier eid def = defsModify $ onDefs \defs ->
    defs { M.quantifiers = Map.insert eid def defs.quantifiers }

-- | Insert a definition qualifier by @ItemId@
bindQualifier :: MonadDefs m => ItemId -> ATag (Qualifier UserType) -> m ()
bindQualifier eid def = defsModify $ onDefs \defs ->
    defs { M.qualifiers = Map.insert eid def defs.qualifiers }

-- | Insert a field definition by @ItemId@
bindField :: MonadDefs m => ItemId -> ATag (Field UserType) -> m ()
bindField eid def = defsModify $ onDefs \defs ->
    defs { M.fields = Map.insert eid def defs.fields }

-- | Insert a type definition by @ItemId@
bindType :: MonadDefs m => ItemId -> ATag UserType -> m ()
bindType eid def = defsModify $ onDefs \defs ->
    defs { M.types = Map.insert eid def defs.types }

-- | Insert a value definition by @ItemId@
bindValue :: MonadDefs m => ItemId -> ATag TokenSeq -> m ()
bindValue eid def = defsModify $ onDefs \defs ->
    defs { M.values = Map.insert eid def defs.values }

-- | Insert an @UnresolvedImports@ definition by @ItemId@
bindImports :: MonadDefs m => ItemId -> ATag M.UnresolvedImports -> m ()
bindImports eid def = defsModify $ onImports $ Map.insert eid def

-- | Insert a parent @ItemId@ for a given @ItemId@
bindParent :: MonadDefs m => ItemId -> ItemId -> m ()
bindParent child parent = defsModify $ onDefs \defs ->
    defs { M.parents = Map.insert child parent defs.parents }

-- | Insert a @Category@ for a given @ItemId@
bindCategory :: MonadDefs m => ItemId -> Category -> m ()
bindCategory eid category = defsModify $ onDefs \defs ->
    defs { M.categories = Map.insert eid category defs.categories }

-- | Insert a @Category@ for a given @ItemId@
bindCategory' :: MonadDefs m => Category -> ItemId -> m ()
bindCategory' = flip bindCategory

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
    Visible M.GroupBinding -> m ()
insertRef b =
    do groupState \g ->
        case M.groupInsertRef b g of
            Left e -> (Just e, g)
            Right g' -> (Nothing, g')
    >>= whenJust \(err :@: at) ->
        reportErrorRefH
            b.value.name.value.tag
            ConflictingDefinition
            (Just $ prettyShow b.value.name.value.value)
            err
            ["it was first defined here:" <+> pPrint at]


-- | Insert a new @Ref@ into a @Group@, bound to a @Visible QualifiedName@,
--   where the @Ref@ is generated from an action producing an @ItemId@
insertNew :: Has m [ Ref, M.Group, Diag ] =>
    Visible QualifiedName -> m ItemId -> m ItemId
insertNew n m = do
    mi <- getModuleId
    ii <- m
    ii <$ insertRef (M.GroupBinding n.value (Ref mi ii) <$ n)



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
    Visible M.UnresolvedAlias -> m ()
insertAlias ua =
    do importsState \i ->
        case M.insertUnresolvedAlias ua i of
            Left e -> (Just e, i)
            Right i' -> (Nothing, i')
    >>= whenJust \(err :@: at) ->
        reportErrorRefH
            ua.value.name.value.tag
            ConflictingDefinition
            (Just $ prettyShow ua.value.name.value.value)
            err
            ["it was first defined here:" <+> pPrint at]


-- | Insert an unresolved blob import by @Path@;
--   This cannot fail, as blobs are allowed to be duplicated
insertBlob :: MonadImports m =>
    Visible M.UnresolvedBlob -> m ()
insertBlob b = importsModify (M.insertUnresolvedBlob b)




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





-- | @MonadState ResolverModule@
type MonadResolverModule = MonadState M.ResolverModule

type instance Has m (M.ResolverModule ': effs) = (MonadResolverModule m, Has m effs)

-- | @state@ specialized to @ResolverModule@
parserModuleState :: MonadResolverModule m =>
    (M.ResolverModule -> (a, M.ResolverModule)) -> m a
parserModuleState = state

-- | @get@ specialized to @ResolverModule@
getResolverModule :: MonadResolverModule m => m M.ResolverModule
getResolverModule = get

-- | @gets@ specialized to @ResolverModule@
getsResolverModule :: MonadResolverModule m => (M.ResolverModule -> a) -> m a
getsResolverModule = gets

-- | @put@ specialized to @ResolverModule@
putResolverModule :: MonadResolverModule m => M.ResolverModule -> m ()
putResolverModule = put

-- | @modify@ specialized to @ResolverModule@
modifyResolverModule :: MonadResolverModule m =>
    (M.ResolverModule -> M.ResolverModule) -> m ()
modifyResolverModule = modify

-- | Lookup a file id in the @ResolverModule@
lookupFileId :: MonadResolverModule m => FilePath -> m (Maybe ItemId)
lookupFileId fp = getsResolverModule $ Map.lookup fp . (.header.analysis.files)

-- | Lookup a dependency id in the @ResolverModule@
lookupDependencyId :: MonadResolverModule m =>
    SimpleName -> m (Maybe ModuleId)
lookupDependencyId n = fmap untag <$> lookupDependencyIdAttr n

-- | Lookup a dependency id in the @ResolverModule@
lookupDependencyIdAttr :: MonadResolverModule m =>
    SimpleName -> m (Maybe (ATag ModuleId))
lookupDependencyIdAttr n = getsResolverModule $
    Map.lookup n . (.header.analysis.dependencies)


-- | Lookup a dependency in the @ResolverModule@
lookupDependency :: Has m [M.ResolverModule, M.ModuleContext] =>
    SimpleName -> m (Maybe M.FinalModule)
lookupDependency n = do
    lookupDependencyId n >>= \case
        Just dr -> getsModCtx $ Map.lookup dr . (.modules)
        _ -> pure Nothing



resolverModuleId :: MonadResolverModule m => m ModuleId
resolverModuleId = getsResolverModule (.header.analysis.moduleId)
