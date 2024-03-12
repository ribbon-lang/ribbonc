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
    , bindGroup, bindGroupHere
    , bindQuantifier, bindQuantifierHere
    , bindQualifier, bindQualifierHere
    , bindField, bindFieldHere
    , bindType, bindTypeHere
    , bindValue, bindValueHere
    , bindImports, bindImportsHere

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
    )where

import Data.Bifunctor

import qualified Data.Map.Strict as Map

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
bindGroup :: MonadDefs m => ItemId -> M.Def M.Group -> m ()
bindGroup eid def = defsModify $ second \defs ->
    defs { M.groups = Map.insert eid def defs.groups }

-- | Insert a definition quantifier by @ItemId@
bindQuantifier :: MonadDefs m => ItemId -> M.Def Quantifier -> m ()
bindQuantifier eid def = defsModify $ second \defs ->
    defs { M.quantifiers = Map.insert eid def defs.quantifiers }

-- | Insert a definition qualifier by @ItemId@
bindQualifier :: MonadDefs m => ItemId -> M.Def (Qualifier TokenSeq) -> m ()
bindQualifier eid def = defsModify $ second \defs ->
    defs { M.qualifiers = Map.insert eid def defs.qualifiers }

-- | Insert a field definition by @ItemId@
bindField :: MonadDefs m => ItemId -> M.Def (Field TokenSeq) -> m ()
bindField eid def = defsModify $ second \defs ->
    defs { M.fields = Map.insert eid def defs.fields }

-- | Insert a type definition by @ItemId@
bindType :: MonadDefs m => ItemId -> M.Def TokenSeq -> m ()
bindType eid def = defsModify $ second \defs ->
    defs { M.types = Map.insert eid def defs.types }

-- | Insert a value definition by @ItemId@
bindValue :: MonadDefs m => ItemId -> M.Def TokenSeq -> m ()
bindValue eid def = defsModify $ second \defs ->
    defs { M.values = Map.insert eid def defs.values }

-- | Insert an @UnresolvedImports@ definition by @ItemId@
bindImports :: MonadDefs m => ItemId -> M.Def M.UnresolvedImports -> m ()
bindImports eid def = defsModify $ second \defs ->
    defs { M.imports = Map.insert eid def defs.imports }


-- | Insert a @Group@ by @ItemId@
bindGroupHere :: Has m [ItemId, M.ParserDefs] =>
    ItemId -> ATag M.Group -> m ()
bindGroupHere eid def = do
    ci <- getItemId
    bindGroup eid (M.Def (Just ci) def)

-- | Insert a definition quantifier by @ItemId@
bindQuantifierHere :: Has m [ItemId, M.ParserDefs] =>
    ItemId -> ATag Quantifier -> m ()
bindQuantifierHere eid def = do
    ci <- getItemId
    bindQuantifier eid (M.Def (Just ci) def)

-- | Insert a definition qualifier by @ItemId@
bindQualifierHere :: Has m [ItemId, M.ParserDefs] =>
    ItemId -> ATag (Qualifier TokenSeq) -> m ()
bindQualifierHere eid def = do
    ci <- getItemId
    bindQualifier eid (M.Def (Just ci) def)

-- | Insert a field definition by @ItemId@
bindFieldHere :: Has m [ItemId, M.ParserDefs] =>
    ItemId -> ATag (Field TokenSeq) -> m ()
bindFieldHere eid def = do
    ci <- getItemId
    bindField eid (M.Def (Just ci) def)

-- | Insert a type definition by @ItemId@
bindTypeHere :: Has m [ItemId, M.ParserDefs] =>
    ItemId -> ATag TokenSeq -> m ()
bindTypeHere eid def = do
    ci <- getItemId
    bindType eid (M.Def (Just ci) def)

-- | Insert a value definition by @ItemId@
bindValueHere :: Has m [ItemId, M.ParserDefs] =>
    ItemId -> ATag TokenSeq -> m ()
bindValueHere eid def = do
    ci <- getItemId
    bindValue eid (M.Def (Just ci) def)

-- | Insert an @UnresolvedImports@ definition by @ItemId@
bindImportsHere :: Has m [ItemId, M.ParserDefs] =>
    ItemId -> ATag M.UnresolvedImports -> m ()
bindImportsHere eid def = do
    ci <- getItemId
    bindImports eid (M.Def (Just ci) def)



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
