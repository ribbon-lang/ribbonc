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

    , MonadGroup
    , groupState
    , groupGet
    , groupPut
    , groupModify
    , insertRef

    , MonadImports
    , importsState
    , importsGet
    , importsPut
    , importsModify
    , insertAlias
    , insertBlob
    )where

import Data.Bifunctor

import Data.Tag
import Data.Attr
import Data.Diagnostic

import Control.Monad.Diagnostics.Class
import Control.Monad.Builder as X
import Control.Has

import Text.Pretty

import Language.Ribbon.Util

import Language.Ribbon.Lexical
import Language.Ribbon.Syntax.Ref
import Language.Ribbon.Syntax.Scheme
import Language.Ribbon.Syntax.Data
import Language.Ribbon.Syntax.Module qualified as M
import qualified Data.Map.Strict as Map

import Language.Ribbon.Analysis.Context


-- | State object for @MonadDefs@
type DefsState = (ItemId, M.ParserDefs)

-- | @MonadBuilder ParserDefs@
type MonadDefs = MonadBuilder DefsState

type instance Has m (M.ParserDefs ': effs) = (MonadDefs m, Has m effs)

-- | @builderState@ specialized to @ParserDefs@
defsState :: MonadDefs m =>
    (DefsState -> (a, DefsState)) -> m a
defsState = builderState

-- | @builderGet@ specialized to @ParserDefs@
defsGet :: MonadDefs m => m DefsState
defsGet = builderGet

-- | @builderPut@ specialized to @ParserDefs@
defsPut :: MonadDefs m => DefsState -> m ()
defsPut = builderPut

-- | @builderModify@ specialized to @ParserDefs@
defsModify :: MonadDefs m => (DefsState -> DefsState) -> m ()
defsModify = builderModify


-- | Get a fresh @ItemId@
freshItemId :: MonadDefs m => m ItemId
freshItemId = defsState \(i, d) -> (i, (i + 1, d))

-- | Get a fresh @ItemId@, then call the given function to bind a @Def@ to it
withFreshItemId :: MonadDefs m =>
    (ItemId -> M.Def a -> m ()) -> M.Def a -> m ()
withFreshItemId f def = freshItemId >>= (`f` def)


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
bindField :: MonadDefs m => ItemId -> M.Def (FieldType TokenSeq) -> m ()
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




-- | @MonadBuilder Group@
type MonadGroup = MonadBuilder M.Group

type instance Has m (M.Group ': effs) = (MonadGroup m, Has m effs)

-- | @builderState@ specialized to @Group@
groupState :: MonadGroup m =>
    (M.Group -> (a, M.Group)) -> m a
groupState = builderState

-- | @builderGet@ specialized to @Group@
groupGet :: MonadGroup m => m M.Group
groupGet = builderGet

-- | @builderPut@ specialized to @Group@
groupPut :: MonadGroup m => M.Group -> m ()
groupPut = builderPut

-- | @builderModify@ specialized to @Group@
groupModify :: MonadGroup m =>
    (M.Group -> M.Group) -> m ()
groupModify = builderModify


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
            (Just n.value.value.name.value)
            err
            ["it was first defined here:" <+> pPrint at]



-- | @MonadBuilder UnresolvedImports@
type MonadImports = MonadBuilder M.UnresolvedImports

type instance Has m (M.UnresolvedImports ': effs) = (MonadImports m, Has m effs)

-- | @builderState@ specialized to @UnresolvedImports@
importsState :: MonadImports m =>
    (M.UnresolvedImports -> (a, M.UnresolvedImports)) -> m a
importsState = builderState

-- | @builderGet@ specialized to @UnresolvedImports@
importsGet :: MonadImports m => m M.UnresolvedImports
importsGet = builderGet

-- | @builderPut@ specialized to @UnresolvedImports@
importsPut :: MonadImports m => M.UnresolvedImports -> m ()
importsPut = builderPut

-- | @builderModify@ specialized to @UnresolvedImports@
importsModify :: MonadImports m =>
    (M.UnresolvedImports -> M.UnresolvedImports) -> m ()
importsModify = builderModify


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
            (Just n.value.name.value)
            err
            ["it was first defined here:" <+> pPrint at]


-- | Insert an unresolved blob import by @Path@;
--   This cannot fail, as blobs are allowed to be duplicated
insertBlob :: MonadImports m =>
    Visible (ATag Path) -> [ATag PathName] -> m ()
insertBlob p h = importsModify (M.insertBlob p h)
