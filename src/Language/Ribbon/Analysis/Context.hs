module Language.Ribbon.Analysis.Context
    ( module X

    , MonadModule
    , getModuleId
    , getsModuleId
    , localModuleId
    , useModuleId

    , MonadItem
    , getItemId
    , getsItemId
    , localItemId
    , useItemId

    , MonadRef
    , getRef
    , reportErrorRef
    , reportErrorRefH

    , MonadFixName
    , getFixName
    , getsFixName
    , localFixName
    , useFixName
    , reportErrorRefName
    , reportErrorRefNameH

    , Location(..)
    , MonadLocation
    , getLocation
    ) where

import Data.Attr
import Data.Diagnostic

import Control.Monad.Diagnostics.Class
import Control.Monad.Context as X
import Control.Has

import Text.Pretty

import Language.Ribbon.Lexical.Name
import Language.Ribbon.Syntax.Ref
import Control.Monad.File.Class



-- | @MonadContext ModuleId@
type MonadModule = MonadContext ModuleId

type instance Has m (ModuleId ': effs) = (MonadModule m, Has m effs)

-- | @getContext@ specialized to @ModuleId@
getModuleId :: MonadModule m => m ModuleId
getModuleId = getContext

-- | @getsContext@ specialized to @ModuleId@
getsModuleId :: MonadModule m => (ModuleId -> a) -> m a
getsModuleId = getsContext

-- | @localContext@ specialized to @ModuleId@
localModuleId :: MonadModule m => (ModuleId -> ModuleId) -> m a -> m a
localModuleId = localContext

-- | @useContext specialized to @ModuleId@
useModuleId :: MonadModule m => ModuleId -> m a -> m a
useModuleId = useContext


-- | @MonadContext ItemId@
type MonadItem = MonadContext ItemId

type instance Has m (ItemId ': effs) = (MonadItem m, Has m effs)

-- | @getContext@ specialized to @ItemId@
getItemId :: MonadItem m => m ItemId
getItemId = getContext

-- | @getsContext@ specialized to @ItemId@
getsItemId :: MonadItem m => (ItemId -> a) -> m a
getsItemId = getsContext

-- | @localContext@ specialized to @ItemId@
localItemId :: MonadItem m => (ItemId -> ItemId) -> m a -> m a
localItemId = localContext

-- | @useContext@ specialized to @ItemId@
useItemId :: MonadItem m => ItemId -> m a -> m a
useItemId = useContext


-- | Combination of @MonadModule@ and @MonadItem@
type MonadRef m = (MonadModule m, MonadItem m)

type instance Has m (Ref ': effs) = (MonadRef m, Has m effs)

-- | @combined @getModuleId@ and @getItemId@
getRef :: Has m '[Ref] => m Ref
getRef = Ref <$> getModuleId <*> getItemId


-- | New error @Diagnostic@ using the current result of @getRef@
reportErrorRef :: Has m [Ref, Diag, With '[Pretty a]] =>
    Attr -> DiagnosticBinderKind -> Maybe FixName -> a -> m ()
reportErrorRef at kind name doc = do
    currentLocation <- getRef
    reportError at
         (DiagnosticBinder kind currentLocation name)
         doc

-- | New error @Diagnostic@ using the current result of @getRef@, with help docs
reportErrorRefH :: Has m [Ref, Diag, With '[Pretty a]] =>
    Attr -> DiagnosticBinderKind -> Maybe FixName -> a -> [Doc] -> m ()
reportErrorRefH at kind name doc help = do
    currentLocation <- getRef
    reportErrorH at
         (DiagnosticBinder kind currentLocation name)
         doc
         help



-- | @MonadContext FixName@
type MonadFixName = MonadContext FixName

type instance Has m (FixName ': effs) = (MonadFixName m, Has m effs)

-- | @getContext@ specialized to @FixName@
getFixName :: MonadFixName m => m FixName
getFixName = getContext

-- | @getsContext@ specialized to @FixName@
getsFixName :: MonadFixName m => (FixName -> a) -> m a
getsFixName = getsContext

-- | @localContext@ specialized to @FixName@
localFixName :: MonadFixName m => (FixName -> FixName) -> m a -> m a
localFixName = localContext

-- | @useContext@ specialized to @FixName@
useFixName :: MonadFixName m => FixName -> m a -> m a
useFixName = useContext

-- | New error @Diagnostic@ using
--   the current result of @getFixName@ and @getRef@
reportErrorRefName :: Has m [Ref, FixName, Diag, With '[Pretty a]] =>
    Attr -> DiagnosticBinderKind -> a -> m ()
reportErrorRefName at kind doc = do
    name <- getFixName
    reportErrorRef at kind (Just name) doc

-- | New error @Diagnostic@ using
--   the current result of @getFixName@ and @getRef@, with help docs
reportErrorRefNameH :: Has m [Ref, FixName, Diag, With '[Pretty a]] =>
    Attr -> DiagnosticBinderKind -> a -> [Doc] -> m ()
reportErrorRefNameH at kind doc help = do
    name <- getFixName
    reportErrorRefH at kind (Just name) doc help


-- | Wrapper for data supplied together
--   by @MonadFile@ and @MonadRef@ under @MonadLocation@
data Location
    = Location
    { ref :: !Ref
    , filePath :: !FilePath
    }
    deriving (Show, Eq, Ord)

instance Pretty Location where
    pPrint Location{..} = pPrint ref <+> "in" <+> pPrint filePath

-- | @MonadRef m, MonadFile m@
type MonadLocation m = (MonadRef m, MonadFile m)

type instance Has m (Location ': effs) = (MonadLocation m, Has m effs)

getLocation :: MonadLocation m => m Location
getLocation = Location <$> getRef <*> getFilePath
