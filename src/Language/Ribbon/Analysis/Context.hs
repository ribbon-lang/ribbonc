module Language.Ribbon.Analysis.Context
    ( module X

    , MonadFile
    , withFilePath
    , getFilePath

    , MonadModule
    , getModuleId
    , getsModuleId
    , localModuleId
    , usingModuleId

    , MonadItem
    , getItemId
    , getsItemId
    , localItemId
    , usingItemId

    , MonadRef
    , getRef
    , reportErrorRef
    , reportErrorRefH
    , diagAssertRef
    , diagAssertRefH

    , MonadFixName
    , getFixName
    , getsFixName
    , localFixName
    , usingFixName
    , reportErrorRefName
    , reportErrorRefNameH
    , diagAssertRefName
    , diagAssertRefNameH

    , Location(..)
    , MonadLocation
    , getLocation
    ) where

import Data.Attr
import Data.Diagnostic

import Control.Has
import Control.Monad.Reader.Dynamic as X

import Text.Pretty

import Language.Ribbon.Analysis.Diagnostics
import Language.Ribbon.Lexical.Name
import Language.Ribbon.Syntax.Ref




type instance Has m (FilePath ': effs) = (MonadFile m, Has m effs)


-- | @MonadReader FilePath m@
type MonadFile = MonadReader FilePath


-- | @using@ specialized to @FilePath@
withFilePath :: MonadFile m => FilePath -> m a -> m a
withFilePath = using

-- | @ask@ specialized to @FilePath@
getFilePath :: MonadFile m => m FilePath
getFilePath = ask




-- | @MonadReader ModuleId@
type MonadModule = MonadReader ModuleId

type instance Has m (ModuleId ': effs) = (MonadModule m, Has m effs)

-- | @ask@ specialized to @ModuleId@
getModuleId :: MonadModule m => m ModuleId
getModuleId = ask

-- | @asks@ specialized to @ModuleId@
getsModuleId :: MonadModule m => (ModuleId -> a) -> m a
getsModuleId = asks

-- | @local@ specialized to @ModuleId@
localModuleId :: MonadModule m => (ModuleId -> ModuleId) -> m a -> m a
localModuleId = local

-- | @using@ specialized to @ModuleId@
usingModuleId :: MonadModule m => ModuleId -> m a -> m a
usingModuleId = using


-- | @MonadReader ItemId@
type MonadItem = MonadReader ItemId

type instance Has m (ItemId ': effs) = (MonadItem m, Has m effs)

-- | @ask@ specialized to @ItemId@
getItemId :: MonadItem m => m ItemId
getItemId = ask

-- | @asks@ specialized to @ItemId@
getsItemId :: MonadItem m => (ItemId -> a) -> m a
getsItemId = asks

-- | @local@ specialized to @ItemId@
localItemId :: MonadItem m => (ItemId -> ItemId) -> m a -> m a
localItemId = local

-- | @using@ specialized to @ItemId@
usingItemId :: MonadItem m => ItemId -> m a -> m a
usingItemId = using


-- | Combination of @MonadModule@ and @MonadItem@
type MonadRef m = (MonadModule m, MonadItem m)

type instance Has m (Ref ': effs) = (MonadRef m, Has m effs)

-- | @combined @getModuleId@ and @getItemId@
getRef :: Has m '[Ref] => m Ref
getRef = Ref <$> getModuleId <*> getItemId


-- | New error @Diagnostic@ using the current result of @getRef@
reportErrorRef :: Has m [Ref, Diag, With '[Pretty a]] =>
    Attr -> DiagnosticBinderKind -> Maybe String -> a -> m ()
reportErrorRef at kind name doc = reportErrorRefH at kind name doc []

diagAssertRef :: Has m [Ref, Diag, With '[Pretty a]] =>
    Bool -> Attr -> DiagnosticBinderKind -> Maybe String -> a -> m ()
diagAssertRef b at kind name doc = diagAssertRefH b at kind name doc []

-- | New error @Diagnostic@ using the current result of @getRef@, with help docs
reportErrorRefH :: Has m [Ref, Diag, With '[Pretty a]] =>
    Attr -> DiagnosticBinderKind -> Maybe String -> a -> [Doc] -> m ()
reportErrorRefH at kind name doc help = do
    currentLocation <- getRef
    reportErrorH at
         (DiagnosticBinder kind currentLocation name)
         doc
         help

diagAssertRefH :: Has m [Ref, Diag, With '[Pretty a]] =>
    Bool -> Attr -> DiagnosticBinderKind -> Maybe String -> a -> [Doc] -> m ()
diagAssertRefH b at kind name doc help = do
    currentLocation <- getRef
    diagAssertH b at
         (DiagnosticBinder kind currentLocation name)
         doc
         help

-- | @MonadReader FixName@
type MonadFixName = MonadReader FixName

type instance Has m (FixName ': effs) = (MonadFixName m, Has m effs)

-- | @ask@ specialized to @FixName@
getFixName :: MonadFixName m => m FixName
getFixName = ask

-- | @asks@ specialized to @FixName@
getsFixName :: MonadFixName m => (FixName -> a) -> m a
getsFixName = asks

-- | @local@ specialized to @FixName@
localFixName :: MonadFixName m => (FixName -> FixName) -> m a -> m a
localFixName = local

-- | @using@ specialized to @FixName@
usingFixName :: MonadFixName m => FixName -> m a -> m a
usingFixName = using

-- | New error @Diagnostic@ using
--   the current result of @getFixName@ and @getRef@
reportErrorRefName :: Has m [Ref, FixName, Diag, With '[Pretty a]] =>
    Attr -> DiagnosticBinderKind -> a -> m ()
reportErrorRefName at kind doc = reportErrorRefNameH at kind doc []

diagAssertRefName :: Has m [Ref, FixName, Diag, With '[Pretty a]] =>
    Bool -> Attr -> DiagnosticBinderKind -> a -> m ()
diagAssertRefName b at kind doc = diagAssertRefNameH b at kind doc []

-- | New error @Diagnostic@ using
--   the current result of @getFixName@ and @getRef@, with help docs
reportErrorRefNameH :: Has m [Ref, FixName, Diag, With '[Pretty a]] =>
    Attr -> DiagnosticBinderKind -> a -> [Doc] -> m ()
reportErrorRefNameH at kind doc help = do
    name <- getFixName
    reportErrorRefH at kind (Just $ prettyShow name) doc help


diagAssertRefNameH :: Has m [Ref, FixName, Diag, With '[Pretty a]] =>
    Bool -> Attr -> DiagnosticBinderKind -> a -> [Doc] -> m ()
diagAssertRefNameH b at kind doc help = do
    name <- getFixName
    diagAssertRefH b at kind (Just $ prettyShow name) doc help


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

