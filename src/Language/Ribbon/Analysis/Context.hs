module Language.Ribbon.Analysis.Context
    ( module X

    , MonadModCtx
    , getModCtx
    , getsModCtx
    , localModCtx
    , usingModCtx
    , lookupModule

    , MonadFile
    , getFilePath
    , getsFilePath
    , localFilePath
    , usingFilePath

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

    , MonadPath
    , getPath
    , getsPath
    , localPath
    , usingPath
    , usingJoinedPath
    , joinedPath

    , Location(..)
    , MonadLocation
    , getLocation
    ) where

import Data.Attr
import Data.Diagnostic

import Control.Has
import Control.Monad.Error.Dynamic
import Control.Monad.Reader.Dynamic as X

import Text.Pretty

import Language.Ribbon.Analysis.Diagnostics
import Language.Ribbon.Lexical.Name
import Language.Ribbon.Syntax.Ref
import Language.Ribbon.Syntax.Module qualified as M
import Language.Ribbon.Lexical.Version
import Language.Ribbon.Lexical.Path



type instance Has m (M.ModuleContext ': effs) = (MonadModCtx m, Has m effs)

-- | @MonadReader ModuleContext@
type MonadModCtx = MonadReader M.ModuleContext

-- | @ask@ specialized to @ModuleContext@
getModCtx :: MonadModCtx m => m M.ModuleContext
getModCtx = ask

-- | @asks@ specialized to @ModuleContext@
getsModCtx :: MonadModCtx m => (M.ModuleContext -> a) -> m a
getsModCtx = asks

-- | @local@ specialized to @ModuleContext@
localModCtx :: MonadModCtx m =>
    (M.ModuleContext -> M.ModuleContext) -> m a -> m a
localModCtx = local

-- | @using@ specialized to @ModuleContext@
usingModCtx :: MonadModCtx m => M.ModuleContext -> m a -> m a
usingModCtx = using

-- | Lookup a @ModuleId@ in the @ModuleContext@ by its name and version
lookupModule :: MonadModCtx m =>
    ATag (String, Version) -> m (Either (Doc, [Doc]) ModuleId)
lookupModule = getsModCtx . M.lookupModule




type instance Has m (FilePath ': effs) = (MonadFile m, Has m effs)


-- | @MonadReader FilePath m@
type MonadFile = MonadReader FilePath

-- | @ask@ specialized to @FilePath@
getFilePath :: MonadFile m => m FilePath
getFilePath = ask

-- | @asks@ specialized to @FilePath@
getsFilePath :: MonadFile m => (FilePath -> a) -> m a
getsFilePath = asks

-- | @local@ specialized to @FilePath@
localFilePath :: MonadFile m => (FilePath -> FilePath) -> m a -> m a
localFilePath = local

-- | @using@ specialized to @FilePath@
usingFilePath :: MonadFile m => FilePath -> m a -> m a
usingFilePath = using



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



-- | @MonadReader Path@
type MonadPath = MonadReader Path

type instance Has m (Path ': effs) = (MonadPath m, Has m effs)

-- | @ask@ specialized to @Path@
getPath :: MonadPath m => m Path
getPath = ask

-- | @asks@ specialized to @Path@
getsPath :: MonadPath m => (Path -> a) -> m a
getsPath = asks

-- | @local@ specialized to @Path@
localPath :: MonadPath m => (Path -> Path) -> m a -> m a
localPath = local

-- | @using@ specialized to @Path@
usingPath :: MonadPath m => Path -> m a -> m a
usingPath = using

-- | Extend the @Path@ with a new right segment, then run the given action;
--   fails if the paths are not joinable, yielding an error doc instead
usingJoinedPath :: Has m [Path, Err Doc] => Path -> m a -> m a
usingJoinedPath p m = do
    pb <- getPath
    case joinPath pb p of
        Just p' -> usingPath p' m
        _ -> throwError $
            hang "invalid path combination" $
                spaceWith "<>" (pPrint pb) (pPrint p)

-- | Extend the @Path@ with a new right segment, and return it;
--   fails if the paths are not joinable, yielding an error doc instead
joinedPath :: Has m [Path, Err Doc] => Path -> m Path
joinedPath p = usingJoinedPath p getPath


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

