module Language.Ribbon.Analysis.Context where


import Control.Monad.Context.Class
import Control.Has

import Language.Ribbon.Syntax.Ref



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
