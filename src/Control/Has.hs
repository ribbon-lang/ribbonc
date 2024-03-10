module Control.Has where

import Control.Applicative (Alternative)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO)

import Data.Kind (Type, Constraint)





-- | Context-builder type family, used to build a context
--   for a monad using convenient shorthand
type family Has (m :: Type -> Type) (effs :: [Type]) :: Constraint

type instance Has m '[] = ()


-- | Helper type for @Has@, used to add additional constraints
--   ie @Has m '[With '[Pretty e]]@ ~ @Pretty e@
type With :: [Constraint] -> Type
data With as

type instance Has m (With (a ': as) ': effs) = (a, Has m (With as ': effs))
type instance Has m (With '[] ': effs) = Has m effs


-- | Marker type for @Has@ ie @Has m '[FailWith e]@ ~ @MonadError e m@
data FailWith e

type instance Has m (FailWith e ': effs) = (MonadError e m, Has m effs)

-- | Marker type for @Has@ ie @Has m '[Fail]@ ~ @MonadFail m@
data Fail

type instance Has m (Fail ': effs) = (MonadFail m, Has m effs)


-- | Marker type for @Has@ ie @Has m '[St s]@ ~ @MonadState s m@
data St s

type instance Has m (St s ': effs) = (MonadState s m, Has m effs)


-- | Marker type for @Has@ ie @Has m '[Rd r]@ ~ @MonadReader r m@
data Rd r

type instance Has m (Rd r ': effs) = (MonadReader r m, Has m effs)


-- | Marker type for @Has@ ie @Has m '[Wr w]@ ~ @MonadWriter w m@
data Wr w

type instance Has m (Wr w ': effs) = (MonadWriter w m, Has m effs)


-- | Marker type for @Has@ ie @Has m '[Alt]@ ~ @(Alternative m, MonadPlus m)@
data Alt

type instance Has m (Alt ': effs) = (Alternative m, MonadPlus m, Has m effs)


-- | Marker type for @Has@ ie @Has m '[OS]@ ~ @MonadIO m@
data OS

type instance Has m (OS ': effs) = (MonadIO m, Has m effs)
