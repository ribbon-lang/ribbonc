module Control.Monad.Diagnostics
    ( module X
    , DiagnosticsT, runDiagnosticsT
    , liftDiagnostics
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad.Trans.Dynamic
import Control.Monad.Writer.Dynamic
import Control.Monad.File.Class
import Control.Monad.Parser.Class
import Control.Monad.Reader.Dynamic.Class
import Control.Monad.State.Dynamic.Class
import Control.Monad.Error.Dynamic.Class

import Control.Monad.Diagnostics.Class as X

import Data.Diagnostic



-- | A monad transformer that can report @Diagnostic@s
newtype DiagnosticsT m a
    = DiagnosticsT
    ( WriterT [Diagnostic] m a )
    deriving
        ( Functor, Applicative, Monad
        , Alternative, MonadPlus, MonadFail
        , MonadTrans, MonadFile
        , MonadIO
        )

deriving instance MonadState s m => MonadState s (DiagnosticsT m)
deriving instance MonadReader r m => MonadReader r (DiagnosticsT m)
deriving instance MonadError e m => MonadError e (DiagnosticsT m)
deriving instance MonadParser i m => MonadParser i (DiagnosticsT m)

instance Monad m => MonadDiagnostics (DiagnosticsT m) where
    reportFull d = DiagnosticsT (tell [d])


-- | Run a @DiagnosticsT@ computation,
--   returning the result and a list of @Diagnostic@s
runDiagnosticsT :: DiagnosticsT m a -> m (a, [Diagnostic])
runDiagnosticsT (DiagnosticsT a) = runWriterT a

liftDiagnostics :: (MonadError [Diagnostic] m) =>
    DiagnosticsT m a -> m a
liftDiagnostics m = do
    (a, ds) <- runDiagnosticsT m
    a <$ unless (null ds) (throwError ds)
