module Language.Ribbon.Analysis.Builder where

import Data.Bifunctor

import Data.Tag
import Data.Attr

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.Writer.Strict qualified as Strict
import Control.Monad.Writer.Lazy qualified as Lazy
import Control.Monad.Writer.Class
import Control.Monad.Reader
import Control.Monad.Except

import Control.Monad.File
import Control.Monad.Parser
import Control.Monad.Diagnostics

import Text.Pretty

import Language.Ribbon.Util

import Language.Ribbon.Lexical
import Language.Ribbon.Syntax


class Monad m => MonadNamespace m where
    -- | Read/Write/Modify the @NamespaceState@ of a @MonadNamespace@
    namespaceState :: (NamespaceState -> (a, NamespaceState)) -> m a

instance MonadNamespace m => MonadNamespace (StateT s m) where
    namespaceState = lift . namespaceState

instance MonadNamespace m => MonadNamespace (Lazy.StateT s m) where
    namespaceState = lift . namespaceState

instance (Monoid w, MonadNamespace m) => MonadNamespace (Strict.WriterT w m) where
    namespaceState = lift . namespaceState

instance (Monoid w, MonadNamespace m) => MonadNamespace (Lazy.WriterT w m) where
    namespaceState = lift . namespaceState

instance MonadNamespace m => MonadNamespace (ReaderT r m) where
    namespaceState = lift . namespaceState

instance MonadNamespace m => MonadNamespace (ExceptT e m) where
    namespaceState = lift . namespaceState

instance MonadNamespace m => MonadNamespace (ParserT i m) where
    namespaceState = lift . namespaceState

instance MonadNamespace m => MonadNamespace (DiagnosticsT m) where
    namespaceState = lift . namespaceState

instance MonadNamespace m => MonadNamespace (FileT m) where
    namespaceState = lift . namespaceState




-- | Transformer for building namespaces, containing a Group, and an UnresolvedImports
newtype NamespaceBuilderT m a
    = NamespaceBuilderT
    ( StateT NamespaceState m a )
    deriving
        ( Functor, Applicative, Monad, Alternative, MonadPlus
        , MonadTrans, MonadFail, MonadIO
        , MonadDiagnostics, MonadFile
        )

deriving instance MonadWriter w m => MonadWriter w (NamespaceBuilderT m)
deriving instance MonadReader s m => MonadReader s (NamespaceBuilderT m)
deriving instance  MonadError e m =>  MonadError e (NamespaceBuilderT m)
deriving instance MonadParser i m => MonadParser i (NamespaceBuilderT m)

type NamespaceState = (Group, UnresolvedImports)

instance MonadState s m => MonadState s (NamespaceBuilderT m) where
    get = lift get
    put = lift . put

instance Monad m => MonadNamespace (NamespaceBuilderT m) where
    namespaceState f = NamespaceBuilderT $ state f

-- | Modify the @NamespaceState@ of a @MonadNamespace@
namespaceModify :: MonadNamespace m => (NamespaceState -> NamespaceState) -> m ()
namespaceModify f = namespaceState \s -> ((), f s)

-- | Read the @NamespaceState@ of a @MonadNamespace@
namespaceGet :: MonadNamespace m => m NamespaceState
namespaceGet = namespaceState \s -> (s, s)

-- | Write the @NamespaceState@ of a @MonadNamespace@
namespacePut :: MonadNamespace m => NamespaceState -> m ()
namespacePut s = namespaceState $ const ((), s)

-- | Insert an unresolved import by @UnresolvedName@ and @Path@;
--   Creates an error @Diagnostic@ if the name is already bound to an import
insertAlias :: (MonadDiagnostics m, MonadNamespace m) =>
    ATag Path -> Visible UnresolvedName -> m ()
insertAlias p n =
    do namespaceState \(g, i) ->
        case insertUnresolvedAlias p n i of
            Left e -> (Just e, (g, i))
            Right i' -> (Nothing, (g, i'))
    >>= whenJust \(err :@: at) -> reportErrorH
        do hang ("at" <+> pPrint n.value.name.tag) err
        ["it was first defined here:" <+> pPrint at]

-- | Insert an unresolved blob import by @Path@;
--   This cannot fail, as blobs are allowed to be duplicated
insertBlob :: MonadNamespace m => ATag Path -> [ATag PathName] -> m ()
insertBlob p h = namespaceModify (second $ insertUnresolvedBlob p h)
