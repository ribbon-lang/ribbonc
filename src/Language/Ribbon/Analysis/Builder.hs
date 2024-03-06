module Language.Ribbon.Analysis.Builder where

import Data.Tag
import Data.Attr

import Control.Monad.State.Strict

import Control.Monad.Diagnostics

import Text.Pretty

import Language.Ribbon.Util

import Language.Ribbon.Lexical
import Language.Ribbon.Syntax


-- | Transformer for building unresolved imports
type UnresolvedImportBuilder = StateT UnresolvedImports

-- | Insert an unresolved import by @UnresolvedName@ and @Path@;
--   Creates an error @Diagnostic@ if the name is already bound to an import
insertAlias :: MonadDiagnostics m =>
    ATag Path -> UnresolvedName -> UnresolvedImportBuilder m ()
insertAlias p n =
    do state \ui ->
        case insertUnresolvedAlias p n ui of
            Left e -> (Just e, ui)
            Right ui' -> (Nothing, ui')
    >>= whenJust \(err :@: at) -> reportErrorH
        do hang ("at" <+> pPrint n.name.tag) err
        ["it was first defined here:" <+> pPrint at]

-- | Insert an unresolved blob import by @Path@;
--   This cannot fail, as blobs are allowed to be duplicated
insertBlob :: Monad m => ATag Path -> UnresolvedImportBuilder m ()
insertBlob p = modify (insertUnresolvedBlob p)
