module Language.Ribbon.Parsing.Parse where


import Data.Functor

import Data.Nil
import Data.Attr
import Data.Diagnostic
import Data.SyntaxError

import Control.Has
import Control.Monad.Error.Dynamic

import Text.Pretty

import Language.Ribbon.Util
import Language.Ribbon.Syntax.Raw
import Language.Ribbon.Syntax.Ref
import Language.Ribbon.Syntax.Module qualified as M
import Language.Ribbon.Parsing.Monad
import Language.Ribbon.Parsing.Lexer qualified as L
import Language.Ribbon.Parsing.Parser
import Language.Ribbon.Lexical
import Language.Ribbon.Analysis.Context
import Language.Ribbon.Analysis.Diagnostics




parseModuleHead :: Has m [OS, Err Doc] =>
    FilePath -> m (ATag RawModuleHeader, [ATag TokenSeq])
parseModuleHead fp = do
    ts <- L.lexFile fp
    res <- runErrorT $ mapError @SyntaxError pPrint $
        runReaderT (evalParserT moduleHead ts) fp
    liftEither res

parseSourceFile ::
    ModuleId -> ItemId -> FilePath ->
        IO ((M.ParserDefs, M.UnresolvedImportMap), [Diagnostic])
parseSourceFile mi ii fp =
    do runWriterT $
        runErrorT @SyntaxError $
        runReaderT' mi $
        sourceFile ii fp
    <&> \case
        (Right x, ds) -> (x, ds)
        (Left e, ds) ->
            ( Nil
            , diagnosticFromSyntaxError
                DiagnosticBinder
                    { kind = BadDefinition
                    , ref = Ref mi ii
                    , name = Just fp
                    }
                e
            : ds
            )

parseSourceFileBody ::
    ModuleId -> ItemId -> FilePath -> [ATag TokenSeq] ->
        IO ((M.ParserDefs, M.UnresolvedImportMap), [Diagnostic])
parseSourceFileBody mi ii fp lns =
    do runWriterT $
        runErrorT @SyntaxError $
        runReaderT' fp $
        runReaderT' mi $
        sourceFileBody ii lns
    <&> \case
        (Right x, ds) -> (x, ds)
        (Left e, ds) ->
            ( Nil
            , diagnosticFromSyntaxError
                DiagnosticBinder
                    { kind = BadDefinition
                    , ref = Ref mi ii
                    , name = Just fp
                    }
                e
            : ds
            )

