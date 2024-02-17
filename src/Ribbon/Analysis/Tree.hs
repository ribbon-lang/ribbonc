module Ribbon.Analysis.Tree where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Ribbon.Util
import Ribbon.Source
import Ribbon.Display
import Ribbon.Syntax.Ast
import Data.Functor
import qualified Data.List as List
import Data.Bifunctor
import qualified Data.Char as Char
import Control.Monad
import Ribbon.Syntax.Text
import Ribbon.Syntax.Token
import qualified Data.Maybe as Maybe
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import qualified Data.Set as Set
import Data.Sequence (Seq)
import Ribbon.Analysis.Context
import Control.Monad.Reader





buildTree :: AnalysisContext -> ProtoModule -> Either Doc MidModule
buildTree ctx proto = first pPrint $ flip runReaderT ctx do
    let protoHead = untag proto.head
        newHead =
            ModuleTreeHead
            { name = protoHead.name
            , version = protoHead.version
            , meta = protoHead.meta
            }

    imports <- processImports protoHead.dependencies

    rootNamespace <- processNamespace proto.root

    pure $ ModuleTree
        { head = newHead
        , imports = imports
        , rootNamespace = rootNamespace
        }



bindDef ::
    (Ord a, MonadAnalysis m, MonadState (Namespace a u) m) =>
        Name -> ATag a -> m ()
bindDef n d = gets (Map.lookup n . (.localDefs)) >>= \case
    Just existing -> do
        when (Set.member d existing) do
            throwError $ AnalysisError $ Tag (tagOf d) $
                hang ("duplicate definition of" <+> pPrint n) do
                    "existing definition here:" <+> pPrint do
                        tagOf (Maybe.fromJust $ find (== d) existing)
        modify \ns -> ns{localDefs =
            Map.insertWith (<>) n (Set.singleton d) ns.localDefs}
    _ -> modify \ns -> ns{localDefs =
        Map.insert n (Set.singleton d) ns.localDefs}

bindEffect ::
    (MonadAnalysis m, MonadState (EffectCaseTable t) m) =>
        Name -> ATag (EffectCase t) -> m ()
bindEffect n e = gets (Map.lookup n) >>= \case
    Just existing -> do
        when (Set.member e existing) do
            throwError $ AnalysisError $ Tag (tagOf e) $
                hang ("duplicate effect case" <+> pPrint n) do
                    "existing case here:" <+> pPrint do
                        tagOf (Maybe.fromJust $ find (== e) existing)
        modify $ Map.insertWith (<>) n (Set.singleton e)
    _ -> modify $ Map.insert n (Set.singleton e)


processImports :: MonadAnalysis m =>
    [ATag ProtoModuleDependency] ->
        m (Map Name (ATag Module))
processImports deps = flip execStateT mempty do
    resolvedDependencies <-
        zip ((.alias) . untag <$> deps) <$> traverse
            (traverse (lookupModule . (.nameVer)))
            deps

    forM_ resolvedDependencies \(alias, tree) -> do
        let actualName = untag (untag tree).head.name

        when (Maybe.isNothing alias && not (isIdentifier actualName)) do
            throwError $ AnalysisError $ Tag (tagOf tree) $
                "imported module name"
                <+> pPrint actualName
                <+> "is not a valid identifier, and must be aliased"

        let dep = case alias of
                Just n -> (untag n, reTagFrom n tree)
                _ -> (Name actualName, tree)

        gets (Map.lookup (fst dep)) >>= \case
            Just existing -> throwError $ AnalysisError $ Tag (tagOf existing) $
                "an import module has already been bound to the name"
                <+> pPrint (fst dep)
            _ -> modify $ uncurry Map.insert dep


processNamespace :: MonadAnalysis m =>
    [ATag ProtoDef] -> m MidNamespace
processNamespace ns = flip execStateT Nil do
    forM_ ns $ compose untag \case
        ProtoType vis (spec :@: a) body ->
            bindDef spec.name $ Tag a $
                DefType
                    vis
                    spec.fixity
                    (Maybe.fromMaybe
                        (defaultPrec spec.fixity)
                        spec.prec)
                    body

        ProtoEffect vis (spec :@: a) body ->
            processEffect body >>=
                bindDef spec.name . Tag a . DefEffect
                    vis
                    spec.fixity
                    (Maybe.fromMaybe
                        (defaultPrec spec.fixity)
                        spec.prec)

        ProtoValue vis (spec :@: a) ty ex ->
            bindDef spec.name $ Tag a $ DefValue
                vis
                spec.fixity
                (Maybe.fromMaybe
                    (defaultPrec spec.fixity)
                    spec.prec)
                (ty, ex)

        ProtoNamespace vis (name :@: a) subDefs -> do
            processNamespace subDefs >>=
                bindDef name . Tag a . DefNamespace vis

        ProtoUse vis use -> do

            todo where


processEffect :: MonadAnalysis m =>
    [ATag ProtoEffectCase] -> m (EffectCaseTable (Seq (ATag Token)))
processEffect pe = flip execStateT mempty do
    forM_ pe \(ProtoEffectCase (sp :@: ea) bd :@: _) ->
        bindEffect sp.name $ Tag ea $ EffectCase
            sp.fixity
            (Maybe.fromMaybe
                (defaultPrec sp.fixity)
                sp.prec)
            bd

processUse ::
    (MonadState MidNamespace m, MonadAnalysis m) =>
        Use -> m ()
processUse (Use mp mt ma) = todo


defaultPrec :: DefFixity -> Prec
defaultPrec = \case
    DefInfixL -> 90
    DefInfixR -> 90
    DefInfix -> 90
    DefPrefix -> 70
    DefPostfix -> 0
    DefAtomic -> 0
