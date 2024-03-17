module Language.Ribbon.Analysis.Resolver where

import Data.Tag

import Control.Has
import Control.Monad.Error.Dynamic

import Text.Pretty

import Language.Ribbon.Util
import Language.Ribbon.Lexical
import Language.Ribbon.Syntax
import Control.Monad.Reader.Dynamic.Class
import Language.Ribbon.Analysis.Builder
import Language.Ribbon.Analysis.Context
import Language.Ribbon.Analysis.Diagnostics
import qualified Data.Map.Strict as Map
import Data.Nil
import Data.Word (Word32)
import Control.Monad
import qualified Data.Sequence as Seq
import Data.Attr
import Data.Diagnostic
import Data.Sequence (Seq)
import Data.Functor
import Control.Monad.Trans.Dynamic
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Fold
import Data.Map.Strict (Map)












resolveAlias :: Has m
    [ ModuleContext, ResolverModule
    , PathStack, Err Diagnostic
    ] => UnresolvedAlias -> m Resolution
resolveAlias = do

    todo


resolveBlob :: forall m. Has m
    [ ModuleContext, ResolverModule
    , PathStack, Err Diagnostic
    ] => UnresolvedBlob -> m ResolvedBlob
resolveBlob ub = do
    (res, px) <- resolvePath ub.path.value
    (`ResolvedBlob` ub.hiding) <$> extractNamespaceResolution ub.path.tag res px
    where
    extractNamespaceResolution at res px = case res of
        NoResolution -> throwError $ diagnosticFromError at
            DiagnosticBinder
                { kind = Unresolved
                , ref = pathStackCurrentRef px
                , name = prettyShow . (.value) <$>
                    getPathName ub.path.value
                }
            (hang "unresolved blob import" $
                "the path" <+> backticked px
                    <+> "resolves to no items")
        BranchResolution rs ->
            extractNamespaceResolutions rs >>= \case
                [g] -> extractNamespaceResolution at g px
                [] -> throwError $ diagnosticFromErrorH at
                    DiagnosticBinder
                        { kind = Unresolved
                        , ref = pathStackCurrentRef px
                        , name = prettyShow . (.value) <$>
                            getPathName ub.path.value
                        }
                    (hang "unresolved blob import" $
                        "the path" <+> backticked px
                            <+> "resolves to no groups")
                    (hang "resolves to" . ($+$ "(not a group)") . pPrint <$> rs)
                gs -> throwError $ diagnosticFromErrorH at
                    DiagnosticBinder
                        { kind = ConflictingDefinition
                        , ref = pathStackCurrentRef px
                        , name = Just $ resolutionName $ head gs
                        }
                    (hang "ambiguous blob import" $
                        "the path" <+> backticked px
                            <+> "resolves to multiple groups")
                    (hang "resolves to" . ($+$ "(not a group)") . pPrint <$> gs)
        FileResolution{} -> pure px
        ModuleResolution{} -> pure px
        BindingResolution b ->
            isGroupItem b >>= selecting
                (pure $ PsCons px b.value.ref b.value.name at)
                do throwError $ diagnosticFromErrorH at
                    DiagnosticBinder
                        { kind = Unresolved
                        , ref = b.value.ref
                        , name = Just $
                            prettyShow b.value.name.value.value.value
                        }
                    (hang "unresolved blob import" $
                        "the path" <+> backticked px
                            <+> "resolves to a single item,"
                            <+> "which is not a group")
                    [hang "resolves to" $ pPrint b $+$ "(not a group)"]

    extractNamespaceResolutions = foldWithM' [] \r rs -> case r of
        NoResolution -> pure rs
        BranchResolution rs' -> (<> rs) <$> extractNamespaceResolutions rs'
        FileResolution{} -> pure (r : rs)
        ModuleResolution{} -> pure (r : rs)
        BindingResolution g ->
            selecting (r : rs) rs <$> isGroupItem g

    isGroupItem g = do
        cmi <- resolverModuleId
        if g.value.ref.moduleId == cmi
            then isGroupItem' g.value.ref.itemId <$> getResolverModule
            else isGroupItem' g.value.ref.itemId <$>
                getContextModule g.value.ref.moduleId

    isGroupItem' ii
          = Maybe.isJust . getGroup ii
        ||| Maybe.isJust . getImport ii

resolvePath :: Has m
    [ ModuleContext, ResolverModule
    , PathStack, Err Diagnostic
    ] =>
    Path -> m (Resolution, PathStack)
resolvePath (Path pb pns) = case pb of
    Nothing -> resolvePathNamesHere
    Just pb' -> case pb'.value of
        PbThis -> resolvePathNamesHere

        PbFile fp ->
            lookupFileId fp >>= \case
                Just ii -> resolvePathNames (FileResolution fp ii) pns

                _ -> do
                    mi <- resolverModuleId
                    throwError $ diagnosticFromError pb'.tag
                        (DiagnosticBinder Unresolved (Ref mi 0) (Just fp))
                        $ "source file" <+> backticked (text fp) <+> "not found"

        PbModule n ->
            lookupDependencyId n >>= \case
                Just mi -> resolvePathNames (ModuleResolution n mi) pns

                _ -> do
                    mi <- resolverModuleId
                    throwError $ diagnosticFromError pb'.tag
                        (DiagnosticBinder Unresolved (Ref mi 0) (Just n.value))
                        $ "import module" <+> backticked n <+> "not found"

        PbRoot -> do
            mi <- resolverModuleId
            resolvePathNames (ModuleResolution (SimpleName "~") mi) pns

        PbUp lvl -> traversePathStack lvl
            >>= compose (fmap resolutionFromPathStack) \case
                Just res -> resolvePathNames res pns

                _ -> do
                    ref <- getCurrentRef
                    px <- getPathStack
                    throwError $ diagnosticFromErrorH pb'.tag
                        (DiagnosticBinder Unresolved ref Nothing)
                        (hang "path stack underflow:" $
                            "cannot resolve relative base" <+> backticked pb'
                                <+> "from" <+> backticked px)
                        [ "the path stack has depth"
                            <+> pPrint (pathStackLength px)
                        , "the relative base has depth" <+> pPrint lvl
                        ]
    where
    resolvePathNamesHere =
        getsPathStack resolutionFromPathStack >>= (`resolvePathNames` pns)

    resolvePathNames res = \case
        Nil -> (res, ) <$> getPathStack
        pn Seq.:<| pns' ->
            resolvePathName res pn >>= \case
                NoResolution -> (NoResolution, ) <$> getPathStack
                r -> resolvePathNames r pns'


resolvePathName :: Has m
    [ ModuleContext, ResolverModule
    , PathStack, Err Diagnostic
    ] =>
    Resolution -> ATag PathName -> m Resolution
resolvePathName res pn = case res of
    NoResolution -> pure NoResolution
    BranchResolution rs ->
        traverse (`resolvePathName` pn) rs <&>
            compose (filter isResolution) \case
                [] -> NoResolution
                [r] -> r
                rs' -> (BranchResolution rs')
    FileResolution fp ii -> mapGroup <$> do
        mi <- resolverModuleId
        usingPathStack
            (PsBase PsFileBase (Ref mi ii) fp pn.tag)
            (resolveName pn)
    ModuleResolution n mi -> mapGroup <$>
        usingPathStack
            (PsBase PsModuleBase (Ref mi 0) n.value pn.tag)
            (resolveName pn)
    BindingResolution g -> mapGroup <$>
        localPathStack
            (PsCons' g.value.ref g.value.name pn.tag)
            (resolveName pn)
    where
    mapGroup = \case
        [] -> NoResolution
        [r] -> BindingResolution r
        rs -> BranchResolution (BindingResolution <$> rs)


resolveName:: forall m. Has m
    [ ModuleContext, ResolverModule
    , PathStack
    , Err Diagnostic
    ] =>
    ATag PathName -> m Group
resolveName pn = do
    ref <- getCurrentRef
    pmi <- resolverModuleId
    if ref.moduleId == pmi
        then getResolverModule >>= searchNamespace unresolvedSearcher
        else getContextModule ref.moduleId >>= searchNamespace resolvedSearcher
    where
    searchNamespace ::
        (Module h (DefSet t v i) -> m Group) ->
            Module h (DefSet t v i) -> m Group
    searchNamespace searchImports m = do
        ii <- getCurrentItemId
        let lbs = Maybe.maybeToList (getGroup ii m)
                >>= searchRef pn.value
        ibs <- searchImports m
        pure (lbs `mergeGroups` ibs)

    unresolvedSearcher :: ResolverModule -> m Group
    unresolvedSearcher m = do
        ii <- getCurrentItemId
        case getImport ii m of
            Just wis -> do
                let candidates = filter todo wis.aliases

                todo
            _ -> pure []

    resolvedSearcher m = do
        ii <- getCurrentItemId
        case getImport ii m of
            Just ris -> searchResolvedImports ris
            _ -> pure []

    searchResolvedImports ris = do
        concat <$> forM ris \blob ->
            fmap (reVis blob.visibility) . filterGroupByVis <$>
                usingPathStack blob.value.pathStack (resolveName pn)
