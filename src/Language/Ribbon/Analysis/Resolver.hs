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
import qualified Data.Either as Either








resolveImports :: forall m. Has m
    [ ModuleContext, ResolverModule
    , PathStack, Err Diagnostic, Diag
    ] => ItemId -> m ResolvedImports
resolveImports ii = do
    getsResolverModule (getImport ii) >>= \case
        Just (imps@WipImports{..} :@: at) ->
            case aliases of
                Visible v ua : uas -> do
                    resolveAlias ua >>= bindAlias at v ua

                    modifyResolverModule $ setImport ii $
                        WipImports
                            { aliases = uas
                            , resolvedBlobs = resolvedBlobs
                            , unresolvedBlobs = unresolvedBlobs
                            } :@: at
                    resolveImports ii
                _ -> case unresolvedBlobs of
                    Visible v ub : ubs -> do
                        rb <- resolveBlob ub
                        modifyResolverModule $ setImport ii $
                            imps { unresolvedBlobs = ubs
                                 , resolvedBlobs = Visible v rb : resolvedBlobs
                                 } :@: at
                        resolveImports ii
                    _ -> pure resolvedBlobs
        _ -> pure Nil
    where
    bindAlias at v ua = \case
        NoResolution -> do
            ref <- getCurrentRef
            px <- getPathStack
            throwError $ diagnosticFromErrorH at
                DiagnosticBinder
                    { kind = Unresolved
                    , name = Just $ prettyShow ua
                    , ref
                    }
                (hang "unresolved alias import" $
                    "the alias" <+> backticked ua
                        <+> "resolves to no items")
                [hang "the path" $ backticked px]

        BranchResolution rs -> bindBranch at v ua rs

        FileResolution _ fi -> do
            cmi <- resolverModuleId
            checkNsImport ua.name
            insertResolvedAlias at v Namespace NonAssociative 0
                ua.name.value (Ref cmi fi)

        ModuleResolution _ mi -> do
            checkNsImport ua.name
            insertResolvedAlias at v Namespace NonAssociative 0
                ua.name.value (Ref mi 0)

        BindingResolution (Visible _ gb) -> do
            when (maybe False (overloadedCategory gb.name.category /=)
                    ua.name.category) $
                reportWarning ua.name.value.tag
                    ("the provided category does not match"
                        <+> "the category of the only resolved item"
                        <+> (paren'd ua.name.category <> ";")
                        <+> "the provided category will be ignored")

            when (overloadedCategory gb.name.category == ONamespace) $
                checkNonAssociative ua.name

            insertResolvedAlias at v gb.name.category
                (maybe
                    gb.name.value.associativity
                    fst
                    ua.name.fixitySpecifics)
                (maybe
                    gb.name.value.precedence
                    snd
                    ua.name.fixitySpecifics)
                ua.name.value gb.ref

    bindBranch at v ua rs = case ua.name.category of
        Just ONamespace ->
            doExtraction ONamespace rs at ua >>= bindAlias at v ua

        Just OInstance ->
            doExtraction OInstance rs at ua >>= bindAlias at v ua

        Just OType ->
            doExtraction OType rs at ua >>= bindAlias at v ua

        Just OValue ->
            doExtraction OValue rs at ua >>= bindAlias at v ua

        Nothing -> do
            doExtraction ONamespace rs at ua >>= bindAlias at v ua
            doExtraction OInstance rs at ua >>= bindAlias at v ua
            doExtraction OType rs at ua >>= bindAlias at v ua
            doExtraction OValue rs at ua >>= bindAlias at v ua

    doExtraction c rs at ua =
        extractResolutions (isOverloadedCategoryItem c) rs >>= \case
            [] -> pure NoResolution
            [g] -> pure g
            gs -> do
                ref <- getCurrentRef
                throwError $ diagnosticFromErrorH at
                    DiagnosticBinder
                        { kind = ConflictingDefinition
                        , name = Just $ prettyShow ua
                        , ref
                        }
                    (hang "ambiguous alias import" $
                        "the alias" <+> backticked ua
                            <+> "resolves to multiple items"
                            <+> "in the category" <+> pPrint c)
                    (hang "resolves to" . pPrint <$> gs)

    checkNonAssociative name =
        when (maybe False ((NonAssociative, 0) /=) name.fixitySpecifics) $
            reportWarning name.value.tag
                ("this item must be non-associative, 0 precedence;"
                    <+> "the provided associativity/precedence will be ignored")

    checkNsImport name = do
        checkNonAssociative name
        when (maybe False (ONamespace /=) name.category) $
            reportWarning name.value.tag
                ("this item must be imported as namespace;"
                    <+> "the provided category will be ignored")

    -- insertResolvedAlias ::
    --     Attr -> Visibility -> Category -> Associativity -> Precedence ->
    --         ATag FixName -> Ref -> m ()
    insertResolvedAlias at v category associativity precedence name ref = do
        Tag gat grp <- Maybe.fromMaybe (Nil :@: at) <$>
            getsResolverModule (getGroup ii)

        let res = groupInsertRef
                (Visible v $ GroupBinding
                    { name = Categorical category
                        QualifiedName
                            { value = name
                            , associativity
                            , precedence
                            }
                    , ref
                    })
                grp

        case res of
            Right grp' ->
                modifyResolverModule $
                    setGroup ii (grp' :@: gat)
            Left (err :@: eat) -> do
                cmi <- resolverModuleId
                throwError $ diagnosticFromErrorH name.tag
                    DiagnosticBinder
                        { kind = ConflictingDefinition
                        , name = Just $ prettyShow name.value
                        , ref = Ref cmi ii
                        }
                    err
                    ["it was first defined here:" <+> pPrint eat]


resolveAlias :: Has m
    [ ModuleContext, ResolverModule
    , PathStack, Err Diagnostic
    ] => UnresolvedAlias -> m Resolution
resolveAlias = fmap fst . resolvePath . (.path.value)


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
            extractResolutions isGroupItem rs >>= \case
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

extractResolutions :: Has m
    [ ModuleContext, ResolverModule
    , PathStack, Err Diagnostic
    ] => (Visible GroupBinding -> m Bool) -> [Resolution] -> m [Resolution]
extractResolutions p = foldWithM' [] \r rs -> case r of
    NoResolution -> pure rs
    BranchResolution rs' -> (<> rs) <$> extractResolutions p rs'
    FileResolution{} -> pure (r : rs)
    ModuleResolution{} -> pure (r : rs)
    BindingResolution g -> selecting (r : rs) rs <$> p g

isOverloadedCategoryItem :: Applicative m =>
    OverloadCategory -> Visible GroupBinding -> m Bool
isOverloadedCategoryItem overloadCategory g =
    pure (overloadedCategory g.value.name.category == overloadCategory)


isCategoryItem :: Applicative m => Category -> Visible GroupBinding -> m Bool
isCategoryItem category g = pure (g.value.name.category == category)

isGroupItem :: Has m
    [ ModuleContext, ResolverModule
    , PathStack, Err Diagnostic
    ] => Visible GroupBinding -> m Bool
isGroupItem g = do
    cmi <- resolverModuleId
    if g.value.ref.moduleId == cmi
        then isGroupItem' g.value.ref.itemId <$> getResolverModule
        else isGroupItem' g.value.ref.itemId <$>
            getContextModule g.value.ref.moduleId
    where
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
                >>= groupSearchRef pn.value . (.value)
        ibs <- searchImports m
        pure (lbs `mergeGroups` ibs)

    unresolvedSearcher :: ResolverModule -> m Group
    unresolvedSearcher m = do
        ii <- getCurrentItemId
        todo

    resolvedSearcher m = do
        ii <- getCurrentItemId
        case getImport ii m of
            Just ris -> searchResolvedImports ris
            _ -> pure []

    searchResolvedImports ris = todo
        -- concat <$> forM ris \blob ->
        --     fmap (reVis blob.visibility) . filterGroupByVis <$>
        --         usingPathStack blob.value.pathStack (resolveName pn)
