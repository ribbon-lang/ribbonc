module Language.Ribbon.Analysis.Loader where

import System.Directory
import System.FilePath

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.List qualified as List

import Data.Word (Word32)

import Data.Function
import Data.Functor
import Data.Bifunctor

import Data.Tag
import Data.Diagnostic
import Data.Nil

import Control.Has
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Error.Dynamic
import Control.Concurrent.ParallelIO.Global

import Text.Pretty

import Language.Ribbon.Util hiding ((</>))
import Language.Ribbon.Lexical
import Language.Ribbon.Syntax.Ref
import Language.Ribbon.Syntax.Module
    ( ModuleContext, ParserModule
    , AnalysisModuleHeader(..), Module(..)
    )
import Language.Ribbon.Analysis.Context
import Language.Ribbon.Analysis.Diagnostics

import Language.Ribbon.Syntax.Raw
import Language.Ribbon.Parsing.Parse qualified as P
import qualified Data.Maybe as Maybe

import Language.Ribbon.Parsing.Text
import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import qualified Data.Either as Either




-- | Load a module head file, in order to construct a @ParserModule@.
--   Traverse the source directories it lists,
--   build a list of files and parse them all,
--   parse the root namespace,
--   and lookup and bind dependencies
loadParserModule :: Has m
    [ Diag, Err Doc
    , OS
    ] => ModuleContext -> ModuleId -> FilePath -> m ParserModule
loadParserModule ctx modId modPath' = do
    -- we allow specifying the module as a directory
    -- with "module.bb" at its root, or the path to the head file itself
    modPath <- if ".bb" `isExtensionOf` modPath'
        then do
            exists <- liftIO $ doesFileExist modPath'
            unless exists do
                throwError $
                    "module header file" <+> backticks (text modPath')
                        <+> "does not exist"
            pure modPath'
        else do
            let modHeadPath = modPath' </> "module.bb"
            exists <- liftIO $ doesFileExist modHeadPath
            unless exists do
                throwError $
                    "module header file" <+> backticks (text modPath')
                        <+> "does not exist"
            pure modHeadPath

    -- All paths are relative to here
    let moduleBasePath = takeDirectory modPath

    -- load and parse the head file
    (rawHeader, rootLns) <- P.parseModuleHead modPath

    -- if no sources were provided,
    -- we use the module directory by passing the empty string here
    let (sources :@: sourcesAttr) =
            if null rawHeader.value.sources
                then ["" :@: rawHeader.tag] :@: rawHeader.tag
                else rawHeader.value.sources :@:
                    foldr1 (<>) (tagOf <$> rawHeader.value.sources)

    -- split sources into individual files and (should-be-)directories
        (uncheckedBaseSourcePaths, uncheckedBaseDirs) =
            List.partition (isExtensionOf ".bb" . untag) sources

    -- check files that are listed individually
    baseSourcePaths <-
        Maybe.catMaybes <$> forM uncheckedBaseSourcePaths \sourcePath -> do
            let path = moduleBasePath </> untag sourcePath

            fileExists <- liftIO (doesFileExist path)

            if fileExists
                then pure (Just sourcePath.value)
                else do
                    reportError sourcePath.tag
                        DiagnosticBinder
                            { kind = BadDefinition
                            , ref = Ref modId 0
                            , name = Just sourcePath.value
                            }
                        $ "source file"
                            <+> brackets (text path)
                            <+> "does not exist"
                    pure Nothing

    -- check base directories
    baseDirs <- Maybe.catMaybes <$> forM uncheckedBaseDirs \dirPath -> do
        let path = moduleBasePath </> untag dirPath
        dirExists <- liftIO do
            doesDirectoryExist path

        if dirExists
            then pure (Just dirPath.value)
            else do
                reportError dirPath.tag
                    DiagnosticBinder
                        { kind = BadDefinition
                        , ref = Ref modId 0
                        , name = Just dirPath.value
                        }
                    $ "source path"
                        <+> brackets (text path)
                        <+> "is not a directory and does not end in `.bb`"
                pure Nothing

    -- traverse the given base directories and get their trees
    -- attach their base dir so we can sort them with the map later
    subSourcePaths <- zip baseDirs <$>
        traverse (liftIO . exploreDir moduleBasePath) baseDirs

    -- split base source paths into base and sub source paths
    let as = second (:[]) . splitFileName <$> baseSourcePaths

    -- filter out any paths that were given as a base path
    -- filter out the module file itself
    -- remove any bases that are empty after this
    -- remove the base from the relative paths
        bs = subSourcePaths
            <&> second do
                filter (`notElem` ("/" <> takeFileName modPath)
                                    : baseSourcePaths)
            & filter (not . null . snd)
            <&> \(base, paths) -> (base, ) do
                paths <&> List.drop (List.length base)

    -- construct a map of base -> [sub path]
        sourceMap =
            foldr (uncurry (Map.insertWith (<>)) . second Set.fromList)
                Nil (as <> bs)

    -- check for any sub paths that show up in multiple bases
        overlaps = Map.filter ((> 1) . length) $
            Map.foldrWithKey insert Nil sourceMap where
                insert base paths acc = foldr (merge base) acc paths
                merge base path = Map.insertWith (<>) path [base]

    unless (Map.null overlaps) do
        forM_ (Map.toList overlaps) \(ovl, bases) ->
            reportErrorH sourcesAttr
                DiagnosticBinder
                    { kind = ConflictingDefinition
                    , ref = Ref modId 0
                    , name = Just ovl
                    }
                ("source path collision for file" <+> brackets (text ovl))
                $ bases <&> \base ->
                    "found in base" <+> brackets (text base)

    -- build up a list of IO actions to load and parse the source files
    ioDiags <- liftIO $ newIORef Nil
    ioDefMap <- liftIO $ newIORef Nil
    ioSourceMap <- liftIO $ newIORef Nil
    ioDependencyMap <- liftIO $ newIORef Nil

    let fileActions = snd $
            foldr mapFold (itemIdIncr, Nil) (Map.toList sourceMap)
        mapFold (b, ps) acc = foldr (setFold b) acc ps
        setFold b path (!itemId, acc) =
            let basedPath = b </> path
                fullPath = moduleBasePath </> basedPath
            in ((itemId + itemIdIncr, ) . (: acc)) do
                putStrLn $ render $
                    "processing file" <+> brackets (text fullPath)
                (!defs, !diags) <- P.parseSourceFile modId itemId fullPath
                modifyIORef' ioDiags (<> diags)
                modifyIORef' ioDefMap (<> defs)
                modifyIORef' ioSourceMap (Map.insert fullPath itemId)

    -- run all the IO actions at once and wait on them all to finish
    exceptions <- Either.lefts <$> liftIO do
        parallelInterleavedE
            $ do -- add the header defs to the list of actions
                putStrLn "processing module root"
                (!defs, !diags) <- P.parseSourceFileBody modId 0 modPath rootLns
                modifyIORef' ioDiags (<> diags)
                modifyIORef' ioDefMap (<> defs)
            : do -- add dependency resolution to the list of actions
                putStrLn "processing module dependencies"
                (depMap, diags) <- runReaderT' ctx $ runWriterT $
                    foldDependencies rawHeader.value.dependencies
                writeIORef ioDependencyMap depMap
                modifyIORef' ioDiags (<> diags)
            : fileActions

    -- report diagnostics and exceptions from the workers
    liftIO (readIORef ioDiags) >>= reportAll

    unless (null exceptions) $ throwError $
        vcat' $ exceptions <&> \e ->
            hang "ICE occurred while processing a source file" $
                shown e

    -- assemble the module
    fullSourceMap <- liftIO (readIORef ioSourceMap)
    fullDefs <- liftIO (readIORef ioDefMap)
    dependencyMap <- liftIO (readIORef ioDependencyMap)

    pure Module
        { header = AnalysisModuleHeader
            { moduleId = modId
            , files = fullSourceMap
            , dependencies = Map.toList dependencyMap
            }
        , meta = rawHeader.value.meta
        , defSet = fullDefs
        }
    where
    -- | we use a @Word64@ for @ItemId@,
    --   and files' sub-items just increment from the base they're given.
    --   for each file we increment by the maxBound of Word32, thus:
    --     maxBound Word32 = 4294967295
    --     maxBound Word64 / maxBound Word32 = 4294967297
    --     = 4294967295 unique items per file
    --     & 4294967297 unique files
    --     this is enough of a margin im not
    --     concerned with checking for overflow
    itemIdIncr = fromIntegral (maxBound :: Word32)

    -- | Traverse a directory and return a list
    --   of all the source files nested in it
    exploreDir moduleBasePath = go where
        go path = do
            let fullPath = moduleBasePath </> path

            subPaths <- fmap (path </>) <$> listDirectory fullPath

            let sourcePaths = filter (isExtensionOf ".bb") subPaths

            subDirectories <-
                filterM (doesDirectoryExist . (moduleBasePath </>)) subPaths

            concat . (sourcePaths :) <$>
                traverse go subDirectories

    -- | Fold over @RawDependencies@ and resolve them to @ModuleId@s
    foldDependencies = foldWithM' Nil \(nameVer, alias) depMap -> do
        let actualName = fst nameVer.value

        -- we allow arbitrary strings as module names,
        -- but not all of them are valid user-accessible names,
        -- so we need to check that there was an alias provided in that case
        diagAssertH (Maybe.isJust alias || isUserSymbol actualName)
            nameVer.tag
                DiagnosticBinder
                    { kind = BadDefinition
                    , ref = Ref modId 0
                    , name = Just actualName
                    }
                ("imported module name"
                    <+> pPrint actualName
                    <+> "is not a valid identifier and must be aliased")
                ["try something like `as MyModule`"
                    <+> "to alias the module name"]

        let depName = case alias of
                Just a -> a
                _ -> SimpleName actualName :@: nameVer.tag

        -- lookupModule provides some suggestions in the event of a failure
        -- so we need to format that into a diagnostic
        depId <- lookupModule nameVer >>= \case
            Left (e, h) ->
                Nil <$ reportErrorH nameVer.tag
                    DiagnosticBinder
                        { kind = BadDefinition
                        , ref = Ref modId 0
                        , name = Just actualName
                        }
                    e h
            Right depId -> pure depId

        -- make sure the name isn't already bound
        case List.find (== depName) (Map.keys depMap) of
            Just existing -> do
                reportErrorH nameVer.tag
                    DiagnosticBinder
                        { kind = ConflictingDefinition
                        , ref = Ref modId 0
                        , name = Just depName.value.value
                        }
                    ("an import module has already been bound to the name"
                        <+> pPrint actualName)
                    ["it was first bound here:" <+> pPrint existing.tag]
                pure depMap
            _ -> pure $ Map.insert depName depId depMap

