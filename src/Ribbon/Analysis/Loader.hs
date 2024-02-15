module Ribbon.Analysis.Loader where

import System.Directory
import System.FilePath hiding  ((</>))

import Data.List qualified as List
import Data.Map.Lazy qualified as Map
import Data.Set qualified as Set

import Data.Function
import Data.Functor
import Data.Bifunctor

import Control.Monad
import Control.Monad.Except
import Control.Concurrent.ParallelIO.Global

import Ribbon.Util
import Ribbon.Source
import Ribbon.Display
import Ribbon.Syntax.Text
import Ribbon.Syntax.Ast
import Ribbon.Syntax.Parser



-- | Errors produced by the module loader
data LoaderError
    -- | Constructor a LoaderError from a Doc with an Attr
    = LoaderErrorIn (ATag (Doc ()))
    | LoaderErrorAt FilePath (Doc ())
    | LoaderErrorPreFormatted (Doc ())
    deriving Show

instance Pretty () LoaderError where
    pPrintPrec l _ = \case
        LoaderErrorIn (msg :@: a) ->
            hang (text "loader error at" <+> pPrintPrec l 0 a)
                msg

        LoaderErrorAt path msg ->
            hang (text "loader error at" <+> text path)
                msg

        LoaderErrorPreFormatted msg -> msg



-- | Load a module head file, and then traverse the source directories it lists,
--   in order to construct a @ProtoModule@
loadProtoModule :: FilePath -> IO (Either (Doc ()) ProtoModule)
loadProtoModule modPath' = first pPrint <$> runExceptT do
    -- we allow specifying the module as a directory
    -- with "module.bb" at its root, or the path to the head file itself
    modPath <- if ".bb" `isExtensionOf` modPath'
        then do
            exists <- liftIO $ doesFileExist modPath'
            unless exists do
                throwError $ LoaderErrorAt modPath' $
                    text "module head file does not exist"
            pure modPath'
        else do
            let modHeadPath = modPath' </> "module.bb"
            exists <- liftIO $ doesFileExist modHeadPath
            unless exists do
                throwError $ LoaderErrorAt modPath' $
                    text "module head file does not exist"
            pure modHeadPath

    -- All paths are relative to here
    let moduleBasePath = takeDirectory modPath

    -- load and parse the head file
    modFile <- liftIO (loadFile modPath (takeFileName modPath))
    (modHead@(T' modHeadContents), rootDefs) <- liftEither
        $ first LoaderErrorPreFormatted
        $ parseModuleFileProtos modFile

    -- if no sources were provided,
    -- we use the module directory by passing the empty string here
    let (sources :@: sourcesAttr) =
            if null (mhSources modHeadContents)
                then ["" :@: tagOf modHead] :@: tagOf modHead
                else mhSources modHeadContents :@:
                    foldr1 (<>) (tagOf <$> mhSources modHeadContents)
    -- split sources into individual files and (should-be-)directories
        (uncheckedBaseSourcePaths, uncheckedBaseDirs) =
            List.partition (isExtensionOf ".bb" . untag) sources

    -- check files that are listed individually
    baseSourcePaths <- forM uncheckedBaseSourcePaths \sourcePath -> do
        let path = moduleBasePath </> untag sourcePath

        fileExists <- liftIO do
            doesFileExist path

        unless fileExists do
            throwError $ LoaderErrorIn $ Tag (tagOf sourcePath) $
                text "source file"
                    <+> brackets (text path)
                    <+> text "does not exist"

        pure (untag sourcePath)

    -- check base directories
    baseDirs <- forM uncheckedBaseDirs \dir -> do
        let path = moduleBasePath </> untag dir
        dirExists <- liftIO do
            doesDirectoryExist path

        unless dirExists do
            throwError $ LoaderErrorIn $ Tag (tagOf dir) $
                text "source path"
                    <+> brackets (text path)
                    <+> text "is not a directory and does not end in `.bb`"

        pure (untag dir)

    -- traverse the given base directories and get their trees
    -- attach their base dir so we can sort them with the map later
    subSourcePaths <- zip baseDirs <$>
        let exploreDir path = do
                let fullPath = moduleBasePath </> path
                subPaths <- fmap (path </>) <$> listDirectory fullPath
                let sourcePaths = filter (isExtensionOf ".bb") subPaths
                subDirectories <-
                    filterM (doesDirectoryExist . (moduleBasePath </>)) subPaths
                concat . (sourcePaths :) <$>
                    traverse exploreDir subDirectories
        in traverse (liftIO . exploreDir) baseDirs

    -- split base source paths into base and sub source paths
    let as = second (:[]) . splitFileName <$> baseSourcePaths

    -- filter out any paths that were given as a base path
    -- filter out the module file itself
    -- remove any bases that are empty after this
    -- remove the base from the relative paths
        bs = subSourcePaths
            <&> second do filter (`notElem` ("/" <> takeFileName modPath) : baseSourcePaths)
            & filter (not . null . snd)
            <&> \(base, paths) -> (base, ) do
                paths <&> List.drop (List.length base)

    -- construct a map of base -> [sub path]
        sourceMap =
            foldr (uncurry (Map.insertWith (<>)) . second Set.fromList)
                mempty (as <> bs)

    -- check for any sub paths that show up in multiple bases
        overlaps = Map.filter ((> 1) . length) $
            Map.foldrWithKey insert mempty sourceMap where
                insert base paths acc = foldr (merge base) acc paths
                merge base path = Map.insertWith (<>) path [base]

    unless (Map.null overlaps) do
        throwError $ LoaderErrorIn $ Tag sourcesAttr $
            hang (text "source paths collide:") $ vcat' do
                Map.toList overlaps <&> \(ovl, bases) ->
                    hang (text "the file" <+> brackets (text ovl)
                            <+> text "can be found in the following bases:") do
                        vcat' $ brackets . text <$> bases

    -- build up a list of IO actions to load and parse the source files
    let fileActions =
            foldr mapFold mempty (Map.toList sourceMap) where
            mapFold (b, ps) acc = foldr (setFold b) acc ps
            setFold b path acc =
                let basedPath = b </> path
                    fullPath = moduleBasePath </> basedPath
                in do
                    f <- loadFile fullPath basedPath
                    let !xf = parseSourceFileProtos f
                    pure (path, xf)
                : acc

    -- run all the IO actions at once and wait on them all to finish
    parseResults <- liftIO $ parallelInterleaved fileActions

    -- fold the results into the final source map and make a list of any errors
    let (errors, files) = foldWith mempty parseResults \(path, x) (le, mf) ->
            case x of
                Left e -> (e : le, mf)
                Right f -> (le, Map.insert path f mf)

    -- finish up by throwing any errors or returning the module proto
    unless (null errors) do
        throwError $ LoaderErrorPreFormatted $ vcat' errors

    pure (ProtoModule modHead rootDefs files)
