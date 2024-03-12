module Language.Ribbon.Syntax.Raw where

import Data.Functor ((<&>))

import Data.Map.Strict qualified as Map

import Data.Tag
import Data.Attr

import Text.Pretty

import Language.Ribbon.Util
import Language.Ribbon.Lexical
import Language.Ribbon.Syntax.Module



-- | Raw output from a parser of the head section of a module
data RawModuleHeader
    = RawModuleHeader
    { name :: !(ATag String)
    , version :: !(ATag Version)
    , sources :: ![ATag FilePath]
    , dependencies :: !RawDependencies
    , meta :: !MetaData
    }
    deriving Show

instance Pretty RawModuleHeader where
    pPrintPrec lvl _ RawModuleHeader{..} =
        "module" <+> pPrintPrec lvl 0 name <+> "@"
                 <+> pPrintPrec lvl 0 version $+$ do
            vcat' $ fmap indent $
                [ hang "sources" $ pPrintPrec lvl 0 sources
                , hang "dependencies" $ pPrintPrec lvl 0 dependencies
                ]
                <> do Map.toList meta <&> \(k, v) ->
                        hang (pPrintPrec lvl 0 k) $
                            pPrintPrec lvl 0 v


-- | Raw output from a parser of the body of a use declaration
data RawUse
    = RawUse
    { basePath :: !(ATag Path)
    , tree :: !(ATag RawUseTree)
    , alias :: !(Maybe QualifiedName)
    }
    deriving (Eq, Ord, Show)

instance Pretty RawUse where
    pPrintPrec lvl _ RawUse{..} =
        let pd = pPrintPrec lvl 0 basePath
            td = pPrintPrec lvl 0 tree
        in do if requiresSlash basePath && not (rawUseTreeIsSingle tree.value)
                then pd </> td
                else pd <> td
            <+> maybeMEmpty (hang "as" . pPrintPrec lvl 0 <$> alias)

-- | Raw output from a parser of the imported item/s in a use declaration
data RawUseTree
    = RawUseBranch ![ATag RawUse]
    | RawUseBlob ![ATag PathName]
    | RawUseSingle
    deriving (Eq, Ord, Show)

instance Pretty RawUseTree where
    pPrintPrec lvl _ = \case
        RawUseBranch subs -> braces do
            lsep (pPrintPrec lvl 0 <$> subs)
        RawUseBlob hiding -> ".." <+> do
            qualH "hiding" $ qualBraces $ lsep do
                pPrintPrec lvl 0 <$> hiding
        RawUseSingle -> mempty

rawUseTreeIsSingle :: RawUseTree -> Bool
rawUseTreeIsSingle = \case
    RawUseSingle -> True
    _ -> False

-- | Extract a hidable @PathName@ from a @RawUse@;
--   this is the path to be ignored by adjacent blobs
--   Fails if:
--   + @getPathName@ fails
hidablePathNameFromUse :: RawUse -> Maybe (ATag PathName)
hidablePathNameFromUse RawUse{..} =
    let pathFixName = untag <$> getPathName basePath.value
        pathCategory = getPathCategory basePath.value
    in if isHidableBase (untag <$> basePath.value.base)
        then pathFixName <&> \f ->
            PathName f pathCategory <$ basePath
        else Nothing
    where
    isHidableBase = \case
        Nothing -> True
        Just PbThis -> True
        _ -> False

-- -- | Extract a @PathName@ from a @RawUse@;
-- --   Fails if:
-- --   + @getPathName@ fails and @alias@ is @Nothing@
-- pathNameFromUse :: RawUse -> Maybe (ATag PathName)
-- pathNameFromUse RawUse{..} =
--     let pathFixName = untag <$> getPathName path.value
--         pathCategory = getPathCategory path.value
--         aliasPathName = alias <&> \a ->
--             let c = case tree.value of
--                     RawUseSingle -> Nothing
--                     _ -> Just ONamespace
--             in PathName (c <|> pathCategory) a.name.value <$ a.name
--         pathPathName = pathFixName <&> \f ->
--             PathName pathCategory f <$ path
--     in aliasPathName <|> pathPathName
