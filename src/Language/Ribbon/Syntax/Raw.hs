module Language.Ribbon.Syntax.Raw where

import Data.Functor ((<&>))

import Data.Map.Strict qualified as Map

import Data.Word (Word32)

import Data.Tag
import Data.Attr

import Text.Pretty

import Language.Ribbon.Util
import Language.Ribbon.Lexical
import Language.Ribbon.Syntax.Scheme
import Language.Ribbon.Syntax.Module




-- | A qualifier with its body not yet fully parsed
type RawQualifier = Qualifier TokenSeq

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


-- | Raw output from a parser of a group of items
newtype RawNamespace a
    = RawNamespace { items :: [ATag a] }
    deriving (Eq, Ord, Show)

instance Pretty a => Pretty (RawNamespace a) where
    pPrintPrec lvl _ (RawNamespace items) =
        vcat' (pPrintPrec lvl 0 <$> items)

-- | Raw output from a parser of a source file
type RawFile = RawNamespace RawItem


-- | Raw output from a parser of a top-level definition
data RawItem
    = RawDefItem
    { visibility :: !Visibility
    , fixity :: !Fixity
    , precedence :: !Precedence
    , name :: !(ATag SimpleName)
    , val :: !RawDef
    }
    | RawUseItem !RawUse
    deriving (Eq, Ord, Show)

instance Pretty RawItem where
    pPrintPrec lvl _ = \case
        RawDefItem{..} ->
            hang (hsep
                [ pPrintPrec lvl 0 fixity
                , pPrintPrec lvl 0 precedence
                , pPrintPrec lvl 0 name
                , "="
                ]) do
                    pPrintPrec lvl 0 val
        RawUseItem u -> pPrint u

-- | Raw output from a parser of an item in the body of an effect
data RawEffectItem
    = RawEffectItem
    { fixity :: !Fixity
    , precedence :: !Precedence
    , name :: !(ATag SimpleName)
    , ty :: !TokenSeq
    }
    deriving (Eq, Ord, Show)

instance Pretty RawEffectItem where
    pPrintPrec lvl _ RawEffectItem{..} =
        hang (hsep [ pPrintPrec lvl 0 fixity
                   , pPrintPrec lvl 0 precedence
                   , pPrintPrec lvl 0 name
                   , ":" ]) do
            pPrintPrec lvl 0 ty

-- | Raw output from a parser of an item in the body of a class
data RawClassItem
    = RawClassAssociate
    { fixity :: !Fixity
    , precedence :: !Precedence
    , name :: !(ATag SimpleName)
    , quantifier :: !Quantifier
    , qualifier :: !RawQualifier
    }
    | RawClassValue
    { fixity :: !Fixity
    , precedence :: !Precedence
    , name :: !(ATag SimpleName)
    , quantifier :: !Quantifier
    , qualifier :: !RawQualifier
    , ty :: !TokenSeq
    }
    deriving (Eq, Ord, Show)

instance Pretty RawClassItem where
    pPrintPrec lvl _ = \case
        RawClassAssociate{..} ->
            hang (hsep [ pPrintPrec lvl 0 fixity
                       , pPrintPrec lvl 0 precedence
                       , pPrintPrec lvl 0 name
                       , ":" ]) do
                hang "type" do
                    pPrintPrec lvl 0 quantifier
                        <+> pPrintPrec lvl 0 qualifier
        RawClassValue{..} ->
            hang (hsep [ pPrintPrec lvl 0 fixity
                       , pPrintPrec lvl 0 precedence
                       , pPrintPrec lvl 0 name
                       , ":" ]) do
                qualH (qual' (hsep [ pPrintPrec lvl 0 quantifier
                                  , pPrintPrec lvl 0 qualifier ]) "=>") do
                    pPrintPrec lvl 0 ty

-- | Raw output from a parser of an item in the body of an instance
data RawInstanceItem
    = RawInstanceAssociate
    { fixity :: !Fixity
    , precedence :: !Precedence
    , name :: !(ATag SimpleName)
    , quantifier :: !Quantifier
    , ty :: !TokenSeq
    }
    | RawInstanceValue
    { fixity :: !Fixity
    , precedence :: !Precedence
    , name :: !(ATag SimpleName)
    , val :: !TokenSeq
    }
    deriving (Eq, Ord, Show)

instance Pretty RawInstanceItem where
    pPrintPrec lvl _ = \case
        RawInstanceAssociate{..} ->
            hang (hsep [ pPrintPrec lvl 0 fixity
                       , pPrintPrec lvl 0 precedence
                       , pPrintPrec lvl 0 name
                       , "=" ]) do
                hang "type" do
                    hang (qual' (pPrintPrec lvl 0 quantifier) "=>") do
                        pPrintPrec lvl 0 ty
        RawInstanceValue{..} ->
            hang (hsep [ pPrintPrec lvl 0 fixity
                       , pPrintPrec lvl 0 precedence
                       , pPrintPrec lvl 0 name
                       , "=" ]) do
                    pPrintPrec lvl 0 val

-- | Raw output from a parser of an item in the body of a data type def
data RawField
    = RawField
    { offset :: !(ATag Word32)
    , name :: !(ATag SimpleName)
    , ty :: !TokenSeq
    }
    deriving (Eq, Ord, Show)

instance Pretty RawField where
    pPrintPrec lvl _ RawField{..} =
        hang (hsep [ pPrintPrec lvl 0 offset
                   , "\\"
                   , pPrintPrec lvl 0 name
                   , ":" ]) do
            pPrintPrec lvl 0 ty

-- | Raw output from a parser of the body of a top-level definition
data RawDef
    = RdNamespace
    { namespaceItems :: !(RawNamespace RawItem) }
    | RdEffect
    { quantifier :: !Quantifier
    , qualifier :: !RawQualifier
    , effectItems :: !(RawNamespace RawEffectItem)
    }
    | RdClass
    { quantifier :: !Quantifier
    , qualifier :: !RawQualifier
    , classItems :: !(RawNamespace RawClassItem)
    }
    | RdInstance
    { quantifier :: !Quantifier
    , qualifier :: !RawQualifier
    , instanceItems :: !(RawNamespace RawInstanceItem)
    }
    | RdTypeAlias
    { quantifier :: !Quantifier
    , body :: !TokenSeq
    }
    | RdStruct
    { quantifier :: !Quantifier
    , structFields :: !(RawNamespace RawField)
    }
    | RdUnion
    { quantifier :: !Quantifier
    , unionFields :: !(RawNamespace RawField)
    }
    | RdDeclVal
    { quantifier :: !Quantifier
    , qualifier :: !RawQualifier
    , ty :: !TokenSeq
    , body :: !TokenSeq
    }
    | RdDecl
    { quantifier :: !Quantifier
    , qualifier :: !RawQualifier
    , ty :: !TokenSeq
    }
    | RdValue
    { body :: !TokenSeq }
    deriving (Eq, Ord, Show)

instance Pretty RawDef where
    pPrintPrec lvl _ = \case
        RdNamespace{..} -> hang "namespace" do
            pPrintPrec lvl 0 namespaceItems
        RdEffect{..} -> hang "effect" do
            hang (qual' (hsep [ pPrintPrec lvl 0 quantifier
                              , pPrintPrec lvl 0 qualifier ]) "=>") do
                pPrintPrec lvl 0 effectItems
        RdClass{..} -> hang "class" do
            hang (qual' (hsep [ pPrintPrec lvl 0 quantifier
                              , pPrintPrec lvl 0 qualifier ]) "=>") do
                pPrintPrec lvl 0 classItems
        RdInstance{..} -> hang "instance" do
            hang (qual' (hsep [ pPrintPrec lvl 0 quantifier
                              , pPrintPrec lvl 0 qualifier ]) "=>") do
                pPrintPrec lvl 0 instanceItems
        RdTypeAlias{..} -> hang "type" do
            hang (qual' (pPrintPrec lvl 0 quantifier) "=>") do
                pPrintPrec lvl 0 body
        RdStruct{..} -> hang "struct" do
            hang (qual' (pPrintPrec lvl 0 quantifier) "=>") do
                pPrintPrec lvl 0 structFields
        RdUnion{..} -> hang "union" do
            hang (qual' (pPrintPrec lvl 0 quantifier) "=>") do
                pPrintPrec lvl 0 unionFields
        RdDeclVal{..} -> vcat
            [ hang ":" do
                hang (qual' (qualH "forall"
                        (hsep [ pPrintPrec lvl 0 quantifier
                              , pPrintPrec lvl 0 qualifier ])) "=>") do
                    pPrintPrec lvl 0 ty
            , hang "=" do
                pPrintPrec lvl 0 body
            ]
        RdDecl{..} -> hang ":" do
            hang (qual' (qualH "forall"
                    (hsep [ pPrintPrec lvl 0 quantifier
                            , pPrintPrec lvl 0 qualifier ])) "=>") do
                pPrintPrec lvl 0 ty
        RdValue{..} -> hang "=" do
            pPrintPrec lvl 0 body


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
            PathName pathCategory f <$ basePath
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
