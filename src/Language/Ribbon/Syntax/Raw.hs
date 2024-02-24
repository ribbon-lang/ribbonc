module Language.Ribbon.Syntax.Raw where

import Data.Map.Strict qualified as Map

import Data.Word (Word32)

import Data.Functor ((<&>))

import Data.Tag
import Data.Attr

import Text.Pretty

import Language.Ribbon.Lexical

import Language.Ribbon.Syntax.Path
import Language.Ribbon.Syntax.Scheme
import Language.Ribbon.Syntax.Visibility
import Language.Ribbon.Syntax.Fixity
import Language.Ribbon.Syntax.Precedence
import Language.Ribbon.Syntax.Module
import Language.Ribbon.Util


-- | A map from locally-appropriate names to module strings and versions
type RawDependencies = [(ATag String, ATag Version, Maybe (ATag Name))]

-- | A qualifier with its body not yet fully parsed
type RawQualifier = Qualifier Token

-- | Raw output from a parser of the head section of a module
data RawModuleHead
    = RawModuleHead
    { name :: !(ATag String)
    , version :: !(ATag Version)
    , sources :: ![ATag FilePath]
    , dependencies :: !RawDependencies
    , meta :: !MetaData
    }
    deriving Show

instance Pretty RawModuleHead where
    pPrintPrec lvl _ RawModuleHead{..} =
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
    , fixity :: !ExactFixity
    , precedence :: !Precedence
    , name :: !(ATag Name)
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
    { fixity :: !ExactFixity
    , precedence :: !Precedence
    , name :: !(ATag Name)
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
    { fixity :: !ExactFixity
    , precedence :: !Precedence
    , name :: !(ATag Name)
    , quantifier :: !Quantifier
    , qualifier :: !RawQualifier
    }
    | RawClassValue
    { fixity :: !ExactFixity
    , precedence :: !Precedence
    , name :: !(ATag Name)
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
    { fixity :: !ExactFixity
    , precedence :: !Precedence
    , name :: !(ATag Name)
    , quantifier :: !Quantifier
    , ty :: !TokenSeq
    }
    | RawInstanceValue
    { fixity :: !ExactFixity
    , precedence :: !Precedence
    , name :: !(ATag Name)
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
    , name :: !(ATag Name)
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

data RawRebind
    = RawRebind
    { visibility :: !Visibility
    , fixity :: !ExactFixity
    , precedence :: !Precedence
    , name :: !(ATag Name)
    }
    deriving (Eq, Ord, Show)

instance Pretty RawRebind where
    pPrintPrec lvl _ RawRebind{..} =
        hsep [ pPrintPrec lvl 0 visibility
             , pPrintPrec lvl 0 fixity
             , pPrintPrec lvl 0 precedence
             , pPrintPrec lvl 0 name
             ]

-- | Raw output from a parser of the body of a use declaration
data RawUse
    = RawUse
    { path :: !(ATag Path)
    , tree :: !(ATag RawUseTree)
    , alias :: !(Maybe (ATag RawRebind))
    }
    deriving (Eq, Ord, Show)

instance Pretty RawUse where
    pPrintPrec lvl _ RawUse{..} =
        let pd = pPrintPrec lvl 0 path
            td = pPrintPrec lvl 0 tree
        in hang "use" do
            do if pathRequiresSlash path.value && not (rawUseTreeIsSingle tree.value)
                then pd </> td
                else pd <> td
            <+> maybeMEmpty (hang "as" . pPrintPrec lvl 0 <$> alias)

-- | Raw output from a parser of the imported item/s in a use declaration
data RawUseTree
    = RawUseBranch ![ATag RawUse]
    | RawUseBlob
    | RawUseSingle
    deriving (Eq, Ord, Show)

instance Pretty RawUseTree where
    pPrintPrec lvl _ = \case
        RawUseBranch subs -> braces do
            lsep (pPrintPrec lvl 0 <$> subs)
        RawUseBlob -> ".."
        RawUseSingle -> mempty

rawUseTreeIsSingle :: RawUseTree -> Bool
rawUseTreeIsSingle = \case
    RawUseSingle -> True
    _ -> False
