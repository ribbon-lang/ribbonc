module Language.Ribbon.Parsing.Parser where

import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe

import Data.Sequence qualified as Seq

import Data.Foldable qualified as Fold

import Data.Word (Word32)

import Data.Nil
import Data.Tag
import Data.Attr

import Control.Applicative
import Control.Monad.State.Strict

import Text.Pretty hiding (parens, brackets, backticks, braces, cat)
import Text.Pretty qualified as Pretty

import Language.Ribbon.Util

import Language.Ribbon.Parsing.Monad.Parser

import Language.Ribbon.Lexical
import Language.Ribbon.Syntax
import Data.Functor ((<&>))




moduleHead :: Parser RawModuleHead
moduleHead = expecting "a valid module header" do
    moduleName <- sym "module" >> tag string
    validateName moduleName
    moduleVersion <- simpleNameOf "@" >> tag version

    (sources, dependencies, meta) <- do
        option mempty grabWhitespaceDomain
            >>= recurseParser keyPairs
            >>= processPairs

    pure RawModuleHead
        { name = moduleName
        , version = moduleVersion
        , sources = sources.value
        , dependencies = dependencies.value
        , meta = meta
        }
    where
    keyPairs = grabLines >>= mapM do
        recurseParser do
            liftA2 (,) (tag simpleName) grabWhitespaceDomain

    processPairs =
        flip (foldWithM mempty) \(k, toks) (sources, dependencies, meta) -> do
            f <- getFilePath
            case k.value.value of
                "sources" ->
                    (, dependencies, meta) . Tag (attrFold (fileAttr f) toks) <$>
                        processSources (tagOf k) sources toks

                "dependencies" ->
                    (sources, , meta) . Tag (attrFold (fileAttr f) toks) <$>
                        processDependencies (tagOf k) dependencies toks

                _ -> (sources, dependencies, ) <$> processMeta k meta toks

    processSources at existing toks = do
        assertAt at (null existing.value) $
            hang "multiple `sources` fields in module head"
                $ "original is here:" <+> pPrint (tagOf existing)
        recurseParser (wsListBody (sym ",") (tag string)) toks

    processDependencies at existing toks = do
        assertAt at (null existing.value) $
            hang "multiple `dependencies` fields in module head"
                $ "original is here:" <+> pPrint (tagOf existing)
        recurseParser (wsListBody (sym ",") dependency) toks

    processMeta key meta toks = do
        assertAt key.tag (Map.notMember key meta) $
            hang ("multiple" <+> backticked key <+> "fields in module head")
                $ "original is here:" <+> do
                    pPrint $ tagOf $ Maybe.fromJust $
                        Fold.find (== key) (Map.keys meta)
        val <- recurseParser (tag string) toks
        pure (Map.insert key val meta)

    dependency = do
        moduleName <- tag string
        moduleVersion <- simpleNameOf "@" >> tag version
        alias <- optional (sym "as" >> noFail (tag simpleName))
        pure (moduleName, moduleVersion, alias)

    validateName s =
        assertAt s.tag ('@' `notElem` s.value) $
            "module name" <+> shown s.value <+> "is invalid,"
                <+> "cannot contain `@` symbol"


file :: Parser RawFile
file = RawNamespace <$> many (tag item)

item :: Parser RawItem
item = asum
    [ RawUseItem <$> do
        sym "use" >> noFail use
    , do
        vis <- option Private visibility
        (fixity, prec, n) <- todo
        RawDefItem vis fixity prec n <$> def
    ]

def :: Parser RawDef
def = asum [ startsEq, decls ] where
    startsEq = sym "=" >> noFail do
        grabWhitespaceDomain >>= recurseParser do
            asum
                [ namespaceDef
                , effectDef
                , classDef
                , instanceDef
                , typeAliasDef
                , structDef
                , unionDef
                , noFail valueDef
                ]

    decls = sym ":" >> noFail do
        grabWhitespaceDomain >>= recurseParser do
            (q, c) <- option mempty do
                sym "forall" >> noFail typeHead
            t <- grabDomain (not . isSymbol "=" . untag)
            option (RdDecl q c t) do
                sym "="
                RdDeclVal q c t <$> noFail grabWhitespaceDomain

namespaceDef :: Parser RawDef
namespaceDef = sym "namespace" >> noFail do
    RdNamespace . RawNamespace <$> do
        grabWhitespaceDomain >>= recurseParser (many $ tag item)

effectDef :: Parser RawDef
effectDef = sym "effect" >> noFail do
    (q, c) <- option mempty typeHead
    RdEffect q c . RawNamespace <$> do
        grabWhitespaceDomain >>= recurseParser (many $ tag effectItem)
    where
    effectItem = do
        (fixity, prec, n) <- todo
        sym ":"
        RawEffectItem fixity prec n <$>
            noFail grabWhitespaceDomain

classDef :: Parser RawDef
classDef = sym "class" >> noFail do
    (q, c) <- option mempty typeHead
    RdClass q c . RawNamespace <$> do
        grabWhitespaceDomain >>= recurseParser (many $ tag classItem)
    where
    classItem = do
        (fixity, prec, n) <- todo
        sym ":"
        grabWhitespaceDomain >>= recurseParser do
            asum [ classValue fixity prec n
                 , classType fixity prec n
                 ]

    classValue fixity prec n = do
        (q, c) <- option mempty typeHead
        RawClassValue fixity prec n q c <$>
            noFail grabWhitespaceDomain

    classType fixity prec n = sym "type" >>
        liftA2 (RawClassAssociate fixity prec n)
            do option mempty quantifier
            do option mempty qualifier

instanceDef :: Parser RawDef
instanceDef = sym "instance" >> noFail do
    (q, c) <- option mempty typeHead
    RdInstance q c . RawNamespace <$> do
        grabWhitespaceDomain >>= recurseParser (many $ tag instanceItem)
    where
    instanceItem = do
        sym "="
        (fixity, prec, n) <- todo
        grabWhitespaceDomain >>= recurseParser do
            asum [ instanceValue fixity prec n
                 , instanceType fixity prec n
                 ]

    instanceValue fixity prec n =
        RawInstanceValue fixity prec n <$>
            noFail grabWhitespaceDomain

    instanceType fixity prec n = sym "type" >>
        liftA2 (RawInstanceAssociate fixity prec n)
            do option mempty (typeHeadOf quantifier)
            do noFail grabWhitespaceDomain

typeAliasDef :: Parser RawDef
typeAliasDef = sym "type" >> noFail do
    liftA2 RdTypeAlias
        do option mempty (typeHeadOf quantifier)
        do noFail grabWhitespaceDomain

structDef :: Parser RawDef
structDef = sym "struct" >> noFail do
    liftA2 RdStruct
        do option mempty (typeHeadOf quantifier)
        do RawNamespace <$> noFail do
            grabWhitespaceDomain >>= recurseParser fields

unionDef :: Parser RawDef
unionDef = sym "union" >> noFail do
    liftA2 RdUnion
        do option mempty (typeHeadOf quantifier)
        do RawNamespace <$> noFail do
            grabWhitespaceDomain >>= recurseParser fields

valueDef :: Parser RawDef
valueDef = RdValue <$> grabWhitespaceDomain


use :: Parser RawUse
use = do
    optional (tag path) >>= \case
        Just p -> do
            liftA2 (RawUse p)
                do option (RawUseSingle :@: p.tag)
                    if requiresSlash p
                        then do
                            at <- attrOf $ connected p.tag (sym "/")
                            noFail (connected at $ tag useTree)
                        else connected p.tag $ tag useTree
                do alias
        _ -> liftA3 RawUse
            do tagApp1 ((`Path` Nil) . Just) <$> PbThis <@> attr
            do tag useTree
            do alias
    where
    useTree = asum
        [ RawUseBlob <$ sym ".."
        , RawUseBranch <$> braces do
            listMany (sym ",") (tag use)
        ]

    alias = optional $ sym "as" >> tag do
        vis <- option Private visibility
        fixity <- option Atom todo
        prec <- option 0 do
            guard (fixity /= Atom)
            precedence
        n <- noFail (tag simpleName)
        pure $ RawRebind vis fixity prec n


path :: Parser Path
path = nextMap \case
    TPath p -> Just p
    _ -> Nothing


anyName :: Parser FixName
anyName = asum
    [ simpleName <&> \n -> FixName (Seq.singleton (FixSimple n))
    , fixName
    ]

fixName :: Parser FixName
fixName = expecting "a fix name" $ nextMap \case
    TPath (SingleNamePath f) -> Just f
    _ -> Nothing

simpleName :: Parser SimpleName
simpleName = expecting "an unreserved identifier or operator" $ nextMap \case
    TPath (SingleNamePath (SimpleFixName n)) -> Just n
    _ -> Nothing

simpleNameOf :: String -> Parser ()
simpleNameOf s = expecting (Pretty.backticks $ text s) $ nextIf_ \case
    TPath (SingleNamePath (SimpleFixName (SimpleName n))) -> n == s
    _ -> False


visibility :: Parser Visibility
visibility = Public <$ sym "pub"


overloadCategory :: Parser OverloadCategory
overloadCategory = asum
    [ ONamespace <$ sym "namespace"
    , OInstance <$ sym "instance"
    , OType <$ sym "type"
    , OValue <$ sym "value"
    ]


precedence :: Parser Precedence
precedence = do
    i :@: at <- tag int
    assertAt at (i <= bound) $
        "precedence level" <+> backticked i
            <+> "is out of range, max is " <+> shown bound
    pure (fromIntegral i) where
    bound = fromIntegral @Precedence @Word32 maxBound

fields :: Parser [ATag RawField]
fields = loop 0 where
    loop i = option [] do
        f <- tag $ field i
        (f :) <$> loop (f.value.offset.value + 1)

field :: Word32 -> Parser RawField
field idx = do
    lbl <- asum [ numbered, nameOnly ]
    noFail do
        sym ":"
        lbl <$> grabWhitespaceDomain
    where
    numbered = do
        off <- tag int
        n <- option (SimpleName (show off.value) :@: off.tag) do
            sym "\\" >> noFail (tag simpleName)
        pure (RawField off n)
    nameOnly = do
        n <- tag simpleName
        pure (RawField (idx :@: n.tag) n)


typeHead :: Parser (Quantifier, RawQualifier)
typeHead = typeHeadOf $ liftA2 (,)
    do option mempty quantifier
    do option mempty qualifier

typeHeadOf :: Nil a => Parser a -> Parser a
typeHeadOf p = do
    r :@: at <- tag $ option Nil p
    guard (not (isNil r))
    r <$ expectingAt at "to close type head" (sym "=>")


quantifier :: Parser Quantifier
quantifier = Quantifier <$> listSome (sym ",") (tag typeBinder)

qualifier :: Parser RawQualifier
qualifier = sym "where" >> noFail do
    Qualifier <$> nextWhileAttr \case
        TSymbol "=>" :@: _ -> False
        _ -> True

typeBinder :: Parser TypeBinder
typeBinder = do
    n <- tag simpleName
    k <- option (KVar n.value.value :@: n.tag) do
        sym ":" >> tag kind
    pure (n `Of` k)

kind :: Parser Kind
kind = tag atom >>= arrows where
    atom = asum
        [ KType <$ sym "type"
        , KNum <$ sym "num"
        , KStr <$ sym "str"
        , KEffect <$ sym "effect"
        , KConstraint <$ sym "constraint"
        , KData <$ sym "data"
        , KEffects <$ sym "effects"
        , parens kind
        ]
    arrows l = option (untag l) do
        sym "->" >> noFail do
            KArrow l <$> tag kind
