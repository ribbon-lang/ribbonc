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

import Text.Pretty hiding (parens, brackets, braces, cat)

import Language.Ribbon.Util

import Language.Ribbon.Parsing.Monad.Parser

import Language.Ribbon.Lexical
import Language.Ribbon.Syntax
import Debug.Trace (traceM)




moduleHead :: Parser RawModuleHead
moduleHead = expecting "a valid module header" do
    moduleName <- sym "module" >> tag string
    validateName moduleName
    moduleVersion <- sym "@" >> tag version

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
    keyPairs = many do
        liftA2 (,) (tag name) grabWhitespaceDomain

    processPairs =
        flip (foldWithM mempty) \(k, toks) (sources, dependencies, meta) ->
            case k.value.value of
                "sources" ->
                    (, dependencies, meta) . Tag (attrFold toks) <$>
                        processSources (tagOf k) sources toks

                "dependencies" ->
                    (sources, , meta) . Tag (attrFold toks) <$>
                        processDependencies (tagOf k) dependencies toks

                _ -> (sources, dependencies, ) <$> processMeta k meta toks

    processSources at existing toks = do
        assertAt at (null existing.value) $
            hang "multiple `sources` fields in module head"
                $ "original is here:" <+> pPrint (tagOf existing)
        recurseParser (wsList (sym ",") (tag string)) toks

    processDependencies at existing toks = do
        assertAt at (null existing.value) $
            hang "multiple `dependencies` fields in module head"
                $ "original is here:" <+> pPrint (tagOf existing)
        recurseParser (wsList (sym ",") dependency) toks

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
        moduleVersion <- sym "@" >> tag version
        alias <- optional (sym "as" >> noFail (tag name))
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
        (fixity, prec, n) <- defName
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
            a <- lastAttr
            t <- grabDomain (wsDominated a &&& not . isSymbol "=" . untag)
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
        (fixity, prec, n) <- defName
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
        (fixity, prec, n) <- defName
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
        (fixity, prec, n) <- defName
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
            traceM $ "got path: " <> prettyShowLevel PrettyVerbose p
            liftA2 (RawUse p)
                do option (RawUseSingle :@: p.tag)
                    if pathRequiresSlash p.value
                        then connected (sym "/") >> noFail (connected $ tag useTree)
                        else connected $ tag useTree
                do alias
        _ -> liftA3 RawUse
            do tagApp1 (`Path` Nil) <$> PbThis <@> attr
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
        fixity <- option Atom exactFixity
        prec <- option 0 do
            guard (fixity /= Atom)
            precedence
        n <- noFail (tag name)
        pure $ RawRebind vis fixity prec n


path :: Parser Path
path = do
    b <- tag $ optional pathBase
    cs <- case untag b of
        Just b' -> option Nil do
            when (pathBaseRequiresSlash b') do
                connected (sym "/")
            comps
        _ -> liftA2 (Seq.:<|)
            do tag pathComponent
            do option Nil $ connected (sym "/") >> comps
    pure (Path (Maybe.fromMaybe PbThis <$> b) cs)
    where
        comps = Seq.fromList <$> listSome
            (connected $ sym "/")
            (connected $ tag pathComponent)

pathBase :: Parser PathBase
pathBase = asum
    [ PbRoot <$ sym "/"
    , PbThis <$ sym "./"
    , PbModule <$> do
        sym "module" >> noFail name
    , PbFile <$> do
        sym "file" >> noFail string
    , PbUp . length <$> some (sym "../")
    ]

pathComponent :: Parser PathComponent
pathComponent = do
    (fixity, cat) :@: at <- tag $ liftA2 (,)
        do option OUnspecified overloadFixity
        do option OUnresolved overloadCategory
    assertAt at (cat /= ONamespace || fixity == OUnspecified)
        "namespace items cannot have fixity"
    PathComponent fixity cat <$> noFailIf
        (fixity /= OUnspecified || cat /= OUnresolved)
        name


defName :: Parser (ExactFixity, Precedence, ATag Name)
defName = do
    fixity <- option Atom exactFixity
    liftA2 (fixity, , )
        do option (defaultPrecedence fixity) do
            guard (fixity /= Atom)
            precedence
        do tag name

name :: Parser Name
name = expecting "an unreserved name" $ nextMap \case
    t@(TSymbol s) | not (isReserved t) -> pure (Name s)
    _ -> empty

visibility :: Parser Visibility
visibility = Public <$ sym "pub"

overloadFixity :: Parser OverloadFixity
overloadFixity = asum
    [ OInfix <$ sym "infix"
    , OAtomPrefix <$ sym "prefix"
    , OPostfix <$ sym "postfix"
    , OAtomPrefix <$ sym "atom"
    ]

overloadCategory :: Parser OverloadCategory
overloadCategory = asum
    [ ONamespace <$ sym "namespace"
    , OInstance <$ sym "instance"
    , OType <$ sym "type"
    , OValue <$ sym "value"
    ]

exactFixity :: Parser ExactFixity
exactFixity = asum
    [ InfixL <$ sym "infixl"
    , InfixR <$ sym "infixr"
    , Infix <$ sym "infix"
    , Prefix <$ sym "prefix"
    , Postfix <$ sym "postfix"
    , Atom <$ sym "atom"
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
        n <- option (Name (show off.value) :@: off.tag) do
            sym "\\" >> noFail (tag name)
        pure (RawField off n)
    nameOnly = do
        n <- tag name
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
    n <- tag name
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
