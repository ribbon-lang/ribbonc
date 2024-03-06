{-# OPTIONS_GHC -Wno-orphans #-}
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

import Control.Monad.File
import Control.Monad.Parser.Class

import Text.Pretty hiding (parens, brackets, backticks, braces, cat)
import Text.Pretty qualified as Pretty

import Language.Ribbon.Util


import Language.Ribbon.Lexical
import Language.Ribbon.Syntax
import Control.Monad.Except
import Data.SyntaxError
import Data.Function
import qualified Data.Set as Set

import Language.Ribbon.Analysis.Builder
import Data.Functor
import Control.Monad.Diagnostics
import Control.Monad.Reader



instance ParseInput TokenSeq where
    type InputElement TokenSeq = Token
    formatInput fp ts = Tag (attrInput fp ts) case ts of
        (t :@: _) Seq.:<| _ -> UnexpectedFailure
            (inputIdentity t <+> inputPretty t)
        _ -> EofFailure
    unconsInput = \case
        (t :@: _) Seq.:<| ts -> Right (t, ts)
        _ -> Left DecodeEof
    attrInput fp ts = case ts of
        t Seq.:<| _ -> t.tag
        _ -> Attr fp Nil
    attrInputDiff fp ts ts' = attrFold (fileAttr fp) do
        Seq.take (Seq.length ts - Seq.length ts') ts




moduleHead :: MonadParse TokenSeq m =>
    m (ATag String, ATag Version, MetaData, RawModuleHeader)
moduleHead = expecting "a valid module header" do
    moduleName <- sym "module" >> tag string
    moduleVersion <- simpleNameOf "@" >> tag version

    (sources, dependencies, meta) <- do
        option mempty grabWhitespaceDomain
            >>= recurseParser keyPairs
            >>= processPairs

    pure $ (moduleName, moduleVersion, meta, )
        RawModuleHeader
        { sources = sources.value
        , dependencies = dependencies.value
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
        assertAt at (null existing.value) Unrecoverable $
            hang "multiple `sources` fields in module head"
                $ "original is here:" <+> pPrint (tagOf existing)
        recurseParser (wsListBody (sym ",") (tag string)) toks

    processDependencies at existing toks = do
        assertAt at (null existing.value) Unrecoverable $
            hang "multiple `dependencies` fields in module head"
                $ "original is here:" <+> pPrint (tagOf existing)
        recurseParser (wsListBody (sym ",") dependency) toks

    processMeta key meta toks = do
        assertAt key.tag (Map.notMember key meta) Unrecoverable $
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


-- file :: MonadParse TokenSeq m => m RawFile
-- file = RawNamespace <$> many (tag item)

-- item :: MonadParse TokenSeq m => m RawItem
-- item = asum
--     [ RawUseItem <$> do
--         sym "use" >> noFail use
--     , do
--         vis <- option Private visibility
--         (fixity, prec, n) <- todo
--         RawDefItem vis fixity prec n <$> def
--     ]

-- def :: MonadParse TokenSeq m => m RawDef
-- def = asum [ startsEq, decls ] where
--     startsEq = sym "=" >> noFail do
--         grabWhitespaceDomain >>= recurseParser do
--             asum
--                 [ namespaceDef
--                 , effectDef
--                 , classDef
--                 , instanceDef
--                 , typeAliasDef
--                 , structDef
--                 , unionDef
--                 , noFail valueDef
--                 ]

--     decls = sym ":" >> noFail do
--         grabWhitespaceDomain >>= recurseParser do
--             (q, c) <- option mempty do
--                 sym "forall" >> noFail typeHead
--             t <- grabDomain (not . isSymbol "=" . untag)
--             option (RdDecl q c t) do
--                 sym "="
--                 RdDeclVal q c t <$> noFail grabWhitespaceDomain

-- namespaceDef :: MonadParse TokenSeq m => m RawDef
-- namespaceDef = sym "namespace" >> noFail do
--     RdNamespace . RawNamespace <$> do
--         grabWhitespaceDomain >>= recurseParser (many $ tag item)

-- effectDef :: MonadParse TokenSeq m => m RawDef
-- effectDef = sym "effect" >> noFail do
--     (q, c) <- option mempty typeHead
--     RdEffect q c . RawNamespace <$> do
--         grabWhitespaceDomain >>= recurseParser (many $ tag effectItem)
--     where
--     effectItem = do
--         (fixity, prec, n) <- todo
--         sym ":"
--         RawEffectItem fixity prec n <$>
--             noFail grabWhitespaceDomain

-- classDef :: MonadParse TokenSeq m => m RawDef
-- classDef = sym "class" >> noFail do
--     (q, c) <- option mempty typeHead
--     RdClass q c . RawNamespace <$> do
--         grabWhitespaceDomain >>= recurseParser (many $ tag classItem)
--     where
--     classItem = do
--         (fixity, prec, n) <- todo
--         sym ":"
--         grabWhitespaceDomain >>= recurseParser do
--             asum [ classValue fixity prec n
--                  , classType fixity prec n
--                  ]

--     classValue fixity prec n = do
--         (q, c) <- option mempty typeHead
--         RawClassValue fixity prec n q c <$>
--             noFail grabWhitespaceDomain

--     classType fixity prec n = sym "type" >>
--         liftA2 (RawClassAssociate fixity prec n)
--             do option mempty quantifier
--             do option mempty qualifier

-- instanceDef :: MonadParse TokenSeq m => m RawDef
-- instanceDef = sym "instance" >> noFail do
--     (q, c) <- option mempty typeHead
--     RdInstance q c . RawNamespace <$> do
--         grabWhitespaceDomain >>= recurseParser (many $ tag instanceItem)
--     where
--     instanceItem = do
--         sym "="
--         (fixity, prec, n) <- todo
--         grabWhitespaceDomain >>= recurseParser do
--             asum [ instanceValue fixity prec n
--                  , instanceType fixity prec n
--                  ]

--     instanceValue fixity prec n =
--         RawInstanceValue fixity prec n <$>
--             noFail grabWhitespaceDomain

--     instanceType fixity prec n = sym "type" >>
--         liftA2 (RawInstanceAssociate fixity prec n)
--             do option mempty (typeHeadOf quantifier)
--             do noFail grabWhitespaceDomain

-- typeAliasDef :: MonadParse TokenSeq m => m RawDef
-- typeAliasDef = sym "type" >> noFail do
--     liftA2 RdTypeAlias
--         do option mempty (typeHeadOf quantifier)
--         do noFail grabWhitespaceDomain

-- structDef :: MonadParse TokenSeq m => m RawDef
-- structDef = sym "struct" >> noFail do
--     liftA2 RdStruct
--         do option mempty (typeHeadOf quantifier)
--         do RawNamespace <$> noFail do
--             grabWhitespaceDomain >>= recurseParser fields

-- unionDef :: MonadParse TokenSeq m => m RawDef
-- unionDef = sym "union" >> noFail do
--     liftA2 RdUnion
--         do option mempty (typeHeadOf quantifier)
--         do RawNamespace <$> noFail do
--             grabWhitespaceDomain >>= recurseParser fields

-- valueDef :: MonadParse TokenSeq m => m RawDef
-- valueDef = RdValue <$> grabWhitespaceDomain




useDef :: (MonadDiagnostics m, MonadParse TokenSeq m) =>
    UnresolvedImportBuilder m ()
useDef = do
    fp <- getFilePath
    runReaderT use (Tag (fileAttr fp) Nil)
    where
    use = do
        -- FIXME currently not supporting aliasing of blobs and branches as defined in the grammar
        optional (tag path) >>= \case
            Just p -> local (<> p) do
                (if requiresSlash p
                    then do
                        at <- attrOf $ connected p.tag (sym "/")
                        noFail (connected at useTree)
                    else connected p.tag useTree) `catchRecoverable` const alias
            _ -> useTree `catchRecoverable` const alias

    useTree = asum
        [ do
            p <- ask
            guard (not $ isNil p.value)
            sym ".."
            -- FIXME this needs to be able to hide aliased imports from the branch
            -- this is currently unsupported by the blob system, blobs in general need a "hiding" qualifier
            -- this will require another state to give us the branch siblings
            lift $ insertBlob p

        , braces do listMany_ (sym ",") use
        ]

    alias = do
        p <- ask
        guard (not $ isNil p.value)
        asum
            [ sym "as" >> noFail do
                (vis, (associativity, prec, fn)) <- fixNameDef
                lift $ insertAlias p UnresolvedName
                    { visibility = vis
                    , category = pathCategory p.value
                    , fixitySpecifics = Just (associativity, prec)
                    , name = fn
                    }
                pure ()
            , pure ()
            ]


path :: MonadParse TokenSeq m => m Path
path = nextMap \case
    TPath p -> Just p
    _ -> Nothing


fixName :: MonadParse TokenSeq m => m FixName
fixName = expecting "a fix name" $ nextMap \case
    TPath (SingleNamePath f) -> Just f
    _ -> Nothing

simpleName :: MonadParse TokenSeq m => m SimpleName
simpleName = expecting "an unreserved identifier or operator" $ nextMap \case
    TPath (SingleNamePath (SimpleFixName n)) -> Just n
    _ -> Nothing

simpleNameOf :: MonadParse TokenSeq m => String -> m ()
simpleNameOf s = expecting (Pretty.backticks $ text s) $ nextIf_ \case
    TPath (SingleNamePath (SimpleFixName (SimpleName n))) -> n == s
    _ -> False


visibility :: MonadParse TokenSeq m => m Visibility
visibility = Public <$ sym "pub"


-- overloadCategory :: MonadParse TokenSeq m => m OverloadCategory
-- overloadCategory = asum
--     [ ONamespace <$ sym "namespace"
--     , OInstance <$ sym "instance"
--     , OType <$ sym "type"
--     , OValue <$ sym "value"
--     ]

fixNameDef :: (MonadDiagnostics m, MonadParse TokenSeq m) =>
    m (Visibility, (Associativity, Precedence, ATag FixName))
fixNameDef = liftA2 (,) (option Private visibility) do
    (n, apt) <- asum
        [ do
            n@(_ :@: at) <- tag fixName
            option (n, Nothing :@: at) do
                (n, ) <$> tag do
                    Just . Right <$> associativePrecedence
        , do
            apt <- tag associativePrecedence
            n <- tag fixName
            pure (n, Just . Left <$> apt)
        ]
    case untag apt of
        Nothing -> pure (NonAssociative, defaultPrecedence (getFixity n), n)
        Just aptLr -> case getFixity n of
            Atom -> do
                reportWarning $
                    hang ("at" <+> (pPrint apt.tag <> ":")) do
                        "atom names do not have precedence;"
                        <+> "this annotation will be ignored"
                pure (NonAssociative, defaultPrecedence Atom, n)
            fx -> case (aptLr, fx) of
                (Left apt', Prefix) -> do
                    reportWarning $
                        hang ("at" <+> (pPrint apt.tag <> ":")) do
                            "prefix names expect precedence on the right"
                    finishPfx apt.tag Prefix apt' n
                (Right apt', Prefix) ->
                    finishPfx apt.tag Prefix apt' n
                (Right apt', Postfix) -> do
                    reportWarning $
                        hang ("at" <+> (pPrint apt.tag <> ":")) do
                            "postfix names expect precedence on the left"
                    finishPfx apt.tag Postfix apt' n
                (Left apt', Postfix) ->
                    finishPfx apt.tag Postfix apt' n
                (Left apt', Infix) ->
                    finishIfx LeftAssociative apt' n
                (Right apt', Infix) ->
                    finishIfx RightAssociative apt' n
    where
    finishIfx lr apt n = pure (select (fst apt) lr NonAssociative, snd apt, n)
    finishPfx at fx apt n = do
        unless (fst apt) $ reportWarning $
            hang ("at" <+> (pPrint at <> ":")) do
                pPrint fx <+> "names are always non-associative;"
                    <+> "parens are not required"
        pure (NonAssociative, snd apt, n)


associativePrecedence :: MonadParse TokenSeq m => m (Bool, Precedence)
associativePrecedence = asum
    [ (False, ) <$> parens precedence
    , (True, ) <$> precedence
    ]

precedence :: MonadParse TokenSeq m => m Precedence
precedence = do
    i :@: at <- tag int
    assertAt at (i <= bound) Unrecoverable $
        "precedence level" <+> backticked i
            <+> "is out of range, max is " <+> shown bound
    pure (fromIntegral i) where
    bound = fromIntegral @Precedence @Word32 maxBound

-- fields :: MonadParse TokenSeq m => m [ATag RawField]
-- fields = loop 0 where
--     loop i = option [] do
--         f <- tag $ field i
--         (f :) <$> loop (f.value.offset.value + 1)

-- field :: Word32 -> Parser RawField
-- field idx = do
--     lbl <- asum [ numbered, nameOnly ]
--     noFail do
--         sym ":"
--         lbl <$> grabWhitespaceDomain
--     where
--     numbered = do
--         off <- tag int
--         n <- option (SimpleName (show off.value) :@: off.tag) do
--             sym "\\" >> noFail (tag simpleName)
--         pure (RawField off n)
--     nameOnly = do
--         n <- tag simpleName
--         pure (RawField (idx :@: n.tag) n)


-- typeHead :: MonadParse TokenSeq m => m (Quantifier, RawQualifier)
-- typeHead = typeHeadOf $ liftA2 (,)
--     do option mempty quantifier
--     do option mempty qualifier

-- typeHeadOf :: Nil a => Parser a -> Parser a
-- typeHeadOf p = do
--     r :@: at <- tag $ option Nil p
--     guard (not (isNil r))
--     r <$ expectingAt at "to close type head" (sym "=>")



quantifier :: MonadParse TokenSeq m => m Quantifier
quantifier = flip evalStateT 0 do
    Quantifier <$> listSome (lift $ sym ",") (tag typeBinder)

qualifier :: MonadParse TokenSeq m => m (Qualifier TokenSeq)
qualifier = sym "where" >> noFail do
    Qualifier . pure <$> tag do
        grabDomain (not . isSymbol "=>" . untag)

typeBinder :: MonadParse TokenSeq m => StateT KindVar m TypeBinder
typeBinder = do
    n <- tag simpleName
    k <- optional $ lift do
        sym ":" >> tag kind
    case k of
        Just k' -> pure (n `Of` k')
        _ -> do
            k' <- state \i -> (i, i + 1)
            pure $ n `Of` (KVar k' :@: n.tag)

kind :: MonadParse TokenSeq m => m Kind
kind = tag atom >>= arrows where
    atom = asum
        [ KType <$ simpleNameOf "Type"
        , KNum <$ simpleNameOf "Num"
        , KStr <$ simpleNameOf "Str"
        , KEffect <$ simpleNameOf "Effect"
        , KConstraint <$ simpleNameOf "Constraint"
        , KData <$ simpleNameOf "Data"
        , KEffects <$ simpleNameOf "Effects"
        , parens kind
        ]
    arrows l = option (untag l) do
        simpleNameOf "->" >> noFail do
            KArrow l <$> tag kind




grabDomain :: MonadParse TokenSeq m => (ATag Token -> Bool) -> m TokenSeq
grabDomain p = do
    ts <- takeParseState
    case ts of
        toks | not (Seq.null toks) ->
            let (a, b) = consume False toks
            in reduceTokenSeq a <$ putParseState (reduceTokenSeq b)
        _ -> do
            fp <- getFilePath
            throwError $ SyntaxError Recoverable $ EofFailure :@: attrInput fp ts
    where
    consume recursed = \case
        (t Seq.:<| ts) | p t ->
            case untag t of
                TTree BkWhitespace ts' ->
                    let (as, bs) = consume True ts'
                    in buildSplit t ts as bs
                    if recursed
                        then recurse
                        else (Seq.singleton t, Nil)
                _ -> recurse
            where
            recurse =
                let (as, bs) = consume recursed ts
                in (t Seq.<| as, bs)

        ts -> (Nil, ts)

    buildSplit t rhs as bs nb
        | Seq.null bs = nb
        | Seq.null as = (Nil, t Seq.<| rhs)
        | otherwise =
            ( Seq.singleton (TTree BkWhitespace as :@: t.tag)
            , (TTree BkWhitespace bs :@: t.tag) Seq.<| rhs
            )

-- | Get the rest of the input delimited by indentation
grabWhitespaceDomain :: MonadParse TokenSeq m => m TokenSeq
grabWhitespaceDomain = grabDomain (const True)

-- | Consume a sequence of lines from the input
grabLines :: MonadParse TokenSeq m => m [TokenSeq]
grabLines = some $ nextMap \case
    TTree BkWhitespace lns -> Just lns
    _ -> Nothing

-- | @wsBlock<elem>++(sep?) | elem (sep elem)*@
wsListBody :: MonadParse TokenSeq m => m sep -> m a -> m [a]
wsListBody ms ma = asum [block, inline] where
    inline = listSome ms ma
    block = do
        lns <- grabLines
        noFail do
            lns & foldWithM' mempty \toks as ->
                (: as) <$> recurseParser
                    do if null as
                        then ma
                        else ma << optional ms
                    toks



-- | Expect a @Version@
version :: MonadParse TokenSeq m => m Version
version = expecting "a version" $ nextMap \case
    TVersion v -> Just v
    _ -> Nothing

-- | Expect a @Literal@
literal :: MonadParse TokenSeq m => m Literal
literal = expecting "literal" $ nextMap \case
    TLiteral lit -> Just lit
    _ -> Nothing

-- | Expect a @Literal@ of @Char@
char :: MonadParse TokenSeq m => m Char
char = expecting "character literal" $ nextMap \case
    TLiteral (LChar c) -> Just c
    _ -> Nothing

-- | Expect a @Literal@ of @Int@
int :: MonadParse TokenSeq m => m Word32
int = expecting "integer literal" $ nextMap \case
    TLiteral (LInt i) -> Just i
    _ -> Nothing

-- | Expect a @Literal@ of @Float@
float :: MonadParse TokenSeq m => m Float
float = expecting "float literal" $ nextMap \case
    TLiteral (LFloat f) -> Just f
    _ -> Nothing

-- | Expect a @Literal@ of @String@
string :: MonadParse TokenSeq m => m String
string = expecting "string literal" $ nextMap \case
    TLiteral (LString s) -> Just s
    _ -> Nothing

-- | Expect any symbol @Token@
anySym :: MonadParse TokenSeq m => m String
anySym = expecting "a symbol" $ nextMap \case
    TSymbol s -> Just s
    _ -> Nothing

-- | Expect a symbol @Token@ that is not reserved
unreserved :: MonadParse TokenSeq m => m String
unreserved = expecting "an unreserved symbol" $ nextMap \case
    tk@(TSymbol s) | not (isReserved tk) -> Just s
    _ -> Nothing

-- | Expect a symbol @Token@ with a specific value
sym :: MonadParse TokenSeq m => String -> m ()
sym s = expecting (Pretty.backticks $ text s) $ nextMap \case
    TSymbol s' | s == s' -> Just ()
    _ -> Nothing

-- | Expect a symbol @Token@ with a specific value from a given set
symOf :: MonadParse TokenSeq m => [String] -> m String
symOf ss = expectingMulti (Pretty.backticks . text <$> ss) $ nextMap \case
    TSymbol s | s `elem` ss -> Just s
    _ -> Nothing

-- | Try and parse a symbol @Token@, returning a boolean indicating success
trySym :: MonadParse TokenSeq m => String -> m Bool
trySym s = peek >>= \case
    TSymbol s' | s == s' -> True <$ advance
    _ -> pure False


-- | Expect a given @Parser@ to be surrounded with parentheses
parens :: MonadParse TokenSeq m => m a -> m a
parens px = do
    body <- nextMap \case
        TTree BkParen ts -> Just ts
        _ -> Nothing
    noFail (recurseParser px body)

-- | Expect a given @Parser@ to be surrounded with braces
braces :: MonadParse TokenSeq m => m a -> m a
braces px = do
    body <- nextMap \case
        TTree BkBrace ts -> Just ts
        _ -> Nothing
    noFail (recurseParser px body)

-- | Expect a given @Parser@ to be surrounded with brackets
brackets :: MonadParse TokenSeq m => m a -> m a
brackets px = do
    body <- nextMap \case
        TTree BkBracket ts -> Just ts
        _ -> Nothing
    noFail (recurseParser px body)
