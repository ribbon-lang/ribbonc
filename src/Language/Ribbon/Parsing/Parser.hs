{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Ribbon.Parsing.Parser where

import Data.Function

import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence qualified as Seq
import Data.Foldable qualified as Fold
import Data.List qualified as List

import Data.Word (Word32)

import Data.Nil
import Data.Tag
import Data.Attr
import Data.Diagnostic
import Data.SyntaxError

import Control.Has
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Dynamic
import Control.Monad.State.Dynamic
import Control.Monad.Error.Dynamic

import Text.Pretty hiding (parens, brackets, backticks, braces, cat)
import Text.Pretty qualified as Pretty

import Language.Ribbon.Util
import Language.Ribbon.Syntax.Raw
import Language.Ribbon.Syntax.Ref
import Language.Ribbon.Syntax.Scheme
import Language.Ribbon.Syntax.Kind
import Language.Ribbon.Syntax.Module qualified as M
import Language.Ribbon.Parsing.Monad
import Language.Ribbon.Parsing.Lexer qualified as L
import Language.Ribbon.Lexical
import Language.Ribbon.Analysis




-- | Marker type for @Has@ ie @Has m '[Parse]@ ~ @MonadParser TokenSeq m@
data Parse

type instance Has m (Parse ': effs) = (MonadParser TokenSeq m, Has m effs)

instance ParseInput TokenSeq where
    type InputElement TokenSeq = Token
    formatInput fp ts = Tag (attrInput fp ts) case ts of
        (t :@: _) Seq.:<| _ -> UnexpectedFailure (inputPretty t)
        _ -> EofFailure
    unconsInput = \case
        (t :@: _) Seq.:<| ts -> Right (t, ts)
        _ -> Left DecodeEof
    attrInput fp ts = case ts of
        t Seq.:<| _ -> t.tag
        _ -> Attr fp Nil
    attrInputDiff fp ts ts' = attrFold (fileAttr fp) do
        Seq.take (Seq.length ts - Seq.length ts') ts




moduleHead :: Has m '[Parse] => m RawModuleHeader
moduleHead = expecting "a valid module header" do
    moduleName <- sym "module" >> tag string
    moduleVersion <- simpleNameOf "@" >> tag version

    (sources, dependencies, meta) <- do
        option mempty grabWhitespaceDomain
            >>= recurseParserAll keyPairs
            >>= processPairs

    pure $ RawModuleHeader
        { name = moduleName
        , version = moduleVersion
        , sources = sources.value
        , dependencies = dependencies.value
        , meta = meta
        }
    where
    keyPairs = grabLines >>= mapM do
        recurseParserAll do
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
        recurseParserAll (wsListBody True (sym ",") (tag string)) toks

    processDependencies at existing toks = do
        assertAt at (null existing.value) Unrecoverable $
            hang "multiple `dependencies` fields in module head"
                $ "original is here:" <+> pPrint (tagOf existing)
        recurseParserAll (wsListBody True (sym ",") dependency) toks

    processMeta key meta toks = do
        assertAt key.tag (Map.notMember key meta) Unrecoverable $
            hang ("multiple" <+> backticked key <+> "fields in module head")
                $ "original is here:" <+> do
                    pPrint $ tagOf $ Maybe.fromJust $
                        Fold.find (== key) (Map.keys meta)
        val <- recurseParserAll (tag string) toks
        pure (Map.insert key val meta)

    dependency = do
        moduleName <- tag string
        moduleVersion <- simpleNameOf "@" >> tag version
        alias <- optional (sym "as" >> noFail (tag simpleName))
        pure (moduleName, moduleVersion, alias)

liftError :: forall e m a. Has m [ Err Doc, With '[Pretty e] ] => ErrorT e m a -> m a
liftError m =
    runErrorT m >>= liftEitherMap pPrint


file :: Has m [ ModuleId, Diag, Err Doc, OS ] =>
    ItemId -> FilePath -> m M.ParserDefs
file i filePath = snd <$>
    flip runReaderT i do
    flip execStateT (i + 1, Nil) do
        flip runReaderT filePath do
            L.lexStreamFromFile filePath
                >>= liftError @SyntaxError . evalParserT L.doc
                >>= liftError @SyntaxError . evalParserT grabLines
                >>= \lns -> do
                    let at = fileAttr filePath

                    (newGroup, newUnresolvedImports) <-
                        flip runStateT Nil $
                        flip execStateT Nil $
                        flip runReaderT (StringFixName filePath) $
                            namespaceBody lns

                    bindGroup i $
                        M.Def Nothing (newGroup :@: at)

                    bindImports i $
                        M.Def Nothing (newUnresolvedImports :@: at)

inNewNamespace :: Has m [ Ref, M.ParserDefs, Diag ] =>
    Attr -> StateT M.Group (StateT M.UnresolvedImports m) a -> m ItemId
inNewNamespace at' action = do
        selfId <- getItemId
        newNamespaceId <- freshItemId

        (newGroup, newUnresolvedImports) <- usingItemId newNamespaceId do
            runStateT (execStateT action Nil) Nil

        bindGroup newNamespaceId $
            M.Def (Just selfId) (newGroup :@: at')

        bindImports newNamespaceId $
            M.Def (Just selfId) (newUnresolvedImports :@: at')

        pure newNamespaceId

namespaceBody :: Has m
    [ Location, FixName
    , M.ParserDefs, Diag, M.Group, M.UnresolvedImports
    ] => [TokenSeq] -> m ()
namespaceBody lns = do
    filePath <- getFilePath
    name <- getFixName
    currentLocation <- getRef
    forM_ lns $ errorToDiagnostic @SyntaxError
        (attrFold' (fileAttr filePath) lns)
        DiagnosticBinder
            { kind = BadDefinition
            , ref = currentLocation
            , name = Just name
            }
        . evalParserT item



item :: Has m
    [ Location
    , M.ParserDefs, M.Group, M.UnresolvedImports
    , Diag, Parse
    ] => m ()
item = asum
    [ tag useDef >>= addUseDef
    ]

-- def :: Has m '[Parse] => m RawDef
-- def = asum [ startsEq, decls ] where
--     startsEq = sym "=" >> noFail do
--         grabWhitespaceDomain >>= recurseParserAll do
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
--         grabWhitespaceDomain >>= recurseParserAll do
--             (q, c) <- option mempty do
--                 sym "forall" >> noFail typeHead
--             t <- grabDomain (not . isSymbol "=" . untag)
--             option (RdDecl q c t) do
--                 sym "="
--                 RdDeclVal q c t <$> noFail grabWhitespaceDomain

-- namespaceDef :: Has m '[Parse] => m RawDef
-- namespaceDef = sym "namespace" >> noFail do
--     RdNamespace . RawNamespace <$> do
--         grabWhitespaceDomain >>= recurseParserAll (many $ tag item)

-- effectDef :: Has m '[Parse] => m RawDef
-- effectDef = sym "effect" >> noFail do
--     (q, c) <- option mempty typeHead
--     RdEffect q c . RawNamespace <$> do
--         grabWhitespaceDomain >>= recurseParserAll (many $ tag effectItem)
--     where
--     effectItem = do
--         (fixity, prec, n) <- Todo
--         sym ":"
--         RawEffectItem fixity prec n <$>
--             noFail grabWhitespaceDomain

-- classDef :: Has m '[Parse] => m RawDef
-- classDef = sym "class" >> noFail do
--     (q, c) <- option mempty typeHead
--     RdClass q c . RawNamespace <$> do
--         grabWhitespaceDomain >>= recurseParserAll (many $ tag classItem)
--     where
--     classItem = do
--         (fixity, prec, n) <- Todo
--         sym ":"
--         grabWhitespaceDomain >>= recurseParserAll do
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

-- instanceDef :: Has m '[Parse] => m RawDef
-- instanceDef = sym "instance" >> noFail do
--     (q, c) <- option mempty typeHead
--     RdInstance q c . RawNamespace <$> do
--         grabWhitespaceDomain >>= recurseParserAll (many $ tag instanceItem)
--     where
--     instanceItem = do
--         sym "="
--         (fixity, prec, n) <- Todo
--         grabWhitespaceDomain >>= recurseParserAll do
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

-- typeAliasDef :: Has m '[Parse] => m RawDef
-- typeAliasDef = sym "type" >> noFail do
--     liftA2 RdTypeAlias
--         do option mempty (typeHeadOf quantifier)
--         do noFail grabWhitespaceDomain

-- structDef :: Has m '[Parse] => m RawDef
-- structDef = sym "struct" >> noFail do
--     liftA2 RdStruct
--         do option mempty (typeHeadOf quantifier)
--         do RawNamespace <$> noFail do
--             grabWhitespaceDomain >>= recurseParserAll fields

-- unionDef :: Has m '[Parse] => m RawDef
-- unionDef = sym "union" >> noFail do
--     liftA2 RdUnion
--         do option mempty (typeHeadOf quantifier)
--         do RawNamespace <$> noFail do
--             grabWhitespaceDomain >>= recurseParserAll fields

-- valueDef :: Has m '[Parse] => m RawDef
-- valueDef = RdValue <$> grabWhitespaceDomain


addUseDef :: Has m
    [ Location
    , M.ParserDefs, M.Group, M.UnresolvedImports
    , Diag
    ] => ATag (Visible RawUse) -> m ()
addUseDef (Visible vis use :@: at) =
    flip runReaderT (Nil :: Path) do
    flip runReaderT (Nil :: [ATag PathName]) do
        addUse vis (use :@: at)

addUse :: Has m
    [ Location
    , M.ParserDefs, M.Group, M.UnresolvedImports
    , Rd [ATag PathName]
    , Rd Path
    , Diag
    ] => Visibility -> ATag RawUse -> m ()
addUse vis (RawUse{..} :@: at) = do
    previousPath <- ask @Path
    maybe
        do reportErrorRef at BadDefinition ((.name.value) <$> alias) $
            hang "cannot combine paths in compound use:" do
                backticked previousPath <+> "and" <+> backticked basePath
        (useBody . (<$ basePath))
        (joinPath previousPath basePath.value)
    where
    useBody fullPath = case alias of
        Just newName ->
            flip runReaderT newName.name.value case makeNewName fullPath newName of
                Just unresolvedName -> case tree.value of
                    RawUseSingle ->
                        insertAlias (Visible vis unresolvedName) fullPath

                    -- FIXME: code duplication
                    RawUseBlob hidden ->
                        let newFullPath = Maybe.fromJust $
                                joinPath
                                    (Path (Just $ PbUp 1 :@: basePath.tag) Nil)
                                    fullPath.value
                        in asNewNamespace unresolvedName do
                            addUseBlob (newFullPath <$ basePath) hidden

                    RawUseBranch subs ->
                        let newFullPath = Maybe.fromJust $
                                joinPath
                                    (Path (Just $ PbUp 1 :@: basePath.tag) Nil)
                                    fullPath.value
                        in asNewNamespace unresolvedName do
                            addUseBranch newFullPath subs

                _ -> getFixName >>= reportInvalidCombo . Just

        _ -> case tree.value of
            RawUseBranch subs -> addUseBranch fullPath.value subs

            RawUseBlob hidden -> addUseBlob fullPath hidden

            RawUseSingle -> case getPathName fullPath.value of
                Just name ->
                    let category = getPathCategory fullPath.value
                        unresolvedName = UnresolvedName category Nothing name
                    in insertAlias (Visible vis unresolvedName) fullPath

                _ -> reportInvalidCombo Nothing

    addUseBlob :: Has m
        [ Location
        , M.ParserDefs, M.Group, M.UnresolvedImports
        , Rd [ATag PathName]
        , Diag
        ] => ATag Path -> [ATag PathName] -> m ()
    addUseBlob fullPath explicit = do
        context <- ask
        insertBlob (Visible vis fullPath)
            if isNil basePath.value
                then context <> explicit
                else explicit

    addUseBranch :: Has m
        [ Location
        , M.ParserDefs, M.Group, M.UnresolvedImports
        , Rd [ATag PathName]
        , Rd Path
        , Diag
        ] => Path -> [ATag RawUse] -> m ()
    addUseBranch fullPath subs = do
        let (blobs, rest) = subs & List.partition \u ->
                case u.value.tree.value of
                    RawUseBlob _ -> True
                    _ -> False

        -- TODO: should this report overlapping blobs?
        -- it should get caught in the analysis phase anyways...

        using fullPath do
            names <- Maybe.catMaybes <$> forM rest \u ->
                hidablePathNameFromUse u.value <$ addUse vis u

            using names (forM_ blobs $ addUse vis)



    asNewNamespace unresolvedName action =
        case groupFromUnresolvedInCategory Namespace unresolvedName of
            Just groupName -> do
                newId <- inNewNamespace
                    unresolvedName.name.tag
                    action
                modId <- getModuleId
                insertRef (Visible Public groupName) (Ref modId newId)
            _ -> getFixName >>= reportInvalidCombo . Just


    makeNewName fullPath =
        unresolvedFromQualified case tree.value of
            RawUseSingle -> getPathCategory fullPath.value
            _ -> Just ONamespace

    reportInvalidCombo :: Has m '[Ref, Diag] => Maybe FixName -> m ()
    reportInvalidCombo referenceName = do
        reportErrorRefH
            basePath.tag
            BadDefinition
            referenceName
            (text "this use has an invalid alias & path or extension combination")
            [ hang "potential problems:" $ bulletList
                [ "you are aliasing a namespace or instance as a"
                    <+> "non-atomic operator"
                , "you have not provided an alias, but the tail of the path"
                    <+> "cannot be used as a symbol,"
                    <+> "such as certain file names"
                ]
            ]


useDef :: Has m '[Parse] => m (Visible RawUse)
useDef = sym "use" >> noFail do visible use where
    use = do
        optional (tag path) >>= \case
            Just p -> do
                liftA2 (RawUse p)
                    do option (RawUseSingle :@: p.tag)
                        if requiresSlash p
                            then do
                                at <- attrOf $ connected p.tag (simpleNameOf "/")
                                noFail (connected at $ tag useTree)
                            else connected p.tag $ tag useTree
                    do alias
            _ -> liftA3 RawUse
                do Path Nothing Nil <@> attr
                do tag useTree
                do alias
    useTree = asum
        [ RawUseBlob <$> do
            sym ".." >> option [] do
                sym "hiding" >> asum
                    [ braces $ listMany (sym ",") (tag pathName)
                    , pure <$> tag pathName
                    ]
        , RawUseBranch <$> braces do
            wsListBody True (sym ",") (tag use)
        ]

    alias = optional $ sym "as" >> noFail qualifiedName


path :: Has m '[Parse] => m Path
path = nextMap \case
    TPath p -> Just p
    _ -> Nothing

pathName :: Has m '[Parse] => m PathName
pathName = nextMap \case
    TPath (SingleNamePath n) -> Just n
    _ -> Nothing

fixName :: Has m '[Parse] => m FixName
fixName = expecting "a fix name" $ nextMap \case
    TPath (SingleNamePath (PathName Nothing f)) -> Just f
    _ -> Nothing

simpleName :: Has m '[Parse] => m SimpleName
simpleName = expecting "an unreserved identifier or operator" $ nextMap \case
    TPath (SingleSimplePath n) -> Just n
    _ -> Nothing

simpleNameOf :: Has m '[Parse] => String -> m ()
simpleNameOf s = expecting (Pretty.backticks $ text s) $ nextIf_ \case
    TPath (SingleSimplePath (SimpleName n)) -> n == s
    _ -> False


visible :: Has m '[Parse] => m a -> m (Visible a)
visible = liftA2 Visible (option Private visibility)

visibility :: Has m '[Parse] => m Visibility
visibility = Public <$ sym "pub"


-- overloadCategory :: Has m '[Parse] => m OverloadCategory
-- overloadCategory = asum
--     [ ONamespace <$ sym "namespace"
--     , OInstance <$ sym "instance"
--     , OType <$ sym "type"
--     , OValue <$ sym "value"
--     ]

qualifiedName :: Has m '[Parse] =>
    m QualifiedName
qualifiedName = do
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
            Nothing -> pure $
                QualifiedName NonAssociative (defaultPrecedence (getFixity n)) n
            Just aptLr -> case getFixity n of
                Atom -> do
                    parseErrorAt apt.tag Unrecoverable
                        "atom names do not have precedence;"
                fx -> case (aptLr, fx) of
                    (Left _, Prefix) -> do
                        parseErrorAt apt.tag Unrecoverable
                            "prefix names expect precedence on the right"
                    (Right apt', Prefix) ->
                        finishPfx apt.tag Prefix apt' n
                    (Right _, Postfix) -> do
                        parseErrorAt apt.tag Unrecoverable
                            "postfix names expect precedence on the left"
                    (Left apt', Postfix) ->
                        finishPfx apt.tag Postfix apt' n
                    (Left apt', Infix) ->
                        finishIfx LeftAssociative apt' n
                    (Right apt', Infix) ->
                        finishIfx RightAssociative apt' n
    where
    finishIfx lr apt n = pure $
        QualifiedName (select (fst apt) lr NonAssociative) (snd apt) n
    finishPfx at fx apt n = do
        unless (fst apt) $ parseErrorAt at Unrecoverable $
            pPrint fx <+> "names are always non-associative;"
                <+> "parens are not allowed"
        pure $
            QualifiedName NonAssociative (snd apt) n


associativePrecedence :: Has m '[Parse] => m (Bool, Precedence)
associativePrecedence = asum
    [ (False, ) <$> parens precedence
    , (True, ) <$> precedence
    ]

precedence :: Has m '[Parse] => m Precedence
precedence = do
    i :@: at <- tag int
    assertAt at (i <= bound) Unrecoverable $
        "precedence level" <+> backticked i
            <+> "is out of range, max is " <+> shown bound
    pure (fromIntegral i) where
    bound = fromIntegral @Precedence @Word32 maxBound

-- fields :: Has m '[Parse] => m [ATag RawField]
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


-- typeHead :: Has m '[Parse] => m (Quantifier, RawQualifier)
-- typeHead = typeHeadOf $ liftA2 (,)
--     do option mempty quantifier
--     do option mempty qualifier

-- typeHeadOf :: Nil a => Parser a -> Parser a
-- typeHeadOf p = do
--     r :@: at <- tag $ option Nil p
--     guard (not (isNil r))
--     r <$ expectingAt at "to close type head" (sym "=>")



quantifier :: Has m '[Parse] => m Quantifier
quantifier = flip evalStateT 0 do
    Quantifier <$> listSome (lift $ sym ",") (tag typeBinder)

qualifier :: Has m '[Parse] => m (Qualifier TokenSeq)
qualifier = sym "where" >> noFail do
    Qualifier . pure <$> tag do
        grabDomain (not . isSymbol "=>" . untag)

typeBinder :: Has m '[Parse] => StateT KindVar m TypeBinder
typeBinder = do
    n <- tag simpleName
    k <- optional $ lift do
        sym ":" >> tag kind
    case k of
        Just k' -> pure (n `Of` k')
        _ -> do
            k' <- state \i -> (i, i + 1)
            pure $ n `Of` (KVar k' :@: n.tag)

kind :: Has m '[Parse] => m Kind
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




grabDomain :: Has m '[Parse] => (ATag Token -> Bool) -> m TokenSeq
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
        x@(t Seq.:<| ts) | p t ->
            case untag t of
                TTree BkLine ts'
                    | recursed -> block BkLine ts'
                    | otherwise -> (Nil, x)

                TTree BkIndent ts' -> block BkIndent ts'
                _ -> recurse
            where
            recurse =
                let (as, bs) = consume recursed ts
                in (t Seq.<| as, bs)

            block k ts' =
                let (as, bs) = consume True ts'
                in buildSplit k t ts as bs
                if recursed
                    then recurse
                    else (Seq.singleton t, Nil)

        ts -> (Nil, ts)

    buildSplit k t rhs as bs nb
        | Seq.null bs = nb
        | Seq.null as = (Nil, t Seq.<| rhs)
        | otherwise =
            ( Seq.singleton (TTree k as :@: t.tag)
            , (TTree k bs :@: t.tag) Seq.<| rhs
            )

-- | Get the rest of the input delimited by indentation
grabWhitespaceDomain :: Has m '[Parse] => m TokenSeq
grabWhitespaceDomain = grabDomain (const True)

-- | Consume a sequence of lines from the input
grabLines :: Has m '[Parse] => m [TokenSeq]
grabLines = do
    some $ nextMap \case
        TTree BkLine lns -> Just lns
        _ -> Nothing

-- | @wsBlock<elem>++(sep?) | elem (sep elem)*@
wsListBody :: Has m '[Parse] => Bool -> m sep -> m a -> m [a]
wsListBody allowTrailing ms ma = asum [block, inline] where
    inline = listSome ms ma
    block = do
        lns <- grabLines
        noFail $ asum [block1 lns, block2 lns]
    block1 lns =
        lns & foldWithM' mempty \toks as ->
            (: as) <$> recurseParserAll
                do ma << when (allowTrailing || null as) (option () $ void ms)
                toks
    block2 =
        flip Fold.foldlM mempty \as toks ->
            (as <>) . pure <$> recurseParserAll
                do if null as
                    then when allowTrailing (option () $ void ms) >> ma
                    else ms >> ma
                toks



-- | Expect a @Version@
version :: Has m '[Parse] => m Version
version = expecting "a version" $ nextMap \case
    TVersion v -> Just v
    _ -> Nothing

-- | Expect a @Literal@
literal :: Has m '[Parse] => m Literal
literal = expecting "literal" $ nextMap \case
    TLiteral lit -> Just lit
    _ -> Nothing

-- | Expect a @Literal@ of @Char@
char :: Has m '[Parse] => m Char
char = expecting "character literal" $ nextMap \case
    TLiteral (LChar c) -> Just c
    _ -> Nothing

-- | Expect a @Literal@ of @Int@
int :: Has m '[Parse] => m Word32
int = expecting "integer literal" $ nextMap \case
    TLiteral (LInt i) -> Just i
    _ -> Nothing

-- | Expect a @Literal@ of @Float@
float :: Has m '[Parse] => m Float
float = expecting "float literal" $ nextMap \case
    TLiteral (LFloat f) -> Just f
    _ -> Nothing

-- | Expect a @Literal@ of @String@
string :: Has m '[Parse] => m String
string = expecting "string literal" $ nextMap \case
    TLiteral (LString s) -> Just s
    _ -> Nothing

-- | Expect any symbol @Token@
anySym :: Has m '[Parse] => m String
anySym = expecting "a symbol" $ nextMap \case
    TSymbol s -> Just s
    _ -> Nothing

-- | Expect a symbol @Token@ that is not reserved
unreserved :: Has m '[Parse] => m String
unreserved = expecting "an unreserved symbol" $ nextMap \case
    tk@(TSymbol s) | not (isReserved tk) -> Just s
    _ -> Nothing

-- | Expect a symbol @Token@ with a specific value
sym :: Has m '[Parse] => String -> m ()
sym s = expecting (Pretty.backticks $ text s) $ nextMap \case
    TSymbol s' | s == s' -> Just ()
    _ -> Nothing

-- | Expect a symbol @Token@ with a specific value from a given set
symOf :: Has m '[Parse] => [String] -> m String
symOf ss = expectingMulti (Pretty.backticks . text <$> ss) $ nextMap \case
    TSymbol s | s `elem` ss -> Just s
    _ -> Nothing

-- | Try and parse a symbol @Token@, returning a boolean indicating success
trySym :: Has m '[Parse] => String -> m Bool
trySym s = peek >>= \case
    TSymbol s' | s == s' -> True <$ advance
    _ -> pure False


-- | Expect a given @Parser@ to be surrounded with parentheses
parens :: Has m '[Parse] => m a -> m a
parens px = do
    body <- expecting "`(`" $ nextMap \case
        TTree BkParen ts -> Just ts
        _ -> Nothing
    noFail (recurseParserAll px body)

-- | Expect a given @Parser@ to be surrounded with braces
braces :: Has m '[Parse] => m a -> m a
braces px = do
    body <- expecting "`{`" $ nextMap \case
        TTree BkBrace ts -> Just ts
        _ -> Nothing
    noFail (recurseParserAll px body)

-- | Expect a given @Parser@ to be surrounded with brackets
brackets :: Has m '[Parse] => m a -> m a
brackets px = do
    body <- expecting "`[`" $ nextMap \case
        TTree BkBracket ts -> Just ts
        _ -> Nothing
    noFail (recurseParserAll px body)
