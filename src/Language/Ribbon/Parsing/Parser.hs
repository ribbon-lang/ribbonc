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
import Language.Ribbon.Analysis.Builder
import Language.Ribbon.Analysis.Context
import Language.Ribbon.Analysis.Diagnostics
import Language.Ribbon.Syntax.Type
import Language.Ribbon.Syntax.Data
import Data.Functor





-- | Marker type for @Has@
--   ie @Has m '[Parse]@ ~ @MonadParser (ATag TokenSeq) m@
data Parse

type instance Has m (Parse ': effs) =
    (MonadParser (ATag TokenSeq) m, Has m effs)

instance ParseInput (ATag TokenSeq) where
    type InputElement (ATag TokenSeq) = Token
    formatInput fp ts = Tag (attrInput fp ts) case untag ts of
        t :@: _ Seq.:<| _ -> UnexpectedFailure (inputPretty t $+$ pPrint ts)
        _ -> EofFailure
    unconsInput = \case
        (t :@: _ Seq.:<| ts) :@: at -> Right (t, ts :@: at)
        _ -> Left DecodeEof
    attrInput _ ts = case untag ts of
        t Seq.:<| _-> t.tag
        _ -> attrFlattenToEnd ts.tag
    attrInputDiff fp ts ts' =
        let tx = Seq.take (Seq.length ts.value - Seq.length ts'.value) ts.value
        in if notNil tx
            then attrFold (fileAttr fp) tx
            else attrFlattenToStart ts'.tag




-- | Parse a @RawModuleHeader@, and return the rest of the file as a line list
moduleHead :: Has m '[Parse] => m (ATag RawModuleHeader, [ATag TokenSeq])
moduleHead = expecting "a valid module header" do
    grabLines >>= \case
        [] -> empty
        (ln : lns) -> (, lns) <$> flip recurseParserAll ln do
            moduleName <- sym "module" >> tag string
            moduleVersion <- simpleNameOf "@" >> tag version

            (sources, dependencies, meta) <- do
                option mempty grabWhitespaceDomain
                    >>= recurseParserAll keyPairs
                    >>= processPairs

            pure $ Tag ln.tag RawModuleHeader
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
        foldWithM' mempty \(k, toks) (sources, dependencies, meta) ->
            case k.value.value of
                "sources" ->
                    (, dependencies, meta) . Tag toks.tag <$>
                        processSources (tagOf k) sources toks

                "dependencies" ->
                    (sources, , meta) . Tag toks.tag <$>
                        processDependencies (tagOf k) dependencies toks

                _ -> (sources, dependencies, ) <$> processMeta k meta toks

    processSources at existing toks = do
        assertAt at (null existing.value) Unrecoverable $
            hang "multiple `sources` fields in module head"
                $ "original is here:" <+> pPrint (tagOf existing)
        recurseParserAll (wsList True "," (tag string)) toks

    processDependencies at existing toks = do
        assertAt at (null existing.value) Unrecoverable $
            hang "multiple `dependencies` fields in module head"
                $ "original is here:" <+> pPrint (tagOf existing)
        recurseParserAll (wsList True "," dependency) toks

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
        pure ( (moduleName.value, moduleVersion.value)
                :@: (moduleName.tag <> moduleVersion.tag)
             , alias
             )

-- | Parse a source file
sourceFile :: Has m [ ModuleId, Diag, Err SyntaxError, OS ] =>
    ItemId -> FilePath -> m (M.ParserDefs, M.UnresolvedImportMap)
sourceFile i filePath =
    runReaderT' filePath do
        L.lexStreamFromFile filePath
            >>= evalParserT (tag L.doc)
            >>= evalParserT grabLines
            >>= sourceFileBody i

sourceFileBody :: Has m
    [ ModuleId, FilePath, Diag, OS ] =>
    ItemId -> [ATag TokenSeq] -> m (M.ParserDefs, M.UnresolvedImportMap)
sourceFileBody itemId lns = defsStateExtract <$> do
    filePath <- getFilePath
    runReaderT' (StringFixName filePath) do
        let at = fileAttr filePath

        runReaderT' itemId $ execStateT' (freshDefsState itemId) do
            (newGroup, newUnresolvedImports) <-
                runStateT' (Nil :: M.UnresolvedImports) $
                execStateT' (Nil :: M.Group) $
                    lineParser item lns

            unless (isNil newGroup) do
                bindGroup itemId (newGroup :@: at)

            unless (isNil newUnresolvedImports) do
                bindImports itemId (newUnresolvedImports :@: at)

            bindCategory itemId Namespace


-- | Parse a top-level item declaration or definition
item :: Has m
    [ Location, FixName
    , M.ParserDefs, M.Group, M.UnresolvedImports
    , Diag, Parse
    ] => m ()
item = option Private visibility >>= \v -> asum
    [ useDef v
    , do
        c <- option Value category
        n <- qualifiedName
        let vqn = Visible v n
        usingFixName n.value.value $ liftedSyntaxErrorRefName BadDefinition do
            parentItemId <- getItemId
            newItemId <- optionalIndent case c of
                Value -> asum
                    [ do
                        newDeclId <- optionalIndent do
                            sym ":"
                            noFail $ valueDec vqn

                        option () $ asum
                            [ consumesAll $ grabLines >>= \case
                                [] -> empty
                                ln1 : lns -> do
                                    ln1' <- snd <$> recurseParser (sym "=") ln1

                                    bindValue newDeclId $
                                        foldWith Nil (ln1' : lns)
                                        \ln (acc :@: at) ->
                                            ((TTree BkWhitespace <$> ln)
                                                Seq.:<| acc)
                                                    :@: (ln.tag <> at)

                            , optionalIndent $ sym "=" >> noFail do
                                grabWhitespaceDomainAll >>=
                                    bindValue newDeclId
                            ]

                        pure newDeclId

                    , valueDef vqn
                    ]

                _ -> asum
                    [ case c of
                        Namespace -> namespaceDef vqn
                        Effect -> effectDef vqn
                        Class -> classDef vqn
                        Instance -> instanceDef vqn
                        Alias -> typeDef vqn
                        Struct -> structDef vqn
                        Union -> unionDef vqn
                        _ -> error "item: unexpected category"
                    , freshItemId
                    ]
            bindParent newItemId parentItemId
    ]




-- | Parse a use declaration
--   @ "use" ... @
useDef :: Has m
    [ Location, M.ParserDefs, M.Group, M.UnresolvedImports
    , Parse
    , Diag
    ] => Visibility -> m ()
useDef vis = sym "use" >> do
    liftingSyntaxErrorRef BadDefinition Nothing
        (tag parseUseDef)
        discardParseState
        \(use :@: at) ->
            runReaderT' (Nil :: Path) $
            runReaderT' (Nil :: [ATag FixName]) $
                addUse (use :@: at)
    where
    parseUseDef = noFail (consumesAll use) where
        use = optional (tag path) >>= \case
            Just p -> do
                liftA2 (RawUse p)
                    do option (RawUseSingle :@: p.tag)
                        if requiresSlash p
                            then do
                                at <- attrOf do
                                    connected p.tag (simpleNameOf "/")
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
                        [ braces $ wsList True "," (tag fixName)
                        , pure <$> tag fixName
                        ]
            , RawUseBranch <$> braces do
                wsList True "," (tag use)
            ]

        alias = optional $ sym "as" >> noFail qualifiedName

    addUse :: Has m
        [ Location
        , M.ParserDefs, M.Group, M.UnresolvedImports
        , Rd [ATag FixName]
        , Rd Path
        , Diag
        ] => ATag RawUse -> m ()
    addUse (RawUse{..} :@: at) = do
        previousPath <- ask @Path
        maybe
            do reportErrorRef at
                BadDefinition (prettyShow . (.value.value) <$> alias) $
                hang "cannot combine paths in compound use:" do
                    backticked previousPath <+> "and" <+> backticked basePath
            (useBody . (<$ basePath))
            (joinPath previousPath basePath.value)
        where
        useBody fullPath = case alias of
            Just newName ->
                runReaderT' newName.value.value
                case unresolvedFromQualified newName of
                    Just unresolvedName -> case tree.value of
                        RawUseSingle ->
                            insertAlias $ Visible vis $
                                M.UnresolvedAlias unresolvedName fullPath

                        RawUseBlob hidden ->
                            asNewNamespace fullPath unresolvedName
                                (`addUseBlob` hidden)

                        RawUseBranch subs ->
                            asNewNamespace fullPath unresolvedName
                                (`addUseBranch` subs)

                    _ -> getFixName >>= reportInvalidCombo . Just

            _ -> case tree.value of
                RawUseBranch subs -> addUseBranch fullPath subs

                RawUseBlob hidden -> addUseBlob fullPath hidden

                RawUseSingle -> case getPathName fullPath.value of
                    Just name ->
                        let unresolvedName =
                                UnresolvedName Nothing name
                        in insertAlias $ Visible vis $
                            M.UnresolvedAlias unresolvedName fullPath

                    _ -> reportInvalidCombo Nothing

        addUseBlob :: Has m
            [ Location
            , M.ParserDefs, M.Group, M.UnresolvedImports
            , Rd [ATag FixName]
            , Diag
            ] => ATag Path -> [ATag FixName] -> m ()
        addUseBlob fullPath explicit = do
            context <- ask
            insertBlob $ Visible vis $
                M.UnresolvedBlob
                    fullPath
                    if isNil basePath.value
                        then context <> explicit
                        else explicit

        addUseBranch :: Has m
            [ Location
            , M.ParserDefs, M.Group, M.UnresolvedImports
            , Rd [ATag FixName]
            , Rd Path
            , Diag
            ] => ATag Path -> [ATag RawUse] -> m ()
        addUseBranch fullPath subs = do
            let (blobs, rest) = subs & List.partition \u ->
                    case u.value.tree.value of
                        RawUseBlob _ -> True
                        _ -> False

            using fullPath.value do
                names <- Maybe.catMaybes <$> forM rest \u ->
                    hidableFixNameFromUse u.value <$ addUse u

                using names (forM_ blobs addUse)


        asNewNamespace fullPath unresolvedName action =
            let newFullPath = Maybe.fromJust $
                    joinPath
                        (Path (Just $ PbUp 1 :@: basePath.tag) Nil)
                        fullPath.value
            in case groupFromUnresolvedInCategory Namespace unresolvedName of
                Just groupName -> do
                    newNamespaceId <- inNewNamespace unresolvedName.value.tag do
                        action (newFullPath <$ basePath)
                    Ref modId parentItemId <- getRef
                    insertRef (Visible Public $
                        M.GroupBinding groupName (Ref modId newNamespaceId))
                    bindParent newNamespaceId parentItemId
                    bindCategory newNamespaceId Namespace
                _ -> getFixName >>= reportInvalidCombo . Just


        reportInvalidCombo :: Has m '[Ref, Diag] => Maybe FixName -> m ()
        reportInvalidCombo referenceName = do
            reportErrorRefH basePath.tag BadDefinition
                (prettyShow <$> referenceName)
                (text "this use has an invalid alias &"
                    <+> "path or extension combination")
                [hang "potential problems:" $ bulletList
                    [ "you are aliasing a namespace or instance as a"
                        <+> "non-atomic operator"
                    , "you have not provided an alias, but the tail of the path"
                        <+> "cannot be used as a symbol,"
                        <+> "such as certain file names"
                    ]]


-- | Parse a namespace definition
namespaceDef :: Has m
    [ Location, FixName
    , M.ParserDefs, M.Group, M.UnresolvedImports
    , Parse
    , Diag
    ] => Visible QualifiedName -> m ItemId
namespaceDef vqn = sym "=" >> noFail do
    diagAssertRefNameH (isSimpleQualifiedName vqn.value)
        vqn.value.value.tag BadDefinition
        (text "namespace definitions must be bound to simple names")
        [backticked vqn <+> "is not a simple name, it should be more like"
            <+> backticked (simplifyFixName vqn.value.value.value)]

    lns <- pTracedM "namespaceLns" do
        tryGrabBlock vqn.value.value.tag

    insertNew vqn do
        inNewNamespace lns.tag do
            lineParser item lns.value

-- | Parse an effect type definition
effectDef :: Has m
    [ Location, FixName
    , M.ParserDefs, M.Group, M.UnresolvedImports
    , Parse
    , Diag
    ] => Visible QualifiedName -> m ItemId
effectDef vqn = noFail do
    (q, c) <- grabDomain (not . isSymbol "=" . untag)
        >>= recurseParserAll (typeHeadNoDelim False)
    optionalIndent (sym "=")

    lns <- tryGrabBlock vqn.value.value.tag

    newGroupId <- insertNew vqn $ inNewGroup lns.tag do
        lineParser caseDef lns.value

    unless (isNil q) (bindQuantifier newGroupId q)
    unless (isNil c) (bindQualifier newGroupId c)

    pure newGroupId
    where
    caseDef = noFail do
        qn <- qualifiedName
        usingFixName qn.value.value $ liftedSyntaxErrorRefName BadDefinition do
            sym ":"
            body <- tag userType
            newCaseId <- insertNew (Visible Public qn) do
                withFreshItemId (withParent bindType) body
            bindCategory newCaseId Case


-- | Parse a type class definition
classDef :: Has m
    [ Location, FixName
    , M.ParserDefs, M.Group, M.UnresolvedImports
    , Parse
    , Diag
    ] => Visible QualifiedName -> m ItemId
classDef vqn = noFail do
    (q, c) <- grabDomain (not . isSymbol "=" . untag)
        >>= recurseParserAll (typeHeadNoDelim False)
    optionalIndent (sym "=")

    lns <- tryGrabBlock vqn.value.value.tag

    newGroupId <- insertNew vqn $ inNewGroup lns.tag do
        lineParser classItem lns.value

    unless (isNil q) (bindQuantifier newGroupId q)
    unless (isNil c) (bindQualifier newGroupId c)

    pure newGroupId
    where
    classItem = noFail do
        c <- option Decl (Alias <$ sym "type")
        qn <- qualifiedName

        usingFixName qn.value.value $ liftedSyntaxErrorRefName BadDefinition do
            asum [grabWhitespaceDomainAll, (Nil :@: qn.value.tag) <$ eof]
                >>= recurseParserAll case c of
                    Alias -> classType qn
                    _ -> classValue qn

    classType qn = noFail do
        (q, c) <- consumesAll $ typeHeadNoDelim False
        newAliasId <- insertNew (Visible Public qn) freshItemId
        unless (isNil q) (bindQuantifier newAliasId q)
        unless (isNil c) (bindQualifier newAliasId c)
        bindCategory newAliasId Alias

    classValue qn = sym ":" >> noFail do
        (q, c) <- option Nil (forTypeHead True)
        body <- tag userType
        newDeclId <- insertNew (Visible Public qn) do
            withFreshItemId (withParent bindType) body
        unless (isNil q) (bindQuantifier newDeclId q)
        unless (isNil c) (bindQualifier newDeclId c)
        bindCategory newDeclId Decl


-- | Parse a class instance definition
instanceDef :: Has m
    [ Location, FixName
    , M.ParserDefs, M.Group, M.UnresolvedImports
    , Parse
    , Diag
    ] => Visible QualifiedName -> m ItemId
instanceDef vqn = noFail do
    (q, c) <- grabDomain (not . isSymbol "for" . untag)
        >>= recurseParserAll (typeHeadNoDelim False)
    optionalIndent (sym "for")
    for <- tag userType
    optionalIndent (sym "=")

    lns <- tryGrabBlock vqn.value.value.tag

    newGroupId <- insertNew vqn $ inNewGroup lns.tag do
        lineParser instanceItem lns.value

    unless (isNil q) (bindQuantifier newGroupId q)
    unless (isNil c) (bindQualifier newGroupId c)
    bindType newGroupId for
    pure newGroupId
    where
    instanceItem = noFail do
        c <- option Value (Alias <$ sym "type")
        qn <- qualifiedName
        usingFixName qn.value.value $ liftedSyntaxErrorRefName BadDefinition do
            grabWhitespaceDomainAll >>= recurseParserAll case c of
                Alias -> instanceType qn
                _ -> sym "=" >> instanceValue qn

    instanceType qn = noFail do
        q <- typeQuantifierEq False
        body <- tag userType
        newAliasId <- insertNew (Visible Public qn) do
            withFreshItemId (withParent bindType) body
        unless (isNil q) (bindQuantifier newAliasId q)
        bindCategory newAliasId Alias

    instanceValue qn = noFail do
        body <- grabWhitespaceDomainAll
        newValueId <- insertNew (Visible Public qn) do
            withFreshItemId (withParent bindValue) body
        bindCategory newValueId Value


-- | Parse a type alias definition
typeDef :: Has m
    [ Location, FixName
    , M.ParserDefs, M.Group, M.UnresolvedImports
    , Parse
    , Diag
    ] => Visible QualifiedName -> m ItemId
typeDef vqn = noFail do
    q <- typeQuantifierEq False
    body <- tag userType
    newAliasId <- insertNew vqn do
        withFreshItemId (withParent bindType) body
    unless (isNil q) (bindQuantifier newAliasId q)
    pure newAliasId


-- | Parse a struct type definition
structDef :: Has m
    [ Location, FixName
    , M.ParserDefs, M.Group, M.UnresolvedImports
    , Parse
    , Diag
    ] => Visible QualifiedName -> m ItemId
structDef vqn = noFail do
    q <- typeQuantifierEq False

    lns <- tryWsListBody vqn.value.value.tag True ","

    newGroupId <- insertNew vqn $ inNewGroup lns.tag do
        (named, diag) <- runWriterT do
            fields (bindingField Projection namedField) lns.value
        if any isError diag
            then fields (bindingField Projection tupleField) lns.value
            else named <$ reportAll diag

    unless (isNil q) (bindQuantifier newGroupId q)
    pure newGroupId
    where
    namedField idx = do
        n <- tag simpleName
        sym ":"
        (idx :@: n.tag, n, )
            <$> tag userType

    tupleField idx = do
        body <- tag userType
        pure
            ( idx :@: body.tag
            , SimpleName (show idx) :@: body.tag
            , body
            )


-- | Parse a union type definition
unionDef :: Has m
    [ Location, FixName
    , M.ParserDefs, M.Group, M.UnresolvedImports
    , Parse
    , Diag
    ] => Visible QualifiedName -> m ItemId
unionDef vqn = noFail do
    q <- typeQuantifierEq False

    lns <- tryWsListBody vqn.value.value.tag True ","

    newGroupId <- insertNew vqn $ inNewGroup lns.tag do
        fields (bindingField Injection field) lns.value

    unless (isNil q) (bindQuantifier newGroupId q)
    pure newGroupId
    where
    field idx = do
        lbl <- asum [ numbered, nameOnly ]
        noFail do
            sym ":"
            lbl <$> tag userType
        where
        numbered = do
            off <- tag int
            n <- option (SimpleName (show off.value) :@: off.tag) do
                simpleNameOf "\\" >> noFail (tag simpleName)
            pure (off, n, )

        nameOnly = do
            n <- tag simpleName
            pure (idx :@: n.tag, n, )

-- | Parse a value declaration
valueDec :: Has m
    [ Location, FixName
    , M.ParserDefs, M.Group, M.UnresolvedImports
    , Parse
    , Diag
    ] => Visible QualifiedName -> m ItemId
valueDec vqn = noFail do
    header <- pTracedM ("valueDec" <+> pPrint vqn) do grabDomain (not . isSymbol "=" . untag)
    (q, c, t) <- flip recurseParserAll header do
        (q, c) <- option Nil $ sym "for" >> noFail do
            sch <- grabDomain (not . isSymbol "=>" . untag)
                >>= recurseParserAll (typeHeadNoDelim False)
            sch <$ optionalIndent (sym "=>")
        t <- tag userType
        pure (q, c, t)

    newDeclId <- insertNew vqn do
        withFreshItemId (withParent bindType) t
    unless (isNil q) (bindQuantifier newDeclId q)
    unless (isNil c) (bindQualifier newDeclId c)
    pure newDeclId

-- | Parse a value definition body
valueDef :: Has m
    [ Location, FixName
    , M.ParserDefs, M.Group, M.UnresolvedImports
    , Parse
    , Diag
    ] => Visible QualifiedName -> m ItemId
valueDef vqn = noFail do
    body <- grabWhitespaceDomainAll
    insertNew vqn do
        withFreshItemId (withParent bindValue) body




-- | Helper for @structDef@ and @unionDef@
bindingField :: Has m
    [ Location, FixName
    , M.ParserDefs, M.Group, M.UnresolvedImports
    , Parse
    , Diag
    ] => Category ->
        (Word32 -> m (ATag Word32, ATag SimpleName, ATag UserType)) ->
            Word32 -> m Word32
bindingField c fm i = do
    (off, n, f) :@: at <- tag $ fm i
    let lbl = Label
            (TConstant . CInt <$> off)
            (TConstant . CString . (.value) <$> n)
    newFieldId <- insertNew (Visible Public $
            QualifiedName NonAssociative 0 $
                SimpleFixName <$> n) do
        withFreshItemId (withParent bindField) (Field lbl f :@: at)
    bindCategory newFieldId c
    pure off.value

-- | Helper for @structDef@ and @unionDef@
fields :: Has m '[Location, FixName, Parse, Diag] =>
    (Word32 -> WriterT [Diagnostic] m Word32) -> [ATag TokenSeq] -> m ()
fields fm = loop 0 where
    loop _ [] = pure ()
    loop i (ln:lns) = do
        name <- getFixName
        ref <- getRef
        liftingSyntaxError
            DiagnosticBinder
                { kind = BadDefinition
                , ref = ref
                , name = Just (prettyShow name)
                }
            (recurseParserAll (fm i) ln)
            (loop i lns)
            \i' -> loop (i' + 1) lns



-- | Run an action inside a new namespace,
--   binding any group or unresolved imports created therein,
--   and returning the id
inNewNamespace :: Has m [ Ref, M.ParserDefs, Diag ] =>
    Attr -> StateT M.Group (StateT M.UnresolvedImports m) a -> m ItemId
inNewNamespace at' action = do
    newNamespaceId <- freshItemId

    (newGroup, newUnresolvedImports) <- usingItemId newNamespaceId do
        runStateT (execStateT action Nil) Nil

    unless (isNil newGroup) do
        bindGroup newNamespaceId (newGroup :@: at')

    unless (isNil newUnresolvedImports) do
        bindImports newNamespaceId (newUnresolvedImports :@: at')

    pure newNamespaceId


inNewGroup :: Has m [ Ref, M.ParserDefs, Diag ] =>
    Attr -> StateT M.Group m a -> m ItemId
inNewGroup at action = do
    newGroupId <- freshItemId

    newGroup <- usingItemId newGroupId do
        execStateT action Nil

    unless (isNil newGroup) do
        bindGroup newGroupId (newGroup :@: at)

    pure newGroupId


-- | Helper for line parsing inside @Diag@;
--   Line failures do not stop the rest of the lines from parsing
lineParser :: Has m
    [ Location, FixName
    , Diag
    ] => ParserT (ATag TokenSeq) (ErrorT SyntaxError m) () ->
        [ATag TokenSeq] -> m ()
lineParser p lns = forM_ lns \ln -> do
    name <- getFixName
    ref <- getRef
    liftSyntaxErrorT
        DiagnosticBinder
            { kind = BadDefinition
            , ref = ref
            , name = Just (prettyShow name)
            }
        (evalParserT p ln)




-- | Lex a @Path@ sequence
path :: Has m '[Parse] => m Path
path = do
    b <- optional $ tag pathBase

    cs <- case b of
        Just base -> option Nil do
            if requiresSlash base
                then do
                    at <- attrOf $ connected base.tag $ simpleNameOf "/"
                    connected at body
                else connected base.tag body
        _ -> body

    pure $ Path
        { base = b
        , components = cs
        }
    where
    body = do
        fc@(_ :@: at) <- tag fixName
        Seq.fromList . (fc :) <$> connectMany at do
            at' <- attrOf $ simpleNameOf "/"
            connected at' (tag fixName)

-- | Parse a @PathBase@
pathBase :: Has m '[Parse] => m PathBase
pathBase = expecting "a path base" $ asum
    [ PbRoot <$ sym "~/"
    , PbThis <$ do
        sym "."
        simpleNameOf "/"
    , PbModule <$> do
        sym "module"
        simpleName
    , PbFile <$> do
        sym "file"
        noFail string
    , PbUp <$> do
        fromIntegral . length <$> some do
            sym ".."
            simpleNameOf "/"
    ]

-- | Parse a @FixName@
--   ie. a @Path@ with only a single unqualified @FixName@
fixName :: Has m '[Parse] => m FixName
fixName = expecting "a fix name" $ nextMap \case
    TFixName f -> Just f
    _ -> Nothing

-- | Parse a @SimpleName@;
--   ie. a @Path@ with only a single unqualified @FixName@,
--   and only a single @SimpleName@ within that
simpleName :: Has m '[Parse] => m SimpleName
simpleName = expecting "an unreserved identifier or operator" $ nextMap \case
    TFixName (SimpleFixName n) -> Just n
    _ -> Nothing

-- | Parse a specific @SimpleName@
simpleNameOf :: Has m '[Parse] => String -> m ()
simpleNameOf s = expecting (Pretty.backticks $ text s) $ nextIf_ \case
    TFixName (SimpleFixName (SimpleName n)) -> n == s
    _ -> False

-- | Run a parser proceeded optionally by a @pub@ qualifier,
--   wrapping the result in a @Visible@
visible :: Has m '[Parse] => m a -> m (Visible a)
visible = liftA2 Visible (option Private visibility)

-- | Parse a @pub@ qualifier as a @Visibility@
visibility :: Has m '[Parse] => m Visibility
visibility = Public <$ sym "pub"

-- | Parse a @Category@
category :: Has m '[Parse] => m Category
category = expecting "a category" $ asum
    [ Namespace <$ sym "namespace"
    , Effect <$ sym "effect"
    , Class <$ sym "class"
    , Instance <$ sym "instance"
    , Alias <$ sym "type"
    , Struct <$ sym "struct"
    , Union <$ sym "union"
    ]

-- | Parse a @QualifiedName@;
--   ie. either a @SimpleName@, or a @FixName@
--   with a leading or trailing associative @Precedence@
qualifiedName :: Has m '[Parse] =>
    m QualifiedName
qualifiedName = expecting "a qualified name" do
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
        pure $ QualifiedName NonAssociative (snd apt) n

-- | Parse an associative @Precedence@ level;
--   Either an integer in the range {@0@, @maxBound Precedence@},
--   or an integer in that range surrounded by parens
associativePrecedence :: Has m '[Parse] => m (Bool, Precedence)
associativePrecedence = asum
    [ (False, ) <$> parens precedence
    , (True, ) <$> precedence
    ]

-- | Parse a @Precedence@ level;
--   simply an integer in the range {@0@, @maxBound Precedence@}
precedence :: Has m '[Parse] => m Precedence
precedence = do
    i :@: at <- tag int
    assertAt at (i <= bound) Unrecoverable $
        "precedence level" <+> backticked i
            <+> "is out of range, max is" <+> shown bound
    pure (fromIntegral i) where
    bound = fromIntegral @Precedence @Word32 maxBound


-- | Parse a type scheme, where the quantifier is proceeded by a @for@ keyword,
--   and the qualifier is delimited by an arrow @=>@; consumes the arrow
forTypeHead :: Has m '[Parse] =>
    Bool -> m (ATag Quantifier, ATag (Qualifier UserType))
forTypeHead nilCheck = typeHeadOf nilCheck (Just "=>") $ const $ liftA2 (,)
    do optionalIndent' $ tag $ option Nil (sym "for" >> quantifier)
    do optionalIndent' $ tag $ option Nil (qualifier $ Just "=>")

-- | Parse a type scheme, delimited by the given symbol; consumes the delimiter
typeHeadDelim :: Has m '[Parse] =>
    Bool -> String -> m (ATag Quantifier, ATag (Qualifier UserType))
typeHeadDelim nilCheck = typeHeadMaybeDelim nilCheck . Just

-- | Parse a type scheme, with no delimiter
typeHeadNoDelim :: Has m '[Parse] =>
    Bool -> m (ATag Quantifier, ATag (Qualifier UserType))
typeHeadNoDelim nilCheck = typeHeadMaybeDelim nilCheck Nothing

-- | Parse a type scheme, delimited by an @=@, and consume the equal sign
typeHeadEq :: Has m '[Parse] =>
    Bool -> m (ATag Quantifier, ATag (Qualifier UserType) )
typeHeadEq nilCheck = typeHeadDelim nilCheck "="

-- | Parse a type scheme, delimited by an arrow @=>@, and consume the arrow
typeHeadArrow :: Has m '[Parse] =>
    Bool -> m (ATag Quantifier, ATag (Qualifier UserType))
typeHeadArrow nilCheck = typeHeadDelim nilCheck "=>"

-- | Parse a type quantifier, delimited by an @=@ and consume the equal sign
typeQuantifierEq :: Has m '[Parse] =>
    Bool -> m (ATag Quantifier)
typeQuantifierEq nilCheck = typeHeadOf nilCheck (Just "=") $ const do
    option Nil (tag quantifier)

-- | Parse a type quantifier, delimited by an arrow @=>@ and consume the arrow
typeQuantifierArrow :: Has m '[Parse] =>
    Bool -> m (ATag Quantifier)
typeQuantifierArrow nilCheck = typeHeadOf nilCheck (Just "=>") $ const do
    option Nil (tag quantifier)

-- | Parse a type scheme, delimited by the given symbol, if one is provided;
--   consumes the delimiter
typeHeadMaybeDelim :: Has m '[Parse] =>
    Bool -> Maybe String -> m (ATag Quantifier, ATag (Qualifier UserType))
typeHeadMaybeDelim nilCheck = flip (typeHeadOf nilCheck) typeHeadMaybeDelimBody

-- | Parse a type scheme body,
--   delimited by the given symbol, if one is provided;
--   does not consume the delimiter
typeHeadMaybeDelimBody :: Has m '[Parse] =>
    Maybe String -> m (ATag Quantifier, ATag (Qualifier UserType))
typeHeadMaybeDelimBody delim = liftA2 (,)
    do optionalIndent $ tag $ option Nil quantifier
    do optionalIndent $ tag $ option Nil (qualifier delim)

-- | Parse a type scheme, with its body given by a function,
--   and delimited by the given symbol, if one is provided;
--   consumes the delimiter
typeHeadOf :: Has m '[Parse, With '[Nil a, Pretty a]] =>
    Bool -> Maybe String -> (Maybe String -> m a) -> m a
typeHeadOf nilCheck delim p = optionalIndent' do
    r :@: at <- tag (p delim)
    guard (not nilCheck || notNil r)
    r <$ case delim of
        Just d -> expectingAt at "to close type head" (sym d)
        _ -> pure ()


-- | Parse a @Quantifier@
quantifier :: Has m '[Parse] => m Quantifier
quantifier = evalStateT' (0 :: KindVar) do
    Quantifier <$> listSome (sym ",") (tag typeBinder)

-- | Parse a type @Qualifier@, with its body as tokens,
--   delimited by the given symbol
qualifier :: Has m '[Parse] => Maybe String -> m (Qualifier UserType)
qualifier _delim = sym "where" >> noFail do
    Qualifier <$> listSome (sym ",") (tag userType)

-- | Parse a @TypeBinder@
typeBinder :: Has m [St KindVar, Parse] => m TypeBinder
typeBinder = do
    n <- tag simpleName
    k <- optional do
        sym ":" >> noFail (tag kind)
    case k of
        Just k' -> pure (n `Of` k')
        _ -> do
            k' <- state \i -> (i, i + 1)
            pure $ n `Of` (KVar k' :@: n.tag)


-- | Parse a type @Kind@
kind :: Has m '[Parse] => m Kind
kind = atom >>= arrows where
    atom = asum
        [ KType <$ sym "type"
        , KInt <$ simpleNameOf "int"
        , KString <$ simpleNameOf "str"
        , KEffect <$ sym "effect"
        , KConstraint <$ simpleNameOf "constraint"
        , KData <$ simpleNameOf "data"
        , KEffects <$ simpleNameOf "effects"
        , parens kind
        ]
    arrows l = option l do
        simpleNameOf "->" >> noFail do
            KArrow l <$> kind


-- | Parse a @UserType@
userType :: Has m '[Parse] => m UserType
userType = optionalIndent $ untag <$> (tag atom >>= infixLoop) where
    atom = asum
        [ sym "'" >> asum
            [ simpleName >>= \n ->
                TVar . TvFree (Just n) <$> option Nothing do
                    sym "of"
                    Just <$> noFail (tag kind)

            , sym "of" >> TVar . TvFree Nothing . Just <$>
                noFail (tag kind)
            ]

        , do kat <- attrOf $ sym "struct"
             TVar . TvInlineConstraint . CData . DcStruct <$>
                liftA2 StructConstraint
                    do tag userType
                    do tag $ option
                        (TVar $ TvFree Nothing $ Just (KType :@: kat))
                        userType

        , do kat <- attrOf $ sym "union"
             TVar . TvInlineConstraint . CData . DcUnion <$>
                liftA2 UnionConstraint
                    do tag userType
                    do tag $ option
                        (TVar $ TvFree Nothing $ Just (KType :@: kat))
                        userType

        , TVar (TvFree Nothing Nothing) <$ sym "_"

        , TVar . TvPath <$> path

        , TEffects <$> brackets typeList

        , TData <$> brackets fieldList

        , parens $ option TUnitCon do
            t1 <- tag userType
            option (untag t1) $ buildTuple . (t1 :) <$> do
                sym "," >> typeList
        ]

    fieldList = listMany (sym ",") field
    field = liftA2 Field
        do asum
            [ liftA2 Label
                do tag (TConstant . CInt <$> int)
                do simpleNameOf "\\"
                   tag $ asum
                        [ TVar . TvFree Nothing . Just <$>
                            KString <@> attrOf (sym "_")
                        , TConstant . CString . (.value) <$> simpleName
                        ]

            -- FIXME: vary over both layout and name
            , todo
            ]
        do sym ":" >> tag userType

    typeList = listMany (sym ",") (tag userType)

    infixLoop l = option l (infixes l >>= infixLoop)

    infixes l = asum
        [ simpleNameOf "->" >> noFail do
            r <- tag userType
            x <- option (TEffects [] :@: attrFlattenToEnd r.tag) do
                sym "in"
                noFail (tag userType)
            pure $ TArrow l r x :@: (l.tag <> r.tag <> x.tag)

        , tag atom <&> \r -> TApp l r :@: (l.tag <> r.tag)
        ]



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
    body <- tag $ expecting "`(`" $ nextMap \case
        TTree BkParen ts -> Just ts
        _ -> Nothing
    noFail (recurseParserAll px body)

-- | Expect a given @Parser@ to be surrounded with braces
braces :: Has m '[Parse] => m a -> m a
braces px = do
    body <- tag $ expecting "`{`" $ nextMap \case
        TTree BkBrace ts -> Just ts
        _ -> Nothing
    noFail (recurseParserAll px body)

-- | Expect a given @Parser@ to be surrounded with brackets
brackets :: Has m '[Parse] => m a -> m a
brackets px = do
    body <- tag $ expecting "`[`" $ nextMap \case
        TTree BkBracket ts -> Just ts
        _ -> Nothing
    noFail (recurseParserAll px body)




-- | Allow an optional indentation of the input to the parser
optionalIndent :: Has m '[Parse] => m a -> m a
optionalIndent p = do
    getParseState >>= \case
        (TTree BkWhitespace ts :@: at Seq.:<| _) :@: _ ->
            advance >> recurseParserAll p (ts :@: at)
        _ -> p

-- | Allow an optional indentation of the input to the parser
optionalIndent' :: Has m '[Parse] => m a -> m a
optionalIndent' p = do
    (a, ps') <- recurseParser p =<< asum [grabWhitespaceDomain, Nil <@> attr]
    a <$ modifyParseState (ps' <>)

-- | Consume the rest of the input,
--   delimited by indentation, as well as a predicate
grabDomain :: Has m '[Parse] => (ATag Token -> Bool) -> m (ATag TokenSeq)
grabDomain p = expecting "a whitespace block" do
    ts <- takeParseState
    case ts of
        Nil :@: _ -> do
            fp <- getFilePath
            throwError $ SyntaxError Recoverable $
                EofFailure :@: attrInput fp ts
        toks -> do
            let (a, b) = consume toks
            fmap reduceTokenSeq a <$ putParseState (fmap reduceTokenSeq b)
    where
    consume = \case
        base@((t Seq.:<| ts) :@: at) | p t ->
            case untag t of
                TTree BkWhitespace ts' -> block (ts' :@: t.tag)
                _ -> continue
            where
            continue =
                let (as, bs) = consume (ts :@: attrSub at t.tag)
                in ((t Seq.<| as.value) :@: (t.tag <> as.tag), bs)

            block ts' =
                let (as, bs) = consume ts'
                in if
                | Seq.null bs.value -> continue
                | Seq.null as.value -> (Nil, base)
                | otherwise ->
                    ( Seq.singleton (TTree BkWhitespace as.value :@: t.tag)
                        :@: t.tag
                    , (TTree BkWhitespace bs.value :@: bs.tag Seq.<| ts)
                        :@: attrSub at as.tag
                    )

        ts -> (Nil :@: attrFlattenToEnd ts.tag, ts)


-- | Get the rest of the input delimited by indentation
grabWhitespaceDomain :: Has m '[Parse] => m (ATag TokenSeq)
grabWhitespaceDomain = grabDomain (const True)

-- | Get the rest of the input delimited by indentation,
--   ensuring that this captures the remainder of the input
grabWhitespaceDomainAll :: Has m '[Parse] => m (ATag TokenSeq)
grabWhitespaceDomainAll = consumesAll grabWhitespaceDomain

-- | Consume a sequence of lines from the input
grabLines :: Has m '[Parse] => m [ATag TokenSeq]
grabLines = some $ tag $ nextMap \case
    TTree BkWhitespace ln -> Just ln
    _ -> Nothing

-- | Consume a sequence of lines from the input
grabLines' :: Has m '[Parse] => m (ATag TokenSeq)
grabLines' = tag do
    Seq.fromList <$> nextWhileAttr \case
        TTree BkWhitespace _ :@: _ -> True
        _ -> False


-- | grab the whitespace domain, and break it into lines;
--   ensures both steps consume all available input
grabBlock :: Has m '[Parse] => m [ATag TokenSeq]
grabBlock = pTracedM "grabBlock" grabWhitespaceDomainAll >>= recurseParserAll do
    asum [pTracedM "grabLines" grabLines, pure <$> takeParseState]

-- | attempt to grab the whitespace domain, and break it into lines;
--   ensures both steps consume all available input;
--   tags the output, and at failure, returns an empty list at the given @Attr@
tryGrabBlock :: Has m '[Parse] => Attr -> m (ATag [ATag TokenSeq])
tryGrabBlock at = option ([] :@: at) (tag grabBlock)

-- | @wsBlock<elem>++(sep?) | elem (sep elem)*@
wsList :: Has m '[Parse] => Bool -> String -> m a -> m [a]
wsList allowTrailing ss ma =
    wsListBody allowTrailing ss >>= traverse (recurseParserAll ma)

-- | @wsBlock<elem>++(sep?) | elem (sep elem)*@
--   tags the output, and at failure, returns an empty list at the given @Attr@
tryWsListBody :: Has m '[Parse] =>
    Attr -> Bool -> String -> m (ATag [ATag TokenSeq])
tryWsListBody at allowTrailing ss =
    option ([] :@: at) (tag $ wsListBody allowTrailing ss)

-- | @wsBlock<elem>++(sep?) | elem (sep elem)*@
wsListBody :: Has m '[Parse] => Bool -> String -> m [ATag TokenSeq]
wsListBody allowTrailing ss =
    grabWhitespaceDomain >>= recurseParserAll (asum [block, inline]) where
    inline =
        listSome ms dom
            << when allowTrailing (option () ms)
    block = do
        lns <- grabLines
        noFail $ asum [block1 lns, block2 lns]
    block1 lns =
        lns & foldWithM' mempty \toks as ->
            (: as) <$> recurseParserAll
                do dom << when (allowTrailing || null as) (option () ms)
                toks
    block2 =
        flip Fold.foldlM mempty \as toks ->
            (as <>) . pure <$> recurseParserAll
                do if null as
                    then when allowTrailing (option () ms) >> dom
                    else ms >> dom
                toks
    ms = sym ss
    dom = grabDomain (not . isSymbol ss . untag)
