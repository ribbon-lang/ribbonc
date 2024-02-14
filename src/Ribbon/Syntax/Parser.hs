module Ribbon.Syntax.Parser where

import Prelude hiding (exp)

import Data.Functor

import Control.Applicative

import Ribbon.Util
import Ribbon.Source
import Ribbon.Display qualified as D
import Ribbon.Syntax.Token
import Ribbon.Syntax.Ast
import Ribbon.Syntax.ParserM
import Debug.Trace (traceM)
import Control.Monad
import Data.Sequence (Seq)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Ribbon.Syntax.Text (parseVersion)



-- parseFile :: File -> Either String Doc
-- parseFile = parseFileWith doc


moduleHead :: forall m. ParserMonad m => m ModuleProtoHead
moduleHead = do
    a <- attr
    sym "module"
    n <- tag string
    validateName n
    sym "="
    body <- grabWhitespaceDomain a
    pairs <- recurseParser keyPairs body
    processPairs a (emptyModuleProtoHead n) pairs
    where
    keyPairs = some labeled
    labeled = do
        a <- attr
        liftA2 (,)
            do tag unreserved
            do sym "="; grabWhitespaceDomain a
    processPairs :: Attr -> ModuleProtoHead -> [(ATag String, Seq (ATag Token))] -> m ModuleProtoHead
    processPairs a m [] = m <$ validateMod a m
    processPairs a m ((k, vs) : ps) = do
        case untag k of
            "version" -> do
                unless (isNil $ untag $ mhVersion m) do
                    parseError' (tagOf k) (D.text "multiple `version` entries in module head")
                v :@: av <- recurseParser (tag string) vs
                v' <- parseVersion' av v
                processPairs a (m { mhVersion = v' :@: av }) ps
            "sources" -> do
                unless (null $ mhSources m) do
                    parseError' (tagOf k) (D.text "multiple `source` entries in module head")
                v <- recurseParser (listSome (sym ",") $ tag string) vs
                processPairs a (m { mhSources = v }) ps
            "dependencies" -> do
                unless (null $ mhDependencies m) do
                    parseError' (tagOf k) (D.text "multiple `dependencies` entries in module head")
                v <- recurseParser (listSome (sym ",") $ tag dependency) vs
                processPairs a (m { mhDependencies = v }) ps
            _ -> do
                unless (Maybe.isNothing $ List.find (\(k', _) -> untag k' == untag k) (mhMeta m)) do
                    parseError' (tagOf k) (D.text "duplicate key `" <> D.text (untag k) <> D.text "` in module head")
                v <- recurseParser (tag string) vs
                processPairs a (m {mhMeta = (k, v) : mhMeta m}) ps

    dependency = do
        nameVerStr <- tag string
        nameVer <- parseDepName nameVerStr
        alias <- optional $ sym "as" >> tag name
        pure $ ModuleProtoDependency nameVer alias

    parseDepName nv =
        case splitOn '@' (untag nv) of
            [n, v] | not (null v)-> do
                v' <- parseVersion' (tagOf nv) v
                pure $ (n, v') :@: tagOf nv
            _ -> err
        where
            err = parseError' (tagOf nv) do
                D.text "dependency specifier must be of the form"
                    D.<+> D.text "`name@version`, e.g. `foo@1.2.3`"

    parseVersion' a v =
        maybe
            do parseError' a $
                D.text "version specifier must be of the form"
                D.<+> D.text "`major.minor.patch`"
                D.<+> D.text "with all parts being integers > 0"
                D.<+> D.text "e.g. `1.2.3`"
            pure
            (parseVersion v)

    validateName n =
        when ('@' `elem` untag n) do
            parseError' (tagOf n) (D.text "module name cannot contain `@`")

    validateMod a (ModuleProtoHead n v _ s _) = do
        when (null $ untag n) do
            parseError' (tagOf n) (D.text "module name is required")
        when (isNil $ untag v) do
            parseError' a (D.text "module version is required")
        when (null s) do
            parseError' a (D.text "at least one module source is required")




-- | Parse a file into a list of ProtoDefs
protoDefs :: ParserMonad m => m [ATag ProtoDef]
protoDefs = many (tag protoDef)

-- | Parse a top-level definition, getting its head
--   but leaving the body as a token list;
--   except in the case of namespaces, which are parsed recursively
protoDef :: ParserMonad m => m ProtoDef
protoDef = noFailBeforeEof do
    a <- attr
    v <- option Private vis
    option DkValue defKind >>= \case
        DkType -> do
            traceM "parsing type definition"
            dn <- tag defName
            ProtoType v dn <$> (sym "=" >> body dn a)

        DkEffect -> do
            traceM "parsing effect definition"
            dn <- tag defName
            ProtoEffect v dn <$> (sym "=" >> effBody dn a)

        DkValue -> do
            traceM "parsing value definition"
            dn <- tag defName
            symOf [":", "="] >>= \case
                ":" -> liftA2 (ProtoValue v dn)
                    do expectB "or `=` delimited type head" dn do
                        grabDomain (wsDominated a &&& notAssign)
                    do option Nil do sym "=" >> noFail (body dn a)
                _ -> ProtoValue v dn Nil <$> body dn a

        DkNamespace -> do
            traceM "parsing namespace definition"
            n <- tag name
            ProtoNamespace v n <$>
                expectB "body" ("namespace " <> D.prettyShow n) do
                    sym "=" >> recurse a

        DkUse -> do
            traceM "parsing use definition"
            ProtoUse v <$> do
                toks <- expecting "a whitespace-delimited body for use" do
                    grabWhitespaceDomain a
                recurseParser (tag use) toks

    where
    notAssign = not . isSymbolToken "=" . untag

    expectB n dn = expecting $
        "a whitespace-delimited " <> n <> " for " <> D.render (D.backticked dn)
    body dn a = expectB "body" dn do grabWhitespaceDomain a

    recurse a = grabWhitespaceDomain a >>= recurseParser protoDefs

    effBody dn a =
        body dn a >>= recurseParser (some $ tag effCase)
    effCase = do
        dn <- tag defName
        sym ":"
        ProtoEffectCase dn <$> body dn (tagOf dn)

    use = do
        a <- optional (tag localPath)
        b <- optional $ tag $ asum
            [ UseBranch <$> braces do
                listMany (sym ",") (tag use)
            , do
                when (maybe False (localPathNeedsDot . untag) a) do
                    sym "."
                UseLeaf <$> useName
            , sym ".." $> UseAll
            ]
        case (a, b) of
            (Nothing, Nothing) -> fail "a name, path, or branch"
            _ -> Use a b <$> useAs

    useAs = optional $ sym "as" >> tag defName





-- | Parse a local path
localPath :: ParserMonad m => m LocalPath
localPath = do
    optional (tag pathBase) >>= \case
        Just base
            | LpModule _ <- untag base -> maybeTail base
            | LpFile _ <- untag base -> maybeTail base
            | otherwise -> LocalPath base <$> manyTail
        _ -> do
            ns :@: a <- tag someTail
            pure $ LocalPath (LpHere :@: a) ns
    where
    maybeTail base = LocalPath base <$> do
        trySym "." >>= selecting someTail (pure Nil)
    someTail = listSome (sym ".") (tag name)
    manyTail = listMany (sym ".") (tag name)
    pathBase = asum
        [ sym "/" >> pure LpRoot
        , some (sym "../") <&> LpUp . length
        , sym "module" >> name <&> LpModule
        , sym "file" >> string <&> LpFile
        , sym "./" >> pure LpHere
        ]


-- | Parse an unreserved symbol as a Name
name :: ParserMonad m => m Name
name = Name <$> unreserved


-- | Parse a UseName, which is an optional fixity, followed by a name
useName :: ParserMonad m => m UseName
useName = expecting "a usable name" do
    k <- optional useKind
    case k of
        Just UseNamespace -> UseName k Nothing <$> name
        _ -> liftA2 (UseName k)
            do optional useFixity
            do name



-- | Parse a DefName, which is an optional fixity and an optional precedence,
--   followed by a name
defName :: ParserMonad m => m DefName
defName = expecting "a definition head" do
    fx <- option DefAtomic defFixity
    case fx of
        DefAtomic -> DefName fx Nothing <$> noFailBeforeEof name
        _ -> liftA2 (DefName fx) (optional int) (noFail name)

vis :: ParserMonad m => m Visibility
vis = sym "pub" $> Public

-- | Parse a use kind name, such as "type" or "value"
useKind :: ParserMonad m => m UseKind
useKind = asum
    [ sym "type" $> UseType
    , sym "effect" $> UseEffect
    , sym "value" $> UseValue
    , sym "namespace" $> UseNamespace
    ]

-- | Parse a definition kind name, such as "type" or "use"
defKind :: ParserMonad m => m DefKind
defKind = asum
    [ sym "type" $> DkType
    , sym "effect" $> DkEffect
    , sym "value" $> DkValue
    , sym "namespace" $> DkNamespace
    , sym "use" $> DkUse
    ]

-- | Parse a fixity specification, such as "infix" or "prefix"
useFixity :: ParserMonad m => m UseFixity
useFixity = asum
    [ sym "infix" $> UseInfix
    , sym "prefix" $> UsePrefix
    , sym "postfix" $> UsePostfix
    , sym "atom" $> UseAtomic
    ]

-- | Parse a fixity specification, such as "infixl" or "prefix"
defFixity :: ParserMonad m => m DefFixity
defFixity = asum
    [ sym "infixl" $> DefInfixL
    , sym "infixr" $> DefInfixR
    , sym "infix" $> DefInfix
    , sym "prefix" $> DefPrefix
    , sym "postfix" $> DefPostfix
    , sym "atom" $> DefAtomic
    ]
