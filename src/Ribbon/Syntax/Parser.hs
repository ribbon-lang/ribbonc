module Ribbon.Syntax.Parser where

import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Data.Functor

import Control.Applicative
import Control.Monad

import Ribbon.Util
import Ribbon.Source
import Ribbon.Display(Doc, (<+>), text, render, prettyShow, backticked)
import Ribbon.Syntax.Text
import Ribbon.Syntax.Token
import Ribbon.Syntax.Ast
import Ribbon.Syntax.ParserM




-- | Parse a module definition file to a series of prototypes
parseModuleFileProtos ::
    File -> Either (Doc ()) (ATag ModuleProtoHead, [ATag ProtoDef])
parseModuleFileProtos = parseFileWith (liftA2 (,) (tag moduleHead) protoDefs)

-- | Parse a source file to a list of prototypes
parseSourceFileProtos :: File -> Either (Doc ()) [ATag ProtoDef]
parseSourceFileProtos = parseFileWith protoDefs



-- | Parse a module head like `module "foo" = ...`
moduleHead :: ParserMonad m => m ModuleProtoHead
moduleHead = expecting "a module head" do
    a <- attr
    sym "module"
    n <- tag string
    validateName n
    sym "="
    body <- grabWhitespaceDomain a
    pairs <- recurseParser keyPairs body
    processPairs a (emptyModuleProtoHead n) pairs
    where
    keyPairs = some do
        a <- attr
        liftA2 (,)
            do tag unreserved
            do sym "="; grabWhitespaceDomain a

    processPairs a m [] = m <$ validateMod a m
    processPairs a m ((k, vs) : ps) = do
        case untag k of
            "version" -> do
                unless (isNil $ untag $ mhVersion m) do
                    parseError' (tagOf k) $
                        text "multiple `version` entries in module head"
                v :@: av <- recurseParser (tag string) vs
                v' <- parseVersion' av v
                processPairs a (m { mhVersion = v' :@: av }) ps
            "sources" -> do
                unless (null $ mhSources m) do
                    parseError' (tagOf k) $
                        text "multiple `source` entries in module head"
                v <- recurseParser (listSome (sym ",") $ tag string) vs
                processPairs a (m { mhSources = v }) ps
            "dependencies" -> do
                unless (null $ mhDependencies m) do
                    parseError' (tagOf k) $
                        text "multiple `dependencies` entries in module head"
                v <- recurseParser (listSome (sym ",") $ tag dependency) vs
                processPairs a (m { mhDependencies = v }) ps
            _ -> do
                unless (Maybe.isNothing $ lookupMeta (mhMeta m)) do
                    parseError' (tagOf k) $
                        text "duplicate key `" <> text (untag k)
                        <> text "` in module head"
                v <- recurseParser (tag string) vs
                processPairs a (m { mhMeta = (k, v) : mhMeta m }) ps
        where
        lookupMeta = List.find \(k', _) -> untag k == untag k'

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
                text "dependency specifier must be of the form"
                    <+> text "`name@version`, e.g. `foo@1.2.3`"

    parseVersion' a v =
        maybe
            do parseError' a $
                text "version specifier must be of the form"
                <+> text "`major.minor.patch`"
                <+> text "with all parts being non-negative integers, "
                <+> text "and at least one > 0; "
                <+> text "e.g. `0.1.0`"
            pure
            (parseVersion v)

    validateName n =
        when ('@' `elem` untag n) do
            parseError' (tagOf n) (text "module name cannot contain `@`")

    validateMod a (ModuleProtoHead n v _ s _) = do
        when (null $ untag n) do
            parseError' (tagOf n) (text "module name is required")
        when (isNil $ untag v) do
            parseError' a (text "module version is required")
        when (null s) do
            parseError' a (text "at least one module source is required")




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
    option DkValue kind >>= \case
        DkType -> do
            dn <- tag defName
            ProtoType v dn <$> (sym "=" >> body dn a)

        DkEffect -> do
            dn <- tag defName
            ProtoEffect v dn <$> (sym "=" >> effBody dn a)

        DkValue -> do
            dn <- tag defName
            symOf [":", "="] >>= \case
                ":" -> liftA2 (ProtoValue v dn)
                    do expectB "or `=` delimited type head" dn do
                        grabDomain (wsDominated a &&& notAssign)
                    do option Nil $ sym "=" >> body dn a
                _ -> ProtoValue v dn Nil <$> body dn a

        DkNamespace -> do
            n <- tag name
            ProtoNamespace v n <$>
                expectB "body" ("namespace " <> prettyShow n) do
                    sym "=" >> recurse a

        DkUse -> do
            ProtoUse v <$> do
                toks <- expecting "a whitespace-delimited body for use" do
                    grabWhitespaceDomain a
                recurseParser (tag use) toks

    where
    notAssign = not . isSymbolToken "=" . untag

    expectB n dn = expecting $
        "a whitespace-delimited " <> n <> " for " <> render (backticked dn)
    body dn a = expectB "body" dn do grabWhitespaceDomain a

    recurse a = grabWhitespaceDomain a >>= recurseParser protoDefs

    vis = sym "pub" $> Public

    kind = asum
        [ sym "type" $> DkType
        , sym "effect" $> DkEffect
        , sym "value" $> DkValue
        , sym "namespace" $> DkNamespace
        , sym "use" $> DkUse
        ]

    defName = expecting "a definition head" do
        fx <- option DefAtomic fixity
        case fx of
            DefAtomic -> DefName fx Nothing <$> noFailBeforeEof name
            _ -> liftA2 (DefName fx) (optional int) (noFail name)

        where
        fixity = asum
            [ sym "infixl" $> DefInfixL
            , sym "infixr" $> DefInfixR
            , sym "infix" $> DefInfix
            , sym "prefix" $> DefPrefix
            , sym "postfix" $> DefPostfix
            , sym "atom" $> DefAtomic
            ]

    effBody dn a =
        body dn a >>= recurseParser (some $ tag effCase)
    effCase = do
        dn <- tag defName
        sym ":"
        ProtoEffectCase dn <$> body dn (tagOf dn)

    use = do
        o <- getOffset
        a <- optional (tag localPath)
        b <- optional $ tag
            case a of
                Just _ -> connected (useTree a)
                _ -> useTree a
        case (a, b) of
            (Nothing, Nothing) -> failAtMulti o ["name", "path", "branch"]
            _ -> Use a b <$> optional do
                sym "as" >> tag defName

    useTree a = case a of
        Just (untag -> av) | localPathNeedsSlash av -> do
            sym "/"
            noFail (connected useTreeBody)
        _ -> useTreeBody
    useTreeBody = asum
        [ UseBranch <$> useBranch
        , UseLeaf <$> useName
        , UseAll <$ sym ".."
        ]
    useBranch = braces do
        listMany (sym ",") (tag use)


-- | Parse a local path
localPath :: ParserMonad m => m LocalPath
localPath = do
    optional (tag pathBase) >>= \case
        Just base -> LocalPath base <$>
            if localPathBaseNeedsSlash (untag base)
                then maybeTail
                else manyTail
        _ -> do
            ns :@: a <- tag do
                liftA2 (:) (tag name) maybeTail
            pure $ LocalPath (LpHere :@: a) ns
    where
    maybeTail = option Nil do
        connected (sym "/")
        listSome
            (connected $ sym "/")
            (connected $ tag name)
    manyTail = listMany (connected $ sym "/") (connected $ tag name)
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
    k <- optional kind
    case k of
        Just UseNamespace -> UseName k Nothing <$> name
        _ -> liftA2 (UseName k)
            do optional fixity
            do name
    where
    fixity = asum
        [ sym "infix" $> UseInfix
        , sym "prefix" $> UsePrefix
        , sym "postfix" $> UsePostfix
        , sym "atom" $> UseAtomic
        ]

    kind = asum
        [ sym "type" $> UseType
        , sym "effect" $> UseEffect
        , sym "value" $> UseValue
        , sym "namespace" $> UseNamespace
        ]
