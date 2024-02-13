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



-- parseFile :: File -> Either String Doc
-- parseFile = parseFileWith doc



-- | Parse a file into a list of ProtoDefs
protoDefs :: ParserMonad m => m [ATag ProtoDef]
protoDefs = many (tag protoDef)

-- | Parse a top-level definition, getting its head
--   but leaving the body as a token list;
--   except in the case of namespaces, which are parsed recursively
protoDef :: ParserMonad m => m ProtoDef
protoDef = noFailBeforeEof do
    a <- attr
    option DkValue defKind >>= \case
        DkType -> do
            traceM "parsing type definition"
            sp <- tag spec
            ProtoType sp <$> (sym "=" >> body sp a)

        DkEffect -> do
            traceM "parsing effect definition"
            sp <- tag spec
            ProtoEffect sp <$> (sym "=" >> effBody sp a)

        DkValue -> do
            traceM "parsing value definition"
            sp <- tag spec
            symOf [":", "="] >>= \case
                ":" -> liftA2 (ProtoValue sp)
                    do expectB "or `=` delimited type head" sp do
                        grabDomain (wsDominated a &&& notAssign)
                    do option Nil do sym "=" >> noFail (body sp a)
                _ -> ProtoValue sp Nil <$> body sp a

        DkNamespace -> do
            traceM "parsing namespace definition"
            n <- tag name
            ProtoNamespace n <$>
                expectB "body" ("namespace " <> D.prettyShow n) do
                    sym "=" >> recurse a

    where
    notAssign = not . isSymbolToken "=" . untag
    expectB n sp = expecting
        $ "a whitespace-delimited " <> n
        <> " for " <> D.render (D.backticked sp)
    body sp a = expectB "body" sp do grabWhitespaceDomain a
    recurse a = grabWhitespaceDomain a
            >>= noFail . liftP . recurseParser protoDefs
    effBody sp a =
        body sp a
        >>= noFail . liftP . recurseParser (some $ tag effCase)
    effCase = do
        sp <- tag spec
        sym ":"
        ProtoEffectCase sp <$> body sp (tagOf sp)


-- | Parse an unreserved symbol as a Name
name :: ParserMonad m => m Name
name = Name <$> unreserved

-- | Parse a spec, which is a fixity and an optional precedence,
--   in this case containing a name
spec :: ParserMonad m => m (Spec Name)
spec = expecting "a definition head" do
    fx <- option Atomic fixity
    case fx of
        Atomic -> Spec fx Nothing <$> noFailBeforeEof name
        _ -> liftA2 (Spec fx) (optional int) (noFail name)

-- | Parse a definition kind name, such as "type" or "value"
defKind :: ParserMonad m => m DefKind
defKind = asum
    [ sym "type" $> DkType
    , sym "effect" $> DkEffect
    , sym "value" $> DkValue
    , sym "namespace" $> DkNamespace
    ]

-- | Parse a fixity specification, such as "infixl" or "prefix"
fixity :: ParserMonad m => m Fixity
fixity = asum
    [ sym "infixl" $> InfixL
    , sym "infixr" $> InfixR
    , sym "infix" $> Infix
    , sym "prefix" $> Prefix
    , sym "postfix" $> Postfix
    , sym "atom" $> Atomic
    ]
