module Language.Ribbon.Parsing.Parser where

import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe

import Data.Foldable qualified as Fold

import Data.Tag
import Data.Attr

import Control.Applicative

import Text.Pretty

import Language.Ribbon.Util

import Language.Ribbon.Parsing.Monad.Parser

import Language.Ribbon.Lexical

import Language.Ribbon.Syntax.Module
import Language.Ribbon.Syntax
import Debug.Trace (traceM)




moduleHead :: Parser ModuleHead
moduleHead = expecting "a valid module header" do
    moduleName <- sym "module" >> tag string
    validateName moduleName
    moduleVersion <- sym "@" >> tag version
    toks <- option mempty grabWhitespaceDomain
    traceM (prettyShow toks)
    pairs <- recurseParser keyPairs toks
    traceM (prettyShow pairs)
    (sources, dependencies, meta) <- processPairs pairs

    pure ModuleHead
        { name = moduleName
        , version = moduleVersion
        , sources = sources.value
        , dependencies = dependencies.value
        , meta = meta
        }
    where
    keyPairs = many do
        liftA2 (,) (tag name) do
            d <- grabWhitespaceDomain
            d <$ traceM (prettyShow d)

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
        assertAt (null existing.value) at do
            hang "multiple `sources` fields in module head"
                $ "original is here:" <+> pPrint (tagOf existing)
        recurseParser (wsList (sym ",") (tag string)) toks

    processDependencies at existing toks = do
        assertAt (null existing.value) at do
            hang "multiple `dependencies` fields in module head"
                $ "original is here:" <+> pPrint (tagOf existing)
        recurseParser (wsList (sym ",") dependency) toks

    processMeta key meta toks = do
        assertAt (Map.notMember key meta) key.tag do
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
        assertAt ('@' `notElem` s.value) s.tag do
            "module name" <+> shown s.value <+> "is invalid,"
                <+> "cannot contain `@` symbol"


name :: Parser Name
name = expecting "an unreserved name" $ nextMap \case
    t@(TSymbol s) | not (isReserved t) -> pure (Name s)
    _ -> empty
