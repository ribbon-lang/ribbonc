module Ribbon.Analysis.Context where

import Data.Functor

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import qualified Data.List as List
import qualified Data.Char as Char

import Ribbon.Util
import Ribbon.Source
import Ribbon.Display
import Ribbon.Syntax.Ast
import Control.Monad.Except
import Control.Monad.Reader.Class


type MonadAnalysis m =
    (MonadError AnalysisError m, MonadReader AnalysisContext m)


newtype AnalysisContext
    = AnalysisContext
    { modules :: Map (String, Version) Module
    }

newtype AnalysisError
    = AnalysisError (ATag Doc)

instance Pretty AnalysisError where
    pPrint (AnalysisError (doc :@: a)) =
        hang ("error at" <+> bracketed a) doc


lookupModule :: MonadAnalysis m =>
    ATag (String, Version) -> m Module
lookupModule nameVer = do
    ctx <- ask
    let nv@(name, ver) = untag nameVer
    case Map.lookup nv ctx.modules of
        Just tree -> pure tree
        _ -> throwError $ AnalysisError $ Tag (tagOf nameVer) $
            hang ("module" <+> text name
                <+> "with version" <+> pPrint ver
                <+> "not found in context")
            case nameFuzzySearchOn fst name (Map.keys ctx.modules) of
                [] -> mempty
                suggestions -> hang "Did you mean:" $ lsep do
                    suggestions <&> \(sn, sv) ->
                        text sn <+> "with version" <+> pPrint sv


nameFuzzySearch :: String -> [String] -> [String]
nameFuzzySearch = nameFuzzySearchOn id

nameFuzzySearchOn :: (a -> String) -> String -> [a] -> [a]
nameFuzzySearchOn f name ss =
    let subs = List.permutations $ List.take 7 $ lower name
    in List.sortOn (length . f) $ removeDuplicates $ foldMap (match0 subs) ss
    where
    lower = fmap Char.toLower
    match0 subs s = foldMap (`match` s) subs
    match sub a = [a | List.isSubsequenceOf sub (lower $ f a)]
    removeDuplicates [] = []
    removeDuplicates (x:xs) =
        x : removeDuplicates (List.filter ((/= f x) . f) xs)
