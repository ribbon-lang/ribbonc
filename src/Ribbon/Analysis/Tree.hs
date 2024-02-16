module Ribbon.Analysis.Tree where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Ribbon.Util
import Ribbon.Source
import Ribbon.Display
import Ribbon.Syntax.Ast
import Data.Functor
import qualified Data.List as List
import Data.Bifunctor
import qualified Data.Char as Char
import Control.Monad
import Ribbon.Syntax.Text
import qualified Data.Maybe as Maybe
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import qualified Data.Set as Set

newtype AnalysisContext
    = AnalysisContext
        { modules :: Map (String, Version) (ModuleTree TreeDef)
        }

newtype TreeError
    = TreeError (ATag Doc)

instance Pretty TreeError where
    pPrint (TreeError (doc :@: a)) =
        hang ("error at" <+> bracketed a) doc



lookupModule ::
    AnalysisContext -> ATag (String, Version) -> Either TreeError (ModuleTree TreeDef)
lookupModule ctx nameVer =
    let nv@(name, ver) = untag nameVer
    in case Map.lookup nv ctx.modules of
        Just tree -> Right tree
        _ -> Left $ TreeError $ Tag (tagOf nameVer) $
            hang ("module" <+> text name
                      <+> "with version" <+> pPrint ver
                      <+> "not found in context")
                case modFuzzySearch name (Map.keys ctx.modules) of
                    [] -> mempty
                    suggestions ->
                        hang "Did you mean:" do
                            lsep (suggestions <&> \(sn, sv) ->
                                text sn <+> "with version" <+> pPrint sv)
    where
    modFuzzySearch :: String -> [(String, Version)] -> [(String, Version)]
    modFuzzySearch name = filter (List.isPrefixOf name . fst)


nameFuzzySearch :: String -> [String] -> [String]
nameFuzzySearch name = todo where
    matches sub ss =
        let subs = take 8 $ List.permutations $ lower sub
        in List.sortOn length $ foldMap (match0 subs) ss
    match0 subs s = foldMap (`match` s) subs
    match sub s = [s | List.isSubsequenceOf sub (lower s)]
    lower = fmap Char.toLower


bindAbsPath :: AbsPath -> ATag a -> StateT (ModuleTree a) (Either TreeError) ()
bindAbsPath p d = todo
    -- m <- get
    -- case Map.lookup p (mtDefs m) of
    --     Just existing -> do
    --         when (Set.member d existing) do
    --             throwError $ TreeError $ Tag (tagOf d) $
    --                 hang ("duplicate definition of" <+> pPrint p) do
    --                     "existing definition here:" <+> pPrint (Set.findMin existing)
    --     _ -> put $ m { mtDefs = Map.insert p (Set.singleton d) (mtDefs m) }

    -- throwError $ TreeError $ Tag (tagOf d) $
    --     hang ("duplicate definition of" <+> pPrint p) do
    --         "existing definition here:" <+>


buildTree :: AnalysisContext -> ProtoModule -> Either Doc (ModuleTree MiddleDef)
buildTree ctx ProtoModule{..} = todo --first pPrint do
    -- let ProtoModuleHead{..} = untag pmHead
    --     newHead =
    --         ModuleTreeHead
    --         { mthName = pmhName
    --         , mthVersion = pmhVersion
    --         , mthMeta = pmhMeta
    --         }
    -- snd <$> flip runStateT (ModuleTree newHead mempty) do
    --     resolvedDependencies <-
    --         zip (mdAlias . untag <$> pmhDependencies) <$>
    --             traverse (traverse (lift . lookupModule ctx . mdNameVer)) pmhDependencies

    --     renamedDependencies <-
    --         forM resolvedDependencies \(alias, tree) -> do
    --             let actualName = untag $ mthName (mtHead $ untag tree)

    --             when (Maybe.isNothing alias && mustAlias actualName) do
    --                 throwError $ TreeError $ Tag (tagOf tree) $
    --                     "imported module name"
    --                     <+> pPrint actualName
    --                     <+> "is not a valid identifier, and must be aliased"

    --             pure $ bimap
    --                 ((`AbsPath` []) . fmap ApModule)
    --                 (fmap $ TreeBinding Private . TreeDependency)
    --                 case alias of
    --                     Just n -> (n, reTagFrom n tree)
    --                     _ -> (Name actualName :@: tagOf tree, tree)

    --     traverse_ (uncurry bindAbsPath ) renamedDependencies


    -- where
    -- mustAlias = not . isIdentifier
