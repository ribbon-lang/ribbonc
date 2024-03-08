module Language.Ribbon.Lexical.Path where

import Data.Foldable qualified as Fold

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.Tag
import Data.Attr

import Text.Pretty

import Language.Ribbon.Util

import Language.Ribbon.Lexical.Fixity
import Language.Ribbon.Lexical.Category
import Language.Ribbon.Lexical.Name
import Data.Nil
import qualified Data.Maybe as Maybe



-- | A class for path types to determine if they require a trailing slash
class RequiresSlash a where
    -- | Determine if a path requires a trailing slash
    requiresSlash :: a -> Bool

instance RequiresSlash a => RequiresSlash (Tag t a) where
    requiresSlash = requiresSlash . untag

instance RequiresSlash a => RequiresSlash (Maybe a) where
    requiresSlash = maybe False requiresSlash

instance RequiresSlash (Seq a) where
    requiresSlash = not . Seq.null



-- | A path to a definition, with a base to start resolving from,
--   and specifiers on names
data Path
    = Path
    { base :: !(Maybe (ATag PathBase))
    , components :: !(Seq (ATag PathName))
    }
    deriving (Eq, Ord, Show)

-- | Pattern alias for a @Path@ with a single @PathName@ component
pattern SingleNamePath :: PathName -> Path
pattern SingleNamePath pn <-
    Path Nothing ((pn :@: _) Seq.:<| Nil)

-- | Pattern alias for a @Path@ with a single @FixName@ component
pattern SingleFixPath :: FixName -> Path
pattern SingleFixPath fn <-
    SingleNamePath (FixPathName fn)

-- | Pattern alias for a @Path@ with a single @SimpleName@ component
pattern SingleSimplePath :: SimpleName -> Path
pattern SingleSimplePath sn <-
    SingleFixPath (SimpleFixName sn)

instance Pretty Path where
    pPrintPrec lvl _ (Path b cs) =
        let csd = hcat $ punctuate "/" (pPrintPrec lvl 0 <$> Fold.toList cs)
            bd = maybeMEmpty $ pPrintPrec lvl 0 <$> b
        in if requiresSlash b
            then bd </> csd
            else bd <> csd

instance RequiresSlash Path where
    requiresSlash lp
        = requiresSlash lp.components
        || requiresSlash lp.base

instance HasFixity Path where
    getFixity (Path _ (_ Seq.:|> c)) = getFixity c.value
    getFixity _ = Atom

instance Semigroup Path where
    Path b1 c1 <> Path b2 c2 =
        case b2 of
            Nothing -> Path b1 (c1 <> c2)
            _ -> Path b2 c2

instance Monoid Path where
    mempty = Path Nothing Nil

instance Nil Path where
    isNil p = Maybe.isNothing p.base && Seq.null p.components


pathCategory :: Path -> Maybe OverloadCategory
pathCategory (Path _ (_ Seq.:|> c)) = c.value.category
pathCategory _ = Just ONamespace



-- | The base component of a @Path@,
--   specifying where to begin looking up components
data PathBase
    -- | Start at the root of the active module
    = PbRoot
    -- | Start in the current namespace
    | PbThis
    -- | Start in a given module
    | PbModule !SimpleName
    -- | Start in a given file
    | PbFile !FilePath
    -- | Start a given number of levels above the current namespace
    | PbUp !Int
    deriving (Eq, Ord, Show)

instance Pretty PathBase where
    pPrint = \case
        PbRoot -> "/"
        PbThis -> "./"
        PbModule n -> "module" <+> pPrint n
        PbFile f -> "file" <+> shown f
        PbUp i -> text (concat $ replicate i "../")

instance RequiresSlash PathBase where
    requiresSlash = \case
        PbModule _ -> True
        PbFile _ -> True
        _ -> False

