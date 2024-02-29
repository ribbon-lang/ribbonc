module Language.Ribbon.Syntax.Path where

import Data.Foldable qualified as Fold

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.Tag
import Data.Attr

import Text.Pretty

import Language.Ribbon.Util

import Language.Ribbon.Syntax.Fixity
import Language.Ribbon.Syntax.Category
import Language.Ribbon.Syntax.Name




-- | A path to a definition, with a base to start resolving from,
--   and specifiers on names
data Path
    = Path
    { base :: !(ATag PathBase)
    , components :: !(Seq (ATag PathComponent))
    }
    deriving (Eq, Ord, Show)

instance Pretty Path where
    pPrintPrec lvl _ (Path b cs) =
        let csd = hcat $ punctuate "/" (pPrintPrec lvl 0 <$> Fold.toList cs)
        in if pathBaseRequiresSlash b.value
            then pPrintPrec lvl 0 b </> csd
            else pPrintPrec lvl 0 b <> csd

pathRequiresSlash :: Path -> Bool
pathRequiresSlash lp
        = not (Seq.null lp.components)
    || pathBaseRequiresSlash lp.base.value

instance CatOverloaded Path where
    overloadedCategory (Path _ (_ Seq.:|> c)) = overloadedCategory c.value
    overloadedCategory _ = ONamespace

instance HasFixity Path where
    getFixity (Path _ (_ Seq.:|> c)) = getFixity c.value
    getFixity _ = Atom


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

pathBaseRequiresSlash :: PathBase -> Bool
pathBaseRequiresSlash = \case
    PbModule _ -> True
    PbFile _ -> True
    _ -> False

-- | A component of a @Path@,
--   specifying a name to look up, at a particular fixity and category
data PathComponent
    = PathComponent
    { category :: !OverloadCategory
    , name :: !FixName
    }
    deriving (Eq, Ord, Show)

instance Pretty PathComponent where
    pPrint = \case
        PathComponent k n -> hsep
            [ pPrint k
            , maybeBackticked (needsEscape n) n
            ]

instance CatOverloaded PathComponent where
    overloadedCategory (PathComponent k _) = k

instance HasFixity PathComponent where
    getFixity (PathComponent _ n) = getFixity n
