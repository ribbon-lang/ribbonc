module Language.Ribbon.Syntax.Path where

import Data.Foldable qualified as Fold

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.Tag
import Data.Attr

import Text.Pretty

import Language.Ribbon.Syntax.Fixity


-- | A definition name, without qualification.
--   Either a symbol or an identifier; never a reserved character sequence
newtype Name
    -- | Construct a @Name@ from a @String@
    = Name
    -- | The @String@ value of a @Name@
    { value :: String }
    deriving (Eq, Ord, Show)

instance Pretty Name where
    pPrint (Name n) = text n



-- | Marker class for paths, allowing to check if they need a slash separator
--   between themselves and a following component
class IsPath b where
    -- | Determine if a path base requires a @\/@ separator
    --   to connect it to components
    pathRequiresSlash :: b -> Bool



-- | A locally-qualified path to a definition,
--   with optional specifiers on names
type LocalPath = Path LocalPathBase LocalPathComponent

-- | A path with its base resolved to an absolute location,
--   with fully resolved specifiers on names
type AbsPath = Path AbsPathBase AbsPathComponent

-- | A path to a definition, with a base to start resolving from,
--   and (depending on instantiation, optionally) specifiers on names
data Path b c
    -- | Construct a @Path@ from a base and a sequence of components
    = Path
    { base :: !(ATag b)
    , components :: !(Seq (ATag c))
    }
    deriving (Eq, Ord, Show)

instance (IsPath b, Pretty b, Pretty c) => Pretty (Path b c) where
    pPrintPrec lvl _ (Path b cs) =
        let csd = hcat $ punctuate "/" (pPrintPrec lvl 0 <$> Fold.toList cs)
        in if pathRequiresSlash b.value && not (Seq.null cs)
            then pPrintPrec lvl 0 b <> "/" <> csd
            else pPrintPrec lvl 0 b <> csd

instance IsPath b => IsPath (Path b c) where
    pathRequiresSlash lp
         = not (Seq.null lp.components)
        || pathRequiresSlash lp.base.value


-- | The root component of an @AbsPath@,
--   specifying where to begin looking up components
data AbsPathBase
    = ApRoot
    -- | Start in a given module
    | ApModule !Name
    -- | Start in a given file
    | ApFile !FilePath
    deriving (Eq, Ord, Show)

instance Pretty AbsPathBase where
    pPrint = \case
        ApRoot -> "/"
        ApModule n -> "module" <+> pPrint n
        ApFile f -> "file" <+> shown f

instance IsPath AbsPathBase where
    pathRequiresSlash = \case
        ApRoot -> False
        _ -> True


-- | The root component of a @LocalPath@,
--   specifying where to begin looking up components
data LocalPathBase
    = LpAbs AbsPathBase
    -- | Start in a given number of levels above the current namespace
    | LpUp !Int
    -- | Start in the current namespace
    | LpThis
    deriving (Eq, Ord, Show)

instance Pretty LocalPathBase where
    pPrint = \case
        LpAbs ab -> pPrint ab
        LpUp i -> text (concat $ replicate i "../")
        LpThis -> "./"

instance IsPath LocalPathBase where
    pathRequiresSlash = \case
        LpAbs ab -> pathRequiresSlash ab
        _ -> False


-- | A component of a @LocalPath@
type LocalPathComponent
    = PathComponent (Maybe PartialFixity) (Maybe ItemDefKind)

-- | A component of an @AbsPath@
type AbsPathComponent
    = PathComponent PartialFixity ItemDefKind

-- | A component of a @Path@, specifying a name to look up, with a specifier
data PathComponent f k
    -- | Specifies a namespace definition
    = PcNamespace !Name
    -- | Specifies a non-namespace definition that may have
    --   a specific fixity and/or a def kind
    | PcItem !f !k !Name
    deriving (Eq, Ord, Show)

instance (Pretty f, Pretty k) => Pretty (PathComponent f k) where
    pPrint = \case
        PcNamespace n -> "namespace" <+> pPrint n
        PcItem f k n -> pPrint f <+> pPrint k <+> pPrint n


-- | A specification for the kind of item definition to look up
data ItemDefKind
    -- | Look up a type definition,
    --   ie either a data type, class, or effect
    = IDefType
    -- | Look up a value definition
    | IDefValue
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty ItemDefKind where
    pPrint = \case
        IDefType -> "type"
        IDefValue -> "value"
