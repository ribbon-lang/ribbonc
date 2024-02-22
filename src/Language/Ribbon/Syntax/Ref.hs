module Language.Ribbon.Syntax.Ref where

import Data.Map.Strict (Map)
import Data.Word (Word32)

import Data.Attr

import Text.Pretty

import Language.Ribbon.Syntax.Category



-- | A definition of some item in a module, with a parent.
--   the parent is either the namespace or type the item was defined in,
--   or the module if it is the root namespace
data Def v
    = Def
    { parent :: !Ref
    , inner :: !(ATag v)
    }
    deriving (Eq, Ord, Show)

-- | A reference to a specific module
newtype ModuleId = ModuleId Word32
    deriving (Eq, Ord, Num, Enum, Bounded, Show, Pretty)

-- | A reference to a specific element in a module
newtype ElementId = ElementId Word32
    deriving (Eq, Ord, Num, Enum, Bounded, Show, Pretty)

-- | A reference to a specific item in a specific module,
--   with its @ExactCategory@
data Ref
    = Ref
    { category :: !ExactCategory
    , moduleId :: !ModuleId
    , elementId :: !ElementId
    }
    deriving (Eq, Ord, Show)

-- | A map from @Ref@s to some category of item
type RefMap = Map Ref

instance CatOverloaded Ref where
    overloadCategory = overloadCategory . (.category)

instance Pretty Ref where
    pPrintPrec _ _ (Ref k m e) =
        pPrint k <> "@" <> pPrint m <> "." <> pPrint e




