module Language.Ribbon.Syntax.Ref where

import Data.Map.Strict (Map)
import Data.Word (Word32)

import Text.Pretty




-- | A map from @Ref@s to some category of item
type RefMap = Map Ref


-- | A reference to a specific module
newtype ModuleId = ModuleId Word32
    deriving (Eq, Ord, Num, Enum, Bounded, Show, Pretty)

-- | A reference to a specific element in a module
newtype ElementId = ElementId Word32
    deriving (Eq, Ord, Num, Enum, Bounded, Show, Pretty)


-- | A reference to a specific item in a specific module
data Ref
    = Ref
    { moduleId :: !ModuleId
    , elementId :: !ElementId
    }
    deriving (Eq, Ord, Show)

instance Pretty Ref where
    pPrintPrec _ _ (Ref m e) =
        pPrint m <> "." <> pPrint e
