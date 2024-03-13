module Language.Ribbon.Syntax.Ref where

import Data.Word (Word32, Word64)

import Data.Nil

import Text.Pretty




-- | A reference to a specific module
newtype ModuleId = ModuleId { value :: Word32 }
    deriving (Eq, Ord, Num, Enum, Bounded, Show)

instance Nil ModuleId where
    nil = ModuleId maxBound
    isNil (ModuleId mi) = mi == maxBound

instance Pretty ModuleId where
    pPrintPrec _ _ (ModuleId m) = pPrint m <> "↓"


-- | A reference to a specific element in a module
newtype ItemId = ItemId { value :: Word64 }
    deriving (Eq, Ord, Num, Enum, Bounded, Show)

instance Nil ItemId where
    nil = ItemId maxBound
    isNil (ItemId ii) = ii == maxBound

instance Pretty ItemId where
    pPrintPrec _ _ (ItemId e) = "↓" <> pPrint e


-- | A reference to a specific item in a specific module
data Ref
    = Ref
    { moduleId :: !ModuleId
    , elementId :: !ItemId
    }
    deriving (Eq, Ord, Show)

instance Pretty Ref where
    pPrintPrec _ _ (Ref (ModuleId m) (ItemId e)) =
        joinWith "↓"
            (pPrint m)
            (pPrint e)
