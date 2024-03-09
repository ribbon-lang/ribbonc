module Language.Ribbon.Syntax.Ref where

import Data.Word (Word32, Word64)

import Text.Pretty



-- | A reference to a specific module
newtype ModuleId = ModuleId Word32
    deriving (Eq, Ord, Num, Enum, Bounded, Show, Pretty)

-- | A reference to a specific element in a module
newtype ItemId = ItemId Word64
    deriving (Eq, Ord, Num, Enum, Bounded, Show, Pretty)


-- | A reference to a specific item in a specific module
data Ref
    = Ref
    { moduleId :: !ModuleId
    , elementId :: !ItemId
    }
    deriving (Eq, Ord, Show)

instance Pretty Ref where
    pPrintPrec _ _ (Ref m e) =
        pPrint m <> "." <> pPrint e
