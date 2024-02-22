module Language.Ribbon.Lexical.Version where

import Data.Nil

import Text.Pretty




-- | A semantic version specifier
data Version
    = Version
    -- | incompatible API changes
    { major :: !Word
    -- | added functionality in a backward compatible manner
    , minor :: !Word
    -- | backward compatible bug fixes
    , patch :: !Word
    }
    deriving (Show, Eq, Ord)

instance Pretty Version where
    pPrintPrec _ _ (Version major minor patch) = do
        shown major <> "." <> shown minor <> "." <> shown patch

instance Nil Version where
    nil = Version 0 0 0
    isNil = (== Nil)
