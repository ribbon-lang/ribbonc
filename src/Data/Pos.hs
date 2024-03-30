module Data.Pos where

import Data.Word (Word32)

import Data.Nil

import Text.Pretty
import Language.Ribbon.Util




-- | Codepoint-indexed position in a source file,
--   as well as its line and column numbers,
--   and an indentation level
data Pos
    = Pos
    { offset :: !Word32
    ,   line :: !Word32
    , column :: !Word32
    }

instance Show Pos where
    show = prettyShowLevel PrettyVerbose

instance Pretty Pos where
    pPrintPrec lvl _ p@(Pos o l c) =
        select (isNil p) "{nil}"
        let s = pPrint l <> ":" <> pPrint c
        in if lvl > PrettyRich
            then s <> parens (pPrint o)
            else s

instance Eq Pos where
    a == b = a.offset == b.offset

instance Ord Pos where
    compare a b = compare a.offset b.offset

instance Nil Pos where
    isNil = (== Nil)
    nil = Pos 0 0 0

-- | Determine if two @Pos@ are adjacent in terms of line and column
posConnected :: Pos -> Pos -> Bool
posConnected a b = a.line == b.line && a.column == b.column
