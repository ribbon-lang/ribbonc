module Language.Ribbon.Syntax.Type where

import Data.Word(Word32)

import Text.Pretty




-- | A type expression ast.
--   Gives the type of a @Value@
data Type
    = TConstant !Constant
    | TFIXME -- placeholder
    deriving (Eq, Ord, Show)

instance Pretty Type where
    pPrintPrec lvl _ = \case
        TConstant a -> pPrintPrec lvl 0 a
        _ -> error "FIXME"


data Constant
    = CInt !Word32
    | CString !String
    deriving (Eq, Ord, Show)

instance Pretty Constant where
    pPrintPrec lvl _ (CInt a) = pPrintPrec lvl 0 a
    pPrintPrec lvl _ (CString a) = pPrintPrec lvl 0 a
