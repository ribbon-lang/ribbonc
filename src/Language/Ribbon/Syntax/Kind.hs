module Language.Ribbon.Syntax.Kind where

import Data.Attr

import Text.Pretty


data Kind
    = KVar !String
    | KConstant !String
    | KArrow !(ATag Kind) !(ATag Kind)
    deriving (Eq, Ord, Show)

instance Pretty Kind where
    pPrintPrec lvl prec = \case
        KVar v -> "''" <> text v
        KConstant c -> text c
        KArrow a b -> pPrintPrec lvl prec a <+> "->" <+> pPrintPrec lvl prec b
