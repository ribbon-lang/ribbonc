module Language.Ribbon.Lexical.Fixity where

import Text.Pretty


-- | Class for name-likes to extract their fixity
class HasFixity a where
    -- | Extract the fixity of a name-like
    getFixity :: a -> Fixity


-- | Fixity of a name
data Fixity
    = Atom
    | Prefix
    | Infix
    | Postfix
    deriving (Eq, Ord, Enum, Bounded, Show)

instance Pretty Fixity where
    pPrint = \case
        Atom -> "atom"
        Prefix -> "prefix"
        Infix -> "infix"
        Postfix -> "postfix"

instance HasFixity Fixity where
    getFixity = id


data OverloadFixity
    = OAtomPrefix
    | OInfix
    | OPostfix
    deriving (Eq, Ord, Enum, Bounded, Show)

instance Pretty OverloadFixity where
    pPrint = \case
        OAtomPrefix -> "atom/prefix"
        OInfix -> "infix"
        OPostfix -> "postfix"

overloadedFixity :: Fixity -> OverloadFixity
overloadedFixity = \case
    Atom -> OAtomPrefix
    Prefix -> OAtomPrefix
    Infix -> OInfix
    Postfix -> OPostfix
