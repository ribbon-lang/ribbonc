module Language.Ribbon.Syntax.Fixity where

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
    deriving (Eq, Ord, Show)

instance Pretty Fixity where
    pPrint = \case
        Atom -> "atom"
        Prefix -> "prefix"
        Infix -> "infix"
        Postfix -> "postfix"

instance HasFixity Fixity where
    getFixity = id

fixityCompare :: Fixity -> Fixity -> Ordering
fixityCompare a b = case (a, b) of
    (Prefix, Atom) -> EQ
    (Atom, Prefix) -> EQ
    _ -> compare a b
