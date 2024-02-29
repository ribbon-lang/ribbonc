module Language.Ribbon.Syntax.Associativity where

import Text.Pretty




-- | Class for name-likes to extract their associativity
class HasAssociativity a where
    -- | Extract the associativity of a name-like
    getAssociativity :: a -> Associativity


-- | Associativity of a name
data Associativity
    = LeftAssociative
    | RightAssociative
    | NonAssociative
    deriving (Eq, Ord, Show)

instance Pretty Associativity where
    pPrint = \case
        LeftAssociative -> "left associative"
        RightAssociative -> "right associative"
        NonAssociative -> "non-associative"

instance HasAssociativity Associativity where
    getAssociativity = id
