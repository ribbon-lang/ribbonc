module Language.Ribbon.Syntax.Fixity where

import Text.Pretty


-- | A class for types that can have a fixity,
--   in the context of an overloaded name
class FixOverloaded a where
    -- | Get the @OverloadFixity@ of a value
    overloadFixity :: a -> OverloadFixity


-- | A specification for the exact fixity of a definition
data ExactFixity
    -- | An infix operator with left associativity
    = InfixL
    -- | An infix operator with right associativity
    | InfixR
    -- | An infix operator with no associativity
    | Infix
    -- | A prefix operator
    | Prefix
    -- | A postfix operator
    | Postfix
    -- | An atomic symbol
    | Atom
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty ExactFixity where
    pPrint = \case
        InfixL -> "infixl"
        InfixR -> "infixr"
        Infix -> "infix"
        Prefix -> "prefix"
        Postfix -> "postfix"
        Atom -> "atom"

instance FixOverloaded ExactFixity where
    overloadFixity = exactFixityToOverload

-- | Convert an @ExactFixity@ to an @OverloadFixity@
exactFixityToOverload :: ExactFixity -> OverloadFixity
exactFixityToOverload = \case
    InfixL -> OInfix
    InfixR -> OInfix
    Infix -> OInfix
    Prefix -> OAtomPrefix
    Postfix -> OPostfix
    Atom -> OAtomPrefix

-- | A fixity specification used by definitions to determine
--   overload compatibility
data OverloadFixity
    -- | An infix operator of any associativity
    = OInfix
    -- | A postfix operator
    | OPostfix
    -- | An atomic symbol or prefix operator
    | OAtomPrefix
    -- | An unspecified fixity
    | OUnspecified
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty OverloadFixity where
    pPrint = \case
        OInfix -> "infix"
        OPostfix -> "postfix"
        OAtomPrefix -> "atom or prefix"
        OUnspecified -> "unspecified fixity"

instance FixOverloaded OverloadFixity where
    overloadFixity = id
