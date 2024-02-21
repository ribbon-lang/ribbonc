module Language.Ribbon.Syntax.Fixity where

import Text.Pretty




-- | A specification for the broad fixity
--   category of the item definition to look up
data PartialFixity
    -- | Look up an infix operator, with unspecified associativity
    = PInfix
    -- | Look up a prefix operator
    | PPrefix
    -- | Look up a postfix operator
    | PPostfix
    -- | Look up an atomic symbol
    | PAtom
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty PartialFixity where
    pPrint = \case
        PInfix -> "infix"
        PPrefix -> "prefix"
        PPostfix -> "postfix"
        PAtom -> "atom"


-- | A specification for the exact fixity of a definition
data ExactFixity
    -- | An infix operator with left associativity
    = EInfixL
    -- | An infix operator with right associativity
    | EInfixR
    -- | An infix operator with no associativity
    | EInfix
    -- | A prefix operator
    | EPrefix
    -- | A postfix operator
    | EPostfix
    -- | An atomic symbol
    | EAtom
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty ExactFixity where
    pPrint = \case
        EInfixL -> "infixl"
        EInfixR -> "infixr"
        EInfix -> "infix"
        EPrefix -> "prefix"
        EPostfix -> "postfix"
        EAtom -> "atom"


-- | A fixity specification used by definitions to determine
--   overload compatibility
data OverloadFixity
    -- | An infix operator of any associativity
    = OInfix
    -- | A postfix operator
    | OPostfix
    -- | An atomic symbol or prefix operator
    | OAtomPrefix
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty OverloadFixity where
    pPrint = \case
        OInfix -> "infix"
        OPostfix -> "postfix"
        OAtomPrefix -> "atom or prefix"

-- | Convert an @ExactFixity@ to an @OverloadFixity@
exactFixityToOverload :: ExactFixity -> OverloadFixity
exactFixityToOverload = \case
    EInfixL -> OInfix
    EInfixR -> OInfix
    EInfix -> OInfix
    EPrefix -> OAtomPrefix
    EPostfix -> OPostfix
    EAtom -> OAtomPrefix
