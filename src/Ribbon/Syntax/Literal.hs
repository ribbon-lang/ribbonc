module Ribbon.Syntax.Literal where

import Ribbon.Display (Display(..))
-- import Ribbon.Display qualified as Display


-- | Source-literal values such as numbers, strings
data Literal
    -- | 32-bit signed integer
    = LInt Int
    -- | 32-bit floating point number
    | LFloat Float
    -- | UTF-32 codepoint
    | LChar Char
    -- | UTF-32 encoded string
    | LString String
    deriving (Eq, Ord, Show)

-- | Kind of a Literal
data LiteralKind
    = LkInt
    | LkFloat
    | LkChar
    | LkString
    deriving (Eq, Ord, Show)


instance Display Literal where
    display = \case
        LInt i -> display i
        LFloat f -> display f
        LChar c -> display c
        LString s -> display s

instance Display LiteralKind where
    display = \case
        LkInt -> "int"
        LkFloat -> "float"
        LkChar -> "char"
        LkString -> "string"


-- | Get the LiteralKind of a Literal
literalKind :: Literal -> LiteralKind
literalKind = \case
    LInt _ -> LkInt
    LFloat _ -> LkFloat
    LChar _ -> LkChar
    LString _ -> LkString
