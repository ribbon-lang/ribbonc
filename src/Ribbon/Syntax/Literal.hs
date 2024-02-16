module Ribbon.Syntax.Literal where

import Ribbon.Display




-- | Source-literal values such as numbers, strings
data Literal
    -- | 32-bit signed integer
    = LInt !Int
    -- | 32-bit floating point number
    | LFloat !Float
    -- | UTF-32 codepoint
    | LChar !Char
    -- | UTF-32 encoded string
    | LString !String
    deriving (Eq, Ord, Show)

-- | Kind of a Literal
data LiteralKind
    -- | The kind of a 32-bit signed integer
    = LkInt
    -- | The kind of a 32-bit floating point number
    | LkFloat
    -- | The kind of a UTF-32 codepoint
    | LkChar
    -- | The kind of a UTF-32 encoded string
    | LkString
    deriving (Eq, Ord, Show)


instance Pretty Literal where
    pPrint = \case
        LInt i -> pPrint i
        LFloat f -> pPrint f
        LChar c -> pPrint c
        LString s -> pPrint s

instance Pretty LiteralKind where
    pPrint = \case
        LkInt -> text "int"
        LkFloat -> text "float"
        LkChar -> text "char"
        LkString -> text "string"


-- | Get the LiteralKind of a Literal
literalKind :: Literal -> LiteralKind
literalKind = \case
    LInt _ -> LkInt
    LFloat _ -> LkFloat
    LChar _ -> LkChar
    LString _ -> LkString
