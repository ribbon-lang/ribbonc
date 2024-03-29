module Language.Ribbon.Lexical.Literal where

import Data.Word(Word32)

import Data.SyntaxError

import Text.Pretty




-- | Source-literal values such as numbers, strings
data Literal
    -- | 32-bit unsigned integer
    = LInt !Word32
    -- | 32-bit floating point number
    | LFloat !Float
    -- | UTF-32 codepoint
    | LChar !Char
    -- | UTF-32 encoded string
    | LString !String
    deriving (Eq, Ord, Show)

instance SyntaxInput Literal where
    inputPretty = \case
        LInt x -> "integer literal" <> backticked x
        LFloat x -> "float literal" <> backticked x
        LString x -> "string literal" <> shown x
        LChar x -> "character literal" <> shown x

instance Pretty Literal where
    pPrint = \case
        LInt i -> pPrint i
        LFloat f -> pPrint f
        LChar c -> shown c
        LString s -> shown s

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

instance Pretty LiteralKind where
    pPrint = \case
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
