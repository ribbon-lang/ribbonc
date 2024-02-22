module Language.Ribbon.Lexical.Token where

import Data.Functor
import Data.Foldable
import Data.Tag
import Data.Attr

import Data.Sequence (Seq)

import Text.Pretty

import Language.Ribbon.Lexical.Literal
import Language.Ribbon.Lexical.Version


-- | An atom of syntax
data Token
    -- | A symbolic token,
    --   either punctuation, reserved or user defined
    = TSymbol !String
    -- | A token indicating a literal value, such as an int or string
    | TLiteral !Literal
    -- | A token indicating a semantic version number
    | TVersion !Version
    -- | End of file token
    | TEof
    deriving (Eq, Ord, Show)

instance Pretty Token where
    pPrint = \case
        TSymbol s -> text s
        TLiteral l -> pPrint l
        TVersion v -> pPrint v
        TEof -> "{EOF}"

instance Pretty (Seq (ATag Token)) where
    pPrint ts = brackets $ vcat' $ toList ts <&> \(t :@: a) ->
        backticked t <+> "@" <+> pPrint a

-- | Check if a token terminates expressions (ie @,@, @}@ etc)
isSentinel :: Token -> Bool
isSentinel = \case
    TEof -> True
    TSymbol s -> s `elem` [")", "]", "}", ",", "=", ":"]
    _ -> False

-- | Check if a token is a symbol with the given value
isSymbol :: String -> Token -> Bool
isSymbol s = \case
    TSymbol s' -> s == s'
    _ -> False

-- | Check if a token has a reserved value; ie cannot be used as a Name
isReserved :: Token -> Bool
isReserved = \case
    TSymbol s -> s `elem` reservedSymbols
    _ -> True

-- | Reserved identifiers
reservedSymbols :: [String]
reservedSymbols =
    [ "type", "effect", "value", "forall", "fun"
    , "infix", "infixl", "infixr", "prefix", "postfix", "atom"
    , "module", "import", "use", "file", "pub", "namespace"
    , "let", "in", "as"
    , "match", "with"
    , "if", "then", "else"
    , "=", ":", "=>", ";", ",", ".", "..", "./", "../", ".*"
    , "{", "}", "(", ")", "[", "]"
    ]



-- | Loosely specifies a pattern for matching against Token
data TokenSpec
    -- | Expect a TSymbol with optional value
    = TsSymbol !String
    -- | Expect a TLiteral with optional kind
    | TsLiteral !(Maybe LiteralKind)
    -- | Expect a TVersion
    | TsVersion
    -- | Expect an end of file token
    | TsEof
    deriving (Eq, Ord, Show)

instance Pretty TokenSpec where
    pPrint = \case
        TsSymbol "" -> "symbol"
        TsSymbol s -> text s
        TsLiteral Nothing -> "literal"
        TsLiteral (Just k) -> pPrint k
        TsVersion -> "version"
        TsEof -> "eof"
