module Ribbon.Syntax.Token where

import Data.Functor
import Data.Foldable

import Data.Sequence (Seq)

import Ribbon.Display
import Ribbon.Source
import Ribbon.Syntax.Literal
import Ribbon.Syntax.Text



-- | Encodes precedence levels
type Prec = Int

-- | An atom of syntax
data Token
    -- | A symbolic token,
    --   either punctuation, reserved or user defined
    = TSymbol !String
    -- | A token indicating a literal value, such as an int or string
    | TLiteral !Literal
    -- | End of file token
    | TEof
    deriving (Eq, Ord, Show)

instance Pretty Token where
    pPrint = \case
        TSymbol s -> text s
        TLiteral l -> pPrint l
        TEof -> text "{EOF}"

instance Pretty (Seq (ATag Token)) where
    pPrint ts = brackets $ vcat' $ toList ts <&> \(t :@: a) ->
        backticked t <+> text "@" <+> pPrint a

-- | Check if a token terminates expressions (ie @,@, @}@ etc)
isSentinelToken :: Token -> Bool
isSentinelToken = \case
    TEof -> True
    TSymbol s -> isSentinel s
    _ -> False

-- | Check if a string is a symbol token with the given value
isSymbolToken :: String -> Token -> Bool
isSymbolToken s = \case
    TSymbol s' -> s == s'
    _ -> False



-- | Loosely specifies a pattern for matching against Token
data TokenSpec
    -- | Expect a TSymbol with optional value
    = TsSymbol !String
    -- | Expect a TLiteral with optional kind
    | TsLiteral !(Maybe LiteralKind)
    -- | Expect an end of file token
    | TsEof
    deriving (Eq, Ord, Show)

instance Pretty TokenSpec where
    pPrint = \case
        TsSymbol "" -> text "symbol"
        TsSymbol s -> text s
        TsLiteral Nothing -> text "literal"
        TsLiteral (Just k) -> pPrint k
        TsEof -> text "eof"
