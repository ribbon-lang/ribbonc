module Ribbon.Syntax.Token where

import Ribbon.Display (Display(..))

import Ribbon.Source
import Ribbon.Syntax.Literal


-- | An atom of syntax
type Token = Syn TokenData

-- | An atom of syntax
data TokenData
    -- | A symbolic identifier token,
    --   either punctuation, reserved or user defined
    = TSymbol TokenSymbolKind String
    -- | A token indicating a literal value, such as an int or string
    | TLiteral Literal
    deriving (Eq, Ord, Show)

-- | Kind of a Token
data TokenKind
    -- | Allow any kind of token when given as part of a parsing predicate
    = TkAny
    -- | Designates a symbolic token,
    --   with an optional kind of symbol when
    --   given as part of a parsing predicate
    | TkSymbol (Maybe TokenSymbolKind)
    -- | Designates a literal token,
    --   with an optional kind of literal when
    --   given as part of a parsing predicate
    | TkLiteral (Maybe LiteralKind)
    deriving (Eq, Ord, Show)

-- | Kind of a TokenSymbol
data TokenSymbolKind
    -- | An unreserved identifier token symbol such as @foo@ or @~>@
    = TsIdentifier
    -- | A reserved identifier token symbol such as @let@ or @=@
    | TsKeyword
    -- | A punctuation token symbol such as @,@ or @[@
    | TsPunctuation
    deriving (Eq, Ord, Show)


instance Display TokenData where
    display = \case
        TSymbol _ s -> s
        TLiteral l -> display l

instance Display TokenKind where
    display = \case
        TkAny -> "{anything}"
        TkSymbol (Just k) -> display k <> " symbol"
        TkSymbol _ -> "any symbol"
        TkLiteral (Just k) -> display k <> " literal"
        TkLiteral _ -> "any literal"


instance Display TokenSymbolKind where
    display = \case
        TsIdentifier -> "identifier"
        TsKeyword -> "keyword"
        TsPunctuation -> "punctuation"


-- | Get the TokenKind of a Token
tokenKind :: TokenData -> TokenKind
tokenKind = \case
    TSymbol k _ -> TkSymbol (Just k)
    TLiteral l -> TkLiteral (Just (literalKind l))
