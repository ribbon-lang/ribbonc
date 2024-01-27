module Ribbon.Syntax.Token where

import Ribbon.Display (Display(..))
import Ribbon.Display qualified as Display

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
    -- | Designates a token of any kind besides
    --   sentinels such as @{@, @]@, @,@ etc
    | TkNonSentinel
    -- | Designates a symbolic token,
    --   with an optional kind of symbol when
    --   given as part of a parsing predicate
    | TkSymbol (Maybe TokenSymbolKind) String
    -- | Designates a literal token,
    --   with an optional kind of literal when
    --   given as part of a parsing predicate
    | TkLiteral (Maybe LiteralKind)
    deriving (Eq, Ord, Show)

-- | Kind of a TokenSymbol
data TokenSymbolKind
    -- | An unreserved identifier token symbol such as @foo@ or @_0@
    = TsIdentifier
    -- | An unreserved operator token symbol such as @+@ or @->@
    | TsOperator
    -- | A reserved identifier token symbol such as @let@ or @=@
    | TsReserved
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
        TkNonSentinel -> "{non-sentinel}"
        TkSymbol (Just k) s -> display k <> " symbol"
            <> if null s then "" else " " <> s
        TkSymbol{} -> "any symbol"
        TkLiteral (Just k) -> display k <> " literal"
        TkLiteral _ -> "any literal"


instance Display TokenSymbolKind where
    display = \case
        TsIdentifier -> "identifier"
        TsOperator -> "operator"
        TsReserved -> "reserved"
        TsPunctuation -> "punctuation"


-- | Get the TokenKind of a Token
tokenKind :: TokenData -> TokenKind
tokenKind = \case
    TSymbol k s -> TkSymbol (Just k) s
    TLiteral l -> TkLiteral (Just (literalKind l))


-- | Get a minimal string representing a token kind.
--   This is like the Display impl, but without the
--   "symbol" etc prefix when symbol values are present,
--   for printing as a concise list
tokenKindName :: TokenKind -> String
tokenKindName = \case
    TkAny -> "{anything}"
    TkNonSentinel -> "{non-sentinel}"
    TkSymbol _ s | not (null s) -> Display.backticks s
    TkSymbol (Just k) _ -> display k
    TkSymbol Nothing _ -> "symbol"
    TkLiteral Nothing -> "literal"
    TkLiteral (Just k) -> display k