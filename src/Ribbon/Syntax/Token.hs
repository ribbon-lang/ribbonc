module Ribbon.Syntax.Token where

import Data.List qualified as List
import Data.Maybe qualified as Maybe

import Ribbon.Display (Display(..))
import Ribbon.Display qualified as Display

import Ribbon.Source
import Ribbon.Syntax.Text
import Ribbon.Syntax.Literal


-- | An atom of syntax
type Token = Tag Attr TokenData

-- | An atom of syntax
data TokenData
    -- | A symbolic identifier token,
    --   either punctuation, reserved or user defined
    = TSymbol !TokenSymbolKind !String
    -- | A token indicating a literal value, such as an int or string
    | TLiteral !Literal
    -- | End of file token
    | TEof
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
    | TkSymbol !(Maybe TokenSymbolKind) !String
    -- | Designates a literal token,
    --   with an optional kind of literal when
    --   given as part of a parsing predicate
    | TkLiteral !(Maybe LiteralKind)
    -- | Designates the end of file
    | TkEof
    deriving (Eq, Ord, Show)

-- | Kind of a TokenSymbol
data TokenSymbolKind
    -- | An unreserved identifier token symbol such as @foo@ or @_0@
    = TsIdentifier
    -- | An unreserved operator token symbol such as @+@ or @->@
    | TsOperator
    -- | A reserved identifier or operator token symbol such as @let@ or @=@
    | TsReserved
    -- | A punctuation token symbol such as @,@ or @[@
    | TsPunctuation
    deriving (Eq, Ord, Show)


instance Display TokenData where
    display = \case
        TSymbol _ s -> s
        TLiteral l -> display l
        TEof -> "{EOF}"

instance Display TokenKind where
    display = \case
        TkAny -> "{anything}"
        TkNonSentinel -> "{non-sentinel}"
        TkSymbol (Just k) s -> display k <> " symbol"
            <> if null s then "" else " " <> s
        TkSymbol{} -> "any symbol"
        TkLiteral (Just k) -> display k <> " literal"
        TkLiteral _ -> "any literal"
        TkEof -> "end of file"


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
    TEof -> TkEof


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
    TkEof -> "{EOF}"


-- | Match a TokenKind to a TokenData
tkSelect :: TokenKind -> TokenData -> Bool
tkSelect = curry \case
    (TkAny, _) -> True

    (TkNonSentinel, TSymbol _ s)
        | not (isSentinel s)
        -> True

    (TkNonSentinel, TLiteral _) -> True

    (TkSymbol ka a, TSymbol kb b)
        | Maybe.isNothing ka || ka == Just kb
        , a == b || a == ""
        -> True

    (TkLiteral (Just a), TLiteral b)
        | a == literalKind b
        -> True

    (TkLiteral Nothing, TLiteral _) -> True

    _ -> False

-- | Find an entry in a TokeKind associative array matching the given TokenData
tkFind :: [(TokenKind, a)] -> TokenData -> Maybe a
tkFind [] _ = Nothing
tkFind ((k, a):rest) t
    | tkSelect k t = Just a
    | otherwise = tkFind rest t

tblExpects :: String -> [(TokenKind, a)] -> [String]
tblExpects opKind es =
    let xs = (tokenKindName . fst <$> es) List.\\ ["{anything}", "{non-sentinel}"]
        (ops, ws) = List.partition (any isOperatorSubsequent) xs
        (ps, ops') = List.partition (any isPunctuation) ops
    in finalizeOps ops' : ws <> ps
    where
        finalizeOps ops = opKind <> " operator of " <> display ops
