module Language.Ribbon.Lexical.Token where

import Data.Functor
import Data.Foldable
import Data.Tag
import Data.Attr

import Data.Sequence (Seq)

import Text.Pretty

import Language.Ribbon.Util

import Language.Ribbon.Lexical.Literal
import Language.Ribbon.Lexical.Path
import Language.Ribbon.Lexical.Name
import Language.Ribbon.Lexical.Version
import Language.Ribbon.Parsing.Text
import Control.Monad

-- | A lexical sequence of @Token@s ready for parsing
type TokenSeq = Seq (ATag Token)

-- | An atom of syntax
data Token
    -- | A symbolic token,
    --   either punctuation, reserved or user defined
    = TSymbol !String
    -- | A token indicating a literal value, such as an int or string
    | TLiteral !Literal
    -- | A token indicating a semantic version number
    | TVersion !Version
    -- | A semantically significant space
    | TSemSpace
    -- | A sequence of tokens delimited by something
    | TTree !TokenTree
    -- | An end of file token
    | TEof
    deriving (Eq, Ord, Show)

instance Pretty Token where
    pPrint = \case
        TSymbol s -> text s
        TLiteral l -> pPrint l
        TVersion v -> pPrint v
        TSemSpace -> "{SEM-SPACE}"
        TTree t -> pPrint t
        TEof -> "{EOF}"

instance Pretty TokenSeq where
    pPrintPrec lvl _ ts = sep $ toList ts <&> \(t :@: a) ->
        pPrint t <+> maybeMEmpty do
            guard (lvl > PrettyNormal)
            Just $ "@" <+> pPrintPrec lvl 0 a

data TokenTree
    = TtBlock !BlockKind !TokenSeq
    | TtPath !Path
    deriving (Eq, Ord, Show)

instance Pretty TokenTree where
    pPrint = \case
        TtBlock k ts -> blockPrint k ts
        TtPath p -> pPrint p

data BlockKind
    = BkParen
    | BkBrace
    | BkBracket
    | BkIndent
    | BkLine
    deriving (Eq, Ord, Show)

instance Pretty BlockKind where
    pPrint = \case
        BkParen -> "parenthesis"
        BkBrace -> "brace"
        BkBracket -> "bracket"
        BkIndent -> "indentation"
        BkLine -> "line"

blockPrint :: BlockKind -> TokenSeq -> Doc
blockPrint = \case
    BkParen -> parens . pPrint
    BkBrace -> braces . pPrint
    BkBracket -> brackets . pPrint
    BkIndent -> indent . vcat' . (["↘"] <>) . (<> ["↖"]) . toList . fmap pPrint
    BkLine -> hsep . (["◁"] <>) . (<> ["▷"]) . toList . fmap pPrint

isEof :: Token -> Bool
isEof = \case
    TEof -> True
    _ -> False

-- | Check if a token terminates expressions (ie @,@, @}@ etc)
isSentinel :: Token -> Bool
isSentinel = \case
    TSemSpace -> True
    TSymbol s -> s `elem` [")", "]", "}", ",", "=", ":"]
    _ -> False

-- | Check if a token is a symbol with the given value
isSymbol :: String -> Token -> Bool
isSymbol s = \case
    TSymbol s' -> s == s'
    _ -> False

-- | Check if a token has a reserved value; and return it if it doesnt
filterUnreserved :: Token -> Maybe String
filterUnreserved = \case
    TSymbol s | s `notElem` reservedSymbols -> Nothing
    _ -> Nothing

-- | Check if a token has a reserved value; ie cannot be used as a Name
isReserved :: Token -> Bool
isReserved = \case
    TSymbol s -> s `elem` reservedSymbols
    _ -> True

-- | Check if a token is a semantic space
isSemSpace :: Token -> Bool
isSemSpace = \case
    TSemSpace -> True
    _ -> False




-- | Loosely specifies a pattern for matching against Token
data TokenSpec
    -- | Expect a TSymbol with optional value
    = TsSymbol !String
    -- | Expect a TLiteral with optional kind
    | TsLiteral !(Maybe LiteralKind)
    -- | Expect a TVersion
    | TsVersion
    -- | Expect a TSemSpace
    | TsSemSpace
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
        TsSemSpace -> "semantic space"
        TsEof -> "eof"
