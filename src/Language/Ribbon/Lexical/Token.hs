module Language.Ribbon.Lexical.Token where

import Data.Functor.Identity
import Data.Foldable

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Data.Tag
import Data.Attr
import Data.Nil
import Data.SyntaxError

import Control.Monad
import Control.Applicative
import Control.Monad.State.Dynamic


import Text.Pretty

import Language.Ribbon.Util
import Language.Ribbon.Lexical.Literal
import Language.Ribbon.Lexical.Version
import Language.Ribbon.Parsing.Text
import Language.Ribbon.Lexical.Name




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
    -- | A sequence of tokens delimited by something
    | TTree !BlockKind !TokenSeq
    -- | A variable name with operator semantics
    | TFixName !FixName
    deriving (Eq, Ord, Show)

instance SyntaxInput Token where
    inputPretty = \case
        TSymbol n -> "symbol" <+> backticks (text n)
        TLiteral l -> inputPretty l
        TVersion v -> "version" <+> backticked v
        TTree k _ -> pPrint k <+> "tree"
        TFixName f -> pPrint f

instance (Applicative m, MonadState BlockCounter m) => PrettyWith m Token where
    pPrintPrecWith lvl prec = \case
        TSymbol s -> pure (surround "#" "#" $ text s)
        TLiteral l -> pPrintPrecWith lvl prec l
        TVersion v -> pPrintPrecWith lvl prec v
        TTree k ts -> blockPrintWith lvl k ts
        TFixName f -> pPrintPrecWith lvl prec f

instance Pretty Token where
    pPrintPrec lvl prec t =
        runIdentity $ evalStateT (pPrintPrecWith lvl prec t) emptyBlockCounter

instance MonadState BlockCounter m => PrettyWith m TokenSeq where
    pPrintPrecWith lvl _ ts = surround "◢ " " ◣" <$> pPrintSeq lvl ts

pPrintSeq :: (MonadState BlockCounter m) => PrettyLevel -> TokenSeq -> m Doc
pPrintSeq lvl ts = hsep <$> do
    forM (toList ts) \(t :@: a) ->
        liftA2 (<+>)
            do pPrintPrecWith lvl 0 t
            if lvl >= PrettyVerbose
                then ("@" <+>) <$> pPrintPrecWith lvl 0 a
                else pure mempty

instance Pretty TokenSeq where
    pPrintPrec lvl p ts =
        runIdentity $ evalStateT (pPrintPrecWith lvl p ts) emptyBlockCounter


data BlockKind
    = BkParen
    | BkBrace
    | BkBracket
    | BkWhitespace
    deriving (Eq, Ord, Show)

instance Pretty BlockKind where
    pPrint = \case
        BkParen -> "parenthesis"
        BkBrace -> "brace"
        BkBracket -> "bracket"
        BkWhitespace -> "whitespace"

type BlockCounter = Map BlockKind Int

emptyBlockCounter :: BlockCounter
emptyBlockCounter = Map.fromList
    [(BkParen, 0), (BkBrace, 0), (BkBracket, 0), (BkWhitespace, 0), (BkWhitespace, 0)]

blockPrintWith :: MonadState BlockCounter m =>
    PrettyLevel -> BlockKind -> TokenSeq -> m Doc
blockPrintWith lvl k ts = do
    i <- fmap superscript . show <$> gets @BlockCounter (Map.! k)
    modify @BlockCounter (Map.adjust (+1) k)
    (text i <>) . (<> text i) <$> case k of
        BkParen -> parens <$> pPrintSeq lvl ts
        BkBrace -> braces <$> pPrintSeq lvl ts
        BkBracket -> brackets <$> pPrintSeq lvl ts
        BkWhitespace -> ("◁" <+>) . (<+> "▷") <$> pPrintSeq lvl ts

blockPrint :: PrettyLevel -> BlockKind -> TokenSeq -> Doc
blockPrint lvl k ts =
    runIdentity $ evalStateT (blockPrintWith lvl k ts) emptyBlockCounter


-- | Check if a token terminates expressions (ie @,@, @}@ etc)
isSentinel :: Token -> Bool
isSentinel = \case
    TSymbol s -> s `elem` [")", "]", "}", ",", "=", ":"]
    _ -> False

-- | Check if a token is a symbol with the given value
isSymbol :: String -> Token -> Bool
isSymbol s = \case
    TSymbol s' -> s == s'
    _ -> False

-- | Check if a token has a reserved value; and return it if it doesn't
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
    _ -> False




-- | Loosely specifies a pattern for matching against Token
data TokenSpec
    -- | Expect a TSymbol with optional value
    = TsSymbol !String
    -- | Expect a TLiteral with optional kind
    | TsLiteral !(Maybe LiteralKind)
    -- | Expect a TVersion
    | TsVersion
    deriving (Eq, Ord, Show)

instance Pretty TokenSpec where
    pPrint = \case
        TsSymbol "" -> "symbol"
        TsSymbol s -> text s
        TsLiteral Nothing -> "literal"
        TsLiteral (Just k) -> pPrint k
        TsVersion -> "version"


nilTree :: Token -> Bool
nilTree = \case
    TTree BkWhitespace ts ->
        isNil ts || all (nilTree . untag) ts
    _ -> False

reduceDocTokenSeq :: TokenSeq -> TokenSeq
reduceDocTokenSeq = eliminateEmpties . fmap (fmap reduceTree)



reduceTokenSeq :: TokenSeq -> TokenSeq
reduceTokenSeq = compose (fmap (fmap reduceTree)) \case
    (TTree BkWhitespace ts :@: _) Seq.:<| Nil ->
        if isNil ts || all (nilTree . untag) ts
            then Nil
            else ts
    ts -> eliminateEmpties ts

eliminateEmpties :: TokenSeq -> TokenSeq
eliminateEmpties = Seq.filter (not . nilTree . untag)

reduceTree :: Token -> Token
reduceTree = \case
    TTree k ts -> TTree k (reduceTokenSeq ts)

    t -> t
