module Ribbon.Syntax.Parser where

import Prelude hiding (exp)

import Data.Sequence qualified as Seq

import Data.Maybe qualified as Maybe
import Data.List qualified as List
import Data.Function
import Data.Functor

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Ribbon.Util
import Ribbon.Display (Display(..))
import Ribbon.Source

import Ribbon.Syntax.ParserM hiding (ParseStream)
import Ribbon.Syntax.ParserM qualified as M

import Ribbon.Syntax.Lexer qualified as L
import Ribbon.Syntax.Text
import Ribbon.Syntax.Token
import Ribbon.Syntax.Literal
import Ribbon.Syntax.Ast


-- parseFile :: File -> Either String Doc
-- parseFile = parseFileWith doc

parseFileWith :: Parser a -> File -> Either String a
parseFileWith p file = L.lexFile file >>= evalParser p file

parseStringWith :: Parser a -> String -> String -> Either String a
parseStringWith p name text =
    let file = newFile name text
    in L.lexFile file >>= evalParser p file


type ParseStream = M.ParseStream TokenData

type Parser = ParserM TokenData

evalParser :: Parser a -> File -> [Token] -> Either String a
evalParser px file toks =
    case runParser (noFail $ consumesAll px)
        (M.ParseStream (Seq.fromList toks) file) 0 of
        Left (ParseError (msg :@: l)) -> Left $ "parse error " ++ show l ++ ": " ++ msg
        Left _ -> undefined
        Right (a, _) -> Right a


literal :: ParserMonad TokenData m => m Literal
literal = expecting "literal" $ nextMap \case
    TLiteral lit -> Just lit
    _ -> Nothing

char :: ParserMonad TokenData m => m Char
char = expecting "character literal" $ nextMap \case
    TLiteral (LChar c) -> Just c
    _ -> Nothing

int :: ParserMonad TokenData m => m Int
int = expecting "integer literal" $ nextMap \case
    TLiteral (LInt i) -> Just i
    _ -> Nothing

float :: ParserMonad TokenData m => m Float
float = expecting "float literal" $ nextMap \case
    TLiteral (LFloat f) -> Just f
    _ -> Nothing

string :: ParserMonad TokenData m => m String
string = expecting "string literal" $ nextMap \case
    TLiteral (LString s) -> Just s
    _ -> Nothing

var :: ParserMonad TokenData m => TokenSymbolKind -> m String
var k = expecting (display k) $ nextMap \case
    TSymbol k' sym | k == k' -> Just sym
    _ -> Nothing

symbol :: ParserMonad TokenData m => String -> m ()
symbol sym = expecting sym $ nextMap \case
    TSymbol _ s | sym == s -> Just ()
    _ -> Nothing


parens :: ParserMonad TokenData m => m a -> m a
parens px = symbol "(" >> px << symbol ")"

braces :: ParserMonad TokenData m => m a -> m a
braces px = symbol "{" >> px << symbol "}"

brackets :: ParserMonad TokenData m => m a -> m a
brackets px = symbol "[" >> px << symbol "]"




typ :: ParserMonad TokenData m => m Type
typ = syn $ asum
    [ var TsIdentifier <&> \case
        "_" -> TFree Nothing
        s -> TFree (Just s)
    , synData <$> parens typ
    ]

exp :: ParserMonad TokenData m => m Expr
exp = liftP $ runPratt prattLoopStart expTbl 0

pat :: ParserMonad TokenData m => m Patt
pat = asum
    [ syn $ var TsIdentifier <&> \case
        "_" -> PWildcard
        s -> PVar s
    ]


type Prec = Int

data OpTable a
    = OpTable
    { nuds :: [(TokenKind, Nud a)]
    , leds :: [(TokenKind, Led a)]
    }

-- NOTE: the following descriptions are from
-- https://www.engr.mun.ca/~theo/Misc/pratt_parsing.htm
data Led a = Led
    -- left binding power "L tokens will have a nonnegative LBP,
    -- while all others have an LBP of -1"
    { ledLbp :: Prec
    -- right binding power "the RBP indicates the 'right binding power'
    -- and determines the left binding power of the lowest precedence operator
    -- that can be in a right operand"
    -- controls associativity, left/non associative operations rbp == lbp + 1,
    -- right associative operations rbp == lbp
    , ledRbp :: Prec
    -- next binding power "the NBP value for a binary or postfix operator
    -- gives the highest precedence of an operator that this operator
    -- can be a left operand of"
    -- for associative operations nbp == lbp, non associative lbp - 1
    , ledNbp :: Prec
    , ledAction :: a -> Pratt a a
    }

data Nud a = Nud
    { nudRbp :: Prec
    , nudAction :: Pratt a a
    }

tkSelect :: TokenKind -> TokenData -> Bool
tkSelect = curry \case
    (TkAny, _) -> True
    (TkNonSentinel, TSymbol _ s) | not (isSentinel (head s)) -> True
    (TkNonSentinel, TLiteral _) -> True
    (TkSymbol ka a, TSymbol kb b) | Maybe.isNothing ka || ka == Just kb, a == b || a == "" -> True
    (TkLiteral (Just a), TLiteral b) | a == literalKind b -> True
    (TkLiteral Nothing, TLiteral _) -> True
    _ -> False

tkFind :: [(TokenKind, a)] -> TokenData -> Maybe a
tkFind [] _ = Nothing
tkFind ((k, a):rest) t
    | tkSelect k t = Just a
    | otherwise = tkFind rest t


tblExpects :: String -> [(TokenKind, a)] -> [String]
tblExpects opKind es =
    let xs = (tokenKindName . fst <$> es) List.\\ ["{anything}", "{non-sentinel}"]
        (ops, ws) = xs & List.partition (any isOperatorSubsequent)
        (ps, ops') = ops & List.partition (any isPunctuation)
    in finalizeOps ops' : ws <> ps
    where
        finalizeOps ops = opKind <> " operator of " <> display ops


runNud :: Nud a -> Pratt a a
runNud (Nud rbp action) = withMbp rbp action

runLed :: Led a -> a -> Pratt a a
runLed (Led _ rbp _ action) left = withMbp rbp $ action left

lAssoc :: Prec -> (a -> Pratt a a) -> Led a
lAssoc prec = Led prec (prec + 1) prec

rAssoc :: Prec -> (a -> Pratt a a) -> Led a
rAssoc prec = Led prec prec prec

nAssoc :: Prec -> (a -> Pratt a a) -> Led a
nAssoc prec = Led prec (prec + 1) (prec - 1)

post :: Prec -> (a -> Pratt a a) -> Led a
post prec = Led prec prec maxBound

pre :: Prec -> Pratt a a -> Nud a
pre = Nud

leaf :: Pratt a a -> Nud a
leaf = Nud (error "recursive leaf")


newtype Pratt o a
    = Pratt { runPratt :: OpTable o -> Prec -> Parser a }

instance Functor (Pratt o) where
    fmap f (Pratt a) = Pratt \tbl mbp -> f <$> a tbl mbp

instance Applicative (Pratt o) where
    pure a = Pratt \_ _ -> pure a
    Pratt f <*> Pratt a = Pratt \tbl mbp -> f tbl mbp <*> a tbl mbp

instance Monad (Pratt o) where
    Pratt a >>= f = Pratt \tbl mbp -> do
        a' <- a tbl mbp
        runPratt (f a') tbl mbp

instance Alternative (Pratt o) where
    empty = liftP empty
    Pratt a <|> Pratt b = Pratt \tbl mbp -> a tbl mbp <|> b tbl mbp

instance MonadFail (Pratt o) where
    fail = liftP . fail

instance MonadError String (Pratt o) where
    throwError = liftP . throwError
    catchError (Pratt a) f = Pratt \tbl mbp -> catchError (a tbl mbp) \e -> runPratt (f e) tbl mbp

instance ParserMonad TokenData (Pratt o) where
    liftP m = Pratt \_ _ -> m
    catchParseExcept (Pratt a) f = Pratt \tbl mbp ->
        a tbl mbp `catchParseExcept` \e ->
            runPratt (f e) tbl mbp

instance MonadState Int (Pratt o) where
    get = liftP get
    put = liftP . put

instance MonadReader ParseStream (Pratt o) where
    ask = liftP ask
    local f (Pratt a) = Pratt \tbl mbp -> local f (a tbl mbp)


opTable :: Pratt o (OpTable o)
opTable = Pratt \tbl _ -> pure tbl

curMbp :: Pratt o Prec
curMbp = Pratt \_ mbp -> pure mbp


withMbp :: Prec -> Pratt o a -> Pratt o a
withMbp mbp (Pratt f) = Pratt \tbl _ -> f tbl mbp


pratt :: Prec -> Pratt a a
pratt bp = withMbp bp prattLoopStart

prattNud :: Pratt a a
prattNud = do
    tbl <- opTable
    expectingMulti (tblExpects "prefix" (nuds tbl)) do
        td <- peek
        maybe empty runNud (tkFind (nuds tbl) td)

prattRecurse :: Pratt a a
prattRecurse = do
    tbl <- opTable
    expectingMulti' (tblExpects "infix/postfix" (leds tbl))
        prattLoopStart

prattLoopStart :: Pratt a a
prattLoopStart = prattNud >>= prattLoop maxBound

prattLoop :: Prec -> a -> Pratt a a
prattLoop rbp left = do
    lbp <- curMbp
    ls <- leds <$> opTable

    tryPeek >>= \case
        Just td
            | TSymbol TsPunctuation s <- td
            , any isSentinel s ->
                pure left

        Just td
            | Just lx <- tkFind ls td
            , let tbp = ledLbp lx
            , lbp <= tbp
            , tbp <= rbp ->
                noFail (runLed lx left) >>= prattLoop (ledNbp lx)

        _ -> pure left


binExpr
    :: (String -> TokenKind)
    -> String
    -> (Prec -> (Expr
          -> Pratt Expr Expr)
          -> b)
    -> Prec
    -> (TokenKind, b)
binExpr kind sym ass prec =
    (kind sym, ass prec \left -> do
        s <- attrNext
        prattRecurse <&> \right ->
            synAppWithB s (EInfix (prec, sym)) left right
    )

preExpr :: (String -> TokenKind) -> String -> Prec -> (TokenKind, Nud Expr)
preExpr kind sym prec =
    (kind sym, pre prec do
        s <- attrNext
        synExtWithA s (EPrefix (prec, sym)) <$> prattRecurse
    )

postExpr :: (String -> TokenKind) -> String -> Prec -> (TokenKind, Led Expr)
postExpr kind sym prec =
    (kind sym, post prec \left -> do
        s <- attrNext
        pure (synExtWithB s (EPostfix (prec, sym)) left)
    )

expTbl :: OpTable Expr
expTbl = OpTable
    { nuds =
        [ (TkLiteral Nothing, leaf do
              fmap ELit <$> syn literal
          )

        , preExpr (TkSymbol (Just TsOperator)) "-" 90

        , (TkSymbol (Just TsIdentifier) "", leaf do
              fmap EVar <$> syn (var TsIdentifier)
          )

        , (TkSymbol (Just TsPunctuation) "(", leaf do
              s1 <- attrNext
              e <- exp
              asum
                  [ do
                      s2 <- attrOf (symbol ")")
                      pure (reSyn (s1 <> s2) e)

                    -- TODO : tuples
                  ]
          )

        , (TkSymbol (Just TsReserved) "fun", leaf $ noFail do
              s1 <- attrNext
              p <- pat
              symbol "=>"
              synExtWithA s1 (EFunction . (p, )) <$> exp
          )
        ]

    , leds =
        [ binExpr (TkSymbol (Just TsOperator)) "+" lAssoc 20
        , binExpr (TkSymbol (Just TsOperator)) "-" lAssoc 20
        , binExpr (TkSymbol (Just TsOperator)) "*" lAssoc 30
        , binExpr (TkSymbol (Just TsOperator)) "/" lAssoc 30
        , binExpr (TkSymbol (Just TsOperator)) "^" rAssoc 40

        , postExpr (TkSymbol (Just TsOperator)) "!" 80

        , (TkSymbol (Just TsOperator) ":", nAssoc 10 \left -> do
              advance
              synApp EAnn left <$> typ
          )

        , (TkNonSentinel, lAssoc 70 \f -> synApp EApp f <$> prattRecurse)
        ]
    }
