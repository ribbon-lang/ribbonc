module Ribbon.Syntax.Parser where

import Prelude hiding (exp)

import Data.Sequence qualified as Seq

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

-- | Perform syntactic analysis on a File, using the given Parser
parseFileWith :: Parser a -> File -> Either String a
parseFileWith p file = L.lexFile file >>= evalParser p file

-- | Perform syntactic analysis on a string, using the given Parser,
--   with errors reported to be in a file with the given name
parseStringWith :: Parser a -> String -> String -> Either String a
parseStringWith p name text =
    let file = newFile name text
    in L.lexFile file >>= evalParser p file


-- | Type alias for ParseStream TokenData
type ParseStream = M.ParseStream TokenData

-- | Type alias for ParserM TokenData
type Parser = ParserM TokenData

-- | Evaluate a Parser on a given File, converting ParseErrors to Strings
evalParser :: Parser a -> File -> [Token] -> Either String a
evalParser px file toks =
    case runParser (noFail $ consumesAll px)
        (M.ParseStream (Seq.fromList toks) file) 0 of
        Left (ParseError (msg :@: l)) -> Left $ "parse error " ++ show l ++ ": " ++ msg
        Left _ -> undefined
        Right (a, _) -> Right a

-- | Expect a Literal
literal :: ParserMonad TokenData m => m Literal
literal = expecting "literal" $ nextMap \case
    TLiteral lit -> Just lit
    _ -> Nothing

-- | Expect a Literal of Char
char :: ParserMonad TokenData m => m Char
char = expecting "character literal" $ nextMap \case
    TLiteral (LChar c) -> Just c
    _ -> Nothing

-- | Expect a Literal of Int
int :: ParserMonad TokenData m => m Int
int = expecting "integer literal" $ nextMap \case
    TLiteral (LInt i) -> Just i
    _ -> Nothing

-- | Expect a Literal of Float
float :: ParserMonad TokenData m => m Float
float = expecting "float literal" $ nextMap \case
    TLiteral (LFloat f) -> Just f
    _ -> Nothing

-- | Expect a Literal of String
string :: ParserMonad TokenData m => m String
string = expecting "string literal" $ nextMap \case
    TLiteral (LString s) -> Just s
    _ -> Nothing

-- | Expect a particular kind of symbol token
symbolK :: ParserMonad TokenData m => TokenSymbolKind -> m String
symbolK k = expecting (display k) $ nextMap \case
    TSymbol k' s | k == k' -> Just s
    _ -> Nothing

-- | Expect a symbol token with a specific value
sym :: ParserMonad TokenData m => String -> m ()
sym s = expecting s $ nextMap \case
    TSymbol _ s' | s == s' -> Just ()
    _ -> Nothing

-- | Expect a given Parser to be surrounded with parentheses
parens :: ParserMonad TokenData m => m a -> m a
parens px = sym "(" >> px << sym ")"

-- | Expect a given Parser to be surrounded with braces
braces :: ParserMonad TokenData m => m a -> m a
braces px = sym "{" >> px << sym "}"

-- | Expect a given Parser to be surrounded with brackets
brackets :: ParserMonad TokenData m => m a -> m a
brackets px = sym "[" >> px << sym "]"



-- | Expect a Type
typ :: ParserMonad TokenData m => m Type
typ = syn $ asum
    [ symbolK TsIdentifier <&> \case
        "_" -> TFree Nothing
        s -> TFree (Just s)
    , synData <$> parens typ
    ]

-- | Expect an Expr
exp :: ParserMonad TokenData m => m Expr
exp = liftP $ runPratt prattLoopStart expTbl 0

-- | Expect a Patt
pat :: ParserMonad TokenData m => m Patt
pat = asum
    [ syn $ symbolK TsIdentifier <&> \case
        "_" -> PWildcard
        s -> PVar s
    ]


-- | Encodes parser precedence levels
type Prec = Int

-- | Pratt parser operator table
data OpTable a
    = OpTable
    { nuds :: [(TokenKind, Nud a)]
    , leds :: [(TokenKind, Led a)]
    }

-- | Pratt parser operator table entry for an infix or postfix operator
--
--   NOTE: the projection descriptions are from
--   https://www.engr.mun.ca/~theo/Misc/pratt_parsing.htm
data Led a =
    -- | Construct a new infix or postfix operator
    Led
    -- | Left binding power "L tokens will have a nonnegative LBP,
    --   while all others have an LBP of -1"
    { ledLbp :: Prec

    -- | Right binding power "the RBP indicates the 'right binding power'
    --   and determines the left binding power of the lowest precedence operator
    --   that can be in a right operand"
    --
    --   Controls associativity, left/non associative operations rbp == lbp + 1,
    --   right associative operations rbp == lbp
    , ledRbp :: Prec

    -- | Next binding power "the NBP value for a binary or postfix operator
    --   gives the highest precedence of an operator that this operator
    --   can be a left operand of"
    --
    --   For associative operations nbp == lbp, non associative lbp - 1
    , ledNbp :: Prec

    -- | The parser action to run when the associated operator is encountered
    , ledAction :: a -> Pratt a a
    }

-- | Pratt parser operator table entry for a prefix operator or leaf node
data Nud a =
    -- | Construct a new prefix operator or leaf node
    Nud
    -- | Right binding power, see @ledRbp@
    { nudRbp :: Prec

    -- | The parser action to run when the associated operator is encountered
    , nudAction :: Pratt a a
    }


-- | Get a Pratt parser nud entry and bind the max binding power to its rbp
runNud :: Nud a -> Pratt a a
runNud (Nud rbp action) = withMbp rbp action

-- | Get a Pratt parser led entry and bind the max binding power to its rbp,
--   passing it the given left operand
runLed :: Led a -> a -> Pratt a a
runLed (Led _ rbp _ action) left = withMbp rbp $ action left

-- | Construct a left-associative led pratt parser with the given precedence
lAssoc :: Prec -> (a -> Pratt a a) -> Led a
lAssoc prec = Led prec (prec + 1) prec

-- | Construct a right-associative led pratt parser with the given precedence
rAssoc :: Prec -> (a -> Pratt a a) -> Led a
rAssoc prec = Led prec prec prec

-- | Construct a non-associative led pratt parser with the given precedence
nAssoc :: Prec -> (a -> Pratt a a) -> Led a
nAssoc prec = Led prec (prec + 1) (prec - 1)

-- | Construct a postfix led pratt parser with the given precedence
post :: Prec -> (a -> Pratt a a) -> Led a
post prec = Led prec prec maxBound

-- | Construct a prefix nud pratt parser
pre :: Prec -> Pratt a a -> Nud a
pre = Nud

-- | Construct a leaf nud pratt parser
leaf :: Pratt a a -> Nud a
leaf = Nud (error "recursive leaf")


-- | Pratt parser monad
newtype Pratt o a
    -- | Wrap a function into the Pratt parser monad
    = Pratt
    -- | Unwrap a Pratt monad into a function
    { runPratt :: OpTable o -> Prec -> Parser a }

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


-- | Get the current operator table of a pratt parser
opTable :: Pratt o (OpTable o)
opTable = Pratt \tbl _ -> pure tbl

-- | Get the current maximum binding power of a pratt parser
curMbp :: Pratt o Prec
curMbp = Pratt \_ mbp -> pure mbp

-- | Set the maximum binding power of a given pratt parser
withMbp :: Prec -> Pratt o a -> Pratt o a
withMbp mbp (Pratt f) = Pratt \tbl _ -> f tbl mbp


-- | Execute the nud step of a pratt parser
prattNud :: Pratt a a
prattNud = do
    tbl <- opTable
    expectingMulti (tblExpects "prefix" (nuds tbl)) do
        td <- peek
        maybe empty runNud (tkFind (nuds tbl) td)

-- | Execute the nud step of a pratt parser, then enter the led loop.
--   This is the action to call when re-entering the pratt parser from
--   a Nud or Led action.
prattRecurse :: Pratt a a
prattRecurse = do
    tbl <- opTable
    expectingMulti' (tblExpects "infix/postfix" (leds tbl))
        prattLoopStart

-- | Execute the nud step of a pratt parser, then enter the led loop.
--   This is the action to call when entering the pratt parser from
--   outside of a Nud or Led action, i.e. within typ, exp, pat, etc
prattLoopStart :: Pratt a a
prattLoopStart = prattNud >>= prattLoop maxBound

-- | Execute the pratt led loop
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

-- | Construct a binary led operator
binExpr
    :: (String -> TokenKind)
    -> String
    -> (Prec -> (Expr
          -> Pratt Expr Expr)
          -> b)
    -> Prec
    -> (TokenKind, b)
binExpr kind symbol ass prec =
    (kind symbol, ass prec \left -> do
        s <- attrNext
        prattRecurse <&> \right ->
            synAppWithB s (EInfix (prec, symbol)) left right
    )

-- | Construct a prefix operator
preExpr :: (String -> TokenKind) -> String -> Prec -> (TokenKind, Nud Expr)
preExpr kind symbol prec =
    (kind symbol, pre prec do
        s <- attrNext
        synExtWithA s (EPrefix (prec, symbol)) <$> prattRecurse
    )

-- | Construct a postfix operator
postExpr :: (String -> TokenKind) -> String -> Prec -> (TokenKind, Led Expr)
postExpr kind symbol prec =
    (kind symbol, post prec \left -> do
        s <- attrNext
        pure (synExtWithB s (EPostfix (prec, symbol)) left)
    )

-- | The default OpTable for Expr
expTbl :: OpTable Expr
expTbl = OpTable
    { nuds =
        [ (TkLiteral Nothing, leaf do
              fmap ELit <$> syn literal
          )

        , preExpr (TkSymbol (Just TsOperator)) "-" 90

        , (TkSymbol (Just TsIdentifier) "", leaf do
              fmap EVar <$> syn (symbolK TsIdentifier)
          )

        , (TkSymbol (Just TsPunctuation) "(", leaf do
              s1 <- attrNext
              e <- exp
              asum
                  [ do
                      s2 <- attrOf (sym ")")
                      pure (reSyn (s1 <> s2) e)

                    -- TODO : tuples
                  ]
          )

        , (TkSymbol (Just TsReserved) "fun", leaf $ noFail do
              s1 <- attrNext
              p <- pat
              sym "=>"
              synExtWithA s1 (EFunction . (p, )) <$> exp
          )
        ]

    , leds =
        [ binExpr (TkSymbol (Just TsOperator)) "+" lAssoc 20
        , binExpr (TkSymbol (Just TsOperator)) "-" lAssoc 20
        , binExpr (TkSymbol (Just TsOperator)) "*" lAssoc 30
        , binExpr (TkSymbol (Just TsOperator)) "/" lAssoc 30
        , binExpr (TkSymbol (Just TsOperator)) "^" rAssoc 40

        , binExpr (TkSymbol (Just TsOperator)) "==" nAssoc 50

        , postExpr (TkSymbol (Just TsOperator)) "!" 80

        , (TkSymbol (Just TsOperator) ":", nAssoc 10 \left -> do
              advance
              synApp EAnn left <$> typ
          )

        , (TkNonSentinel, lAssoc 70 \f -> synApp EApp f <$> prattRecurse)
        ]
    }
