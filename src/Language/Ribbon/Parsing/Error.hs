module Language.Ribbon.Parsing.Error where

import Data.List qualified as List

import Data.Tag
import Data.Attr

import Text.Pretty

import Language.Ribbon.Util

import Language.Ribbon.Lexical.Token
import Language.Ribbon.Lexical.Literal




class (Eq a) => ParseInput a where
    inputIdentity :: a -> String
    inputPretty :: a -> Doc

instance ParseInput Token where
    inputIdentity = \case
        TSymbol _ -> "symbol"
        TLiteral l -> inputIdentity l
        TVersion _ -> "version"
        TSemSpace -> "semantic space"
        TTree _ -> "tree"
        TEof -> "end of file"
    inputPretty = pPrint

instance ParseInput Literal where
    inputIdentity = \case
        LInt _ -> "integer literal"
        LFloat _ -> "float literal"
        LString _ -> "string literal"
        LChar _ -> "character literal"
    inputPretty = pPrint

instance ParseInput Char where
    inputIdentity = \case
        '\\' -> "backslash"
        '\a' -> "alert"
        '\b' -> "backspace"
        '\n' -> "newline"
        '\r' -> "carriage return"
        '\v' -> "vertical tab"
        '\f' -> "form feed"
        '\t' -> "tab"
        '\0' -> "null"
        _ -> "character"
    inputPretty = text . escapeChar

-- | Wrapper for all errors triggered in a @Parser@,
--   with the specific @ParseFail@ tagged with an @Attr@,
--   and a flag indicating whether the error is recoverable
data ParseError a
    = ParseError
    { recoverability :: !Recoverability
    , failure :: !(ATag (ParseFail a))
    }
    deriving Show

-- | Indicates whether a @ParseError@ can be recovered
--   by the @Alternative Parser@ instance
data Recoverability
    = Recoverable
    | Unrecoverable
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Semigroup Recoverability where
    Recoverable <> Recoverable = Recoverable
    _ <> _ = Unrecoverable

instance Pretty Recoverability where
    pPrint = \case
        Recoverable -> "recoverable"
        Unrecoverable -> "unrecoverable"

instance ParseInput a => Pretty (ParseError a) where
    pPrintPrec lvl _ (ParseError r (f :@: x)) =
        let rd = select (lvl > PrettyNormal) (pPrint r) mempty
        in hang (rd <+> "syntax error at" <+> (pPrint x <> ":")) do
            formatFailure [x] f

instance ParseInput a => Semigroup (ParseError a) where
    ea <> eb = ParseError
        (ea.recoverability <> eb.recoverability)
        (ea.failure <> eb.failure)

-- | Specific type of failure for a @ParseError@
data ParseFail a
    = EofFailure
    | UnexpectedFailure a
    | SingleFailure !Doc
    | ExpectationFailure ![Doc] !(ATag (ParseFail a))
    | AlternativeFailure ![ATag (ParseFail a)]
    deriving (Eq, Show)

instance ParseInput a => Pretty (ParseFail a) where
    pPrint = formatFailure []

instance ParseInput a => Pretty (ATag (ParseFail a)) where
    pPrint (f :@: x) = hang ("at" <+> (pPrint x <> ":")) (formatFailure [x] f)

instance ParseInput a => Semigroup (ATag (ParseFail a)) where
    (<>) ta tb | Just merge <- joinExpectation ta tb = merge where
        joinExpectation (a :@: xa) (b :@: xb) = case (a, b) of
            (ExpectationFailure m1 i1, ExpectationFailure m2 i2)
                | xa == xb, i1 == i2 ->
                    Just (ExpectationFailure (m1 `List.union` m2) i1 :@: xa)
            (ExpectationFailure{}, AlternativeFailure bs)
                | Just merge <- joinExpectationFold ta bs ->
                    Just (AlternativeFailure merge :@: xb)
            (AlternativeFailure as, ExpectationFailure{})
                | Just merge <- joinExpectationFold tb as ->
                    Just (AlternativeFailure merge :@: xa)
            _ -> Nothing
        joinExpectationFold _ [] = Nothing
        joinExpectationFold eb (ea : as) = case joinExpectation ea eb of
            Just a' -> Just (a' : as)
            _ -> (ea :) <$> joinExpectationFold eb as
    (<>) ta@(a :@: xa) tb@(b :@: xb) = Tag (xa <> xb) case (a, b) of
        (UnexpectedFailure _, UnexpectedFailure _) | xa == xb -> a
        (EofFailure, EofFailure) -> EofFailure
        (_, UnexpectedFailure _) -> a
        (UnexpectedFailure _, _) -> b
        (_, EofFailure) -> a
        (EofFailure, _) -> b
        (AlternativeFailure as, AlternativeFailure bs) ->
            AlternativeFailure (as `List.union` bs)
        (AlternativeFailure as, _) -> AlternativeFailure (as `List.union` [tb])
        (_, AlternativeFailure bs) -> AlternativeFailure ([ta] `List.union` bs)
        _ -> case List.nub [ta, tb] of
            [e] -> e.value
            es -> AlternativeFailure es

-- | Format a @ParseFail@ into a @Doc@, discarding any @Attr@s
--   that have already been printed in the given tree
formatFailure :: ParseInput a => [Attr] -> ParseFail a -> Doc
formatFailure ignore = \case
        EofFailure -> "unexpected end of input"
        UnexpectedFailure tok ->
            "unexpected" <+> text (inputIdentity tok)
                <+> backticks (inputPretty tok)
        SingleFailure doc -> doc
        ExpectationFailure msg incite ->
            hang ("expected" <+> (lsep msg <> ":")) do
                go ignore incite
        AlternativeFailure [sub] -> go ignore sub
        AlternativeFailure subs ->
            hang "all alternatives failed:" do
                vcat' (go ignore <$> subs)
    where
    go ig (f :@: x) =
        if x `elem` ig
            then formatFailure ig f
            else hang ("at" <+> (pPrint x <> ":")) do
                formatFailure (x : ig) f
