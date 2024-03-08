module Data.SyntaxError
    ( DecodeProblem(..)
    , SyntaxInput(..), SyntaxError(..), Recoverability(..), SyntaxFail(..)
    , formatProblem, formatFailure
    , seqErr
    ) where

import Data.List qualified as List

import Data.Tag
import Data.Attr

import Text.Pretty

import Language.Ribbon.Util ( select, escapeChar )





data DecodeProblem
    = DecodeBadEncoding
    | DecodeEof
    deriving Show

formatProblem ::
    Attr -> DecodeProblem -> ATag SyntaxFail
formatProblem at = \case
    DecodeBadEncoding ->
        SingleFailure "invalid utf8" :@: at
    DecodeEof ->
        EofFailure :@: at



class (Eq a) => SyntaxInput a where
    inputPretty :: a -> Doc


instance SyntaxInput Char where
    inputPretty c = text (inputIdentity c) <+> backticks (text $ escapeChar c) where
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

-- | Wrapper for all errors triggered in a @Parser@ or @Lexer@,
--   with the specific @SyntaxFail@ tagged with an @Attr@,
--   and a flag indicating whether the error is recoverable
data SyntaxError
    = SyntaxError
    { recoverability :: !Recoverability
    , failure :: !(ATag SyntaxFail)
    }
    deriving Show

-- | Indicates whether a @SyntaxError@ can be recovered
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

instance Pretty SyntaxError where
    pPrintPrec lvl _ (SyntaxError r (f :@: x)) =
        let rd = select (lvl > PrettyNormal) (pPrint r) mempty
        in hang (rd <+> "syntax error at" <+> (pPrint x <> ":")) do
            formatFailure [x] f

instance Semigroup SyntaxError where
    ea <> eb = SyntaxError
        (ea.recoverability <> eb.recoverability)
        (ea.failure <> eb.failure)

-- | Specific type of failure for a @SyntaxError@
data SyntaxFail
    = EofFailure
    | UnexpectedFailure Doc
    | SingleFailure !Doc
    | ExpectationFailure ![Doc] !(ATag SyntaxFail)
    | AlternativeFailure ![ATag SyntaxFail]
    deriving (Eq, Show)

instance Pretty SyntaxFail where
    pPrint = formatFailure []

instance Pretty (ATag SyntaxFail) where
    pPrint (f :@: x) = hang ("at" <+> (pPrint x <> ":")) (formatFailure [x] f)

instance Semigroup (ATag SyntaxFail) where
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


-- | Format a @SyntaxFail@ into a @Doc@, discarding any @Attr@s
--   that have already been printed in the given tree
formatFailure :: [Attr] -> SyntaxFail -> Doc
formatFailure ignore = \case
        EofFailure -> "unexpected end of input"
        UnexpectedFailure msg ->
            "unexpected" <+> msg
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


-- | Create an error message for a failed expectation of some @seq@ of @kind@,
--   with the given @Attr@ as the location
seqErr :: String -> String -> Attr -> SyntaxError
seqErr sub kind at = SyntaxError Unrecoverable $ Tag at $ SingleFailure $
    "cannot parse" <+> backticks (text sub)
        <+> "as a" <+> text kind

