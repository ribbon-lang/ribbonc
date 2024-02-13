{-# OPTIONS_GHC -Wno-orphans #-}

module Ribbon.Display
    ( module X
    , shown
    , vcat'
    , hang, indent
    , backticks, backticked, maybeBackticks, maybeBackticked
    , hashes, hashed, maybeHashes, maybeHashed
    , quoted, maybeQuoted
    , doubleQuoted, maybeDoubleQuoted
    , paren'd, maybeParen'd
    , braced, maybeBraced
    , bracketed, maybeBracketed
    , Pretty(..)
    , PrettyLevel(..)
    , prettyShow, prettyShowLevel
    , prettyPrint, prettyPrintLevel
    ) where

import Text.PrettyPrint.Annotated.HughesPJ qualified as X
import Text.PrettyPrint.Annotated.HughesPJ as X hiding
    ((<>), empty, ptext, char, hang
    , int, integer, float, double, rational
    , semi, comma, space, equals, lparen, rparen, lbrack, rbrack, lbrace, rbrace
    )

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Sequence (Seq)

import Data.Functor
import Data.Foldable
import Data.Word (Word8)


-- | Designates a level of verbosity for pretty printing
data PrettyLevel
    -- | The default, base level of pretty printing
    = PrettyNormal
    -- | Attach more information to the pretty printed output
    | PrettyRich
    -- | Print the most information possible
    | PrettyVerbose
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Pretty printing class, similar to `Show`,
--   interfacing with `Text.PrettyPrint` `Doc`s
class Pretty ann a where
    -- | Pretty print a value with a given level of verbosity and precedence
    pPrintPrec :: PrettyLevel -> Word8 -> a -> Doc ann
    -- | Pretty print a value with the default level of verbosity and precedence
    pPrint :: a -> Doc ann

    pPrint = pPrintPrec PrettyNormal 0
    pPrintPrec _ _ = pPrint

instance {-# OVERLAPPABLE #-} Show a => Pretty ann a where
    pPrint = shown

instance Pretty ann Char where
    pPrintPrec _ _ = shown

instance Pretty ann String where
    pPrintPrec _ _ = shown

instance {-# OVERLAPPABLE #-} (Pretty ann a) => Pretty ann [a] where
    pPrintPrec lvl _ = brackets . fsep . punctuate (text ",") . fmap (pPrintPrec lvl 0)

instance {-# OVERLAPPING #-} (Pretty ann a, Pretty ann b) => Pretty ann (a, b) where
    pPrintPrec lvl _ (a, b) = parens do
        pPrintPrec lvl 0 a <+> text "," <+> pPrintPrec lvl 0 b

instance {-# OVERLAPPING #-} (Pretty ann a, Pretty ann b, Pretty ann c) => Pretty ann (a, b, c) where
    pPrintPrec lvl _ (a, b, c) = parens do
        pPrintPrec lvl 0 a <+> text "," <+> pPrintPrec lvl 0 b
        <+> text "," <+> pPrintPrec lvl 0 c

instance {-# OVERLAPPING #-} (Pretty ann a, Pretty ann b, Pretty ann c, Pretty ann d)
    => Pretty ann (a, b, c, d) where
        pPrintPrec lvl _ (a, b, c, d) = parens do
            pPrintPrec lvl 0 a <+> text "," <+> pPrintPrec lvl 0 b
            <+> text "," <+> pPrintPrec lvl 0 c
            <+> text "," <+> pPrintPrec lvl 0 d

instance {-# OVERLAPPING #-} (Pretty ann a) => Pretty ann (Maybe a) where
    pPrintPrec lvl prec = \case
        Just a -> maybeParens (prec > 0) do
            hang (text "Just")
                (pPrintPrec lvl 0 a)
        _ -> text "Nothing"

instance {-# OVERLAPPING #-} (Pretty ann a, Pretty ann b) => Pretty ann (Either a b) where
    pPrintPrec lvl prec = maybeParens (prec > 0) . \case
        Left a -> hang (text "Left")
            (pPrintPrec lvl 0 a)
        Right b -> hang (text "Right")
            (pPrintPrec lvl 0 b)

instance {-# OVERLAPPING #-} (Pretty ann k, Pretty ann v) => Pretty ann (Map k v) where
    pPrintPrec lvl _ m
        = braces . fsep . punctuate (text ",")
        $ Map.toList m <&> \(k, v) ->
            pPrintPrec lvl 0 k <+> text "=" <+> pPrintPrec lvl 0 v

instance {-# OVERLAPPING #-} (Pretty ann k) => Pretty ann (Set k) where
    pPrintPrec lvl _ s
        = braces . fsep . punctuate (text ",")
        $ Set.toList s <&> pPrintPrec lvl 0

instance {-# OVERLAPPING #-} (Pretty ann a) => Pretty ann (Seq a) where
    pPrintPrec lvl _ s
        = hashes . brackets . fsep . punctuate (text ",")
        $ toList s <&> pPrintPrec lvl 0

-- | Pretty print a value with the default level of verbosity and precedence,
--   and convert the resulting @Doc@ to a @String@
prettyShow :: forall ann a. Pretty ann a => a -> String
prettyShow = prettyShowLevel @ann PrettyNormal

-- | Pretty print a value with a given level of verbosity and precedence,
--   and convert the resulting @Doc@ to a @String@
prettyShowLevel :: forall ann a. Pretty ann a => PrettyLevel -> a -> String
prettyShowLevel lvl = render . pPrintPrec @ann lvl 0

-- | Pretty print a value with the default level of verbosity and precedence,
--   and print the resulting @Doc@ to the console
prettyPrint :: forall ann a. Pretty ann a => a -> IO ()
prettyPrint = prettyPrintLevel @ann PrettyNormal

-- | Pretty print a value with a given level of verbosity and precedence,
--   and print the resulting @Doc@ to the console
prettyPrintLevel :: forall ann a. Pretty ann a => PrettyLevel -> a -> IO ()
prettyPrintLevel lvl = putStrLn . prettyShowLevel @ann lvl

-- | The usual `hang` with a consistent indentation of 4 spaces
hang :: Doc ann -> Doc ann -> Doc ann
hang a = X.hang a 4

-- | The usual `nest` with a consistent indentation of 4 spaces
indent :: Doc ann -> Doc ann
indent = nest 4

-- | @text . show@
shown :: Show a => a -> Doc ann
shown = text . show

-- | list version of @($+$)@
vcat' :: [Doc ann] -> Doc ann
vcat' = foldr ($+$) mempty


-- | Enclose a @Doc@ in backticks ``
backticks :: Doc ann -> Doc ann
backticks d = text "`" <> d <> text "`"

-- | Pretty print a value and enclose it in backticks ``
backticked :: Pretty ann a => a -> Doc ann
backticked = backticks . pPrint

-- | Enclose a @Doc@ in backticks `` if the boolean is true
maybeBackticks :: Bool -> Doc ann -> Doc ann
maybeBackticks True = backticks
maybeBackticks False = id

-- | Pretty print a value and enclose it in backticks `` if the boolean is true
maybeBackticked :: Pretty ann a => Bool -> a -> Doc ann
maybeBackticked b = maybeBackticks b . pPrint

-- | Enclose a @Doc@ in hashes ##
hashes :: Doc ann -> Doc ann
hashes d = text "#" <> d <> text "#"

-- | Pretty print a value and enclose it in hashes ##
hashed :: Pretty ann a => a -> Doc ann
hashed = hashes . pPrint

-- | Enclose a @Doc@ in hashes ## if the boolean is true
maybeHashes :: Bool -> Doc ann -> Doc ann
maybeHashes True = hashes
maybeHashes False = id

-- | Pretty print a value and enclose it in hashes ## if the boolean is true
maybeHashed :: Pretty ann a => Bool -> a -> Doc ann
maybeHashed b = maybeHashes b . pPrint

-- | Pretty print a value and enclose it in single quotes ''
quoted :: Pretty ann a => a -> Doc ann
quoted = quotes . pPrint

-- | Pretty print a value and enclose it in single quotes ''
--   if the boolean is true
maybeQuoted :: Pretty ann a => Bool -> a -> Doc ann
maybeQuoted b = maybeQuotes b . pPrint

-- | Pretty print a value and enclose it in double quotes ""
doubleQuoted :: Pretty ann a => a -> Doc ann
doubleQuoted = doubleQuotes . pPrint

-- | Pretty print a value and enclose it in double quotes ""
--   if the boolean is true
maybeDoubleQuoted :: Pretty ann a => Bool -> a -> Doc ann
maybeDoubleQuoted b = maybeDoubleQuotes b . pPrint

-- | Pretty print a value and enclose it in parens ()
paren'd :: Pretty ann a => a -> Doc ann
paren'd = parens . pPrint

-- | Enclose a @Doc@ in parens () if the boolean is true
maybeParen'd :: Pretty ann a => Bool -> a -> Doc ann
maybeParen'd b = maybeParens b . pPrint

-- | Pretty print a value and enclose it in braces {}
braced :: Pretty ann a => a -> Doc ann
braced = braces . pPrint

-- | Pretty print a value and enclose it in braces {}
--   if the boolean is true
maybeBraced :: Pretty ann a => Bool -> a -> Doc ann
maybeBraced b = maybeBraces b . pPrint

-- | Pretty print a value and enclose it in brackets []
bracketed :: Pretty ann a => a -> Doc ann
bracketed = brackets . pPrint

-- | Pretty print a value and enclose it in brackets []
--   if the boolean is true
maybeBracketed :: Pretty ann a => Bool -> a -> Doc ann
maybeBracketed b = maybeBrackets b . pPrint
