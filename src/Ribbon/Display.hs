{-# OPTIONS_GHC -Wno-orphans #-}

module Ribbon.Display
    ( module X
    , shown
    , vcat', vcatDouble
    , hang, indent, lsep
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
    , maybePPrint, maybePPrintPrec
    ) where

import Text.PrettyPrint.HughesPJ qualified as X
import Text.PrettyPrint.HughesPJ as X hiding
    ((<>), empty, ptext, char, hang
    , int, integer, float, double, rational
    , semi, comma, space, equals, lparen, rparen, lbrack, rbrack, lbrace, rbrace
    , first
    )

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Sequence (Seq)
import Data.Word (Word8)

import Data.Functor
import Data.Foldable




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
class Pretty a where
    -- | Pretty print a value with a given level of verbosity and precedence
    pPrintPrec :: PrettyLevel -> Word8 -> a -> Doc
    -- | Pretty print a value with the default level of verbosity and precedence
    pPrint :: a -> Doc

    pPrint = pPrintPrec PrettyNormal 0
    pPrintPrec _ _ = pPrint

instance {-# OVERLAPPABLE #-} Show a => Pretty a where
    pPrint = shown

instance Pretty Char where
    pPrintPrec _ _ = shown

instance Pretty String where
    pPrintPrec _ _ = shown

instance {-# OVERLAPPABLE #-} (Pretty a) => Pretty [a] where
    pPrintPrec lvl _ = brackets . lsep . fmap (pPrintPrec lvl 0)

instance {-# OVERLAPPABLE #-} (Pretty a, Pretty b) => Pretty (a, b) where
    pPrintPrec lvl _ (a, b) = parens $ lsep
        [ pPrintPrec lvl 0 a
        , pPrintPrec lvl 0 b
        ]

instance {-# OVERLAPPABLE #-} (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
    pPrintPrec lvl _ (a, b, c) = parens $ lsep
        [ pPrintPrec lvl 0 a
        , pPrintPrec lvl 0 b
        , pPrintPrec lvl 0 c
        ]

instance {-# OVERLAPPABLE #-} (Pretty a, Pretty b, Pretty c, Pretty d)
    => Pretty (a, b, c, d) where
        pPrintPrec lvl _ (a, b, c, d) = parens $ lsep
            [ pPrintPrec lvl 0 a
            , pPrintPrec lvl 0 b
            , pPrintPrec lvl 0 c
            , pPrintPrec lvl 0 d
            ]

instance {-# OVERLAPPABLE #-} (Pretty a) => Pretty (Maybe a) where
    pPrintPrec lvl prec = \case
        Just a -> maybeParens (prec > 0) do
            hang "Just"
                (pPrintPrec lvl 0 a)
        _ -> "Nothing"

instance {-# OVERLAPPABLE #-} (Pretty a, Pretty b) => Pretty (Either a b) where
    pPrintPrec lvl prec = maybeParens (prec > 0) . \case
        Left a -> hang "Left"
            (pPrintPrec lvl 0 a)
        Right b -> hang "Right"
            (pPrintPrec lvl 0 b)

instance {-# OVERLAPPABLE #-} (Pretty k, Pretty v) => Pretty (Map k v) where
    pPrintPrec lvl _ m
        = braces . lsep
        $ Map.toList m <&> \(k, v) ->
            pPrintPrec lvl 0 k <+> "=" <+> pPrintPrec lvl 0 v

instance {-# OVERLAPPABLE #-} (Pretty k) => Pretty (Set k) where
    pPrintPrec lvl _ s
        = braces . lsep
        $ Set.toList s <&> pPrintPrec lvl 0

instance {-# OVERLAPPABLE #-} (Pretty a) => Pretty (Seq a) where
    pPrintPrec lvl _ s
        = hashes . brackets . lsep
        $ toList s <&> pPrintPrec lvl 0



-- | Pretty print a value with the default level of verbosity and precedence,
--   and convert the resulting @Doc@ to a @String@
prettyShow :: Pretty a => a -> String
prettyShow = prettyShowLevel PrettyNormal

-- | Pretty print a value with a given level of verbosity and precedence,
--   and convert the resulting @Doc@ to a @String@
prettyShowLevel :: Pretty a => PrettyLevel -> a -> String
prettyShowLevel lvl = render . pPrintPrec lvl 0

-- | Pretty print a value with the default level of verbosity and precedence,
--   and print the resulting @Doc@ to the console
prettyPrint :: Pretty a => a -> IO ()
prettyPrint = prettyPrintLevel PrettyNormal

-- | Pretty print a value with a given level of verbosity and precedence,
--   and print the resulting @Doc@ to the console
prettyPrintLevel :: Pretty a => PrettyLevel -> a -> IO ()
prettyPrintLevel lvl = putStrLn . prettyShowLevel lvl

-- | Pretty print the value if it exists, otherwise print nothing
maybePPrint :: Pretty a => Maybe a -> Doc
maybePPrint = maybe mempty pPrint

-- | Pretty print the value with a given level of verbosity and precedence,
--   if it exists, otherwise print nothing
maybePPrintPrec :: Pretty a => PrettyLevel -> Word8 -> Maybe a -> Doc
maybePPrintPrec lvl prec = maybe mempty (pPrintPrec lvl prec)

-- | The usual `hang` with a consistent indentation of 4 spaces
hang :: Doc -> Doc -> Doc
hang a = X.hang a 4

-- | The usual `nest` with a consistent indentation of 4 spaces
indent :: Doc -> Doc
indent = nest 4

-- | @text . show@
shown :: Show a => a -> Doc
shown = text . show

-- | list version of @($+$)@
vcat' :: [Doc] -> Doc
vcat' = foldr ($+$) mempty

-- | Concatenate a list with double new lines between elements
vcatDouble :: [Doc] -> Doc
vcatDouble = (`foldr` mempty) \a ->
    \case
        (isEmpty -> True) -> a
        b -> a $+$ zeroWidthText "" $+$ b

-- | @sep . punctuate ","@
lsep :: [Doc] -> Doc
lsep = fsep . punctuate ","

-- | Enclose a @Doc@ in backticks ``
backticks :: Doc -> Doc
backticks d = "`" <> d <> "`"

-- | Pretty print a value and enclose it in backticks ``
backticked :: Pretty a => a -> Doc
backticked = backticks . pPrint

-- | Enclose a @Doc@ in backticks `` if the boolean is true
maybeBackticks :: Bool -> Doc -> Doc
maybeBackticks True = backticks
maybeBackticks False = id

-- | Pretty print a value and enclose it in backticks `` if the boolean is true
maybeBackticked :: Pretty a => Bool -> a -> Doc
maybeBackticked b = maybeBackticks b . pPrint

-- | Enclose a @Doc@ in hashes ##
hashes :: Doc -> Doc
hashes d = "#" <> d <> "#"

-- | Pretty print a value and enclose it in hashes ##
hashed :: Pretty a => a -> Doc
hashed = hashes . pPrint

-- | Enclose a @Doc@ in hashes ## if the boolean is true
maybeHashes :: Bool -> Doc -> Doc
maybeHashes True = hashes
maybeHashes False = id

-- | Pretty print a value and enclose it in hashes ## if the boolean is true
maybeHashed :: Pretty a => Bool -> a -> Doc
maybeHashed b = maybeHashes b . pPrint

-- | Pretty print a value and enclose it in single quotes ''
quoted :: Pretty a => a -> Doc
quoted = quotes . pPrint

-- | Pretty print a value and enclose it in single quotes ''
--   if the boolean is true
maybeQuoted :: Pretty a => Bool -> a -> Doc
maybeQuoted b = maybeQuotes b . pPrint

-- | Pretty print a value and enclose it in double quotes ""
doubleQuoted :: Pretty a => a -> Doc
doubleQuoted = doubleQuotes . pPrint

-- | Pretty print a value and enclose it in double quotes ""
--   if the boolean is true
maybeDoubleQuoted :: Pretty a => Bool -> a -> Doc
maybeDoubleQuoted b = maybeDoubleQuotes b . pPrint

-- | Pretty print a value and enclose it in parens ()
paren'd :: Pretty a => a -> Doc
paren'd = parens . pPrint

-- | Enclose a @Doc@ in parens () if the boolean is true
maybeParen'd :: Pretty a => Bool -> a -> Doc
maybeParen'd b = maybeParens b . pPrint

-- | Pretty print a value and enclose it in braces {}
braced :: Pretty a => a -> Doc
braced = braces . pPrint

-- | Pretty print a value and enclose it in braces {}
--   if the boolean is true
maybeBraced :: Pretty a => Bool -> a -> Doc
maybeBraced b = maybeBraces b . pPrint

-- | Pretty print a value and enclose it in brackets []
bracketed :: Pretty a => a -> Doc
bracketed = brackets . pPrint

-- | Pretty print a value and enclose it in brackets []
--   if the boolean is true
maybeBracketed :: Pretty a => Bool -> a -> Doc
maybeBracketed b = maybeBrackets b . pPrint
