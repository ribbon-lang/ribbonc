{-# OPTIONS_GHC -Wno-orphans #-}

module Text.Pretty
    ( module X
    , shown
    , vcat', vcatDouble, ($++$), with, joinWith, spaceWith, linesWith
    , hang, qual, qual', qualH, indent, lsep
    , backticks, backticked, maybeBackticks, maybeBackticked
    , hashes, hashed, maybeHashes, maybeHashed
    , quoted, maybeQuoted
    , doubleQuoted, maybeDoubleQuoted
    , paren'd, maybeParen'd
    , braced, maybeBraced
    , bracketed, maybeBracketed
    , prettyShowLevel, prettyPrint, prettyPrintLevel
    , maybePPrint, maybePPrintPrec
    , pattern PrettyNormal, pattern PrettyRich, pattern PrettyVerbose
    ) where

import Text.PrettyPrint.HughesPJClass qualified as X
import Text.PrettyPrint.HughesPJClass as X hiding
    ((<>), empty, ptext, char, hang
    , int, integer, float, double, rational
    , semi, comma, space, equals, lparen, rparen, lbrack, rbrack, lbrace, rbrace
    , first
    )

import Data.Functor((<&>))
import Data.Foldable qualified as Fold

import Data.Word (Word8, Word32)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Sequence (Seq)

import Data.Nil


instance Nil Doc where
    nil = mempty
    isNil = isEmpty


instance Pretty Word8 where
    pPrint = shown @Integer . fromIntegral

instance Pretty Word32 where
    pPrint = shown @Integer . fromIntegral

instance Pretty Doc where
    pPrint = id

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
        $ Fold.toList s <&> pPrintPrec lvl 0

-- | The default, base level of pretty printing
pattern PrettyNormal :: PrettyLevel
pattern PrettyNormal <- ((== prettyNormal) -> True)
    where PrettyNormal = prettyNormal

-- | Attach more information to the pretty printed output
pattern PrettyRich :: PrettyLevel
pattern PrettyRich = PrettyLevel 1

-- | Print the most information possible
pattern PrettyVerbose :: PrettyLevel
pattern PrettyVerbose = PrettyLevel 2



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
maybePPrint = maybe Nil pPrint

-- | Pretty print the value with a given level of verbosity and precedence,
--   if it exists, otherwise print nothing
maybePPrintPrec :: Pretty a => PrettyLevel -> Rational -> Maybe a -> Doc
maybePPrintPrec lvl prec = maybe Nil (pPrintPrec lvl prec)

-- | The usual `hang` with a consistent indentation of 4 spaces
hang :: Doc -> Doc -> Doc
hang a = X.hang a 4

-- | `<+>` where the first document is removed if the second is empty
qual :: Doc -> Doc -> Doc
qual _ Nil = Nil
qual a b = a <+> b

-- | `<+>` where the second document is removed if the first is empty
qual' :: Doc -> Doc -> Doc
qual' Nil _ = Nil
qual' a b = a <+> b

-- | `hang` where the first document is removed if the second is empty
qualH :: Doc -> Doc -> Doc
qualH _ Nil = Nil
qualH a b = hang a b

-- | The usual `nest` with a consistent indentation of 4 spaces
indent :: Doc -> Doc
indent = nest 4

-- | @text . show@
shown :: Show a => a -> Doc
shown = text . show

-- | list version of @($+$)@
vcat' :: [Doc] -> Doc
vcat' = foldr ($+$) Nil

-- | Concatenate two documents with double new lines between them
($++$) :: Doc -> Doc -> Doc
($++$) = linesWith (zeroWidthText "")

with :: (Doc -> Doc -> Doc) -> Doc -> Doc -> Doc -> Doc
with _ _ a Nil = a
with _ _ Nil b = b
with f s a b = a `f` s `f` b

-- | Join two documents together with new lines and a doc, if they are not empty
linesWith :: Doc -> Doc -> Doc -> Doc
linesWith = with ($+$)

-- | Join two documents together with spaces and a doc, if they are not empty
spaceWith :: Doc -> Doc -> Doc -> Doc
spaceWith = with (<+>)

-- | Join two documents together with a doc, if they are not empty
joinWith :: Doc -> Doc -> Doc -> Doc
joinWith = with (<>)

-- | Concatenate a list with double new lines between elements
vcatDouble :: [Doc] -> Doc
vcatDouble = foldr ($++$) Nil

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
