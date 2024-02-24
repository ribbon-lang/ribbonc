module Language.Ribbon.Parsing.Text where

import Numeric

import Data.Word (Word32)

import Data.Char qualified as Char

import Data.Foldable qualified as Fold

import Data.Nil

import Language.Ribbon.Util

import Language.Ribbon.Lexical




-- | Parse a @Char@ from an escape @String@
parseChar :: String -> Maybe Char
parseChar s =
    case s of
        [c] -> Just c

        "\\\\" -> Just '\\'
        "\\\'" -> Just '\''
        "\\\"" -> Just '\"'
        "\\n" -> Just '\n'
        "\\r" -> Just '\r'
        "\\t" -> Just '\t'
        "\\0" -> Just '\0'

        ['\\', 'x', a, b] ->
            Char.chr . fromIntegral <$> parseHexInt [a, b]

        '\\':'u':'{':cs ->
            Char.chr . fromIntegral <$> parseHexInt (dropTail 1 cs)

        _ -> Nothing

-- | Attempt to convert a @String@ to a @Word32@, using base 2
parseBinInt :: String -> Maybe Word32
parseBinInt s =
    case readInt 2 (`elem` ['0', '1']) Char.digitToInt s of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Attempt to convert a @String@ to a @Word32@, using base 10
parseDecInt :: String -> Maybe Word32
parseDecInt s =
    case readDec s of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Attempt to convert a @String@ to a @Word@, using base 10
parseWord :: String -> Maybe Word
parseWord s =
    case readDec s of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Attempt to convert a @String@ to a @Word32@, using base 16
parseHexInt :: String -> Maybe Word32
parseHexInt s =
    case readHex s of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Attempt to convert a @String@ to a @Float@
parseFloat :: String -> Maybe Float
parseFloat s =
    case readFloat s of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Attempt to convert a @String@ to a @Version@
parseVersion :: String -> Maybe Version
parseVersion s =
    case splitOn '.' s of
        [major, minor, patch] -> do
            major' <- parseWord major
            minor' <- parseWord minor
            patch' <- parseWord patch
            Fold.find (/= Nil) (Just $ Version major' minor' patch')
        _ -> Nothing
