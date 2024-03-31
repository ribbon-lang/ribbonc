module Language.Ribbon.Parsing.Text where

import Data.Foldable qualified as Fold

import Data.Word (Word32)
import Data.Char qualified as Char

import Data.Nil

import Numeric

import Language.Ribbon.Util
import Language.Ribbon.Lexical.Version




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


-- | List of @Char@s that are considered vertical whitespace
verticalSpaces :: String
verticalSpaces = "\n\r\v\f"

-- | List of @Char@s that must be escaped,
--   when occurring in a string or character literal
mustEscapes :: String
mustEscapes =  "\\\a\b\f\n\r\t\v\0"

-- | List of @String@s that are lexically reserved, ie keywords and special ops
reservedSymbols :: [String]
reservedSymbols =
    [ "alias", "effect", "class", "instance", "struct"
    , "union", "namespace"
    , "type", "value"
    , "module", "use", "file", "pub", "hiding"
    , "in", "as", "of", "do", "with", "where", "for"
    , "fun", "let", "match", "if", "then", "else"
    , "'", "`"
    , "=", ":", "=>", ";", ",", "|", "~/"
    , "{", "}", "(", ")", "[", "]"
    , "_"
    ]

-- | List of @Char@s that are considered punctuation
--   (non-combining operators)
punctuations :: String
punctuations = userPunctuations <> reservedPunctuations <> blockPunctuations

-- | List of @Char@s that are considered "user-accessible" punctuation,
--   ie ',;
userPunctuations :: String
userPunctuations = "',;"

-- | List of @Char@s that are considered reserved punctuation,
--   ie .\`"
reservedPunctuations :: String
reservedPunctuations = ".`\""

-- | List of @Char@s that are considered block delimiting punctuation,
--   ie ()[]{}
blockPunctuations :: String
blockPunctuations = "()[]{}"

-- | Determines if a @String@ is a reserved name
isReserved :: String -> Bool
isReserved = (`elem` reservedSymbols)

-- | Determines if a @String@ is valid as a user-defined name;
--   NOTE: does not check if it is actually well-formed,
--   just not that is not reserved. Use @isIdentifier@/@isOperator@ for that.
--   Alternatively, use @isUserSymbol@ for a complete test
notReserved :: String -> Bool
notReserved = not . isReserved

-- | Determines if a @String@ is valid as a user-defined name
isUserSymbol :: String -> Bool
isUserSymbol = (isOperator ||| isIdentifier) &&& notReserved

-- | Determines if a @String@ is valid as an identifier;
--   NOTE: does not check if it is a reserved identifier,
--   use @isReserved@ / @notReserved@ for that.
--   Alternatively, use @isUserSymbol@ for a complete test
isIdentifier :: String -> Bool
isIdentifier s
     = not (null s)
    && (Char.isAlpha ||| (== '_')) (head s)
    && all (Char.isAlphaNum ||| (== '_')) (tail s)

-- | Determines if a @String@ is valid as an operator;
--   NOTE: does not check if it is a reserved operator,
--   use @isReserved@ / @notReserved@ for that.
--   Alternatively, use @isUserSymbol@ for a complete test
isOperator :: String -> Bool
isOperator s
     = not (null s)
    && all isSymbolic s

-- | Check if a @Char@ is a "symbolic" character;
--   ie. a punctuation or symbol character,
--   not including '_' or reserved punctuations
isSymbolic :: Char -> Bool
isSymbolic
    = Char.isPunctuation ||| Char.isSymbol
    &&& not . (`elem` '_' : punctuations)
