module Ribbon.Syntax.Text where

import Data.Char qualified as Char

import Numeric

import Data.Foldable

import Ribbon.Util




-- | Predicate checking if a character is part of a whitespace segment,
--   but not a newline
isWhitespace :: Char -> Bool
isWhitespace = (`elem` " \t\r\n")


-- | Predicate checking if a character is a valid
--   identifier token start
isIdentifierStart :: Char -> Bool
isIdentifierStart c
     = Char.isAlpha c
    || c `elem` "_"

-- | Predicate checking if a character is a valid
--   identifier token subsequent
isIdentifierSubsequent :: Char -> Bool
isIdentifierSubsequent c
     = isIdentifierStart c
    || Char.isDigit c
    || c `elem` "'"


-- | Predicate checking if a character is a valid
--   operator token subsequent
isOperator :: Char -> Bool
isOperator c
     = not (Char.isSpace c)
    && not (Char.isControl c)
    && not (Char.isAlphaNum c)
    && c `notElem` "()[]{}\"\'`,;."


-- | Predicate checking if a character is syntactic punctuation
isPunctuation :: Char -> Bool
isPunctuation c = c `elem` "()[]{},;"

-- | Predicate checking if a string is a syntactic sentinel,
--  such as closing parens or commas
isSentinel :: String -> Bool
isSentinel c = c `elem` [")", "]", "}", ",", "=", "with", "in"]

-- | Predicate checking if a character is a dot operator
isDot :: Char -> Bool
isDot = (== '.')


-- | Reserved identifiers
reservedSymbols :: [String]
reservedSymbols =
    [ "type", "effect", "value", "forall", "fun"
    , "infix", "infixl", "infixr", "prefix", "postfix", "atom"
    , "module", "import", "use", "file", "pub", "namespace"
    , "let", "in", "as"
    , "match", "with"
    , "if", "then", "else"
    , "=", ":", "=>", ";", ",", ".", "..", "./", "../", ".*"
    , "{", "}", "(", ")", "[", "]"
    ]

-- | Predicate checking if a string is a reserved identifier or operator
isReserved :: String -> Bool
isReserved = (`elem` reservedSymbols)

-- | Parse a character literal from a String
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
        ['\\', 'x', a, b] -> Char.chr <$> parseHexInt [a, b]
        '\\':'u':'{':cs -> Char.chr <$> parseHexInt (dropTail 1 cs)
        _ -> Nothing

-- | Attempt to convert a String to an Int, using base 2
parseBinInt :: String -> Maybe Int
parseBinInt s =
    case readInt 2 (`elem` ['0', '1']) Char.digitToInt s of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Attempt to convert a String to an Int, using base 10
parseDecInt :: String -> Maybe Int
parseDecInt s =
    case readDec s of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Attempt to convert a String to a Word, using base 10
parseWord :: String -> Maybe Word
parseWord s =
    case readDec s of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Attempt to convert a String to an Int, using base 16
parseHexInt :: String -> Maybe Int
parseHexInt s =
    case readHex s of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Attempt to convert a String to a Float
parseFloat :: String -> Maybe Float
parseFloat s =
    case readFloat s of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Attempt to convert a String to a Version
parseVersion :: String -> Maybe Version
parseVersion s =
    case splitOn '.' s of
        [major, minor, patch] -> do
            major' <- parseWord major
            minor' <- parseWord minor
            patch' <- parseWord patch
            find (/= Nil) (Just $ Version major' minor' patch')
        _ -> Nothing
