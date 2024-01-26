module Ribbon.Syntax.Text where

import Numeric

import Data.Char qualified as Char


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
--   operator token start
isOperatorStart :: Char -> Bool
isOperatorStart
     = isOperatorSubsequent

-- | Predicate checking if a character is a valid
--   operator token subsequent
isOperatorSubsequent :: Char -> Bool
isOperatorSubsequent c
     = not (Char.isSpace c)
    && not (Char.isControl c)
    && not (Char.isAlphaNum c)
    && c `notElem` "()[]{}\"\',;."


-- | Predicate checking if a character is syntactic punctuation
isPunctuation :: Char -> Bool
isPunctuation c = c `elem` "()[]{},;"

-- | Predicate checking if a character is a dot operator
isDot :: Char -> Bool
isDot = (== '.')


-- | Reserved identifiers
reserved :: [String]
reserved =
    [ "type", "effect", "forall", "fun"
    , "let", "in"
    , "match", "with"
    , "if", "then", "else"
    , "=", ":"
    ]

-- | Predicate checking if a string is a reserved identifier
isReservedIdentifier :: String -> Bool
isReservedIdentifier = (`elem` reserved)


-- | Attempt to convert a String to an Int, using base 2
parseBinInt :: String -> Maybe Int
parseBinInt s =
    case readInt 2 (`elem` ['0', '1']) Char.digitToInt s of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Attempt to convert a String to an Int, using base 8
parseDecInt :: String -> Maybe Int
parseDecInt s =
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
