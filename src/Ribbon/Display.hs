module Ribbon.Display where

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Map (Map)
import Data.Map qualified as Map

import Data.Sequence (Seq)

import Data.List qualified as List

import Data.Foldable (toList)


-- | Like Show, but with user-friendly formatting
class Display a where
    -- | Display a value with user-friendly formatting using default precedence
    display :: a -> String

    -- | Display a value with user-friendly formatting using the given precedence
    displayPrec :: Int -> a -> String

    display = displayPrec 0
    displayPrec _ = display

instance {-# OVERLAPPABLE #-} Show a => Display a where
    display = show

instance {-# OVERLAPPING #-} Display String where
    display = id

instance {-# OVERLAPPING #-} Display a => Display [a] where
    display = brackets . commas . fmap display

instance {-# OVERLAPPING #-} Display a => Display (Seq a) where
    display = hashes . display . toList

instance {-# OVERLAPPING #-} Display a => Display (Set a) where
    display = braces . commas . fmap display . Set.toList

instance {-# OVERLAPPING #-} (Display k, Display a) => Display (Map k a) where
    display = braces . commas
            . fmap (\(k, a) -> display k <> " = " <> display a)
            . Map.toList

instance {-# OVERLAPPING #-} Display a => Display (Maybe a) where
    display = maybe "Nothing" (parens . ("Just " <>) . display)

instance {-# OVERLAPPING #-} (Display a, Display b)
    => Display (Either a b) where
    display = parens . either (("Left " <>) . display) (("Right " <>) . display)

instance {-# OVERLAPPING #-} (Display a, Display b) => Display (a, b) where
    display (a, b) = parens $
        display a <> ", " <> display b

instance {-# OVERLAPPING #-} (Display a, Display b, Display c)
    => Display (a, b, c) where
    display (a, b, c) = parens $
        display a <> ", " <> display b <> ", " <> display c

instance {-# OVERLAPPING #-} (Display a, Display b, Display c, Display d)
    => Display (a, b, c, d) where
    display (a, b, c, d) = parens $
        display a <> ", " <> display b <> ", " <> display c <> ", " <> display d



-- | Join a list of strings with commas
commas :: [String] -> String
commas = List.intercalate ", "

-- | Join a list of strings with semicolons
semis :: [String] -> String
semis = List.intercalate "; "

-- | Join a list of strings with spaces
spaces :: [String] -> String
spaces = unwords

-- | Join a list of strings with newlines
newlines :: [String] -> String
newlines = unlines

-- | Join a list of strings with semicolons and newlines
semiLines :: [String] -> String
semiLines = List.intercalate ";\n"


-- | Place parens around a string
parens :: String -> String
parens s = "(" <> s <> ")"

-- | Place square brackets around a string
brackets :: String -> String
brackets s = "[" <> s <> "]"

-- | Place curly braces around a string
braces :: String -> String
braces s = "{" <> s <> "}"

-- Place hashes around a string
hashes :: String -> String
hashes s = "#" <> s <> "#"


-- | Place parens around a string if the given condition is true
parensIf :: Bool -> String -> String
parensIf True = parens
parensIf False = id

-- | Place square brackets around a string if the given condition is true
bracketsIf :: Bool -> String -> String
bracketsIf True = brackets
bracketsIf False = id

-- | Place curly braces around a string if the given condition is true
bracesIf :: Bool -> String -> String
bracesIf True = braces
bracesIf False = id


-- | Create a function that places parens around a string
--   if the input precedence is greater than the one given to the builder
parensPrec :: Int -> (a -> String) -> (Int -> a -> String)
parensPrec l f p = parensIf (p > l) . f

-- | Create a function that places square brackets around a string
--   if the input precedence is greater than the one given to the builder
bracketsPrec :: Int -> (a -> String) -> (Int -> a -> String)
bracketsPrec l f p = bracketsIf (p > l) . f

-- | Create a function that places curly braces around a string
--   if the input precedence is greater than the one given to the builder
bracesPrec :: Int -> (a -> String) -> (Int -> a -> String)
bracesPrec l f p = bracesIf (p > l) . f
