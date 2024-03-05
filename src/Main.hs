{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Data.Functor
import Control.Applicative

import Data.ByteString.Lazy qualified as ByteString
import Data.Sequence (Seq)

import Data.Attr

import Text.Pretty as Pretty

import Language.Ribbon.Util

import Language.Ribbon.Syntax
import Language.Ribbon.Lexical

import Language.Ribbon.Parsing.Lexer qualified as L
import Control.Monad.Parser

import Language.Ribbon.Parsing.Parser qualified as P
import Control.Monad
import Data.SyntaxError
import Control.Monad.Except




main :: IO ()
main = putStrLn "Hello, Ribbon!"
