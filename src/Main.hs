{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main(main, pLexFileWith, pParseFileWith) where

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
import Language.Ribbon.Parsing.Monad.Lexer qualified as L

import Language.Ribbon.Parsing.Parser qualified as P
import Language.Ribbon.Parsing.Monad.Parser qualified as P
import Control.Monad




main :: IO ()
main = putStrLn "Hello, Ribbon!"

pLexFileWith :: Pretty a => L.Lexer a -> FilePath -> IO ()
pLexFileWith p = L.lexFileWith p >=> \case
    Left e -> prettyPrint e
    Right ts -> prettyPrint ts

pParseFileWith :: Pretty a => P.Parser a -> FilePath -> IO ()
pParseFileWith p = P.parseFileWith p >=> \case
    Left e -> prettyPrint e
    Right a -> prettyPrint a
