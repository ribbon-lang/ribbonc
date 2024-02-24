{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main(main, lexFile, parseFileWith) where

import Data.Functor
import Control.Applicative

import Data.ByteString.Lazy qualified as ByteString
import Data.Sequence (Seq)

import Data.Attr

import Text.Pretty as Pretty

import Language.Ribbon.Util

import Language.Ribbon.Syntax
import Language.Ribbon.Lexical

import Language.Ribbon.Parsing.Lexer as L

import Language.Ribbon.Parsing.Parser as P
import Language.Ribbon.Parsing.Monad.Parser as P




main :: IO ()
main = putStrLn "Hello, Ribbon!"

lexFile :: FilePath -> IO (Either Doc TokenSeq)
lexFile p = ByteString.readFile p <&> lexByteString p

parseFileWith :: Parser a -> FilePath -> IO (Either Doc a)
parseFileWith px p = ByteString.readFile p <&> parseByteStringWith px p
