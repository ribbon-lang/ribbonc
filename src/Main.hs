{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Data.Functor
import Control.Applicative

import Data.ByteString.Lazy qualified as ByteString
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Sequence (Seq)

import Data.Attr
import Data.Nil

import Text.Pretty as Pretty

import Language.Ribbon.Util

import Language.Ribbon.Syntax
import Language.Ribbon.Lexical

import Language.Ribbon.Parsing.Lexer qualified as L
import Control.Monad.File
import Control.Monad.Parser

import Language.Ribbon.Parsing.Parser qualified as P
import Control.Monad
import Data.SyntaxError
import Control.Monad.Except
import qualified Data.Text.Encoding.Error as TextErr


lexFileWith ::
    ParserT L.LexStream (FileT (ExceptT SyntaxError IO)) a
        -> FilePath -> IO (Either SyntaxError a)
lexFileWith p fp = do
    lx <- L.lexStreamFromFile fp
    runExceptT (runFileT (evalParserT p lx) fp)

parseFileWith ::
    ParserT TokenSeq (FileT (ExceptT SyntaxError IO)) a
        -> FilePath -> IO (Either SyntaxError a)
parseFileWith p fp = do
    lexFileWith L.doc fp >>= \case
        Left e -> pure $ Left e
        Right ts -> do
            putStrLn "toks:"
            prettyPrint ts
            runExceptT $ runFileT (evalParserT p ts) fp

main :: IO ()
main = putStrLn "Hello, Ribbon!"
