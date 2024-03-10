{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Data.Functor
import Control.Applicative
import Control.Monad.Diagnostics

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
import Language.Ribbon.Analysis
import Data.Diagnostic


lexFileWith ::
    ParserT L.LexStream (FileT (ExceptT SyntaxError IO)) a
        -> FilePath -> IO (Either Doc a)
lexFileWith p fp = runExceptT do
    lx <- L.lexStreamFromFile fp
    mapError pPrint $ runFileT (evalParserT p lx) fp

parseFileWith ::
    ParserT TokenSeq (FileT (ExceptT SyntaxError IO)) a
        -> FilePath -> IO (Either Doc a)
parseFileWith p fp = lexFileWith L.doc fp >>= \case
    Left e -> pure $ Left e
    Right ts -> do
        putStrLn "toks:"
        prettyPrint ts
        runExceptT $ mapError pPrint $
            runFileT (evalParserT p ts) fp

parseFile ::
    ModuleId -> ItemId -> FilePath ->
        IO (Either Doc (ParserDefs, [Diagnostic]))
parseFile mi ii
    = runExceptT
    . runDiagnosticsT
    . flip runContextT mi
    . P.file ii



main :: IO ()
main = putStrLn "Hello, Ribbon!"
