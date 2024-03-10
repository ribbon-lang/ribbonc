{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Data.Functor

import Data.ByteString.Lazy qualified as ByteString
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Sequence (Seq)

import Data.Attr
import Data.Diagnostic
import Data.SyntaxError
import Data.Nil

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Dynamic

import Text.Pretty as Pretty

import Language.Ribbon.Util
import Language.Ribbon.Syntax
import Language.Ribbon.Lexical
import Language.Ribbon.Parsing.Monad
import Language.Ribbon.Parsing.Lexer qualified as L
import Language.Ribbon.Parsing.Parser qualified as P
import Language.Ribbon.Analysis
import Language.Ribbon.Syntax.Raw (RawModuleHeader)


lexFileWith ::
    ParserT L.LexStream (ReaderT FilePath (ErrorT SyntaxError IO)) a
        -> FilePath -> IO (Either Doc a)
lexFileWith p fp = runErrorT do
    lx <- L.lexStreamFromFile fp
    mapError pPrint $ runReaderT (evalParserT p lx) fp

parseFileWith ::
    ParserT TokenSeq (ReaderT FilePath (ErrorT SyntaxError IO)) a
        -> FilePath -> IO (Either Doc a)
parseFileWith p fp = lexFileWith L.doc fp >>= \case
    Left e -> pure $ Left e
    Right ts -> do
        putStrLn "toks:"
        prettyPrint ts
        runErrorT $ mapError pPrint $
            runReaderT (evalParserT p ts) fp


parseModuleHead ::
    FilePath -> IO (Either Doc (RawModuleHeader, [TokenSeq]))
parseModuleHead = parseFileWith P.moduleHead

parseFile ::
    ModuleId -> ItemId -> FilePath ->
        IO (Either Doc (ParserDefs, [Diagnostic]))
parseFile mi ii
    = runErrorT
    . runWriterT
    . runReaderT' mi
    . P.file ii



main :: IO ()
main = putStrLn "Hello, Ribbon!"
