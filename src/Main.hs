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
import Language.Ribbon.Syntax.Raw
import qualified Data.Map.Strict as Map
import System.IO
import Control.Concurrent.ParallelIO


lexFile :: FilePath -> IO (Either Doc (ATag TokenSeq))
lexFile fp = runErrorT do
    lx <- mapError @SyntaxError pPrint $ L.lexStreamFromFile fp
    mapError @SyntaxError pPrint $ runReaderT (evalParserT (tag L.doc) lx) fp

lexStringWith ::
    ParserT L.LexStream (ReaderT FilePath (ErrorT SyntaxError IO)) a
        -> String -> IO (Either Doc a)
lexStringWith p s = runErrorT do
    let lx = L.lexStreamFromString s
    mapError pPrint $ runReaderT (evalParserT p lx) "stdin"

parseStringWith ::
    ParserT (ATag TokenSeq) (ReaderT FilePath (ErrorT SyntaxError IO)) a
        -> String -> IO (Either Doc a)
parseStringWith p s = lexStringWith (tag L.doc) s >>= \case
    Left e -> pure $ Left e
    Right ts -> do
        putStrLn "toks:"
        prettyPrint ts
        runErrorT $ mapError pPrint $
            runReaderT (evalParserT p ts) "stdin"


parseFileWith ::
    ParserT (ATag TokenSeq) (ReaderT FilePath (ErrorT SyntaxError IO)) a
        -> FilePath -> IO (Either Doc a)
parseFileWith p fp = lexFile fp >>= \case
    Left e -> pure $ Left e
    Right ts -> do
        putStrLn "toks:"
        prettyPrint ts
        runErrorT $ mapError pPrint $
            runReaderT (evalParserT p ts) fp


parseModuleHead ::
    FilePath -> IO (Either Doc (ATag RawModuleHeader, [ATag TokenSeq]))
parseModuleHead fp = lexFile fp >>= \case
    Left e -> pure $ Left e
    Right ts -> do
        putStrLn "toks:"
        prettyPrint ts
        runErrorT $ mapError @SyntaxError pPrint $
            runReaderT (evalParserT P.moduleHead ts) fp

parseSourceFile ::
    ModuleId -> ItemId -> FilePath ->
        IO (Either SyntaxError (ParserDefs, [Diagnostic]))
parseSourceFile mi ii
    = runErrorT
    . runWriterT
    . runReaderT' mi
    . P.sourceFile ii


parseModule ::
    FilePath -> IO (Either Doc (ParserModule, [Diagnostic]))
parseModule fp = do
    hSetBuffering stdout LineBuffering
    runErrorT $ runWriterT $ loadParserModule ctx 1 fp
    where
    ctx = ModuleContext
        { modules = mempty
        , moduleLookup = Map.fromList [(("core", Version 0 1 0), 1)]
        }


main :: IO ()
main = putStrLn "Hello, Ribbon!"
