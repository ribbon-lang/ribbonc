{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main
    ( main
    , pLexStringWith, pParseStringWith
    , pLexFileWith, pParseFileWith
    )
    where

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

pLexStringWith :: Pretty a => L.Lexer a -> String -> IO ()
pLexStringWith p = L.lexStringWith p "repl" <&> \case
    Left e -> prettyPrint e
    Right ts -> prettyPrint ts

pParseStringWith :: Pretty a => P.Parser a -> String -> IO ()
pParseStringWith p = L.lexStringWith L.doc "repl" <&> \case
    Left e -> prettyPrint e
    Right toks -> do
        prettyPrint (hang "tokens:" $ pPrint toks)
        case P.evalParser p "repl" toks of
            Left e -> prettyPrint e
            Right ast -> do
                prettyPrint (hang "ast:" $ pPrint ast)

pLexFileWith :: Pretty a => L.Lexer a -> FilePath -> IO ()
pLexFileWith p = L.lexFileWith p >=> \case
    Left e -> prettyPrint e
    Right ts -> prettyPrint ts

pParseFileWith :: Pretty a => P.Parser a -> FilePath -> IO ()
pParseFileWith p fp = L.lexFileWith L.doc fp >>= \case
    Left e -> prettyPrint e
    Right toks -> do
        prettyPrint (hang "tokens:" $ pPrint toks)
        case P.evalParser p fp toks of
            Left e -> prettyPrint e
            Right ast -> do
                prettyPrint (hang "ast:" $ pPrint ast)
