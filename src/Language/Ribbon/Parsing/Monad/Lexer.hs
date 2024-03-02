module Language.Ribbon.Parsing.Monad.Lexer where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text

import Data.Char qualified as Char


import Data.Foldable
import Data.Functor

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class

import Data.Tag
import Data.Pos
import Data.Range
import Data.Attr
import Data.Nil

import Text.Pretty

import Language.Ribbon.Util

import Language.Ribbon.Lexical

import Language.Ribbon.Parsing.Error




data DecodeProblem
    = DecodeBadEncoding
    | DecodeEof
    deriving Show

data LexStream
    = LexStream
    { pos :: !Pos
    , input :: Text
    }

lsEof :: LexStream -> Bool
lsEof = Text.null . (.input)

lsRange :: Pos -> LexStream -> Range
lsRange pos = Range pos . (.pos)

lsUnitRange :: LexStream -> Range
lsUnitRange = unitRange . (.pos)

lsAttr :: FilePath -> Pos -> LexStream -> Attr
lsAttr path = Attr path .: lsRange

lsUnitAttr :: FilePath -> LexStream -> Attr
lsUnitAttr path = Attr path . lsUnitRange

lsReadChar :: LexStream -> Either DecodeProblem (Char, LexStream)
lsReadChar ls = do
    (h, input') <- maybeError DecodeEof do
        Text.uncons ls.input

    size <- byteWidth $ Char.ord h

    let offset' = size + ls.pos.offset
        (line', column') =
            if h == '\n'
                then (ls.pos.line + 1, 1)
                else (ls.pos.line, ls.pos.column + 1)

    pure (h, ls {
        input = input',
        pos = Pos offset' line' column'
    }) where
    byteWidth c
        | c == 0xFFFD = Left DecodeBadEncoding
        | c <= 0x7f = pure 1
        | c <= 0x7ff = pure 2
        | c <= 0xffff = pure 3
        | otherwise = pure 4

formatProblem :: FilePath -> LexStream -> DecodeProblem -> ATag (ParseFail Char)
formatProblem fp ls = \case
    DecodeBadEncoding ->
        SingleFailure "invalid utf8" :@: lsUnitAttr fp ls
    DecodeEof ->
        EofFailure :@: lsUnitAttr fp ls

formatInput :: FilePath -> LexStream -> ATag (ParseFail Char)
formatInput fp ls = case lsReadChar ls of
    Left e -> formatProblem fp ls e
    Right (c, ls') ->
        UnexpectedFailure c :@: lsAttr fp ls.pos ls'

newtype Lexer a
    = Lexer
    ( FilePath -> LexStream -> Either (ParseError Char) (a, LexStream) )

runLexer :: Lexer a -> FilePath -> LexStream -> Either (ParseError Char) (a, LexStream)
runLexer (Lexer f) = f

-- | Perform lexical analysis on @Text@ using the given @Lexer@,
--   with errors reported to be in the provided @FilePath@
lexTextWith :: Lexer a -> FilePath -> Text -> Either Doc a
lexTextWith lx fp txt = case runLexer lx fp (LexStream Nil txt) of
    Left e -> Left $ pPrint e
    Right (a, _) -> Right a

-- | Perform lexical analysis on a @ByteString@ using the given @Lexer@,
--   with errors reported to be in the provided @FilePath@
lexByteStringWith :: Lexer a -> FilePath -> ByteString -> Either Doc a
lexByteStringWith lx filePath =
    lexTextWith lx filePath . Text.decodeUtf8With Text.lenientDecode

-- | Perform lexical analysis on a @String@ using the given @Lexer@,
--   with errors reported to be in the provided @FilePath@
lexStringWith :: Lexer a -> FilePath -> String -> Either Doc a
lexStringWith lx name = lexTextWith lx name . Text.pack

-- | Perform lexical analysis on a file using the given @Lexer@
lexFileWith :: Lexer a -> FilePath -> IO (Either Doc a)
lexFileWith lx fp = do
    txt <- ByteString.readFile fp
    pure $ lexByteStringWith lx fp txt

instance Functor Lexer where
    fmap f (Lexer g) = Lexer \fp ls -> case g fp ls of
        Left e -> Left e
        Right (a, ls') -> Right (f a, ls')

instance Applicative Lexer where
    pure a = Lexer \_ ls -> Right (a, ls)
    Lexer f <*> Lexer g = Lexer \fp ls -> case f fp ls of
        Left e -> Left e
        Right (a, ls') -> case g fp ls' of
            Left e -> Left e
            Right (b, ls'') -> Right (a b, ls'')

instance Monad Lexer where
    Lexer f >>= g = Lexer \fp ls -> case f fp ls of
        Left e -> Left e
        Right (a, ls') -> runLexer (g a) fp ls'

instance Alternative Lexer where
    empty = Lexer (Left . ParseError Recoverable .: formatInput)
    Lexer f <|> Lexer g = Lexer \fp ls -> case f fp ls of
        Left (ParseError Recoverable e) ->
            case g fp ls of
                Left (ParseError Recoverable e') ->
                    Left $ ParseError Recoverable (e <> e')
                b -> b
        a -> a

instance MonadFail Lexer where
    fail msg = Lexer \fp ls -> Left $ ParseError Recoverable $
        SingleFailure (text msg) :@: lsUnitAttr fp ls

instance MonadError (ParseError Char) Lexer where
    throwError e = Lexer \_ _ -> Left e
    catchError (Lexer f) h = Lexer \fp ls -> case f fp ls of
        Left e -> runLexer (h e) fp ls
        Right a -> Right a

instance MonadState LexStream Lexer where
    state f = Lexer \_ ls -> Right (f ls)

instance MonadReader FilePath Lexer where
    ask = Lexer (Right .: (,))
    local f (Lexer g) = Lexer (g . f)


-- | Run a @Lexer@, and if it fails, make the @ParseError@ unrecoverable
noFail :: Lexer a -> Lexer a
noFail m = m `catchError` \(ParseError _ f) -> Lexer \_ _ ->
    Left $ ParseError Unrecoverable f

-- | Run a @Lexer@, and if it fails, make the @ParseError@ unrecoverable
--   when the given condition is @True@
noFailIf :: Bool -> Lexer a -> Lexer a
noFailIf p m = if p then noFail m else m

-- | Ensure that the given @Lexer@ action consumes the remaining @ParseStream@
consumesAll :: Lexer a -> Lexer a
consumesAll m = Lexer \fp ls ->
    case runLexer m fp ls of
        Left e -> Left e
        Right (a, ls') -> if lsEof ls'
            then Right (a, ls')
            else Left $ ParseError Recoverable $ formatInput fp ls


-- | Trigger an unrecoverable @ParseError@
--   with a message at the current location
lexError :: Doc -> Lexer a
lexError msg = attr >>= (`lexErrorAt` msg)

-- | Trigger an unrecoverable @ParseError@
--   with a message at the given @Attr@
lexErrorAt :: Attr -> Doc -> Lexer a
lexErrorAt x msg =
    throwError $ ParseError Unrecoverable $ SingleFailure msg :@: x


-- | Trigger an unrecoverable @ParseError@
--   with a message at the current location,
--   if the given condition is false
assert :: Bool -> Doc -> Lexer ()
assert p expStr = unless p (lexError expStr)

-- | Trigger an unrecoverable @ParseError@
--   with a message at the given @Attr@,
--   if the condition is false
assertAt :: Attr -> Bool -> Doc -> Lexer ()
assertAt x p expStr = unless p (lexErrorAt x expStr)




-- | Advance the offset in the current @Lexer@ action
advance :: Lexer ()
advance = Lexer \fp ls ->
    case lsReadChar ls of
        Left e -> Left $ ParseError Recoverable (formatProblem fp ls e)
        Right (_, ls') -> Right ((), ls')


-- | Get the currently selected @Char@ in the @Lexer@ action
peek :: Lexer Char
peek = Lexer \fp ls ->
    case lsReadChar ls of
        Left e -> Left $ ParseError Recoverable (formatProblem fp ls e)
        Right (c, _) -> Right (c, ls)

-- | Get the currently selected @Char@ in the @Lexer@ action,
--   returning @Nothing@ if the stream is empty;
--   note that this still errors on invalid utf8
tryPeek :: Lexer (Maybe Char)
tryPeek = Lexer \fp ls ->
    case lsReadChar ls of
        Left DecodeEof -> Right (Nothing, ls)
        Left e -> Left $ ParseError Recoverable (formatProblem fp ls e)
        Right (c, _) -> Right (Just c, ls)

-- | Get a location @Attr@ for the selected position in the @Lexer@ action
attr :: Lexer Attr
attr = Lexer \fp ls -> Right (lsUnitAttr fp ls, ls)

-- | Get the @FilePath@ of the @Lexer@ action
getFilePath :: Lexer FilePath
getFilePath = ask

-- | Get the @Pos@ the @Lexer@ action is on
getPos :: Lexer Pos
getPos = gets (.pos)

-- | Get the @Attr@ from a given @Pos@ to the one the @Lexer@ action is on
getAttr :: Pos -> Lexer Attr
getAttr pos = getFilePath >>= gets . (`lsAttr` pos)

-- | Get the @Attr@ the @Lexer@ action is on
getUnitAttr :: Lexer Attr
getUnitAttr = getFilePath >>= gets . lsUnitAttr

-- | Wraps the output of a @Lexer@ in an @ATag@ for the range consumed
tag :: Lexer a -> Lexer (ATag a)
tag p = Lexer \fp ls -> case runLexer p fp ls of
    Left e -> Left e
    Right (a, ls') -> Right (a :@: lsAttr fp ls.pos ls', ls')


-- | Execute @tag@ and discard the result, keeping only the @Attr@ generated
attrOf :: Lexer a -> Lexer Attr
attrOf = fmap tagOf . tag

-- | Execute @tag@ and discard the result,
--   keeping only the first @Pos@ of the @Attr@ generated
posOf :: Lexer a -> Lexer Pos
posOf = fmap ((.start) . (.range)) . attrOf

-- | Get the currently selected @Char@ in the @Lexer@ action,
--   and advance the stream offset
next :: Lexer Char
next = Lexer \fp ls ->
    case lsReadChar ls of
        Left e -> Left $ ParseError Recoverable (formatProblem fp ls e)
        Right (a, ls') -> Right (a, ls')

-- | Get the currently selected @Char@'s @Attr@ in the @Lexer@ action
peekAttr :: Lexer (ATag Char)
peekAttr = peek <*@*> attr

-- | Get the currently selected @Char@'s @Attr@ in the @Lexer@ action,
--   and wrap it around the current @Char@ using @Tag@,
--   then advance the stream offset
nextAttr :: Lexer (ATag Char)
nextAttr = tag next

-- | Advance the @Lexer@ stream offset,
--   if the current @Char@ satisfies the predicate;
--   returning the matched @Char@
nextIf :: (Char -> Bool) -> Lexer Char
nextIf p = do
    a <- peek
    if p a
        then a <$ advance
        else empty

-- | Advance the @Lexer@ stream offset,
--   if the current @Char@ satisfies the predicate;
--   returning the matched @Char@ along with its @Attr@
nextIfAttr :: (ATag Char -> Bool) -> Lexer (ATag Char)
nextIfAttr p = do
    t <- peekAttr
    if p t
        then t <$ advance
        else empty

-- | Advance the @Lexer@ stream offset,
--   if the current @Char@ satisfies the predicate;
--   discarding the matched @Char@
nextIf_ :: (Char -> Bool) -> Lexer ()
nextIf_ p = do
    a <- peek
    if p a
        then advance
        else empty

-- | Advance the @Lexer@ stream offset,
--   as long as the current @Char@ satisfies the predicate;
--   returning the matched @Char@s as a list
nextWhile :: (Char -> Bool) -> Lexer String
nextWhile p = some (nextIf p)

-- | Advance the @Lexer@ stream offset,
--   as long as the current @Char@ satisfies the predicate;
--   returning the matched @Char@s as a list
nextWhileAttr :: (ATag Char -> Bool) -> Lexer [ATag Char]
nextWhileAttr p = some (nextIfAttr p)

-- | Advance the @Lexer@ stream offset,
--   as long as the current @Char@ satisfies the predicate;
--   discarding the matched @Char@s
nextWhile_ :: (Char -> Bool) -> Lexer ()
nextWhile_ p = some_ (nextIf_ p)

-- | Advance the @Lexer@ stream offset,
--   if the current @Char@ satisfies a mapping predicate;
--   returning the mapped value
nextMap :: (Char -> Maybe a) -> Lexer a
nextMap p = Lexer \fp ls ->
    case lsReadChar ls of
        Left e -> Left $ ParseError Recoverable (formatProblem fp ls e)
        Right (c, ls') -> case p c of
            Just a -> Right (a, ls')
            _ -> Left $ ParseError Recoverable (formatInput fp ls)


-- | Wrap the @ParseError@s of a given @Lexer@
--   in an explanation of the expectation that was had of it
expecting :: Doc -> Lexer a -> Lexer a
expecting msg px = px `catchError` \(ParseError r f) -> Lexer \fp ls ->
    Left $ ParseError r $ ExpectationFailure [msg] f :@: lsUnitAttr fp ls

-- | Wrap the @ParseError@s of a given @Lexer@
--   in an explanation of the expectations that were had of it
expectingMulti :: [Doc] -> Lexer a -> Lexer a
expectingMulti = expecting . lsep


-- | Wrap the @ParseError@s of a given @Lexer@
--   in an explanation of the expectation that was had of it
expectingAt :: Attr -> Doc -> Lexer a -> Lexer a
expectingAt at msg px = px `catchError` \(ParseError r f) -> Lexer \_ _ ->
    Left $ ParseError r $ ExpectationFailure [msg] f :@: at

-- | Wrap the @ParseError@s of a given @Lexer@
--   in an explanation of the expectations that were had of it
expectingMultiAt :: Attr -> [Doc] -> Lexer a -> Lexer a
expectingMultiAt at = expectingAt at . lsep


-- | @nextIf_ (== e)@
expect' :: Char -> Lexer ()
expect' e = nextIf_ (== e)

-- | @nextIf (`elem` es)@
expectAny' :: String -> Lexer Char
expectAny' es = nextIf (`elem` es)

-- | Consume an expected sequence of inputs
expectSeq' :: String -> Lexer ()
expectSeq' = traverse_ \e -> do
    a <- peek
    if a == e
        then advance
        else empty

-- | Consume one of any expected sequences of inputs
expectAnySeq' :: [String] -> Lexer String
expectAnySeq' es = do
    asum (es <&> \e -> e <$ expectSeq' e)

-- | @expecting (pPrint e) (expect' e)@
expect :: Char -> Lexer ()
expect e = expecting (pPrint e) (expect' e)

-- | @expectingMulti (pPrint <$> es) (expectAny' es)@
expectAny :: String -> Lexer Char
expectAny es = expectingMulti (pPrint <$> es) (expectAny' es)

-- | @expecting (pPrint e) (expectSeq' e)@
expectSeq :: String -> Lexer ()
expectSeq e = expecting (pPrint e) (expectSeq' e)

-- | @expectingMulti (pPrint <$> es) (expectAnySeq' es)@
expectAnySeq :: [String] -> Lexer String
expectAnySeq es = expectingMulti (pPrint <$> es) (expectAnySeq' es)

-- | Expect a list of @Lexer a@ separated by @Lexer s@.
--   The list must contain at least one @Char@
listSome :: Lexer s -> Lexer a -> Lexer [a]
listSome s p = liftA2 (:) p (many $ s >> p)

-- | Expect a list of @Lexer a@ separated by @Lexer s@.
--   The list may be empty
listMany :: Lexer s -> Lexer a -> Lexer [a]
listMany s p = option [] (listSome s p)

hScan :: Lexer ()
hScan = option () $ nextWhile_ (== ' ')

hScanning :: Lexer a -> Lexer a
hScanning = (hScan *>)

connected :: Attr -> Lexer a -> Lexer a
connected at lx = do
    x :@: at' <- tag lx
    x <$ guard (attrConnected at at')

connectSome :: Attr -> Lexer a -> Lexer [a]
connectSome at lx = do
    a :@: at' <- tag (connected at lx)
    (a :) <$> connectMany at' lx

connectMany :: Attr -> Lexer a -> Lexer [a]
connectMany at lx = option [] (connectSome at lx)
