module Language.Ribbon.Parsing.Monad.Parser where

import Data.Functor
import Data.Foldable

import Data.String (fromString)
import Data.ByteString.Lazy (ByteString)

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.List qualified as List

import Data.Nil
import Data.Tag
import Data.Pos
import Data.Range
import Data.Attr

import Text.Pretty

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Language.Ribbon.Util

import Language.Ribbon.Lexical

import Language.Ribbon.Parsing.Lexer qualified as L




-- | Wrapper for all errors triggered in a @Parser@,
--   with the specific @ParseFail@ tagged with an @Attr@,
--   and a flag indicating whether the error is recoverable
data ParseError
    = ParseError
    { recoverability :: !Recoverability
    , failure :: !(ATag ParseFail)
    }
    deriving Show

-- | Indicates whether a @ParseError@ can be recovered
--   by the @Alternative Parser@ instance
data Recoverability
    = Recoverable
    | Unrecoverable
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Semigroup Recoverability where
    Recoverable <> Recoverable = Recoverable
    _ <> _ = Unrecoverable

instance Pretty Recoverability where
    pPrint = \case
        Recoverable -> "recoverable"
        Unrecoverable -> "unrecoverable"

instance Pretty ParseError where
    pPrintPrec lvl _ (ParseError r (f :@: x)) =
        let rd = select (lvl > PrettyNormal) (pPrint r) mempty
        in hang (rd <+> "syntax error at" <+> (pPrint x <> ":")) do
            formatFailure [x] f

instance Semigroup ParseError where
    ea <> eb = ParseError
        (ea.recoverability <> eb.recoverability)
        (ea.failure <> eb.failure)


-- | Specific type of failure for a @ParseError@
data ParseFail
    = EofFailure
    | UnexpectedFailure Token
    | SingleFailure !Doc
    | ExpectationFailure !Doc !(ATag ParseFail)
    | AlternativeFailure ![ATag ParseFail]
    deriving (Eq, Show)

instance Pretty ParseFail where
    pPrint = formatFailure []

instance Pretty (ATag ParseFail) where
    pPrint (f :@: x) = hang ("at" <+> (pPrint x <> ":")) (formatFailure [x] f)

instance Semigroup (ATag ParseFail) where
    (<>) ta@(a :@: xa) tb@(b :@: xb) = Tag (xa <> xb) case (a, b) of
        (UnexpectedFailure _, UnexpectedFailure _) | xa == xb -> a
        (AlternativeFailure as, AlternativeFailure bs) ->
            AlternativeFailure (as `List.union` bs)
        (AlternativeFailure as, _) -> AlternativeFailure (as `List.union` [tb])
        (_, AlternativeFailure bs) -> AlternativeFailure ([ta] `List.union` bs)
        _ -> case List.nub [ta, tb] of
            [e] -> e.value
            es -> AlternativeFailure es

-- | Format a @ParseFail@ into a @Doc@, discarding any @Attr@s
--   that have already been printed in the given tree
formatFailure :: [Attr] -> ParseFail -> Doc
formatFailure ignore = \case
        EofFailure -> "unexpected end of input"
        UnexpectedFailure tok ->
            "unexpected token" <+> backticked tok
        SingleFailure doc -> doc
        ExpectationFailure msg incite ->
            hang (hang "expected" (msg <> ":")) do
                go ignore incite
        AlternativeFailure [sub] -> go ignore sub
        AlternativeFailure subs ->
            hang "all alternatives failed:" do
                vcat' (go ignore <$> subs)
    where
    go ig (f :@: x) =
        if x `elem` ig
            then formatFailure ig f
            else hang ("at" <+> (pPrint x <> ":")) do
                formatFailure (x : ig) f



-- | Input type for Parser
data ParseStream
    = ParseStream
    { input :: !(Seq (ATag Token))
    , filePath :: !FilePath
    }
    deriving Show

-- | Create a @ParseFail@ from a @ParseStream@ and an offset into it;
--   Either an @UnexpectedFailure currentToken@ or @EofFailure@
formatInput :: ParseStream -> Int -> ATag ParseFail
formatInput ps i = Tag (psAttr ps i) case psToken ps i of
    Just t -> UnexpectedFailure t
    _ -> EofFailure



-- | Parsing monad
newtype Parser a
    = Parser (ParseStream -> Int -> Either ParseError (a, Int))
    deriving Functor

-- | Unwrap a @Parser a@ to a function
runParser :: Parser a -> ParseStream -> Int -> Either ParseError (a, Int)
runParser (Parser m) = m

instance Applicative Parser where
    pure a = Parser \_ i -> Right (a, i)
    Parser f <*> Parser a = Parser \s i -> do
        (f', i') <- f s i
        (a', i'') <- a s i'
        pure (f' a', i'')

instance Monad Parser where
    Parser a >>= f = Parser \s i -> do
        (a', i') <- a s i
        runParser (f a') s i'

instance Alternative Parser where
    empty = Parser \s i -> Left $ ParseError Recoverable $ formatInput s i
    Parser a <|> Parser b = Parser \s i ->
        case a s i of
            Left (ParseError Recoverable fa) -> case b s i of
                Left (ParseError r fb)
                    | Recoverable <- r -> Left $ ParseError r (fa <> fb)
                    | otherwise -> Left $ ParseError r fb
                x -> x
            x -> x

instance MonadFail Parser where
    fail msg = Parser \s i -> Left $ ParseError Recoverable $
        SingleFailure (text msg) :@: psAttr s i

instance MonadError ParseError Parser where
    throwError e = Parser \_ _ -> Left e
    catchError (Parser a) f = Parser \s i ->
        case a s i of
            Left e -> runParser (f e) s i
            x -> x

instance MonadReader ParseStream Parser where
    ask = Parser (curry Right)
    local f (Parser a) = Parser \s _ ->
        let s' = f s in a s' 0

instance MonadState Int Parser where
    get = Parser \_ i -> Right (i, i)
    put i = Parser \_ _ -> Right ((), i)

-- | Get the @Attr@ for a particular offset in a @ParseStream@,
--   or the end of the input, if the offset is out of bounds
psAttr :: ParseStream -> Int -> Attr
psAttr ps i = case Seq.lookup i ps.input of
    Just (_ :@: x) -> x
    _ -> case Seq.lookup (psLength ps - 1) ps.input of
        Just (_ :@: x) -> x
        _ -> Attr ps.filePath Nil

-- | Get the @Token@ for a particular offset in a @ParseStream@
psToken :: ParseStream -> Int -> Maybe Token
psToken ps i = untag <$> Seq.lookup i ps.input

-- | Get the length of a @ParseStream@
psLength :: ParseStream -> Int
psLength = Seq.length . (.input)



-- | Get the @ParseStream@ associated with the current @Parser@ action
getStream :: Parser ParseStream
getStream = Parser (curry Right)

-- | Get the @Seq (ATag Token)@ associated with the current @Parser@ action
getTokenSeq :: Parser (Seq (ATag Token))
getTokenSeq = (.input) <$> getStream

-- | Get the @FilePath@ associated with the current @Parser@ action
getFilePath :: Parser FilePath
getFilePath = (.filePath) <$> getStream

-- | Get the offset associated with the current @Parser@ action
modOffset :: (Int -> Int) -> Parser Int
modOffset f = Parser \_ i -> let i' = f i in Right (i, i')




-- | Run a @Parser@, and if it fails, make the @ParseError@ unrecoverable
noFail :: Parser a -> Parser a
noFail m = m `catchError` \(ParseError _ f) -> Parser \_ _ ->
    Left $ ParseError Unrecoverable f

-- | Ensure that the given @Parser@ action consumes the remaining @ParseStream@
consumesAll :: Parser a -> Parser a
consumesAll m = do
    a <- m
    s <- getStream
    i <- getOffset
    if i >= psLength s
        then pure a
        else let Tag x failure = formatInput s i
        in throwError $ ParseError Recoverable $ failure :@: x


-- | Trigger an unrecoverable @ParseError@
--   with a message at the current location
parseError :: Doc -> Parser a
parseError msg = attr >>= (`parseErrorAt` msg)

-- | Trigger an unrecoverable @ParseError@
--   with a message at the given @Attr@
parseErrorAt :: Attr -> Doc -> Parser a
parseErrorAt x msg =
    throwError $ ParseError Unrecoverable $ SingleFailure msg :@: x


-- | Trigger an unrecoverable @ParseError@
--   with a message at the current location,
--   if the given condition is false
assert :: Bool -> Doc -> Parser ()
assert p expStr = unless p (parseError expStr)

-- | Trigger an unrecoverable @ParseError@
--   with a message at the given @Attr@,
--   if the condition is false
assertAt :: Attr -> Bool -> Doc -> Parser ()
assertAt x p expStr = unless p (parseErrorAt x expStr)


-- | Get the current offset in the current @Parser@ action
getOffset :: Parser Int
getOffset = modOffset id

-- | Set the current offset in the current @Parser@ action
setOffset :: Int -> Parser ()
setOffset i = void $ modOffset (const i)


-- | Advance the offset in the current @Parser@ action
advance :: Parser ()
advance = Parser \_ i -> Right ((), i + 1)

-- | Get the currently selected @Token@ in the @Parser@ action
peek :: Parser Token
peek = Parser \s i ->
    case psToken s i of
        Just a -> Right (a, i)
        _ -> Left $ ParseError Recoverable $ EofFailure :@: psAttr s i

-- | Get the currently selected @Token@ in the @Parser@ action,
--   returning @Nothing@ if the stream is empty
tryPeek :: Parser (Maybe Token)
tryPeek = Parser \s i ->
    case psToken s i of
        Just a -> Right (Just a, i)
        _ -> Right (Nothing, i)

-- | Get a location @Attr@ for the selected position in the @Parser@ action
attr :: Parser Attr
attr = Parser \s i -> Right (psAttr s i, i)

-- | Get the @Attr@ of the last @Token@ consumed,
--   or the current one if none have been consumed
lastAttr :: Parser Attr
lastAttr = Parser \s i -> Right
    if i == 0
        then (psAttr s i, i)
        else (psAttr s (i - 1), i)


-- | Wraps the output of a @Parser@ in an @ATag@ for the range consumed
tag :: Parser a -> Parser (ATag a)
tag p = do
    i1 <- getOffset
    a <- p
    i2 <- getOffset
    (a :@:) <$> buildAttr i1 i2

-- | Build an @Attr@ representing a segment of the @Parser@'s @ParseStream@
buildAttr :: Int -> Int -> Parser Attr
buildAttr i1 i2 = do
    s <- getStream
    pure (psAttr s i1 <> psAttr s (i2 - 1))

-- | Execute @tag@ and discard the result, keeping only the @Attr@ generated
attrOf :: Parser a -> Parser Attr
attrOf = fmap tagOf . tag

-- | Execute @tag@ and discard the result,
--   keeping only the first @Pos@ of the @Attr@ generated
posOf :: Parser a -> Parser Pos
posOf = fmap ((.start) . (.range)) . attrOf

-- | Get the offset of the currently selected @Token@ in the @Parser@ action,
--   then execute the given @Parser@, returning only the offset
offsetOf :: Parser a -> Parser Int
offsetOf = (getOffset <<)

-- | Get the currently selected @Token@ in the @Parser@ action,
--   and advance the stream offset
next :: Parser Token
next = peek << advance

-- | Get the currently selected @Token@'s @Attr@ in the @Parser@ action
peekAttr :: Parser (ATag Token)
peekAttr = peek <*@*> attr

-- | Get the currently selected @Token@'s @Attr@ in the @Parser@ action,
--   and wrap it around the current @Token@ using @Tag@,
--   then advance the stream offset
nextAttr :: Parser (ATag Token)
nextAttr = tag next

-- | Advance the @Parser@ stream offset,
--   if the current @Token@ satisfies the predicate;
--   returning the matched @Token@
nextIf :: (Token -> Bool) -> Parser Token
nextIf p = do
    a <- peek
    if p a
        then a <$ advance
        else empty

-- | Advance the @Parser@ stream offset,
--   if the current @Token@ satisfies the predicate;
--   returning the matched @Token@ along with its @Attr@
nextIfAttr :: (ATag Token -> Bool) -> Parser (ATag Token)
nextIfAttr p = do
    t <- peekAttr
    if p t
        then t <$ advance
        else empty

-- | Advance the @Parser@ stream offset,
--   if the current @Token@ satisfies the predicate;
--   discarding the matched @Token@
nextIf_ :: (Token -> Bool) -> Parser ()
nextIf_ p = do
    a <- peek
    if p a
        then advance
        else empty

-- | Advance the @Parser@ stream offset,
--   as long as the current @Token@ satisfies the predicate;
--   returning the matched @Token@s as a list
nextWhile :: (Token -> Bool) -> Parser [Token]
nextWhile p = some (nextIf p)

-- | Advance the @Parser@ stream offset,
--   as long as the current @Token@ satisfies the predicate;
--   discarding the matched @Token@s
nextWhile_ :: (Token -> Bool) -> Parser ()
nextWhile_ p = some_ (nextIf_ p)

-- | Advance the @Parser@ stream offset,
--   if the current @Token@ satisfies a mapping predicate;
--   returning the mapped value
nextMap :: (Token -> Maybe a) -> Parser a
nextMap p = do
    a <- peek
    case p a of
        Just a' -> a' <$ advance
        _ -> empty


-- | Wrap the @ParseError@s of a given @Parser@
--   in an explanation of the expectation that was had of it
expecting :: Doc -> Parser a -> Parser a
expecting msg px = px `catchError` \(ParseError r f) -> Parser \s i ->
    Left $ ParseError r $ ExpectationFailure msg f :@: psAttr s i

-- | Wrap the @ParseError@s of a given @Parser@
--   in an explanation of the expectations that were had of it
expectingMulti :: [Doc] -> Parser a -> Parser a
expectingMulti = expecting . lsep


-- | @nextIf_ (== e)@
expect' :: Token -> Parser ()
expect' e = nextIf_ (== e)

-- | @nextIf (`elem` es)@
expectAny' :: [Token] -> Parser Token
expectAny' es = nextIf (`elem` es)

-- | Consume an expected sequence of inputs
expectSeq' :: [Token] -> Parser ()
expectSeq' = traverse_ \e -> do
    a <- peek
    if a == e
        then advance
        else empty

-- | Consume one of any expected sequences of inputs
expectAnySeq' :: [[Token]] -> Parser [Token]
expectAnySeq' es = do
    asum (es <&> \e -> e <$ expectSeq' e)

-- | @expecting (pPrint e) (expect' e)@
expect :: Token -> Parser ()
expect e = expecting (pPrint e) (expect' e)

-- | @expectingMulti (pPrint <$> es) (expectAny' es)@
expectAny :: [Token] -> Parser Token
expectAny es = expectingMulti (pPrint <$> es) (expectAny' es)

-- | @expecting (pPrint e) (expectSeq' e)@
expectSeq :: [Token] -> Parser ()
expectSeq e = expecting (pPrint e) (expectSeq' e)

-- | @expectingMulti (pPrint <$> es) (expectAnySeq' es)@
expectAnySeq :: [[Token]] -> Parser [Token]
expectAnySeq es = expectingMulti (pPrint <$> es) (expectAnySeq' es)

-- | Expect a list of @Parser a@ separated by @Parser s@.
--   The list must contain at least one @Token@
listSome :: Parser s -> Parser a -> Parser [a]
listSome s p = liftA2 (:) p (many $ s >> p)

-- | Expect a list of @Parser a@ separated by @Parser s@.
--   The list may be empty
listMany :: Parser s -> Parser a -> Parser [a]
listMany s p = option [] (listSome s p)

-- | Use a predicate to grab a sequence of @Token@s from the @Parser@ stream.
--   Fails if this results in an empty sequence
grabDomain :: (ATag Token -> Bool) -> Parser (Seq (ATag Token))
grabDomain f = do
    toks <- Seq.fromList <$> some (nextIfAttr $ f &&& notEOF)
    end <- mapTag attrFlattenToEnd <$> TEof <@> lastAttr
    pure (toks Seq.:|> end)
    where notEOF = \case T' TEof -> False; _ -> True

-- | Grab a sequence of @Token@s from the @Parser@ stream,
--   delimited by their line/indentation relative to the last @Attr@.
--   Fails if this results in an empty sequence
grabWhitespaceDomain :: Parser (Seq (ATag Token))
grabWhitespaceDomain = lastAttr >>= grabWhitespaceDomainOf

-- | Grab a sequence of @Token@s from the @Parser@ stream,
--   delimited by their line/indentation relative to a given @Attr@.
--   Fails if this results in an empty sequence
grabWhitespaceDomainOf :: Attr -> Parser (Seq (ATag Token))
grabWhitespaceDomainOf = grabDomain . wsDominated

-- | A predicate-builder for @grabDomain@ that matches @ATag Token@s
--   on the same line or with a higher indentation level
--   than that of the given @Attr@
wsDominated :: Attr -> ATag Token -> Bool
wsDominated a1 (_ :@: a2) =
    let Pos _ l1 _ i1 = a1.range.start
        Pos _ l2 _ i2 = a2.range.start
    in l1 == l2 || i1 < i2

-- | Grammatically,
--   @WsList sep elem = wsBlock<wsBlock<elem>++(sep?) | elem (sep elem)*>@
wsList :: Parser sep -> Parser a -> Parser [a]
wsList ms ma = grabWhitespaceDomain >>= recurseParser (block <|> inline) where
    inline = listSome ms ma
    block = some (attr >>= grabWhitespaceDomainOf)
        >>= foldWithM' mempty \toks as ->
            (: as) <$> recurseParser
                do if null as
                    then ma
                    else ma << optional ms
                toks


-- | Perform syntactic analysis on a @ByteString@ using the given @Parser@,
--   with errors reported to be in the provided @FilePath@
parseByteStringWith :: Parser a -> FilePath -> ByteString -> Either Doc a
parseByteStringWith p filePath fileContent = do
    toks <- L.lexByteString filePath fileContent
    evalParser p filePath toks

-- | Perform syntactic analysis on a @String@ using the given @Parser@,
--   with errors reported to be in the provided @FilePath@
parseStringWith :: Parser a -> FilePath -> String -> Either Doc a
parseStringWith p name = parseByteStringWith p name . fromString


-- | Evaluate a @Parser@ on a given @Seq (ATag Token)@,
--   converting @ParseError@s to @Doc@s and discarding the final offset,
--   after ensuring that all input was consumed
evalParser :: Parser a -> FilePath -> Seq (ATag Token) -> Either Doc a
evalParser px filePath toks =
    case runParser
        (consumesAll (px << eof))
        (ParseStream toks filePath) 0 of
            Left e -> Left $ pPrint e
            Right (a, _) -> Right a

-- | Evaluate a @Parser@ on a sub-stream of @Tokens@.
--   The parser must consume all tokens (except eof), or fail
recurseParser :: Parser a -> Seq (ATag Token) -> Parser a
recurseParser px toks = do
    f <- getFilePath
    case runParser (consumesAll (px << eof)) (ParseStream toks f) 0 of
        Left e -> Parser \_ _ -> Left e
        Right (a, _) -> pure a


-- | Expect the @Eof@ token
eof :: Parser ()
eof = nextMap \case
    TEof -> Just ()
    _ -> Nothing

-- | Expect a @Version@
version :: Parser Version
version = expecting "a version" $ nextMap \case
    TVersion v -> Just v
    _ -> Nothing

-- | Expect a @Literal@
literal :: Parser Literal
literal = expecting "literal" $ nextMap \case
    TLiteral lit -> Just lit
    _ -> Nothing

-- | Expect a @Literal@ of @Char@
char :: Parser Char
char = expecting "character literal" $ nextMap \case
    TLiteral (LChar c) -> Just c
    _ -> Nothing

-- | Expect a @Literal@ of @Int@
int :: Parser Int
int = expecting "integer literal" $ nextMap \case
    TLiteral (LInt i) -> Just i
    _ -> Nothing

-- | Expect a @Literal@ of @Float@
float :: Parser Float
float = expecting "float literal" $ nextMap \case
    TLiteral (LFloat f) -> Just f
    _ -> Nothing

-- | Expect a @Literal@ of @String@
string :: Parser String
string = expecting "string literal" $ nextMap \case
    TLiteral (LString s) -> Just s
    _ -> Nothing

-- | Expect any symbol @Token@
anySym :: Parser String
anySym = expecting "a symbol" $ nextMap \case
    TSymbol s -> Just s
    _ -> Nothing

-- | Expect a symbol @Token@ that is not reserved
unreserved :: Parser String
unreserved = expecting "an unreserved symbol" $ nextMap \case
    tk@(TSymbol s) | not (isReserved tk) -> Just s
    _ -> Nothing

-- | Expect a symbol @Token@ with a specific value
sym :: String -> Parser ()
sym s = expecting (text s) $ nextMap \case
    TSymbol s' | s == s' -> Just ()
    _ -> Nothing

-- | Expect a symbol @Token@ with a specific value from a given set
symOf :: [String] -> Parser String
symOf ss = expectingMulti (text <$> ss) $ nextMap \case
    TSymbol s | s `elem` ss -> Just s
    _ -> Nothing

-- | Try and parse a symbol @Token@, returning a boolean indicating success
trySym :: String -> Parser Bool
trySym s = peek >>= \case
    TSymbol s' | s == s' -> True <$ advance
    _ -> pure False


-- | Expect a given @Parser@ to be surrounded with parentheses
parens :: Parser a -> Parser a
parens px = do
    a <- attrOf (sym "(")
    noFail do
        px << expecting (") to close ( at " <> pPrint a) do sym ")"

-- | Expect a given @Parser@ to be surrounded with braces
braces :: Parser a -> Parser a
braces px = do
    a <- attrOf (sym "{")
    noFail do
        px << expecting ("} to close { at " <> pPrint a) do sym "}"

-- | Expect a given @Parser@ to be surrounded with brackets
brackets :: Parser a -> Parser a
brackets px = do
    a <- attrOf (sym "[")
    noFail do
        px << expecting ("] to close [ at " <> pPrint a) do sym "]"

-- | Expect a given @Parser@'s first input @Token@ to be directly adjacent,
--   in terms of line and column position, to the previous @Token@ consumed.
--   Note that if this is used on the first token in an input it has no effect
connected :: Parser a -> Parser a
connected px = do
    f <- getFilePath
    Parser \s i ->
        let a = psAttr s (i - 1)
            a' = psAttr s i
        in if
        | i == 0 -> runParser px s i
        | attrConnected a a' -> runParser px s i
        | otherwise -> runParser
            (expecting (text "a connected token") px)
            (ParseStream (Seq.fromList [Tag a' TEof]) f)
            0
