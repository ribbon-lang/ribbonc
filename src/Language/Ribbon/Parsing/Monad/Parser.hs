module Language.Ribbon.Parsing.Monad.Parser where

import Data.Functor
import Data.Foldable

import Data.String (fromString)
import Data.ByteString.Lazy (ByteString)

import Data.Sequence qualified as Seq


import Data.Word (Word32)

import Data.Nil
import Data.Tag
import Data.Pos
import Data.Range
import Data.Attr

import Text.Pretty as Pretty

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Language.Ribbon.Util

import Language.Ribbon.Lexical

import Language.Ribbon.Parsing.Error
import Language.Ribbon.Parsing.Lexer qualified as L
import Data.Text.Lazy (Text)
import Data.Function ((&))






-- | Create a @ParseFail@ from a @TokenSeq@ and an offset into it;
--   Either an @UnexpectedFailure currentToken@ or @EofFailure@
formatInput :: FilePath -> TokenSeq -> ATag (ParseFail Token)
formatInput fp ts = Tag (psAttr fp ts) case ts of
    (t :@: _) Seq.:<| _ -> UnexpectedFailure t
    _ -> EofFailure



-- | Parsing monad
newtype Parser a
    = Parser
    ( FilePath -> TokenSeq -> Either (ParseError Token) (a, TokenSeq) )
    deriving Functor

-- | Unwrap a @Parser a@ to a function
runParser ::
    Parser a -> FilePath -> TokenSeq ->
        Either (ParseError Token) (a, TokenSeq)
runParser (Parser m) = m

instance Applicative Parser where
    pure a = Parser \_ ts -> Right (a, ts)
    Parser f <*> Parser a = Parser \fp ts -> do
        (f', ts') <- f fp ts
        (a', ts'') <- a fp ts'
        pure (f' a', ts'')

instance Monad Parser where
    Parser a >>= f = Parser \fp ts -> do
        (a', ts') <- a fp ts
        runParser (f a') fp ts'

instance Alternative Parser where
    empty = Parser \fp ts -> Left $ ParseError Recoverable $ formatInput fp ts
    Parser a <|> Parser b = Parser \fp ts ->
        case a fp ts of
            Left (ParseError Recoverable fa) -> case b fp ts of
                Left (ParseError Recoverable fb) ->
                    Left $ ParseError Recoverable (fa <> fb)
                x -> x
            x -> x

instance MonadFail Parser where
    fail msg = Parser \fp ts -> Left $ ParseError Recoverable $
        SingleFailure (text msg) :@: psAttr fp ts

instance MonadError (ParseError Token) Parser where
    throwError e = Parser \_ _ -> Left e
    catchError (Parser a) f = Parser \fp ts ->
        case a fp ts of
            Left e -> runParser (f e) fp ts
            x -> x

instance MonadReader FilePath Parser where
    ask = Parser (curry Right)
    local f (Parser a) = Parser \fp ts ->
        let fp' = f fp in a fp' ts

instance MonadState TokenSeq Parser where
    get = Parser \_ ts -> Right (ts, ts)
    put ts = Parser \_ _ -> Right ((), ts)

-- | Get the @Attr@ for a particular offset in a @TokenSeq@,
--   or the end of the input, if the offset is out of bounds
psAttr :: FilePath -> TokenSeq -> Attr
psAttr fp ts = case ts of
    (_ :@: x) Seq.:<| _ -> x
    _ -> Attr fp Nil




-- | Get the @TokenSeq@ associated with the current @Parser@ action
getTokenSeq :: Parser TokenSeq
getTokenSeq = get

-- | Get the @FilePath@ associated with the current @Parser@ action
getFilePath :: Parser FilePath
getFilePath = ask





-- | Run a @Parser@, and if it fails, make the @ParseError@ unrecoverable
noFail :: Parser a -> Parser a
noFail m = m `catchError` \(ParseError _ f) -> Parser \_ _ ->
    Left $ ParseError Unrecoverable f

-- | Run a @Parser@, and if it fails, make the @ParseError@ unrecoverable
--   when the given condition is @True@
noFailIf :: Bool -> Parser a -> Parser a
noFailIf p m = if p then noFail m else m

-- | Ensure that the given @Parser@ action consumes the remaining @TokenSeq@
consumesAll :: Parser a -> Parser a
consumesAll m = Parser \fp ts -> do
    (a, ts') <- runParser m fp ts
    if null ts'
        then Right (a, ts')
        else
            let Tag x failure = formatInput fp ts
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




-- | Advance the offset in the current @Parser@ action
advance :: Parser ()
advance = Parser \_ -> (Right . ((), )) . \case
    _ Seq.:<| ts' -> ts'
    ts -> ts

-- | Get the currently selected @Token@ in the @Parser@ action
peek :: Parser Token
peek = Parser \fp ts ->
    case ts of
        T' a Seq.:<| _ -> Right (a, ts)
        _ -> Left $ ParseError Recoverable $ EofFailure :@: psAttr fp ts

-- | Get the currently selected @Token@ in the @Parser@ action,
--   returning @Nothing@ if the stream is empty
tryPeek :: Parser (Maybe Token)
tryPeek = Parser \_ ts ->
    Right $ ( , ts) case ts of
        T' a Seq.:<| _ -> Just a
        _ -> Nothing

-- | Get a location @Attr@ for the selected position in the @Parser@ action
attr :: Parser Attr
attr = Parser \fp ts -> Right (psAttr fp ts, ts)



-- | Wraps the output of a @Parser@ in an @ATag@ for the range consumed
tag :: Parser a -> Parser (ATag a)
tag p = Parser \fp ts -> do
    (a, ts') <- runParser p fp ts
    let at = attrFold (fileAttr fp) $ Seq.take (Seq.length ts - Seq.length ts') ts
    Right (a :@: at, ts')


-- | Execute @tag@ and discard the result, keeping only the @Attr@ generated
attrOf :: Parser a -> Parser Attr
attrOf = fmap tagOf . tag

-- | Execute @tag@ and discard the result,
--   keeping only the first @Pos@ of the @Attr@ generated
posOf :: Parser a -> Parser Pos
posOf = fmap ((.start) . (.range)) . attrOf

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
--   returning the matched @Token@s as a list
nextWhileAttr :: (ATag Token -> Bool) -> Parser [ATag Token]
nextWhileAttr p = some (nextIfAttr p)

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
expecting msg px = px `catchError` \(ParseError r f) -> Parser \fp ts ->
    Left $ ParseError r $ ExpectationFailure [msg] f :@: psAttr fp ts

-- | Wrap the @ParseError@s of a given @Parser@
--   in an explanation of the expectations that were had of it
expectingMulti :: [Doc] -> Parser a -> Parser a
expectingMulti = expecting . lsep


-- | Wrap the @ParseError@s of a given @Parser@
--   in an explanation of the expectation that was had of it
expectingAt :: Attr -> Doc -> Parser a -> Parser a
expectingAt at msg px = px `catchError` \(ParseError r f) -> Parser \_ _ ->
    Left $ ParseError r $ ExpectationFailure [msg] f :@: at

-- | Wrap the @ParseError@s of a given @Parser@
--   in an explanation of the expectations that were had of it
expectingMultiAt :: Attr -> [Doc] -> Parser a -> Parser a
expectingMultiAt at = expectingAt at . lsep


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

takeSeq :: Parser TokenSeq
takeSeq = Parser \_ ts -> Right (ts, Nil)

grabDomain :: (ATag Token -> Bool) -> Parser TokenSeq
grabDomain p = Parser \fp ts ->
    case ts of
        toks | not (Seq.null toks) ->
            let (a, b) = consume False toks
            in Right (reduceTokenSeq a, reduceTokenSeq b)
        _ -> Left $ ParseError Recoverable $ EofFailure :@: psAttr fp ts
    where
    consume recursed = \case
        (t Seq.:<| ts) | p t ->
            case untag t of
                TTree BkWhitespace ts' ->
                    let (as, bs) = consume True ts'
                    in buildSplit t ts as bs
                    if recursed
                        then recurse
                        else (Seq.singleton t, Nil)
                _ -> recurse
            where
            recurse =
                let (as, bs) = consume recursed ts
                in (t Seq.<| as, bs)

        ts -> (Nil, ts)

    buildSplit t rhs as bs nb
        | Seq.null bs = nb
        | Seq.null as = (Nil, t Seq.<| rhs)
        | otherwise =
            ( Seq.singleton (TTree BkWhitespace as :@: t.tag)
            , (TTree BkWhitespace bs :@: t.tag) Seq.<| rhs
            )

-- | Get the rest of the input delimited by indentation
grabWhitespaceDomain :: Parser TokenSeq
grabWhitespaceDomain = grabDomain (const True)

-- | Consume a sequence of lines from the input
grabLines :: Parser [TokenSeq]
grabLines = some $ nextMap \case
    TTree BkWhitespace lns -> Just lns
    _ -> Nothing

-- | @wsBlock<elem>++(sep?) | elem (sep elem)*@
wsListBody :: Parser sep -> Parser a -> Parser [a]
wsListBody ms ma = asum [block, inline] where
    inline = listSome ms ma
    block = do
        lns <- grabLines
        noFail do
            lns & foldWithM' mempty \toks as ->
                (: as) <$> recurseParser
                    do if null as
                        then ma
                        else ma << optional ms
                    toks

-- | Perform syntactic analysis on @Text@ using the given @Parser@,
--   with errors reported to be in the provided @FilePath@
parseTextWith :: Parser a -> FilePath -> Text -> Either Doc a
parseTextWith p filePath fileContent = do
    toks <- L.lexText filePath fileContent
    evalParser p filePath toks

-- | Perform syntactic analysis on a @ByteString@ using the given @Parser@,
--   with errors reported to be in the provided @FilePath@
parseByteStringWith :: Parser a -> FilePath -> ByteString -> Either Doc a
parseByteStringWith p filePath fileContent = do
    toks <- L.lexByteString filePath fileContent
    evalParser p filePath toks

-- | Perform syntactic analysis on a @String@ using the given @Parser@,
--   with errors reported to be in the provided @FilePath@
parseStringWith :: Parser a -> FilePath -> String -> Either Doc a
parseStringWith p filePath = parseByteStringWith p filePath . fromString


parseFileWith :: Pretty a => Parser a -> FilePath -> IO (Either Doc a)
parseFileWith px filePath = L.lexFile filePath <&> (>>= evalParser px filePath)


-- | Evaluate a @Parser@ on a given @TokenSeq@,
--   converting @ParseError@s to @Doc@s and discarding the final offset,
--   after ensuring that all input was consumed
evalParser :: Parser a -> FilePath -> TokenSeq -> Either Doc a
evalParser px filePath toks =
    case runParser
        (consumesAll px)
        filePath toks of
            Left e -> Left $ pPrint e
            Right (a, _) -> Right a

-- | Evaluate a @Parser@ on a sub-stream of @Tokens@.
--   The parser must consume all tokens (except eof), or fail
recurseParser :: Parser a -> TokenSeq -> Parser a
recurseParser px toks = do
    f <- getFilePath
    case runParser (consumesAll px) f toks of
        Left e -> Parser \_ _ -> Left e
        Right (a, _) -> pure a


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
int :: Parser Word32
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
sym s = expecting (Pretty.backticks $ text s) $ nextMap \case
    TSymbol s' | s == s' -> Just ()
    _ -> Nothing

-- | Expect a symbol @Token@ with a specific value from a given set
symOf :: [String] -> Parser String
symOf ss = expectingMulti (Pretty.backticks . text <$> ss) $ nextMap \case
    TSymbol s | s `elem` ss -> Just s
    _ -> Nothing

-- | Try and parse a symbol @Token@, returning a boolean indicating success
trySym :: String -> Parser Bool
trySym s = peek >>= \case
    TSymbol s' | s == s' -> True <$ advance
    _ -> pure False

-- | Expect a given @Parser@ to be surrounded with backticks
backticks :: Parser a -> Parser a
backticks px = do
    at <- attrOf (sym "`")
    noFail do
        px << expectingAt at "to close `" (sym "`")

-- | Expect a given @Parser@ to be surrounded with parentheses
parens :: Parser a -> Parser a
parens px = do
    at <- attrOf (sym "(")
    noFail do
        px << expectingAt at "to close (" (sym ")")

-- | Expect a given @Parser@ to be surrounded with braces
braces :: Parser a -> Parser a
braces px = do
    at <- attrOf (sym "{")
    noFail do
        px << expectingAt at "to close {" (sym "}")

-- | Expect a given @Parser@ to be surrounded with brackets
brackets :: Parser a -> Parser a
brackets px = do
    at <- attrOf (sym "[")
    noFail do
        px << expectingAt at "to close [" (sym "]")

-- | Expect a given @Parser@'s @Attr@ to be directly adjacent,
--   in terms of line and column position, to the given @Attr@
connected :: Attr -> Parser a -> Parser a
connected at px = Parser \fp ts -> do
    case ts of
        (_ :@: at') Seq.:<| _ | attrConnected at at' -> runParser px fp ts
        _ -> runParser
            (expecting "a token directly connected to the previous" px)
            fp Nil
