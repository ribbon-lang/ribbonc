module Language.Ribbon.Util where

import Data.Foldable

import Control.Applicative
import Control.Monad.Except

import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Either qualified as Either

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

import Text.Pretty
import Data.Functor




-- | Marks something not yet implemented
todo :: a
todo = error "TODO"

-- | Drop elements from the end of a list
dropTail :: Int -> [a] -> [a]
dropTail n = reverse . drop n . reverse

-- | ByteString -> String
bytesToString :: ByteString -> String
bytesToString
    = Text.unpack
    . Text.decodeUtf8
    . ByteString.toStrict

-- | Branch on a boolean, selecting a or b for true or false, respectively
select :: Bool -> a -> a -> a
select True a _ = a
select False _ b = b

-- | Branch on a boolean, selecting a or b for true or false, respectively
selecting :: a -> a -> Bool -> a
selecting a b p = select p a b

-- | equivalent of Applicative.some (one or more) with unit return value
some_ :: Alternative f => f a -> f ()
some_ a = some_v where
    many_v = option () some_v
    some_v = a *> many_v

-- | equivalent of Applicative.many (zero or more) with unit return value
many_ :: Alternative f => f a -> f ()
many_ a = many_v where
    many_v = option () some_v
    some_v = a *> many_v

-- | equivalent of Applicative.many (zero or more) with indexing
manyN :: (Num n, Alternative f) => (n -> f a) -> f [a]
manyN f = many_v 0 where
    many_v n = option [] (liftA2 (:) (f n) (many_v (n + 1)))

-- | equivalent of Applicative.some (one or more) with indexing
someN :: (Num n, Alternative f) => (n -> f a) -> f [a]
someN f = liftA2 (:) (f 0) (many_v 1) where
    many_v n = option [] (liftA2 (:) (f n) (many_v (n + 1)))

-- | equivalent of Applicative.some (one or more) with base value
someWith :: Alternative f => [a] -> f a -> f [a]
someWith base a = some_v where
    many_v = option base some_v
    some_v = liftA2 (:) a many_v

-- | equivalent of Applicative.many (zero or more) with base value
manyWith :: Alternative f => [a] -> f a -> f [a]
manyWith base a = many_v where
    many_v = option base some_v
    some_v = liftA2 (:) a many_v

-- | Expect a list of @m a@ separated by @m s@.
--   The list must contain at least one result
listSome :: (Monad m, Alternative m) => m s -> m a -> m [a]
listSome s p = liftA2 (:) p (many $ s >> p)

-- | Expect a list of @m a@ separated by @m s@.
--   The list may be empty
listMany :: (Monad m, Alternative m) => m s -> m a -> m [a]
listMany s p = option [] (listSome s p)



-- | Expect a sequence of @m a@ separated by @m s@, and discard the results.
--   The sequence must contain at least one result
listSome_ :: (Monad m, Alternative m) => m s -> m a -> m ()
listSome_ s p = p *> many_ (s >> p)

-- | Expect a sequence of @m a@ separated by @m s@, and discard the results.
--   The sequence may be empty
listMany_ :: (Monad m, Alternative m) => m s -> m a -> m ()
listMany_ s p = option () (listSome_ s p)

-- | @optional@, with a default
option :: Alternative f => a -> f a -> f a
option a fa = fa <|> pure a

-- | The reverse of @(.)@
compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g a = g (f a)

-- | The reverse of @(.:)@
compose2 :: (a -> b -> c) -> (c -> d) -> (a -> b -> d)
compose2 f g a b = g (f a b)

-- | Compose a list of functions
composeAll :: [a -> a] -> a -> a
composeAll = foldr compose id

-- | Compose a binary function
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 8 .:


-- | Split a list into multiple sub-lists
--   at each element that satisfies a predicate
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p = go where
    go [] = []
    go xs = case break p xs of
        (a, _ : b) -> a : go b
        _ -> [xs]

-- | Split a list into multiple sub-lists
--   at each element that is equal to a given value
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = splitWith . (==)

-- | Lookup a value in a list with a mapping predicate
lookupWith :: (a -> Maybe b) -> [a] -> Maybe b
lookupWith f = foldWith' Nothing \a -> (f a <|>)

-- | Compositional @&&@
(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&&) f g a = f a && g a
infix 7 &&&

-- | Compositional @||@
(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(|||) f g a = f a || g a
infix 8 |||

-- | Compositional @not@
not'd :: (a -> Bool) -> (a -> Bool)
not'd f a = not (f a)

-- | The reverse of (>>)
(<<) :: Monad m => m b -> m a -> m b
(<<) ma mb = do a <- ma; a <$ mb

-- | `show s` without the quotes
escapeString :: String -> String
escapeString = init . tail . show

-- | `show c` without the quotes
escapeChar :: Char -> String
escapeChar = escapeString . pure

-- | Maybe -> Monad with monadic failure case
liftMaybe :: Monad m => m a -> Maybe a -> m a
liftMaybe failed = Maybe.maybe failed pure

-- | Maybe -> Monad with MonadFail string case
maybeFail :: MonadFail m => String -> Maybe a -> m a
maybeFail msg = liftMaybe (fail msg)

-- | Maybe -> Monad with MonadError error case
maybeError :: MonadError e m => e -> Maybe a -> m a
maybeError e = liftMaybe (throwError e)

-- | Maybe -> Monad with MonadError if Just err
maybeIsError :: MonadError e m => Maybe e -> m ()
maybeIsError = \case Just e -> throwError e; _ -> pure ()

-- | Maybe -> Monad with mapped Just
whenJust :: Monad m => (a -> m ()) -> Maybe a -> m ()
whenJust = Maybe.maybe (pure ())

-- | Maybe -> Monad with mapped Nothing
whenNothing :: Monad m => m () -> Maybe a -> m ()
whenNothing m = Maybe.maybe m (const (pure ()))


-- | Maybe -> Alternative with empty case
maybeEmpty :: Alternative m => Maybe a -> m a
maybeEmpty = Maybe.maybe empty pure

-- | Maybe -> Monoid with mempty case
maybeMEmpty :: Monoid a => Maybe a -> a
maybeMEmpty = Maybe.fromMaybe mempty

-- | ExceptT based error mapping
mapError :: (Functor m) => (e -> e') -> ExceptT e m a -> ExceptT e' m a
mapError f m = ExceptT do
    runExceptT m <&> \case
        Left e -> Left (f e)
        Right a -> Right a

-- | Either -> Monad with mapping
liftEitherHandler :: Applicative m => (a -> m b) -> Either a b -> m b
liftEitherHandler f = Either.either f pure

-- | Either -> MonadError with mapping
liftEitherMap :: MonadError e m => (a -> e) -> Either a b -> m b
liftEitherMap f = Either.either (throwError . f) pure


-- | Either -> Left with exception on Right
forceLeft :: Either a b -> a
forceLeft = Either.fromLeft (error "expected left")

-- | Either -> Right with exception on Left
forceRight :: Either a b -> b
forceRight = Either.fromRight (error "expected right")

-- | Lift a 4 argument function into an Applicative
liftA4 :: Applicative m =>
    (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
liftA4 f ma mb mc md = liftA3 f ma mb mc <*> md

-- | Fail with an expectation message if the condition is false
guardFail :: MonadFail m => Bool -> String -> m ()
guardFail p expStr = if p then pure () else fail expStr

-- | `foldr` with the function taken last
foldWith :: Foldable t => b -> t a -> (a -> b -> b) -> b
foldWith b as f = foldr f b as

-- | `foldr` with the function taken second
foldWith' :: Foldable t => b -> (a -> b -> b) -> t a -> b
foldWith' = flip foldr

-- | `foldrM` with the function taken last
foldWithM :: (Foldable t, Monad m) => b -> t a -> (a -> b -> m b) -> m b
foldWithM b as f = foldrM f b as

-- | `foldrM` with the function taken second
foldWithM' :: (Foldable t, Monad m) => b -> (a -> b -> m b) -> t a -> m b
foldWithM' = flip foldrM

-- | Continue evaluating `m a` while `m Bool` is `True`
whileM :: Monad m => m Bool -> m a -> m [a]
whileM p a = p >>= selecting
    do liftA2 (:) a (whileM p a)
    do pure []

-- | Continue evaluating `m a` while `m Bool` is `False`
untilM :: Monad m => m Bool -> m a -> m [a]
untilM p a = p >>= selecting
    do pure []
    do liftA2 (:) a (untilM p a)

-- | Continue evaluating `m a` while `m Bool` is `True`,
--   discarding results
whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ p a = p >>= selecting
    do a *> whileM_ p a
    do pure ()

-- | Continue evaluating `m a` while `m Bool` is `False`,
--   discarding results
untilM_ :: Monad m => m Bool -> m a -> m ()
untilM_ p a = p >>= selecting
    do pure ()
    do a *> untilM_ p a

-- | Force a string literal to be a @String@ under @OverloadedStrings@
pattern String :: String -> String
pattern String s = s

-- | Force a string literal to be a @Doc@ under @OverloadedStrings@
pattern Doc :: Doc -> Doc
pattern Doc s = s

-- | Utility class for the (</>) slash-connection concatenation operator
class SlashConnect a where
    -- | Concatenate with a slash between the elements
    (</>) :: a -> a -> a
    infixr 6 </>


instance SlashConnect String where
    (</>) a b
        | "/" `List.isSuffixOf` a || "/" `List.isPrefixOf` b = a <> b
        | otherwise = a <> "/" <> b

instance SlashConnect Doc where
    (</>) a b = a <> "/" <> b


superscript :: Char -> Char
superscript c = Maybe.fromMaybe c do
    List.lookup c $ zip
        "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "⁰¹²³⁴⁵⁶⁷⁸⁹ᵃᵇᶜᵈᵉᶠᵍʰᶦʲᵏˡᵐⁿᵒᵖᵠʳˢᵗᵘᵛʷˣʸᶻᴬᴮᶜᴰᴱᶠᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᵠᴿˢᵀᵁⱽᵂˣʸᶻ"
