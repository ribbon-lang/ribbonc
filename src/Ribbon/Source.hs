module Ribbon.Source where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Ribbon.Display (Display(..))
import Control.Exception
import qualified Data.Maybe as Maybe
-- import Ribbon.Display qualified as Display

-- | Wrapper for syntax objects, with source attribution
data Syn a
    -- | Attaches source attribution to a syntax object
    = a :@: Attr
    deriving (Show, Functor, Foldable, Traversable)
infixl 9 :@:


-- | Get the syntax object inside Syn
synData :: Syn a -> a
synData (a :@: _) = a

-- | Get the source attribution of a Syn object
synAttr :: Syn a -> Attr
synAttr (_ :@: x) = x

-- | Pattern alias for Syn without source attribution
pattern Sn :: a -> Syn a
pattern Sn a <- a :@: _
{-# COMPLETE Sn #-}

-- | Source attribution
data Attr
    -- | Source attribution for a range of characters in a file
    = Attr
    -- | Start of the codepoint-indexed range of an Attr
    { attrStart :: Pos
    -- | End of the codepoint-indexed range of an Attr
    , attrEnd :: Pos
    -- | File containing the range of an Attr
    , attrFile :: File
    }
    deriving (Eq, Ord)

-- | Codepoint-indexed position in a source file
type Pos = Int

-- | Line and column in a source file
data Lc
    = Lc Int Int
    | EOF
    deriving (Eq)

instance Ord Lc where
    compare EOF EOF = EQ
    compare EOF _ = GT
    compare _ EOF = LT
    compare (Lc l1 c1) (Lc l2 c2) = compare (l1, c1) (l2, c2)


-- | Source file
data File
    -- | Source file with name, text, and line and column database
    = File
    -- | Name of a File
    { fileName :: String
    -- | Content of a File
    , fileText :: String
    -- | Line and column database of a File
    , lineAndColumnDb :: Seq Lc
    }


instance Eq a => Eq (Syn a) where
    a == b = synData a == synData b

instance Ord a => Ord (Syn a) where
    compare a b = compare (synData a) (synData b)

instance Display a => Display (Syn a) where
    display = display . synData


instance Semigroup Attr where
    a <> b =
        assert (attrFile a == attrFile b) $
        Attr
            (min (attrStart a) (attrStart b))
            (max (attrEnd a) (attrEnd b))
            (attrFile a)

instance Show Attr where
    show attr =
        fileName (attrFile attr)
        <> case attrRangeLc attr of
            (Lc ls cs, Lc le ce)
                -> ":" <> show ls <> ":" <> show cs
                <> "-" <> show le <> ":" <> show ce
            (EOF, EOF) -> ":EOF-EOF"
            (Lc ls cs, EOF) -> ":" <> show ls <> ":" <> show cs <> "-EOF"
            (EOF, Lc le ce) -> ":EOF-" <> show le <> ":" <> show ce


instance Eq File where
    a == b = fileName a == fileName b

instance Ord File where
    compare a b = compare (fileName a) (fileName b)

instance Show File where
    show = fileName

-- | Create an Attr with a unit range
unitAttr :: Pos -> File -> Attr
unitAttr pos = Attr pos (pos + 1)

-- | Get a line and column for the start of a source attribution.
attrStartLc :: Attr -> Lc
attrStartLc attr = getLc (attrFile attr) (attrStart attr)

-- | Get a line and column for the end of a source attribution.
attrEndLc :: Attr -> Lc
attrEndLc attr = getLc (attrFile attr) (attrEnd attr)

-- | Get a pair of lines and columns for the range of a source attribution.
attrRangeLc :: Attr -> (Lc, Lc)
attrRangeLc attr = (attrStartLc attr, attrEndLc attr)


-- | Create a new File given its name and text.
--   Calls computeLineAndColumnDb
newFile :: String -> String -> File
newFile name text = File name text (computeLineAndColumnDb text)

-- | Read the contents of a file from disk, given its name,
--   and call newFile with the name and contents.
--   Throws an exception if the file does not exist
loadFile :: String -> IO File
loadFile name = newFile name <$> readFile name

-- | Compute the lineAndColumnDb for a given text
computeLineAndColumnDb :: String -> Seq Lc
computeLineAndColumnDb text = go 1 1 text Seq.empty where
    go _ _ [] acc = acc
    go line col (c:cs) acc
        | c == '\n' = go (line + 1) 1 cs (acc Seq.:|> Lc line col)
        | otherwise = go line (col + 1) cs (acc Seq.:|> Lc line col)

-- | Get a line and column for a given file and position.
getLc :: File -> Pos -> Lc
getLc file pos = Maybe.fromMaybe EOF (lineAndColumnDb file Seq.!? pos)




-- | Convert a Syn to a pair
synSplit :: Syn a -> (a, Attr)
synSplit (a :@: attr) = (a, attr)

synExtWithA :: Attr -> (Syn a -> b) -> (Syn a -> Syn b)
synExtWithA t f x = f x :@: (t <> synAttr x)

synExtWithB :: Attr -> (Syn a -> b) -> (Syn a -> Syn b)
synExtWithB t f x =  f x :@: (synAttr x <> t)

synExt :: (Syn a -> b) -> (Syn a -> Syn b)
synExt f x = f x :@: synAttr x

synApp :: (Syn a -> Syn b -> c) -> Syn a -> Syn b -> Syn c
synApp f x y = f x y :@: (synAttr x <> synAttr y)

synAppWithA :: Attr -> (Syn a -> Syn b -> c) -> Syn a -> Syn b -> Syn c
synAppWithA t f x y = f x y :@: (t <> synAttr x <> synAttr y)

synAppWithB :: Attr -> (Syn a -> Syn b -> c) -> Syn a -> Syn b -> Syn c
synAppWithB t f x y = f x y :@: (synAttr x <> t <> synAttr y)

synAppWithC :: Attr -> (Syn a -> Syn b -> c) -> Syn a -> Syn b -> Syn c
synAppWithC t f x y = f x y :@: (synAttr x <> synAttr y <> t)


reSyn :: Attr -> Syn a -> Syn a
reSyn x (a :@: _) = a :@: x

reSynFrom :: Syn a -> Syn b -> Syn b
reSynFrom (_ :@: x) = (:@: x) . synData

takeSyn :: Syn a -> b -> Syn b
takeSyn (_ :@: x) = (:@: x)

mapSyn :: (Attr -> Attr) -> Syn a -> Syn a
mapSyn f (a :@: x) = a :@: f x


synCon2 :: Attr -> (Syn a -> Syn b -> c) -> a -> b -> Syn c
synCon2 t f a b = f (a :@: t) (b :@: t) :@: t
