module Ribbon.Syntax.Token where

import Ribbon.Display (Display(..))

import Ribbon.Source
import Ribbon.Syntax.Literal


-- | An atom of syntax
type Token = Tag Attr TokenData

-- | An atom of syntax
data TokenData
    -- | A symbolic token,
    --   either punctuation, reserved or user defined
    = TSymbol !String
    -- | A token indicating a literal value, such as an int or string
    | TLiteral !Literal
    -- | End of file token
    | TEof
    deriving (Eq, Ord, Show)


instance Display TokenData where
    display = \case
        TSymbol s -> s
        TLiteral l -> display l
        TEof -> "{EOF}"
