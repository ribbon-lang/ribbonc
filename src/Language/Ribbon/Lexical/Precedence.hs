module Language.Ribbon.Lexical.Precedence where

import Data.Word (Word8)

import Language.Ribbon.Lexical.Fixity




-- | Encodes precedence levels of operators
type Precedence = Word8

-- | The default @Precedence@ of a given fixity
defaultPrecedence :: Fixity -> Precedence
defaultPrecedence = \case -- FIXME: placeholder, gives random values
    Infix -> 7
    Prefix -> 8
    Postfix -> 0
    Atom -> 0
