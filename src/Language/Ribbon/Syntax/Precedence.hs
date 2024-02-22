module Language.Ribbon.Syntax.Precedence where

import Data.Word (Word8)

import Language.Ribbon.Syntax.Fixity




-- | Encodes precedence levels of operators
type Precedence = Word8



-- | The default @Precedence@ of a given fixity
defaultPrecedence :: ExactFixity -> Precedence
defaultPrecedence = \case -- FIXME: placeholder, gives random values
    InfixL -> 7
    InfixR -> 7
    Infix -> 7
    Prefix -> 8
    Postfix -> 0
    Atom -> 0
