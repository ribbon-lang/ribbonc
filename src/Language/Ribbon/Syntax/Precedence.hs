module Language.Ribbon.Syntax.Precedence where

import Data.Word (Word8)

import Language.Ribbon.Syntax.Fixity




-- | Encodes precedence levels
type Precedence = Word8


-- FIXME: these values are completely random
-- | The default @Precedence@ of a given fixity
defaultPrecedence :: ExactFixity -> Precedence
defaultPrecedence = \case
    EInfixL -> 7
    EInfixR -> 7
    EInfix -> 7
    EPrefix -> 8
    EPostfix -> 0
    EAtom -> 0
