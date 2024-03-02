module Language.Ribbon.Syntax.Binding where

import Text.Pretty

import Language.Ribbon.Lexical.Visibility
import Language.Ribbon.Lexical.Category
import Language.Ribbon.Lexical.Fixity
import Language.Ribbon.Lexical.Associativity
import Language.Ribbon.Lexical.Precedence




-- | Binds an overloaded name to a value; either a @Ref@ or a @Path@,
--   depending on compilation phase. The @Fixity@ and @Precedence@ of
--   this usage of the overload are stored here as well. The element @e@ must
--   instance @CatOverloaded@ for @Binding e@ to be usable in @Set@s as intended
data Binding e
    = Binding
    { visibility :: !Visibility
    , fixity :: !Fixity
    , associativity :: !Associativity
    , precedence :: !Precedence
    , elem :: !e
    }
    deriving Show

instance CatOverloaded e => CatOverloaded (Binding e) where
    overloadedCategory = overloadedCategory . (.elem)

instance HasFixity (Binding e) where
    getFixity = (.fixity)

instance CatOverloaded e => Eq (Binding e) where
    a == b = compare a b == EQ

instance CatOverloaded e => Ord (Binding e) where
    compare a b
         = compare (overloadedCategory a) (overloadedCategory b)
        <> fixityCompare (getFixity a) (getFixity b)

instance Pretty e => Pretty (Binding e) where
    pPrintPrec lvl _ (Binding v f a p i) =
        hang (hsep [ pPrintPrec lvl 0 v
                   , pPrintPrec lvl 0 f
                   , pPrintPrec lvl 0 a
                   , pPrintPrec lvl 0 p
                   , "::" ]) do
            pPrintPrec lvl 0 i
