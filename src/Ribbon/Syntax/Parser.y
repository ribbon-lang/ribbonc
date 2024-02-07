{
module Ribbon.Syntax.Parser where

import Ribbon.Source
import Ribbon.Display (Display(..))
import Ribbon.Display qualified as Display
import Ribbon.Syntax.Token
import Ribbon.Syntax.Lexer
import Ribbon.Syntax.ParserM
}

%name parse

%monad { Parser }
%lexer { (next >>=) } { T' TEof }
%error { tokenError }
%tokentype { Token }

%token
    IDENT { T' (TSymbol TsIdentifier $$) }

%%

ident :: { String }
    : IDENT { $1 }

{
tokenError :: Token -> Parser a
tokenError t = failWith $ ParseUnexpectedInput $ rangeStart $ attrRange $ tagOf t
}
