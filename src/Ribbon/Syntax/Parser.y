{
module Ribbon.Syntax.Parser
    ( expr
    , patt
    ) where

import Data.Functor
import Data.Bifunctor

import Ribbon.Source
import Ribbon.Display (Display(..))
import Ribbon.Display qualified as Display
import Ribbon.Syntax.Literal
import Ribbon.Syntax.Token
import Ribbon.Syntax.Ast
import Ribbon.Syntax.Lexer
import Ribbon.Syntax.ParserM
}


%name expr expr
%name patt patt

%monad { Parser }
%lexer { (next >>=) } { T' TEof }
%error { tokenError }
%tokentype { Token }


%token
    "let"       { T' (TSymbol "let") }
    -- "in"        { T' (TSymbol "in") }
    -- "as"        { T' (TSymbol "as") }
    "fun"       { T' (TSymbol "fun") }
    "match"     { T' (TSymbol "match") }
    -- "with"      { T' (TSymbol "with") }
    -- "do"        { T' (TSymbol "do") }
    -- "handler"   { T' (TSymbol "handler") }
    -- "module"    { T' (TSymbol "module") }
    -- "import"    { T' (TSymbol "import") }
    -- "use"       { T' (TSymbol "use") }
    -- "pub"       { T' (TSymbol "pub") }
    -- "namespace" { T' (TSymbol "namespace") }
    -- "type"      { T' (TSymbol "type") }
    -- "effect"    { T' (TSymbol "effect") }
    -- "class"     { T' (TSymbol "class") }
    -- "instance"  { T' (TSymbol "instance") }
    -- "infix"     { T' (TSymbol "infix") }
    -- "infixl"    { T' (TSymbol "infixl") }
    -- "infixr"    { T' (TSymbol "infixr") }
    -- "prefix"    { T' (TSymbol "prefix") }
    -- "postfix"   { T' (TSymbol "postfix") }
    "="         { T' (TSymbol "=") }
    "=>"        { T' (TSymbol "=>") }
    "|"         { T' (TSymbol "|") }
    -- "."         { T' (TSymbol ".") }
    -- ".."        { T' (TSymbol "..") }
    ";"         { T' (TSymbol ";") }

    -- "["         { T' (TSymbol "[") }
    -- "]"         { T' (TSymbol "]") }
    -- "{"         { T' (TSymbol "{") }
    -- "}"         { T' (TSymbol "}") }
    "("         { T' (TSymbol "(") }
    ")"         { T' (TSymbol ")") }
    ","         { T' (TSymbol ",") }

    IDENT_T       { T' (TSymbol _) }
    INT_T         { T' (TLiteral (LInt _)) }
    FLOAT_T       { T' (TLiteral (LFloat _)) }
    CHAR_T        { T' (TLiteral (LChar _)) }
    STRING_T      { T' (TLiteral (LString _)) }


%nonassoc "fun" "=>"
%right ";"
%nonassoc "let" "match" "with" "=" "|" "," "(" ")" "{" "}"

%% -------------------------------------------------------------------------- %%


IDENT :: { Tag Attr String }
    : IDENT_T   { $1 <&> \(TSymbol x) -> x }

LITERAL :: { Tag Attr Literal }
    : INT_T     { $1 <&> \(TLiteral x) -> x }
    | FLOAT_T   { $1 <&> \(TLiteral x) -> x }
    | CHAR_T    { $1 <&> \(TLiteral x) -> x }
    | STRING_T  { $1 <&> \(TLiteral x) -> x }


expr :: { Expr }
    : exprCompound %shift { $1 }
    | expr ";" expr { ESequence $1 $3 :@: (tagOf $1 <> tagOf $3) }

exprAtom :: { Expr }
    : "let" patt "=" expr
        { ELet $2 $4 :@: (tagOf $1 <> tagOf $4) }
    | "fun" patt "=>" expr
        { EFunction $2 $4 :@: (tagOf $1 <> tagOf $4) }
    | "match" expr caseList
        { EMatch $2 (untag $3) :@: (tagOf $1 <> tagOf $3) }
    | IDENT
        { $1 <&> EVar }
    | LITERAL
        { $1 <&> ELit }
    | "(" ")"
        { EVar "()" :@: tagOf $1 }
    | "(" expr ")"
        { reTag (tagOf $1 <> tagOf $3) $2 }
    | "(" expr "," ")"
        { EProductConstructor [("0", $2)]
            :@: (tagOf $1 <> tagOf $4) }
    | "(" expr "," exprList ")"
        { EProductConstructor (zip ([0..] <&> show) ($2 : untag $4))
            :@: (tagOf $1 <> tagOf $5) }

exprCompound :: { Expr }
    : exprAtom { $1 }
    | exprCompound exprAtom
        { case (untag $1, untag $2) of
            (ECompound as, _) -> ECompound (as <> [$2]) :@: (tagOf $1 <> tagOf $2)
            _ -> ECompound [$1, $2] :@: (tagOf $1 <> tagOf $2)
        }

exprList :: { Tag Attr [Expr] }
    : expr
        { [$1] :@: tagOf $1 }
    | exprList "," expr
        { bimap (<> tagOf $3) (<> [$3]) $1 }


patt :: { Patt }
    : IDENT
        { $1 <&> PVar }


case :: { Tag Attr Case }
    : "|" patt "=>" expr
        { ($2, $4) :@: (tagOf $1 <> tagOf $4) }

caseList :: { Tag Attr [Case] }
    : case
        { [untag $1] :@: tagOf $1 }
    | caseList case
        { bimap (<> tagOf $2) (<> [untag $2]) $1 }

{
tokenError :: Token -> Parser a
tokenError t = failWith $ ParseUnexpectedInput $ rangeStart $ attrRange $ tagOf t
}
