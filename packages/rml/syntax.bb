()[]{}
; these are interchangeable as far as the interpreter is concerned
(def fun foo [a] {print a})
{def fun foo (a) [print a]}
; etc

(match foo
    (sym expr) ; match foo with any value and bind it to the symbol sym, then run expr

    (1 expr) ; match foo with the number one, no binding

    ((as sym "foo") expr) ; match foo with the literal string foo, then bind it to the symbol sym

     ; match foo with any kind of data structure that contains data structures beginning
     ; with the literal symbol foo and containing at least one more value
    ((as sym (* ('foo (+ sym2)))) expr) ; resulting env: sym: []Env where each env = { sym: []Object }

    (_ expr) ; match foo with any value, no binding
    (else expr) ; else clause is optional
)



; full pattern syntax

_                    ;wildcard
x y z                ;variable
nil true 1 'c' "foo" ;literal
() [] {}             ;interchangeable block syntax
~() ~[] ~{}          ;literal block syntax
$() $[] ${}          ;not-a-block syntax
*(foo?) *(@foo x y)  ;procedural literal syntax
'foo '(foo)          ;value-wise quotation
`foo `(foo)          ;pattern-wise quotation
,foo ,@foo           ;unquote, unquote-splicing
(as symbol patt)     ;aliasing ;outer block is not-a-block
(? patt)             ;optional ;outer block is not-a-block
(* patt)             ;zero or more ;outer block is not-a-block
(+ patt)             ;one or more ;outer block is not-a-block
(! patt)             ;negation ;outer block is not-a-block
(| patt patt)        ;alternation ;outer block is not-a-block
