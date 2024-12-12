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

_                           ;wildcard
symbol                      ;variable
1 'c' "foo"                 ;literal
() [] {}                    ;interchangeable block syntax
~() ~[] ~{}                 ;literal block syntax
$() $[] ${}                 ;not-a-block syntax
*[] *{} *(foo?) *(@foo x y) ;array, map, procedural literal syntax
'foo '(foo)                 ;value-wise quotation
`foo `(foo)                 ;pattern-wise quotation
,foo ,@foo                  ;unquote, unquote-splicing
(as symbol patt)            ;aliasing
(? patt)                    ;optional
(* patt)                    ;zero or more
(+ patt)                    ;one or more
(! patt)                    ;negation
(| patt patt)               ;alternation
