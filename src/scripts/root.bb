; V = kebab-sym
;   | bool_lit | int_lit | float_lit | char_lit | str_lit
;   | ('fun (kebab-sym T?)* [-> T]? => V)
;   | (V ': T)
;   | ('do V++';)
;   | (V+)

; T = CamelCaseSym
;   | ('type-var . kebab-sym)
;   | (T '-> T)
;   | (T T+)

; Q = (('for kekab-sym+)? T)

; D = (kebab-sym [': Q]? '= V)
;   | ('type CamelCaseSym '= Q)

; S = (D+)


(import Subst)

(def subst '(
    (a . bool)
    (b . float)
))

(def expr `(fun ((type-var . a) int) (type-var . b) in ()))

(print-ln subst)
(print-ln expr)
(print-ln (Subst/apply expr subst))
