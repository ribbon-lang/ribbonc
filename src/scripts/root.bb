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


(import "Subst.bb")

(def subst '(
    (a . bool)
    (b . float)
))

(def expr `(fun ((type-var . a) int) (type-var . b) in ()))

(def applied (Subst/apply expr subst))

(print-ln subst)
(print-ln expr)
(print-ln applied)
(assert-eq '(fun (bool int) float in ()) applied)
(assert (Subst/type-var? '(type-var . a)))
(assert (not (Subst/type-var? '(typeVar . a))))
(assert-eq 'a (Subst/symbol<-type-var (Subst/type-var<-symbol 'a)))

