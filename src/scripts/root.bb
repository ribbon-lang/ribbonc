(module Compiler)

(import Subst)

(def subst '(
    (a . bool)
    (b . float)
))

(def expr `(fun ((type-var . a) int) (type-var . b) in ()))

(print-ln subst)
(print-ln expr)
(print-ln (Subst/apply expr subst))
