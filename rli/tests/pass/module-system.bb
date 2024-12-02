(module foo)

(import test)
(print-ln x)

(import test t (
    (b . a)
    c
))
(print-ln t/a)
(print-ln t/c)


(export a 100)
(export fun bar (x) (print-ln (+ x 1)))
(export as (a . y))

(import foo f)
(print-ln f/a)
(print-ln f/y)
(f/bar 10)
