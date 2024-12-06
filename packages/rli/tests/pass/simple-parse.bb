(import attr)
(import string)
(import parser)
(import pair)
(import meta)

(def p (parser/new))
(def first-ln "(print-ln \"hello world\")")
(def line-len (string/length first-ln))
(parser/filename! p "foo")
(parser/input! p
    (string/intercalate "\n"
        first-ln
        "(print-ln \"goodbye world\")"
        "(+ 1 2)")
    '((2 . 1) . 0))
(def res1 (parser/parse-sexpr! p))
(assert-eq
    (pair/cdr (attr/range (attr/of res1)))
    (pair/cons (pair/cons 2 (+ 1 line-len)) line-len))
(meta/eval res1)
(meta/eval (parser/parse-sexpr! p))
(assert-eq (meta/eval (parser/parse-sexpr! p)) 3)
(assert (parser/eof? p))