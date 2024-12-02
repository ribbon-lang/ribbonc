(def p (parser-new))
(def first-ln "(print-ln \"hello world\")")
(def line-len (string-length first-ln))
(parser-filename! p "foo")
(parser-input! p
    (string-intercalate "\n"
        first-ln
        "(print-ln \"goodbye world\")"
        "(+ 1 2)")
    '((2 . 1) . 0))
(def res1 (parse-sexpr! p))
(assert-eq
    (cdr (attr-range (attr-of res1)))
    (cons (cons 2 (+ 1 line-len)) line-len))
(eval res1)
(eval (parse-sexpr! p))
(assert-eq (eval (parse-sexpr! p)) 3)
(assert (parser-eof? p))
