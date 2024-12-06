(when true
    (print-ln "when0"))
(when false
    (panic "when1"))

(unless true
    (panic "unless0"))
(unless false
    (print-ln "unless1"))

(if true
    (print-ln "if0"))
(if false
    (panic "if1")
    (print-ln "if2"))

(cond
    ((== 1 2) (panic "cond0"))
    ((== 1 1) (print-ln "cond1")))
(cond
    ((== 1 2) (panic "cond2"))
    (else (print-ln "cond3")))
(cond
    ((== 1 1) (print-ln "cond4"))
    ((== 1 2) (panic "cond5")))

(match '(1 2 3)
    ((1 2 3) (print-ln "match0"))
    ((1 2) (panic "match1")))
(match '(1 2)
    ((1 2) (print-ln "match2"))
    ((1 2 _) (panic "match3")))
(match '(1 2 3)
    ((1 2) (panic "match4"))
    (_ (print-ln "match5")))
(match '(1 2 3)
    ((1 . rest) (print-ln "match6"))
    ((1 2 3) (panic "match7")))
(match '(1 2 3)
    ((1 _ 3) (print-ln "match8"))
    ((1 2 3) (panic "match9")))


