(match '(1 2 3 4)
    ((a b (... rest))
        (print-ln a)
        (print-ln b)
        (print-ln rest))
    (_ (panic "expected a b ... rest")))
(match '(1 2)
    ((a b (... rest))
        (print-ln a)
        (print-ln b)
        (print-ln rest))
    (_ (panic "expected a b ... rest")))
(match '(1 2 3 4)
    ((a b . rest)
        (print-ln a)
        (print-ln b)
        (print-ln rest))
    (_ (panic "expected a b . rest")))
(match '(1 2)
    ((a b . rest)
        (print-ln a)
        (print-ln b)
        (print-ln rest))
    (_ (panic "expected a b . rest")))
(match '(1 2 3 4)
    ((a b (? c) (? d))
        (print-ln a)
        (print-ln b)
        (print-ln c)
        (print-ln d))
    (_ (panic "expected a b c d")))
(match '(1 2 3 4)
    ((a b) (panic "should not match"))
    (_ (print-ln "no match")))
(match '(1 2 3 4)
    ((a (? b) (... rest))
        (print-ln a)
        (print-ln b)
        (print-ln rest))
    (_ (panic "expected a b ... rest")))
(match '(1 2 3)
    ((a (? b) (... rest))
        (print-ln a)
        (print-ln b)
        (print-ln rest))
    (_ (panic "expected a b ... rest")))
(match '(1 2)
    ((a (? b) (... rest))
        (print-ln a)
        (print-ln b)
        (print-ln rest))
    (_ (panic "expected a b ... rest")))
