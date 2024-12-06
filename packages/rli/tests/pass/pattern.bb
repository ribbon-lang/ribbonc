(import type)
(import pattern)

(print-ln (pattern/run (a b) '(1 2)))

(assert-eq '((b . 2) (a . 1))
    (pattern/run (a b) '(1 2)))

(assert-eq 'okay
    (with ((fun fail () (terminate 'okay)))
        (pattern/run-f (a b) '(1))))

(assert-eq '((a . 1))
    (pattern/run (@ a 1) 1))

(assert-eq ()
    (pattern/run 1 1))

(assert-eq '((x . 1))
    (pattern/run x 1))

(assert-eq '((x . 1))
    (pattern/run (x . 2) '(1 . 2)))

(assert-eq '((rest . (3 4)) (b . 2) (a . 1))
    (pattern/run (a b . rest) '(1 2 3 4)))

(assert-eq '((rest . (3 4)) (b . 2) (a . 1))
    (pattern/run (a b (... rest)) '(1 2 3 4)))

(assert-eq '((c . 3) (b . 2))
    (pattern/run
        (-> (fun (x) (f-assert-eq x 2) (list x 3)) . (b c))
        2))

(assert-eq '((x . (2 3)))
    (pattern/run
        (-> (fun (x) (f-assert-eq x 2) (list x 3)) . x)
        2))

(assert-eq '((x . 2))
    (pattern/run
        (-> (fun (x) (f-assert-eq x 2) x) . x)
        2))

(assert-eq 'okay
    (with ((fun fail () (terminate 'okay)))
        (pattern/run-f
            (-> (fun (x) (f-assert-eq x 1) (list x)) a)
            2)))

(assert-eq ()
    (pattern/run
        (: type/pair?)
        '(1 . 2)))

(assert-eq 'okay
    (with ((fun fail () (terminate 'okay)))
        (pattern/run-f
            (: type/pair?)
            1)))

(assert-eq '((x . (1 . 2)))
    (pattern/run
        (@ x (: type/pair?))
        '(1 . 2)))

(assert-eq 'okay
    (with ((fun fail () (terminate 'okay)))
        (pattern/run-f
            (@ x (: type/pair?))
            1)))
