(print-ln (run-lambda-list (a b) '(1 2)))

(assert-eq '((b . 2) (a . 1))
    (run-lambda-list (a b) '(1 2)))

(assert-eq 'okay
    (with ((fun fail () (terminate 'okay)))
        (f-lambda-list (a b) '(1))))

(assert-eq '((a . 1))
    (run-lambda-list (@ a 1) 1))

(assert-eq ()
    (run-lambda-list 1 1))

(assert-eq '((x . 1))
    (run-lambda-list x 1))

(assert-eq '((x . 1))
    (run-lambda-list (x . 2) '(1 . 2)))

(assert-eq '((rest . (3 4)) (b . 2) (a . 1))
    (run-lambda-list (a b . rest) '(1 2 3 4)))

(assert-eq '((rest . (3 4)) (b . 2) (a . 1))
    (run-lambda-list (a b (... rest)) '(1 2 3 4)))

(assert-eq '((c . 3) (b . 2))
    (run-lambda-list
        (-> (fun (x) (f-assert-eq x 2) (list x 3)) . (b c))
        2))

(assert-eq '((x . (2 3)))
    (run-lambda-list
        (-> (fun (x) (f-assert-eq x 2) (list x 3)) . x)
        2))

(assert-eq '((x . 2))
    (run-lambda-list
        (-> (fun (x) (f-assert-eq x 2) x) . x)
        2))

(assert-eq 'okay
    (with ((fun fail () (terminate 'okay)))
        (f-lambda-list
            (-> (fun (x) (f-assert-eq x 1) (list x)) a)
            2)))

(assert-eq ()
    (run-lambda-list
        (: pair?)
        '(1 . 2)))

(assert-eq 'okay
    (with ((fun fail () (terminate 'okay)))
        (f-lambda-list
            (: pair?)
            1)))

(assert-eq '((x . (1 . 2)))
    (run-lambda-list
        (@ x (: pair?))
        '(1 . 2)))

(assert-eq 'okay
    (with ((fun fail () (terminate 'okay)))
        (f-lambda-list
            (@ x (: pair?))
            1)))
