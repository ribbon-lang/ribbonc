(import pattern)

(assert-eq 'okay
    (with ((return terminate))
        (let ((fun test (x) (prompt return x)))
            ((pattern/run (-> test) 'okay)))))

(assert-eq '((z . okay))
    (with ((fun check (x) (f-assert-eq x 100) 'okay))
        (let ((fun test (y) (prompt check y)))
            (pattern/run (-> test . z) 100))))
