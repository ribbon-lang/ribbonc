(assert-eq 'okay
    (with ((var return terminate))
        (let ((fun test (x) (prompt return x)))
            ((run-lambda-list (-> test) 'okay)))))

(assert-eq '((z . okay))
    (with ((fun check (x) (f-assert-eq x 100) 'okay))
        (let ((fun test (y) (prompt check y)))
            (run-lambda-list (-> test . z) 100))))
