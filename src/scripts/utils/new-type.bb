(module new-type)

(import meta)
(import symbol)

(export fun new-type (env kind name inner-path)
    (def inner-name (symbol/basename inner-path))
    (def inner-base (symbol/dirname inner-path))
    (def body-match
        (if inner-base
            (symbol/concat inner-base '@ inner-name)
            (symbol/concat '@ inner-name)))

    (eval `(,kind fun ,(symbol/concat '@ name) (arg)
            (match arg
                ((,(to-quote name) . (-> ,body-match body)) (list body))
                (else (stop))))
        env)

    (eval `(,kind fun ,(symbol/concat name '?) (arg)
            (match arg
                ((-> ,(symbol/concat '@ name) _) true)
                (else false)))
        env)

    (eval `(,kind fun ,(symbol/concat inner-name '<- name) (arg)
            (match arg
                ((-> ,(symbol/concat '@ name) x) x)
                (else (panic "expected a " ,(to-quote name) ", got " arg))))
        env)

    (eval `(,kind fun ,(symbol/concat name '<- inner-name) (arg)
            (match arg
                ((@ x (: ,body-match)) ,(to-quasi `(,name . ,',x)))
                (else (panic "expected a " ,(to-quote inner-name) ", got " arg))))
        env)

    nil)

; (export macro def (name inner-path)
;     (new-type (get-env 'caller) 'def name inner-path))

(export macro export (name inner-path)
    (new-type (get-env 'caller) 'export name inner-path))
