(module new-type)

(import meta)
(import symbol)

(export fun new-type (env kind name inner-name body-match)
    (meta/eval `(,kind fun ,(symbol/concat '@ name) (arg)
            (match arg
                ((,(to-quote name) . (@ body (: ,body-match))) (list body))
                (else (stop))))
        env)

    (meta/eval `(,kind fun ,(symbol/concat name '?) (arg)
            (match arg
                ((-> ,(symbol/concat '@ name) _) true)
                (else false)))
        env)

    (meta/eval `(,kind fun ,(symbol/concat inner-name '<- name) (arg)
            (match arg
                ((-> ,(symbol/concat '@ name) x) x)
                (else (panic "expected a " ,(to-quote name) ", got " arg))))
        env)

    (meta/eval `(,kind fun ,(symbol/concat name '<- inner-name) (arg)
            (match arg
                ((@ x (: ,body-match)) ,(to-quasi `(,name . ,',x)))
                (else (panic "expected a " ,(to-quote inner-name) ", got " arg))))
        env)

    nil)

(export macro fun (name inner-name body-match)
    (new-type (meta/get-env 'caller) 'fun name inner-name body-match))

(export macro export (name inner-name body-match)
    (new-type (meta/get-env 'caller) 'export name inner-name body-match))
