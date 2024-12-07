(module data-type)

(import symbol)
(import alist)

(export fun new-type (env kind name . field-decls)
    (eval `(,kind fun ,(symbol/concat '@ name) (arg)
            (match arg
                ((,(to-quote name) . (-> @list body)) body)
                (else (stop))))
        env)

    (eval `(,kind fun ,(symbol/concat name '?) (arg)
            (match arg
                ((,(to-quote name) . (: list?)) true)
                (else false)))
        env)

    (each field-decls (field-name)
        (eval `(,kind fun ,(symbol/concat name '/ inner-name) (arg)
                (match arg
                    ((@ ,(symbol/concat '@ ,field-name) body) (alist/lookup ,(to-quote field-name) body))
                    (else (panic "expected a " ,(to-quote name) ", got " arg))))
            env)

        (eval `(,kind fun ,(symbol/concat name '/set- inner-name) (arg)
                (match arg
                    ((@ ,(symbol/concat '@ ,field-name) body) (alist/set! ,(to-quote field-name) body))
                    (else (panic "expected a " ,(to-quote name) ", got " arg))))
            env)))

(export macro fun (name inner-path)
    (new-type (get-env 'caller) 'fun name inner-path))

(export macro export (name inner-path)
    (new-type (get-env 'caller) 'export name inner-path))
