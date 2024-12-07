(module Tc)

(import attr)
(import type)
(import list)

(import "Subst.bb")
(import "Env.bb")
(import "utils/Log.bb")


(def fun as (unannotated-expr type)
    `(,unannotated-expr : ,type))


(export fun unify (expected found subst)
    (match (cons expected found)
        ((x . x) x)
        (((-> Subst/@TypeVar va) . ((-> Subst/@TypeVar vb)))
            ())
        (else (throw `((,(attr/of found) . type) . ,(format "expected `" expected "`\n\t   found `" found "`"))))))


(export fun mk-arrow (a b) `(-> ,a ,b))
(export fun function-type (attr param-types return-type)
    (attr/set! attr (foldr param-types return-type (rhs x) (mk-arrow x rhs))))

(export fun check (expect value subst env)
    (let (((@ out (_ ': ann)) (infer value subst env)))
        (unify expect ann subst)
        out))

(Log/export fun (value subst env)
    (match value
        ((: symbol?)
            (as value (Env/lookup-e value env)))

        ((: nil?)
            (as value ()))
        ((: bool?)
            (as value 'Bool))
        ((: int?)
            (as value 'Int))
        ((: float?)
            (as value 'Float))
        ((: char?)
            (as value 'Char))
        ((: string?)
            (as value 'String))

        ((v ': t) (check t v subst env))

        (((@ head 'fun (? (param (? i))) (? '-> o)) body)
            (let ((param-type (or i (if param (Subst/fresh) ())))
                  (body-type  (or o (Subst/fresh)))
                  (result-type `(function-type (attr/of head) ,param-type ,body-type))
                  (env (if param (Env/append param param-type env) env)))
                (check result-type body subst env)
                (as value body-type)))

        (('do . body)
            (each body (v) (infer v subst))
            (as value ()))

        ((f . vs)
            (let ((ts (map vs (v) (Subst/fresh)))
                  (ft (function-type (attr/of value) ts (Subst/fresh))))
                (check ft f subst env)
                (each (list/zip vs ts) (v t) (check t v subst env))
                (as value ft))))))
