(module Env)

(import alist)
(import attr)

(export fun @Env (arg)
    (match arg
        ((: list?) (list arg))
        (else (stop))))

(export fun Env? (arg)
    (match arg
        ((: list?) true)
        (else false)))

(export fun lookup (var env)
    (alist/lookup var env))

(export fun lookup-f (var env)
    (alist/lookup-f var env))

(export fun lookup-e (var env)
    (with ((fun fail ()
            (throw `(,(attr/of var) ,(format "cannot find variable `" var "`"))))
        (alist/lookup-f var env))))

(export fun append (var type env)
    (alist/append var type env))

(export fun set! (var type env)
    (alist/set! var type env))

(export fun parameters (env)
    (alist/keys env))

(export fun member? (var env)
    (alist/member? var env))

(export fun empty? (env)
    (nil? env))

(export fun length (env)
    (list/length env))

(export fun each (env)
    (alist/each env))

