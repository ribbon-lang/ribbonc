(module Subst)

(import list)
(import alist)
(import type)

(export lookup alist/lookup)
(export lookup-f alist/lookup-f)
(export append alist/append)
(export set! alist/set!)
(export parameters alist/keys)
(export member? alist/member?)
(export empty? type/nil?)
(export length list/length)
(export each alist/each)

(export fun @type-var (arg)
    (match arg
        (('type-var . (@ var (: type/symbol?))) (list var))
        (else (stop))))

(export fun apply (term subst)
    (match term
        ((-> @type-var var)
            (or-else (apply (lookup-f var subst) subst) term))
        ((x . y)
            `(,(apply x subst) . ,(apply y subst)))
        (else term)))
