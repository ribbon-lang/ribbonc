(module Subst)

(import list)
(import pair)
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

(export fun apply (term subst)
    (match term
        (('type-var . (@ var (: type/symbol?)))
            (or-else (alist/lookup-f var subst) term))
        ((x . y)
            `(,(apply x subst) . ,(apply y subst)))
        (else term)))
