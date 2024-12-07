(module Subst)

(import list)
(import alist)
(import type)
(import meta)

(import "utils/new-type.bb")

(new-type/export TypeVar symbol)


(new-type/export Subst list)

(export fun lookup (var subst)
    (alist/lookup var (list<-Subst subst)))

(export fun lookup-f (var subst)
    (alist/lookup-f var (list<-Subst subst)))

(export fun append! (var type subst)
    (alist/append var type (list<-Subst subst)))

(export fun set! (var type subst)
    (alist/set! var type (list<-Subst subst)))

(export fun parameters (subst)
    (alist/keys (list<-Subst subst)))

(export fun member? (var subst)
    (alist/member? var (list<-Subst subst)))

(export fun empty? (subst)
    (nil? (list<-Subst subst)))

(export fun empty ()
    (Subst<-list nil))

(export fun length (subst)
    (list/length (list<-Subst subst)))

(export fun each (subst)
    (alist/each (list<-Subst subst)))

(export fun fresh ()
    (TypeVar<-symbol (gensym)))


(export fun apply (term subst)
    (match term
        ((-> @TypeVar var)
            (or-else (apply (lookup-f var subst) subst) term))
        ((x . y)
            `(,(apply x subst) . ,(apply y subst)))
        (else term)))
