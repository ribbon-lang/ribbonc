(import alist)

(def alist '((x . 1) (y . 2) (z . 3)))

(assert-eq '((x . 1)) (alist/append 'x 1 ()))

(assert-eq 1 (alist/lookup 'x alist))
(assert-eq 2 (alist/lookup 'y alist))
(assert-eq 3 (alist/lookup 'z alist))
(with ((fail terminate))
    (assert-eq () (alist/lookup 'a alist)))

(set! alist (alist/append 'a 4 alist))
(assert-eq 4 (alist/lookup 'a alist))

(alist/set! 'x 100 alist)
(assert-eq 100 (alist/lookup 'x alist))

(alist/each alist (fun (k v)
    (print-ln k " => " v)))
