(with-global
    (fun fail ()
        (terminate "uncaught fail")))

(let ((p (parser-new)))
    (parse-sexpr! p))
