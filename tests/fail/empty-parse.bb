(with-global
    (fun fail ()
        (terminate "uncaught fail")))

(let ((var p (parser-new)))
    (parse-sexpr! p))
