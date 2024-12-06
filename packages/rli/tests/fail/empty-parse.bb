(import parser)

(with-global fun fail ()
    (terminate "uncaught fail"))

(let ((p (parser/new)))
    (parser/parse-sexpr! p))
