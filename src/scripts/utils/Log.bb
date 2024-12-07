(module Log)

(export macro log (name . args) `(prompt log (format ,"log(" ,name "): " ,@args)))


(def fun wrap (kind name args body)
    `(,kind ,name ,args
        (,(to-quote log) ,name ,@args)
        (let ((result ,@body))
            (,(to-quote log) ,name result)
            result)))

(export macro export (kind name args . body)
    (let ((q (wrap kind name args body)))
        (eval `(export ,@q) (get-env 'caller))))
