(each (env-get) (lambda (frame)
    (print "(")
    (cond
        ((nil? frame))
        ((pair? frame)
            (cond
                ((nil? (cdr frame))
                    (print (car (car frame)) " => " (cdr (car frame))))
                (else
                    (print-ln " " (car (car frame)) " => " (cdr (car frame)))
                    (alist-each (cdr frame) (lambda (key value)
                        (print-ln "  " key " => " value))))))
        (else (print "invalid frame of type " (type-of frame) ": `" frame "`")))
    (print-ln ")\n")))
