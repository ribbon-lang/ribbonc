(with-global
    (fun exception (e)
         (print-ln "exception: " e))
    (fun fail ()
         (print-ln "fail")))

(prompt exception 'test)
(prompt fail)
