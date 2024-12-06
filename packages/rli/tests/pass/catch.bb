(with-global fun exception (e)
    (print-ln "exception: " e))

(with-global fun fail ()
    (print-ln "fail"))

(prompt exception 'test)
(prompt fail)
