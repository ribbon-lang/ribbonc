(import string)

(with-global
    (fun exception (e)
        (terminate (string/concat "uncaught " (stringify e)))))

(unstringify "(foo bar) ")
