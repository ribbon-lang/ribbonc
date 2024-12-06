(import parser)

(def fun my-reader (parser start-pos comments)
    (print-ln "(my-reader " start-pos " " comments ")")
    (parser/parse-sexpr! parser))

(print-ln
    ; test comment
    ;! test doc
    #my-reader "test pass through")
