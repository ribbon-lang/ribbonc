(import string)

(def fun manyprint (s x y)
    (if (< x y)
        (manyprint (string/concat s "i") (+ x 1) y)
        s))

(manyprint "" 0 1024)
