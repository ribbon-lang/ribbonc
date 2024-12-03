(import attr)

(def foo 100)

(print-ln (attr/of foo))
(print-ln (attr/of '@))
(print-ln (attr/new "foo" '(((100 . 50) . 150) . ((200 . 22) . 250))))
(print-ln (attr/new "foo" '(() . ())))
(print-ln (attr/new "foo" ()))
(attr/here)
