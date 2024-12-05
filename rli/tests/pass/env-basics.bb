(def foo "bar")

(def fun env-check ()
  (print-ln foo))
(env-check)

(print-ln "env-done")
