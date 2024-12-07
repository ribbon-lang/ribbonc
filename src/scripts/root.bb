; V = kebab-sym
;   | () | bool_lit | int_lit | float_lit | char_lit | string_lit
;   | ('fun (kebab-sym T?)* [-> T]? => V)
;   | (V ': T)
;   | ('do V+)
;   | (V+)

; T = CamelCaseSym
;   | '->
;   | ('type-var . kebab-sym)
;   | (T T+)

; Q = (('for kebab-sym+)? T)

; D = (kebab-sym [': Q]? '= V)
;   | ('type CamelCaseSym '= Q)

; S = (D+)


(import attr)

(import "Env.bb")
(import "Subst.bb")
(import "Tc.bb")


(def expr
    '(fun (x) (fun (y) (+ x y))))
(print-ln "expr: " expr)

(def type
    (with ((fun fail ()
                (print-ln "ICE")
                (terminate))
           (fun log (msg)
                (print-ln msg))
           (fun exception (... args)
                (match args
                    ((((attr . kind) . msg))
                        (print-ln kind "Error[" attr "]:\n\t" msg)
                        (terminate))
                    (else
                        (print-ln "uncaught exception" args)
                        (terminate)))))
        (Tc/infer expr (Subst/empty) '((+ . (-> Int (-> Int Int)))))))
(print-ln "type: " type)
