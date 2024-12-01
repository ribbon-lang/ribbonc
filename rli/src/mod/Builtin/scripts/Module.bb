(def-var module
    (def-var *modules* `(
        (test
            (env
                ,@(env-new '((a . 1)
                             (b . 2)
                             (c . 3))))
            (exports (a . x) b c))
        ))

    (def-macro or-else (f x)
        `(with ((fun fail () ,x)) ,f))

    (def-macro or-panic (f . msg)
        `(or-else ,f (panic ,@msg)))

    (def-macro or-panic-at (f at . msg)
        `(or-else ,f (panic-at ,at ,@msg)))

    (def-fun symbol-pair ((a . b))
        (f-assert (symbol? a))
        (f-assert (symbol? b))
        (list a b))

    (def-var definition-sym (symbol<-string "#MODULE-DEFINITION#"))

    (def-fun bind-export (name env)
        (let ((var mod-name
                (or-panic-at (env-lookup definition-sym env)
                    (attr-of name) "export must be called within a module (use `(module name)` to declare the current file as a module)"))
              (var mod (alist-lookup mod-name *modules*))
              (var exports (alist-pair 'exports mod)))
            (set-cdr! exports (cons name (cdr exports)))))

    (def-macro export-var (name . body)
        (bind-export name (get-env 'caller))
        `(def-var ,name ,@body))

    (def-macro export-fun (name . body)
        (bind-export name (get-env 'caller))
        `(def-fun ,name ,@body))

    (def-macro export-macro (name . body)
        (bind-export name (get-env 'caller))
        `(def-macro ,name ,@body))

    (def-macro export (... args)
        (let ((var env (get-env 'caller)))
            (each args (lambda (arg)
                (match arg
                    ((: symbol?) (bind-export arg env))
                    ((-> symbol-pair original alias) (bind-export (cons original alias) env))
                    (else (panic-at (attr-of arg) "expected a symbol or a pair of symbols in export list, got " (type-of arg) ": `" arg "`")))))))

    (def-macro import (name . args)
        (let ((var env (get-env 'caller)))
            (match args
                ((prefix import-list) (build-import env name prefix import-list))
                (((@ x (: symbol?))) (build-import env name x nil))
                (((@ x (: pair?))) (build-import env name nil x))
                ((x) (panic-at (attr-of x) "expected a module name and an optional prefix, followed by an optional import list"))
                (() (build-import env name nil nil))
                (else (panic-at (attr-of args) "expected a module name and an optional prefix, followed by an optional import list")))))

    (def-fun build-import (env name prefix import-list)
        (assert-at (attr-of name) (symbol? name)
            "expected a symbol for imported module name")
        (assert-at (attr-of prefix) (or (nil? prefix) (symbol? prefix))
            "expected a symbol for imported module prefix")
        (assert-at (attr-of import-list) (or (nil? import-list) (pair? import-list))
            "expected a list for imported module import list")
        (let ((var mod (alist-lookup name *modules*)))
            (assert-at (attr-of name) mod "module `" name "` not found")
            (let ((var mod-exports
                    (or-panic-at (alist-lookup 'exports mod)
                        (attr-of mod) "failed to get exports from module `" name "`"))
                  (var mod-env
                    (or-panic-at (alist-lookup 'env mod)
                        (attr-of mod) "failed to get env from module `" name "`"))
                  (var frame ()))
                (each mod-exports (lambda (entry)
                    (let ((var internal-key nil)
                          (var external-key nil))
                        (match entry
                            ((: symbol?)
                                (set! 'internal-key entry)
                                (set! 'external-key entry))
                            ((-> symbol-pair a b)
                                (set! 'internal-key a)
                                (set! 'external-key b))
                            (else (panic-at (attr-of entry) "expected a symbol or a pair of symbols in export list, got " (type-of entry) ": `" entry "`")))
                        (let ((var import-listed-key (apply-import-list external-key import-list))
                              (var value
                                (or-panic (env-lookup internal-key mod-env)
                                    "failed to find exported value `" internal-key "` in module `" name "`: " mod-env)))
                            (if import-listed-key
                                (set! 'frame
                                    (alist-put
                                        (if prefix
                                            (symbol-intercalate '/' prefix import-listed-key)
                                            import-listed-key)
                                        value
                                        frame)))))))
                (alist-each frame (lambda (key value)
                    (eval `(def-var ,key ,`(,'quote ,value)) env))))))

    (def-fun apply-import-list (key import-list)
        (if import-list
            (with ((var result terminate))
                (each import-list (lambda (entry)
                    (match entry
                        ((: symbol?)
                            (if (eq? entry key)
                                (prompt result entry)))
                        ((-> symbol-pair original alias)
                            (if (eq? original key)
                                (prompt result alias)))
                        (else (panic-at (attr-of entry) "expected a symbol or a pair in import list, got " (type-of entry) ": `" entry "`")))))
                nil)
            key))

    (macro (name)
        (let ((var env (get-env 'caller))
              (var mod
                `(,name
                    ,(cons 'env env)
                    (exports . ()))))
            (assert (symbol? name)
                "expected a symbol for module name, got " (type-of name) ": `" name "`")
            (assert (not (alist-member? name *modules*))
                "module `" name "` already exists")
            (assert (not (element (env-keys env) definition-sym))
                "file " (attr-filename (attr-of name)) " already has a module definition")
            (attr-set! mod (attr-of name))
            (set! '*modules* (cons mod *modules*))
            (env-put! 'export-var   export-var   env)
            (env-put! 'export-fun   export-fun   env)
            (env-put! 'export-macro export-macro env)
            (env-put! 'export       export       env)
            (env-put! 'import       import       env)
            `(def-var ,definition-sym ,`(,'quote ,name)))))
