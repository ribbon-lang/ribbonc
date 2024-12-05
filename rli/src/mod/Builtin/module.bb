(def *modules* `(
    (test
        (env
            ,@(env/new '((a . 1)
                         (b . 2)
                         (c . 3))))
        (exports
            (a . x) b c))))

(def definition-sym (symbol<-string "#MODULE-DEFINITION#"))

(def macro or-else (body fail-handler) `(with ((fun fail () ,fail-handler)) ,body))
(def macro or-panic (body . msg) `(or-else ,body (panic ,@msg)))
(def macro or-panic-at (body attr . msg) `(or-else ,body (panic-at ,attr ,@msg)))

(with-global fun fail ()
    (panic "uncaught fail"))

(with-global fun exception (value)
    (panic "uncaught exception: " (attr/of value) value))

(def fun symbol-pair (... args)
    (match args
        (((a . b))
            (f-assert (type/symbol? a))
            (f-assert (type/symbol? b))
            (list a b))
        (else stop)))

(def fun bind-export (name env)
    (let ((mod-name
            (or-panic-at (env/lookup definition-sym env) (attr/of name)
                "export must be called within a module (use `(module name)` to declare the current file as a module)"))
          (mod (alist/lookup mod-name *modules*))
          (exports (alist/pair 'exports mod)))
        (pair/set-cdr! exports (pair/cons name (pair/cdr exports)))))

(def fun module/new (env exports)
    `(,(pair/cons 'env env)
      ,(pair/cons 'exports exports)))

(def fun build-import (env name prefix import-list)
    (assert-at (attr/of name) (type/symbol? name)
        "expected a symbol for imported module name, got " (type/of name) ": `" name "`")
    (assert-at (attr/of prefix) (or (type/nil? prefix) (type/symbol? prefix))
        "expected a symbol for imported module prefix")
    (assert-at (attr/of import-list) (or (type/nil? import-list) (type/pair? import-list))
        "expected a list for imported module import list")
    (let ((mod
            (or-panic-at (alist/lookup-f name *modules*)
                (attr/of name) mod "module `" name "` not found"))
          (mod-exports
            (or-panic-at (alist/lookup-f 'exports mod)
                (attr/of mod) "failed to get exports from module `" name "`"))
          (mod-env
            (or-panic-at (alist/lookup-f 'env mod)
                (attr/of mod) "failed to get env from module `" name "`"))
          (frame ())
          (final-prefix (cond
                ((type/nil? prefix) (symbol/concat name))
                ((eq? prefix 'no-prefix) "")
                (else prefix))))
        (list/each mod-exports (fun (entry)
            (let ((internal-key nil)
                  (external-key nil))
                (match entry
                    ((: type/symbol?)
                        (set! internal-key entry)
                        (set! external-key entry))
                    ((-> symbol-pair a b)
                        (set! internal-key a)
                        (set! external-key b))
                    (else (panic-at
                        (attr/of entry) "expected a symbol or a pair of symbols in export list, got " (type/of entry) ": `" entry "`")))
                (let ((import-listed-key (apply-import-list external-key import-list))
                      (value (or-panic (env/lookup-f internal-key mod-env)
                        "failed to find exported value `" internal-key "` in module `" name "`: " mod-env)))
                    (if import-listed-key
                        (set! frame
                            (alist/append
                                (symbol/concat final-prefix '/' import-listed-key)
                                value
                                frame)))))))
        (alist/each frame (fun (key value)
            (meta/eval `(def ,key ,`(,'quote ,value)) env)))))

(def fun apply-import-list (key import-list)
    (if import-list
        (with ((result terminate))
            (list/each import-list (fun (entry)
                (match entry
                    ((: type/symbol?)
                        (if (eq? entry key)
                            (prompt result entry)))
                    ((-> symbol-pair original alias)
                        (if (eq? original key)
                            (prompt result alias)))
                    (else (panic-at (attr/of entry) "expected a symbol or a pair in import list, got " (type/of entry) ": `" entry "`")))))
            nil)
        key))

(def *file-cache* ())

(def fun read-module (current-file filename)
    (let ((abs-path (io/resolve-path (io/dirname current-file) filename))
          (mod-name (io/stem filename)))
        (assert (type/string? mod-name)
            "expected a string for module path, got " (type/of mod-name) ": `" mod-name "`")
        (set! mod-name (symbol<-string mod-name))
        (or-else (alist/lookup-f abs-path *file-cache*)
            (begin
                (assert-at (attr/of filename) (not (alist/member? mod-name *modules*))
                    "module `" mod-name "` already exists")
                (let ((data (io/run-file abs-path)))
                    (set! *file-cache* (alist/append abs-path mod-name *file-cache*))
                    (when (not (alist/member? mod-name *modules*))
                        ; FIXME: this should be a warning
                        ; (print-ln "import file [" abs-path "] did not define a module named `" mod-name "`; binding return value")
                        (set! *modules* (alist/append mod-name (module/new (env/new data) (alist/keys data)) *modules*)))
                    mod-name)))))

(def macro import (name . args)
    (when (type/string? name)
        (let ((my-file-name (attr/filename (attr/of name))))
            (set! name (read-module my-file-name name))))
    (let ((env (meta/get-env 'caller)))
        (match args
            ((prefix import-list) (build-import env name prefix import-list))
            (((@ x (: type/symbol?))) (build-import env name x nil))
            (((@ x (: type/pair?))) (build-import env name nil x))
            ((x) (panic-at (attr/of x)
                "expected a module name and an optional prefix, followed by an optional import list"))
            (() (build-import env name nil nil))
            (else (panic-at (attr/of args)
                "expected a module name and an optional prefix, followed by an optional import list")))))

(def module
    (def macro export (... args)
        (match args
            (('macro name . body)
                (bind-export name (meta/get-env 'caller))
                `(def macro ,name ,@body))
            (('fun name . body)
                (bind-export name (meta/get-env 'caller))
                `(def fun ,name ,@body))
            (('as . rest)
                (let ((env (meta/get-env 'caller)))
                    (list/each rest (fun (arg)
                        (match arg
                            ((: type/symbol?) (bind-export arg env))
                            ((-> symbol-pair original alias) (bind-export (pair/cons original alias) env))
                            (else (panic-at (attr/of arg)
                                "expected a symbol or a pair of symbols in export list, got " (type/of arg) ": `" arg "`")))))))
            ((name . body)
                (bind-export name (meta/get-env 'caller))
                `(def ,name ,@body))))

    (macro (name)
        (let ((env (meta/get-env 'caller))
              (mod (module/new env ())))
            (assert (type/symbol? name)
                "expected a symbol for module name, got " (type/of name) ": `" name "`")
            (assert (not (alist/member? name *modules*))
                "module `" name "` already exists")
            (assert (not (list/member? (env/keys env) definition-sym))
                "file " (attr/filename (attr/of name)) " already has a module definition")
            (attr/set! mod (attr/of name))
            (set! *modules* (alist/append name mod *modules*))
            (env/put! 'export export env)
            (env/put! 'import import env)
            `(def ,definition-sym ,`(,'quote ,name)))))
