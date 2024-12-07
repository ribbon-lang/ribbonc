export const Keywords = [
    "do", "match", "cond", "unless", "when", "if", "else",
];

export const Module = [
    "module", "import", "export"
];

export const Definition = [
    "let", "def",
];

export const Object = [
    "macro", "fun",
];

export const Effectful = [
    "with", "with-global",
    "prompt", "fetch",
    "print-ln", "print",
    "terminate",
];

export const Special = [
    "quote", "quasiquote", "unquote", "unquote-splicing",
    "to-quote", "to-quasi",
    "apply", "gensym", "eval", "get-env",
]

export const ControlFlow = [
    "assert", "stop", "throw", "panic", "else",
    "or-else", "or-panic", "or-panic-at",
];

export const Operators = [
    "or", "and", "not",
    // "pow", "mod", "div", "mul", "sub", "add",
    "format","unstringify", "stringify",
    "frac", "round", "ceil", "floor",
    "of", "in", "at", "is", "as", "to", "from",
    "each", "foldl", "foldr", "map", "filter",
    "cdr", "car", "cons", "list"
];

export const Constant = [
    "false", "true", "min-int", "max-int", "nan", "inf", "nil", "epsilon",
    "std-err", "std-out", "std-in",
];

// classified by predicate matcher
//     callable? builtin? extern-function? extern-data? macro? lambda? function?
//     symbol? string? float? char? int? bool? pair? nil? utf-case-insensitive-eq?
//     utf-uppercase? utf-lowercase? utf-hex? utf-decimal? utf-digit? utf-numeric? utf-diacritic?
//     utf-hex-digit? utf-space? utf-xid-continue? utf-xid-start? utf-id-continue? utf-id-start?
//     utf-alphabetic? utf-math? utf-symbol? utf-separator? utf-punctuation? utf-number? utf-mark?
//     utf-letter? utf-control? empty-symbol? empty-string? parser-eof? truthy?
//     bound? +inf? -inf? inf? nan? alist-member?

// classified by setter matcher
//     attr-set! parse-sexpr! parser-input! parser-filename! env-put! env-set! set-cdr!
//     set-car! file-cursor! set! alist-set!

// classified by conversion matcher
//     byte-offset<-index index<-byte-offset string<-list list<-string
//     symbol<-string string<-symbol float<-int int<-float char<-int int<-char int<-bool bool<-int

// classified by operator matcher
//     ^  %  /  *  -  +

// standard identifiers
//      process-args
//      utf-display-width utf-byte-count utf-casefold utf-uppercase utf-lowercase utf-describe-category utf-category
//      subsymbol symbol-intercalate symbol-concat symbol-find symbol-length
//      substring-byte-offset substring string-intercalate string-concat nth-char string-find-byte-offset string-find string-length
//      attr-new attr-range attr-filename attr-here
//      parser-input parser-filename parser-new
//      set-global-evidence get-global-evidence
//      replace-env get-env take-env swap-env env-pop env-push env-get-frame env-new env-copy env-pair env-lookup env-keys
//      element each map length
//      run-lambda-list e-lambda-list f-lambda-list lambda-list-binders validate-lambda-list
//      file-cursor file-end write-ln write-file read-ln read-file open-file
//      alist-keys alist-each  alist-put  alist-lookup alist-pair
