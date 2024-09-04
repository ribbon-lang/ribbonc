const std = @import("std");

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Eval = Core.Eval;

pub const Doc =
    \\##### Supported lambda list syntax
    \\- `optional` = `(? var)`
    \\    > if there are arguments remaining, apply `var` to the next argument; otherwise any bindings within `var` are `nil`
    \\- `rest` = `(... symbol)`
    \\    > if there are arguments remaining, bind the rest of the arguments to `symbol`; otherwise bind `nil`
    \\- `cons` = `. var`
    \\    > if there are arguments remaining, bind the rest of the arguments to `var`; otherwise bind `nil`
    \\- `var` =
    \\    + `(var* optional* rest? cons?)`
    \\        > expect a list of `var`s, or the empty list
    \\    + `,expr`
    \\        > same as quasiquote's unquote, evaluate `expr` and apply it to the next argument
    \\    + `(@ symbol var)`
    \\        > match the given `var`, then bind it to the given `symbol` (ie `(@ foo 1)` matches the number `1` and binds it to `foo`)
    \\    + `(: predicate)`
    \\        > expect a value that satisfies the given `predicate`
    \\    + `(-> translator-function var*)`
    \\        > apply the given `translator-function` to the argument, and bind the results to the given `var`s if any are present;
    \\        > translator function can `(prompt fail)` to reject the match
    \\    + `symbol`
    \\        > bind any value to the given `symbol`
    \\    + `'symbol`
    \\        > expect a literal `symbol`
    \\    + expect an exact match to the given atom:
    \\        * `int` (ie `1`, `9900`, etc)
    \\        * `float` (ie `1.`, `3.14e-2`, etc)
    \\        * `char` (ie `'a'`, `'\n'`, etc)
    \\        * `string` (ie `"hello"`, `"\x00\""`, etc)
    \\        * `bool` (ie `true`, `false`)
    \\
    \\Additionally, repeated binding symbols (ie `(a a)`) are allowed, and will have their bound values checked for equality with `eq?`
    \\
    \\> ##### Example
    \\> ```lisp
    \\> (assert-eq
    \\>     (run-lambda-list (a b) '(1 2))
    \\>     '((b . 2) (a . 1)))
    \\>
    \\> (assert-eq
    \\>     (with ((fun fail () (terminate ())))
    \\>         (f-lambda-list (a b) '(1)))
    \\>     ())
    \\>
    \\> (assert-eq
    \\>     (run-lambda-list (@ a 1) 1)
    \\>     '((a . 1)))
    \\>
    \\> (assert-eq
    \\>     (run-lambda-list 1 1)
    \\>     ())
    \\>
    \\> (assert-eq '((x . (2 3)))
    \\>     (run-lambda-list
    \\>         (-> (lambda (x) (f-assert-eq x 2) (list x 3)) . x)
    \\>         2))
    \\>
    \\> (assert-eq 'okay
    \\>     (with ((fun fail () (terminate 'okay)))
    \\>         (f-lambda-list
    \\>             (-> (lambda (x) (f-assert-eq x 1) (list x)) a)
    \\>             2)))
    \\>
    \\> (assert-eq '((x . (1 . 2)))
    \\>     (run-lambda-list
    \\>         (@ x (: pair?))
    \\>         '(1 . 2)))
    \\>
    \\> (assert-eq 'okay
    \\>     (with ((fun fail () (terminate 'okay)))
    \\>         (f-lambda-list
    \\>             (@ x (: pair?))
    \\>             1)))
    \\> ```
;

pub const Env = .{
    .{ "validate-lambda-list", "given a lambda list, returns a boolean indicating whether it is valid", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const llist = try eval.resolve1(args);
            if (Eval.LambdaListRich.validate(eval, llist)) |_| {
                return try SExpr.Bool(at, true);
            } else |_| {
                return try SExpr.Bool(at, false);
            }
        }
    } },
    .{ "lambda-list-binders", "given a lambda list, returns a list of the symbols that will be bound if it is successfully run on an input", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const llist = try eval.resolve1(args);
            const binders = try Eval.LambdaListLite.binders(eval, at, llist);
            defer eval.context.allocator.free(binders);
            return SExpr.List(at, binders);
        }
    } },
    .{ "f-lambda-list", "given a lambda list and an input, returns an env frame binding the symbols of the list to the values of the input, or prompts fail", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const buf = try eval.expect2(args);
            const llist = buf[0];
            const largs = try eval.resolve(buf[1]);
            const result = try Eval.LambdaListLite.run(eval, at, llist, largs);
            switch (result) {
                .Okay => |env| return env,
                .Error => return eval.nativePrompt(at, "fail", &[0]SExpr{}),
            }
        }
    } },
    .{ "e-lambda-list", "given a lambda list and an input, returns an env frame binding the symbols of the list to the values of the input, or prompts an exception on failure", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const buf = try eval.expect2(args);
            const llist = buf[0];
            const largs = try eval.resolve(buf[1]);
            const result = try Eval.LambdaListRich.run(eval, at, llist, largs);
            switch (result) {
                .Okay => |env| return env,
                .Error => |err| {
                    const msg = try std.fmt.allocPrint(eval.context.allocator, "{}", .{err});
                    return eval.nativePrompt(at, "exception", &[1]SExpr{try SExpr.StringPreallocated(at, msg)});
                },
            }
        }
    } },
    .{ "run-lambda-list", "given a lambda list and an input, returns an env frame binding the symbols of the list to the values of the input, or causes a compile time error on failure", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const buf = try eval.expect2(args);
            const llist = buf[0];
            const largs = try eval.resolve(buf[1]);
            const result = try Eval.LambdaListRich.run(eval, at, llist, largs);
            switch (result) {
                .Okay => |env| return env,
                .Error => |err| return eval.abort(err.err, err.attr orelse at, "lambda list failed: {s}", .{err.msg orelse "no message provided"}),
            }
        }
    } },
};
