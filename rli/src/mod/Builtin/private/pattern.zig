const std = @import("std");

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Interpreter = Core.Interpreter;

pub const Doc =
    \\##### Supported pattern syntax
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
    \\>     (pattern/run (a b) '(1 2))
    \\>     '((b . 2) (a . 1)))
    \\>
    \\> (assert-eq
    \\>     (with ((fun fail () (terminate ())))
    \\>         (pattern/run-f (a b) '(1)))
    \\>     ())
    \\>
    \\> (assert-eq
    \\>     (pattern/run (@ a 1) 1)
    \\>     '((a . 1)))
    \\>
    \\> (assert-eq
    \\>     (pattern/run 1 1)
    \\>     ())
    \\>
    \\> (assert-eq '((x . (2 3)))
    \\>     (pattern/run
    \\>         (-> (lambda (x) (f-assert-eq x 2) (list x 3)) . x)
    \\>         2))
    \\>
    \\> (assert-eq 'okay
    \\>     (with ((fun fail () (terminate 'okay)))
    \\>         (pattern/run-f
    \\>             (-> (lambda (x) (f-assert-eq x 1) (list x)) a)
    \\>             2)))
    \\>
    \\> (assert-eq '((x . (1 . 2)))
    \\>     (pattern/run
    \\>         (@ x (: pair?))
    \\>         '(1 . 2)))
    \\>
    \\> (assert-eq 'okay
    \\>     (with ((fun fail () (terminate 'okay)))
    \\>         (pattern/run-f
    \\>             (@ x (: pair?))
    \\>             1)))
    \\> ```
;

pub const Env = .{
    .{ "pattern/validate", "given a pattern, returns a boolean indicating whether it is valid", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const llist = try interpreter.eval1(args);
            if (Interpreter.PatternRich.validate(interpreter, llist)) |_| {
                return try SExpr.Bool(at, true);
            } else |_| {
                return try SExpr.Bool(at, false);
            }
        }
    } },
    .{ "pattern/binders", "given a pattern, returns a list of the symbols that will be bound if it is successfully run on an input", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const llist = try interpreter.eval1(args);
            const binders = try Interpreter.PatternLite.binders(interpreter, at, llist);
            defer interpreter.context.allocator.free(binders);
            return SExpr.List(at, binders);
        }
    } },
    .{ "pattern/run-f", "given a pattern and an input, returns an env frame binding the symbols of the list to the values of the input, or prompts fail", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const buf = try interpreter.expect2(args);
            const llist = buf[0];
            const largs = try interpreter.eval(buf[1]);
            const result = try Interpreter.PatternLite.run(interpreter, at, llist, largs);
            switch (result) {
                .Okay => |env| return env,
                .Error => return interpreter.nativePrompt(at, "fail", &[0]SExpr{}),
            }
        }
    } },
    .{ "pattern/run-e", "given a pattern and an input, returns an env frame binding the symbols of the list to the values of the input, or prompts an exception on failure", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const buf = try interpreter.expect2(args);
            const llist = buf[0];
            const largs = try interpreter.eval(buf[1]);
            const result = try Interpreter.PatternRich.run(interpreter, at, llist, largs);
            switch (result) {
                .Okay => |env| return env,
                .Error => |err| {
                    const msg = try std.fmt.allocPrint(interpreter.context.allocator, "{}", .{err});
                    return interpreter.nativePrompt(at, "exception", &[1]SExpr{try SExpr.StringPreallocated(at, msg)});
                },
            }
        }
    } },
    .{ "pattern/run", "given a pattern and an input, returns an env frame binding the symbols of the list to the values of the input, or causes a compile time error on failure", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const buf = try interpreter.expect2(args);
            const llist = buf[0];
            const largs = try interpreter.eval(buf[1]);
            const result = try Interpreter.PatternRich.run(interpreter, at, llist, largs);
            switch (result) {
                .Okay => |env| return env,
                .Error => |err| return interpreter.abort(err.err, err.attr orelse at, "pattern failed: {s}", .{err.msg orelse "no message provided"}),
            }
        }
    } },
};
