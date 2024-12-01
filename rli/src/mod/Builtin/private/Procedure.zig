const std = @import("std");

const MiscUtils = @import("Utils").Misc;

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Interpreter = Core.Interpreter;

pub const Doc =
    \\This module provides the primitive `lambda` and `macro` special forms,
    \\which can be used anywhere to create closure-binding procedural abstractions.
    \\
    \\The only difference between `lambda` and `macro` is the timing of evaluation:
    \\+ `lambda` evaluates its arguments at the time of invocation,
    \\  and evaluates its return value in its own environment.
    \\+ `macro` does not evaluate its arguments,
    \\  and evaluates its return value in the environment of the caller.
    \\
    \\> ##### Example
    \\> ```lisp
    \\> (lambda (x y) (+ x y))
    \\> ```
    \\> ```lisp
    \\> (macro (x y) `(+ ,x ,y))
    \\> ```
;

pub const Env = .{
    .{ "lambda", "inline function definition", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            return function(interpreter, at, .Lambda, args);
        }
    } },
    .{ "macro", "inline macro definition", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            return function(interpreter, at, .Macro, args);
        }
    } },
    .{ "apply", "apply a function to a list of arguments", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.eval2(args);
            return try interpreter.nativeInvoke(at, rargs[0], rargs[1]);
        }
    } },
};

pub fn function(interpreter: *Interpreter, at: *const Source.Attr, kind: SExpr.Types.Function.Kind, args: SExpr) Interpreter.Result!SExpr {
    const rargs = try interpreter.expectAtLeast1(args);
    try Interpreter.LambdaListRich.validate(interpreter, rargs.head);
    return try SExpr.Function(at, kind, rargs.head, interpreter.env, rargs.tail);
}
