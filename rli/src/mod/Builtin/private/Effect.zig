const std = @import("std");

const Binding = @import("Builtin:Binding");

const MiscUtils = @import("Utils").Misc;

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Interpreter = Core.Interpreter;

pub const Doc =
    \\This module provides an api to trigger and handle
    \\arbitrary user-defined side effects at compile time.
    \\The special forms `with` and `with-global` provide a `let`-like syntax for binding
    \\functions, macros, and variables to symbols in a special dynamic environment,
    \\which can be accessed with the `fetch` and `prompt` special forms.
    \\
    \\The three kinds of bindings are discriminated
    \\by a keyword at the head of the binding:
    \\> ```lisp
    \\> (with ((kind name def)...) body...)
    \\> (with-global (kind name def)...)
    \\> ```
    \\> Where `kind` is one of:
    \\> + `fun` for lambda-like effect handlers
    \\> + `macro` for macro-like effect handlers
    \\> + [none] simple variable bindings
    \\
    \\Values created via `with` and `with-global` are provided a special binding, `terminate`,
    \\which can be called to cancel the inner computation of
    \\the `with` they are bound to, and return a value in its place.
    \\> [!Caution]
    \\> In the case of terminate being called from a `with-global` binding,
    \\> the computation being terminated is the entire compilation.
    \\
    \\> [!Caution]
    \\> Macros calling `terminate` will need to manually evaluate
    \\> their termination value if it requires it;
    \\> all termination values are passed as-is
    \\
    \\> ##### Example
    \\> ```lisp
    \\> (with ((fun abort (x) (terminate x))
    \\>        (macro error (x) (prompt abort (interpreter x)))
    \\>        (abort2 terminate))
    \\>   (action1)
    \\>   (action2))
    \\> ```
    \\
    \\Some builtin effects can also be handled via `with`/`with-global`;
    \\for example `exception` and `fail`, which are triggered by some builtins.
    \\
    \\Items bound this way can be accessed with `prompt` and `fetch`:
    \\+ `fetch` simply retrieves the value
    \\+ `prompt` retrieves and then invokes the value with a given list of arguments
    \\
;


pub const Env = .{
    .{ "with-global", "provide one or more named *top-level* effect handlers to serve as a last resort; note that the `terminate` which is provided to handlers bound this way will terminate the interpreter", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            _ = try bindDefs(interpreter, at, args, "global-terminator", terminator, bindGlobal);
            return SExpr.Nil(at);
        }
        fn bindGlobal(interpreter: *Interpreter, name: SExpr, eff: SExpr) Interpreter.Result!void {
            try Interpreter.extendFrame(name.getAttr(), name, eff, &interpreter.globalEvidence);
        }
        fn terminator(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const buf = try interpreter.expect3(args);
            const ctxId = buf[0];
            const promptName = buf[1];
            const value = try interpreter.eval(buf[2]);
            return interpreter.abort(Interpreter.Error.Panic, ctxId.getAttr(), "global prompt `{}` terminated with:\n\t\t{}: {display}", .{ promptName, value.getAttr(), value });
        }
    } },
    .{ "with", "provide one or more named effect handlers, and an expression to execute under them", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const xp = try interpreter.castList(at, args);
            const defs = xp.car;
            const body = xp.cdr;
            const baseEv = interpreter.evidence;
            try Interpreter.pushNewFrame(at, &interpreter.evidence);
            defer interpreter.evidence = baseEv;
            const contextId = try bindDefs(interpreter, at, defs, "local-terminator", Interpreter.valueTerminator, bindLocal);
            return interpreter.runProgram(body) catch |res| {
                if (res == Interpreter.Signal.Terminate) {
                    const terminationData = interpreter.terminationData orelse {
                        return Interpreter.Error.MissingTerminationData;
                    };
                    if (MiscUtils.equal(terminationData.ctxId, contextId)) {
                        const out = terminationData.value;
                        interpreter.terminationData = null;
                        return out;
                    }
                }
                return res;
            };
        }
        fn bindLocal(interpreter: *Interpreter, name: SExpr, eff: SExpr) Interpreter.Result!void {
            try Interpreter.extendEnvFrame(name.getAttr(), name, eff, interpreter.evidence);
        }
    } },
    .{ "fetch", "get a dynamically bound variable or effect handler from its binding symbol", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const sym = try interpreter.expect1(args);
            try interpreter.validateSymbol(at, sym);
            return interpreter.liftFetch(at, sym);
        }
    } },
    .{ "prompt", "defer execution to a named effect handler; `(prompt sym args...)` is equivalent to `((fetch sym) args...)`", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const res = try interpreter.expectAtLeast1(args);
            const promptVal = res.head;
            try interpreter.validateSymbol(at, promptVal);
            return try interpreter.liftPrompt(at, promptVal, res.tail);
        }
    } },
};

fn bindDefs(interpreter: *Interpreter, at: *const Source.Attr, defs: SExpr, comptime terminatorName: []const u8, comptime terminator: fn (*Interpreter, *const Source.Attr, SExpr) Interpreter.Result!SExpr, comptime bind: fn (*Interpreter, SExpr, SExpr) Interpreter.Result!void) Interpreter.Result!SExpr {
    const contextId = try SExpr.Int(at, @intCast(interpreter.context.genId()));

    var iter = try interpreter.argIterator(false, defs);

    while (try iter.next()) |info| {
        var res = try interpreter.expectAtLeast1(info);

        const kind = try Binding.DefKind.matchSymbol(interpreter, res.head);
        if (kind != .Var) {
            res = try interpreter.expectAtLeast1(res.tail);
        }

        const nameSymbol = res.head;
        try interpreter.validateSymbol(nameSymbol.getAttr(), nameSymbol);

        const originalEnv = interpreter.env;
        var contextEnv = originalEnv;

        try Interpreter.pushNewFrame(at, &contextEnv);

        const terminateSym = try SExpr.Symbol(at, "terminate");
        const terminate = try Interpreter.wrapTerminator(interpreter, at, contextId, nameSymbol, terminatorName, terminator);
        try Interpreter.extendEnvFrame(at, terminateSym, terminate, contextEnv);

        interpreter.env = contextEnv;
        defer interpreter.env = originalEnv;

        const obj = try kind.constructObject(interpreter, nameSymbol.getAttr(), res.tail);

        try bind(interpreter, nameSymbol, obj);
    }

    return contextId;
}
