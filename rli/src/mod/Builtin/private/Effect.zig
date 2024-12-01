const std = @import("std");

const Binding = @import("Builtin:Binding");

const MiscUtils = @import("Utils").Misc;

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Eval = Core.Eval;

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
    \\> + `var` for simple variable bindings
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
    \\>        (macro error (x) (prompt abort (eval x)))
    \\>        (var abort2 terminate))
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
    .{ "with-global", "provide one or more named *top-level* effect handlers to serve as a last resort; note that the `terminate` which is provided to handlers bound this way will terminate compilation", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            _ = try bindDefs(eval, at, args, "global-terminator", terminator, bindGlobal);
            return SExpr.Nil(at);
        }
        fn bindGlobal(eval: *Eval, name: SExpr, eff: SExpr) Eval.Result!void {
            try Eval.extendFrame(name.getAttr(), name, eff, &eval.globalEvidence);
        }
        fn terminator(eval: *Eval, _: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const buf = try eval.expect3(args);
            const ctxId = buf[0];
            const promptName = buf[1];
            const value = try eval.resolve(buf[2]);
            return eval.abort(Eval.Error.Panic, ctxId.getAttr(), "global prompt `{}` terminated with:\n\t\t{}: {display}", .{ promptName, value.getAttr(), value });
        }
    } },
    .{ "with", "provide one or more named effect handlers, and an expression to execute under them", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const xp = try eval.castList(at, args);
            const defs = xp.car;
            const body = xp.cdr;
            const baseEv = eval.evidence;
            try Eval.pushNewFrame(at, &eval.evidence);
            defer eval.evidence = baseEv;
            const contextId = try bindDefs(eval, at, defs, "local-terminator", Eval.valueTerminator, bindLocal);
            return eval.runProgram(body) catch |res| {
                if (res == Eval.Signal.Terminate) {
                    const terminationData = eval.terminationData orelse {
                        return Eval.Error.MissingTerminationData;
                    };
                    if (MiscUtils.equal(terminationData.ctxId, contextId)) {
                        const out = terminationData.value;
                        eval.terminationData = null;
                        return out;
                    }
                }
                return res;
            };
        }
        fn bindLocal(eval: *Eval, name: SExpr, eff: SExpr) Eval.Result!void {
            try Eval.extendEnvFrame(name.getAttr(), name, eff, eval.evidence);
        }
    } },
    .{ "fetch", "get a dynamically bound variable or effect handler from its binding symbol", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const sym = try eval.expect1(args);
            try eval.validateSymbol(at, sym);
            return eval.liftFetch(at, sym);
        }
    } },
    .{ "prompt", "defer execution to a named effect handler; `(prompt sym args...)` is equivalent to `((fetch sym) args...)`", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const res = try eval.expectAtLeast1(args);
            const promptVal = res.head;
            try eval.validateSymbol(at, promptVal);
            return try eval.liftPrompt(at, promptVal, res.tail);
        }
    } },
};

fn bindDefs(eval: *Eval, at: *const Source.Attr, defs: SExpr, comptime terminatorName: []const u8, comptime terminator: fn (*Eval, *const Source.Attr, SExpr) Eval.Result!SExpr, comptime bind: fn (*Eval, SExpr, SExpr) Eval.Result!void) Eval.Result!SExpr {
    const contextId = try SExpr.Int(at, @intCast(eval.context.genId()));

    var iter = try eval.argIterator(false, defs);

    while (try iter.next()) |info| {
        const res = try eval.expectAtLeast2(info);

        const kind = try Binding.DefKind.matchSymbol(eval, res.head[0]);

        const nameSymbol = res.head[1];
        try eval.validateSymbol(nameSymbol.getAttr(), nameSymbol);

        const originalEnv = eval.env;
        var contextEnv = originalEnv;

        try Eval.pushNewFrame(at, &contextEnv);

        const terminateSym = try SExpr.Symbol(at, "terminate");
        const terminate = try Eval.wrapTerminator(eval, at, contextId, nameSymbol, terminatorName, terminator);
        try Eval.extendEnvFrame(at, terminateSym, terminate, contextEnv);

        eval.env = contextEnv;
        defer eval.env = originalEnv;

        const obj = try kind.constructObject(eval, nameSymbol.getAttr(), res.tail);

        try bind(eval, nameSymbol, obj);
    }

    return contextId;
}
