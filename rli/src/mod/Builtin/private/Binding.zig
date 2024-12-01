const std = @import("std");

const Procedure = @import("Builtin:Procedure");

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Eval = Core.Eval;

pub const Doc =
    \\This module provides special forms for defining and manipulating
    \\bindings in the current environment.
    \\
    \\`let` creates a new environment frame with the given bindings,
    \\and evaluates the body in that frame.
    \\> ##### Example
    \\> ```lisp
    \\> (let ((var x (+ 1 1))
    \\>       (fun f (x) (+ x 1))
    \\>       (macro m (x) `(f ,x)))
    \\>   (action1 x)
    \\>   (action2 (f x))
    \\>   (action3 (m x)))
    \\> ```
    \\
    \\`def-var`, `def-fun`, and `def-macro` forms mirror the syntax of `let`,
    \\but bind individual symbols in the current environment.
    \\> ##### Example
    \\> ```lisp
    \\> (def-var x (+ 1 1))
    \\> (action x)
    \\> ```
    \\
    \\`bound?` and `set!` can be used to query and manipulate
    \\bindings created with any of the above forms.
    \\
;

pub const Env = .{
    .{ "def-var", "define a new variable", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const res = try eval.expectAtLeast1(args);
            const name = res.head;
            try eval.validateSymbol(at, name);
            const value = value: {
                const baseEnv = eval.env;
                try Eval.pushNewFrame(at, &eval.env);
                defer eval.env = baseEnv;
                break :value try eval.runProgram(res.tail);
            };
            try Eval.extendEnvFrame(at, name, value, eval.env);
            return value;
        }
    } },
    .{ "def-fun", "define a new function", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const res = try eval.expectAtLeast1(args);
            const name = res.head;
            try eval.validateSymbol(at, name);
            const body = res.tail;
            const func = try Procedure.function(eval, at, .Lambda, body);
            try Eval.extendEnvFrame(at, name, func, eval.env);
            return func;
        }
    } },
    .{ "def-macro", "define a new macro function", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const res = try eval.expectAtLeast1(args);
            const name = res.head;
            try eval.validateSymbol(at, name);
            const body = res.tail;
            const func = try Procedure.function(eval, at, .Macro, body);
            try Eval.extendEnvFrame(at, name, func, eval.env);
            return func;
        }
    } },
    .{ "let", "create local value bindings", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const res = try eval.expectAtLeast1(args);
            const defs = res.head;
            const body = res.tail;
            const baseEnv = eval.env;
            try Eval.pushNewFrame(at, &eval.env);
            defer eval.env = baseEnv;
            try bindDefs(eval, defs, struct {
                fn fun(eval2: *Eval, name: SExpr, obj: SExpr) Eval.Result!void {
                    try Eval.extendEnvFrame(name.getAttr(), name, obj, eval2.env);
                }
            }.fun);
            return try eval.runProgram(body);
        }
    } },
    .{ "bound?", "check if a given symbol is bound in the current env", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const symbol = try eval.expect1(args);
            try eval.validateSymbol(at, symbol);
            return try SExpr.Bool(at, try Eval.envLookupPair(symbol, eval.env) != null);
        }
    } },
    .{ "set!", "set the value of a symbol in the current env. symbol must already be bound. returns old value", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const buf = try eval.resolve2(args);
            const symbol = buf[0];
            try eval.validateSymbol(at, symbol);
            const value = buf[1];
            if (try Eval.envLookupPair(symbol, eval.env)) |pair| {
                const xp = pair.forceCons();
                const out = xp.cdr;
                xp.cdr = value;
                return out;
            } else {
                return eval.abort(Eval.Error.UnboundSymbol, at, "cannot set unbound symbol `{}`", .{symbol});
            }
        }
    } },
};

pub const DefKind = enum {
    Fun,
    Macro,
    Var,

    pub fn matchSymbol(eval: *Eval, kindSymbol: SExpr) Eval.Result!DefKind {
        const kStr = try eval.castSymbolSlice(kindSymbol.getAttr(), kindSymbol);
        return if (std.mem.eql(u8, kStr, "fun")) .Fun else if (std.mem.eql(u8, kStr, "macro")) .Macro else if (std.mem.eql(u8, kStr, "var")) .Var else {
            return eval.abort(Eval.Error.TypeError, kindSymbol.getAttr(), "unknown binding kind `{}`, expected `fun`, `macro`, or `var`", .{kindSymbol});
        };
    }

    pub fn constructObject(self: DefKind, eval: *Eval, at: *const Source.Attr, def: SExpr) Eval.Result!SExpr {
        return switch (self) {
            .Fun => Procedure.function(eval, at, .Lambda, def),
            .Macro => Procedure.function(eval, at, .Macro, def),
            .Var => eval.runProgram(def),
        };
    }
};

pub fn bindDefs(eval: *Eval, defs: SExpr, comptime bind: fn (*Eval, SExpr, SExpr) Eval.Result!void) Eval.Result!void {
    var iter = try eval.argIterator(false, defs);

    while (try iter.next()) |info| {
        const res = try eval.expectAtLeast2(info);

        const kind = try DefKind.matchSymbol(eval, res.head[0]);

        const nameSymbol = res.head[1];
        try eval.validateSymbol(nameSymbol.getAttr(), nameSymbol);

        const obj = try kind.constructObject(eval, nameSymbol.getAttr(), res.tail);

        try bind(eval, nameSymbol, obj);
    }
}
