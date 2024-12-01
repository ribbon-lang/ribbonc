const std = @import("std");

const Procedure = @import("Builtin:Procedure");

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Interpreter = Core.Interpreter;

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
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const res = try interpreter.expectAtLeast1(args);
            const name = res.head;
            try interpreter.validateSymbol(at, name);
            const value = value: {
                const baseEnv = interpreter.env;
                try Interpreter.pushNewFrame(at, &interpreter.env);
                defer interpreter.env = baseEnv;
                break :value try interpreter.runProgram(res.tail);
            };
            try Interpreter.extendEnvFrame(at, name, value, interpreter.env);
            return value;
        }
    } },
    .{ "def-fun", "define a new function", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const res = try interpreter.expectAtLeast1(args);
            const name = res.head;
            try interpreter.validateSymbol(at, name);
            const body = res.tail;
            const func = try Procedure.function(interpreter, at, .Lambda, body);
            try Interpreter.extendEnvFrame(at, name, func, interpreter.env);
            return func;
        }
    } },
    .{ "def-macro", "define a new macro function", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const res = try interpreter.expectAtLeast1(args);
            const name = res.head;
            try interpreter.validateSymbol(at, name);
            const body = res.tail;
            const func = try Procedure.function(interpreter, at, .Macro, body);
            try Interpreter.extendEnvFrame(at, name, func, interpreter.env);
            return func;
        }
    } },
    .{ "let", "create local value bindings", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const res = try interpreter.expectAtLeast1(args);
            const defs = res.head;
            const body = res.tail;
            const baseEnv = interpreter.env;
            try Interpreter.pushNewFrame(at, &interpreter.env);
            defer interpreter.env = baseEnv;
            try bindDefs(interpreter, defs, struct {
                fn fun(interpreter2: *Interpreter, name: SExpr, obj: SExpr) Interpreter.Result!void {
                    try Interpreter.extendEnvFrame(name.getAttr(), name, obj, interpreter2.env);
                }
            }.fun);
            return try interpreter.runProgram(body);
        }
    } },
    .{ "bound?", "check if a given symbol is bound in the current env", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const symbol = try interpreter.expect1(args);
            try interpreter.validateSymbol(at, symbol);
            return try SExpr.Bool(at, try Interpreter.envLookupPair(symbol, interpreter.env) != null);
        }
    } },
    .{ "set!", "set the value of a symbol in the current env. symbol must already be bound. returns old value", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const buf = try interpreter.eval2(args);
            const symbol = buf[0];
            try interpreter.validateSymbol(at, symbol);
            const value = buf[1];
            if (try Interpreter.envLookupPair(symbol, interpreter.env)) |pair| {
                const xp = pair.forceCons();
                const out = xp.cdr;
                xp.cdr = value;
                return out;
            } else {
                return interpreter.abort(Interpreter.Error.UnboundSymbol, at, "cannot set unbound symbol `{}`", .{symbol});
            }
        }
    } },
};

pub const DefKind = enum {
    Fun,
    Macro,
    Var,

    pub fn matchSymbol(interpreter: *Interpreter, kindSymbol: SExpr) Interpreter.Result!DefKind {
        const kStr = try interpreter.castSymbolSlice(kindSymbol.getAttr(), kindSymbol);
        return if (std.mem.eql(u8, kStr, "fun")) .Fun else if (std.mem.eql(u8, kStr, "macro")) .Macro else if (std.mem.eql(u8, kStr, "var")) .Var else {
            return interpreter.abort(Interpreter.Error.TypeError, kindSymbol.getAttr(), "unknown binding kind `{}`, expected `fun`, `macro`, or `var`", .{kindSymbol});
        };
    }

    pub fn constructObject(self: DefKind, interpreter: *Interpreter, at: *const Source.Attr, def: SExpr) Interpreter.Result!SExpr {
        return switch (self) {
            .Fun => Procedure.function(interpreter, at, .Lambda, def),
            .Macro => Procedure.function(interpreter, at, .Macro, def),
            .Var => interpreter.runProgram(def),
        };
    }
};

pub fn bindDefs(interpreter: *Interpreter, defs: SExpr, comptime bind: fn (*Interpreter, SExpr, SExpr) Interpreter.Result!void) Interpreter.Result!void {
    var iter = try interpreter.argIterator(false, defs);

    while (try iter.next()) |info| {
        const res = try interpreter.expectAtLeast2(info);

        const kind = try DefKind.matchSymbol(interpreter, res.head[0]);

        const nameSymbol = res.head[1];
        try interpreter.validateSymbol(nameSymbol.getAttr(), nameSymbol);

        const obj = try kind.constructObject(interpreter, nameSymbol.getAttr(), res.tail);

        try bind(interpreter, nameSymbol, obj);
    }
}
