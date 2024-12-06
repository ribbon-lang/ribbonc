const std = @import("std");

const Rli = @import("../root.zig");
const Source = Rli.Source;
const SExpr = Rli.SExpr;
const Interpreter = Rli.Interpreter;

pub const Doc =
    \\This module contains functions for converting data to syntax,
    \\and for direct access and manipulation of the execution environments.
    \\
    \\See [Syntax](#syntax) for more information on the syntax of `quote` and `quasiquote`.
    \\
;

pub const Decls = .{
    .{ "meta/apply", "apply a function to a list of arguments", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalN(2, args);
            return try interpreter.nativeInvoke(at, rArgs[0], rArgs[1]);
        }
    } },
    .{ "quasiquote", "a quote accepting `unquote` and `unquote-splicing` in its body", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const esc = (try interpreter.expectN(1, args))[0];
            const res = try impl(interpreter, esc);
            if (res.mode != .Value) {
                return interpreter.abort(Interpreter.Error.TypeError, esc.getAttr(), "expected a value, got {s}", .{res.mode});
            }
            return res.value;
        }
        const Mode = enum {
            Value,
            Append,
            Splice,
            pub fn format(self: Mode, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                switch (self) {
                    .Value => try writer.writeAll("Value"),
                    .Append => try writer.writeAll("unquote"),
                    .Splice => try writer.writeAll("unquote-splicing"),
                }
            }
        };
        const ImplResult = struct {
            mode: Mode,
            value: SExpr,
        };
        fn Value(value: SExpr) ImplResult {
            return ImplResult{ .mode = .Value, .value = value };
        }
        fn Append(value: SExpr) ImplResult {
            return ImplResult{ .mode = .Append, .value = value };
        }
        fn Splice(value: SExpr) ImplResult {
            return ImplResult{ .mode = .Splice, .value = value };
        }
        fn handleResult(interpreter: *Interpreter, list: *std.ArrayList(SExpr), res: ImplResult) Interpreter.Result!void {
            switch (res.mode) {
                .Splice => {
                    var it = try interpreter.argIterator(false, res.value);
                    while (try it.next()) |value| {
                        try list.append(value);
                    }
                },
                else => try list.append(res.value),
            }
        }
        fn impl(interpreter: *Interpreter, sexpr: SExpr) Interpreter.Result!ImplResult {
            if (sexpr.castCons()) |cons| {
                if (cons.car.isExactSymbol("unquote")) {
                    return Append(try unquote(interpreter, cons.attr, cons.cdr));
                } else if (cons.car.isExactSymbol("unquote-splicing")) {
                    return Splice(try unquote(interpreter, cons.attr, cons.cdr));
                } else if (cons.car.isExactSymbol("quote")) {
                    return Value(sexpr);
                }
            } else {
                return Value(sexpr);
            }
            var newList = std.ArrayList(SExpr).init(interpreter.context.allocator);
            defer newList.deinit();
            var it = sexpr;
            while (!it.isNil()) {
                var cons = it.castCons() orelse {
                    return Value(try SExpr.ListTail(sexpr.getAttr(), newList.items, it));
                };
                if (cons.car.isExactSymbol("unquote")) {
                    const tail = try unquote(interpreter, cons.attr, cons.cdr);
                    return Value(try SExpr.ListTail(sexpr.getAttr(), newList.items, tail));
                } else if (cons.car.isExactSymbol("unquote-splicing")) {
                    const tail = try unquote(interpreter, cons.attr, cons.cdr);
                    return Value(try SExpr.ListTail(sexpr.getAttr(), newList.items, tail));
                } else if (cons.car.isExactSymbol("quote")) {
                    try handleResult(interpreter, &newList, Value(it));
                    break;
                } else {
                    try handleResult(interpreter, &newList, try impl(interpreter, cons.car));
                }
                it = cons.cdr;
            }
            return Value(try SExpr.List(sexpr.getAttr(), newList.items));
        }
        fn unquote(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.expectN(1, args))[0];
            return interpreter.eval(arg);
        }
    } },
    .{ "quote", "makes a given input into its literal equivalent by skipping an evaluation step", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            return (try interpreter.expectN(1, args))[0];
        }
    } },
    .{ "to-quote", "evaluates its input, then wraps it in a quote", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            return SExpr.Quote((try interpreter.evalN(1, args))[0]);
        }
    } },

    .{ "meta/eval", "evaluate a given expression in the current env or an optional provided env", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var buf = [2]SExpr{ undefined, undefined };
            const len = try interpreter.evalSmallList(args, 1, &buf);
            if (len == 1) {
                return try interpreter.eval(buf[0]);
            } else {
                const oldEnv = interpreter.env;
                interpreter.env = buf[1];
                defer interpreter.env = oldEnv;
                return try interpreter.eval(buf[0]);
            }
        }
    } },

    .{ "meta/gensym", "generate a unique symbol", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            try interpreter.expect0(args);
            return try SExpr.GenSymbol(at);
        }
    } },



    .{ "meta/swap-env", "replace the current environment with the given one, returning the old one; optionally accepts `'self` `'caller` or `'evidence` symbols indicating which environment to effect", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rargs = [2]SExpr{ undefined, undefined };
            const len = try interpreter.evalSmallList(args, 1, &rargs);
            const newEnv = rargs[0];
            try Interpreter.validateEnv(newEnv);
            if (len == 1 or rargs[1].isExactSymbol("self")) {
                const outEnv = interpreter.env;
                interpreter.env = newEnv;
                return outEnv;
            } else {
                const which = rargs[1];
                if (which.isExactSymbol("caller")) {
                    const outEnv = interpreter.callerEnv;
                    interpreter.callerEnv = newEnv;
                    return outEnv;
                } if (which.isExactSymbol("evidence")) {
                    const outEnv = interpreter.evidence;
                    interpreter.evidence = newEnv;
                    return outEnv;
                } else {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "expected `self` or `parent`, got {}", .{which});
                }
            }
        }
    } },
    .{ "meta/take-env", "take the current environment, leaving it empty; optionally accepts `'self` `'caller` or `'evidence` symbols indicating which environment to effect", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.evalMaybe1(args);
            const newEnv = try SExpr.Nil(at);
            if (arg) |which| {
                if (which.isExactSymbol("caller")) {
                    const outEnv = interpreter.callerEnv;
                    interpreter.callerEnv = newEnv;
                    return outEnv;
                } else if (which.isExactSymbol("evidence")) {
                    const outEnv = interpreter.evidence;
                    interpreter.evidence = newEnv;
                    return outEnv;
                } else if (!which.isExactSymbol("self")) {
                    return interpreter.abort(Interpreter.Error.TypeError, which.getAttr(), "expected `self` or `parent`, got {}", .{which});
                }
            }
            const outEnv = interpreter.env;
            interpreter.env = newEnv;
            return outEnv;
        }
    } },
    .{ "meta/get-env", "take a copy of the current environment, leaving it in place; optionally accepts `'self` `'caller` or `'evidence` symbols indicating which environment to effect", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.evalMaybe1(args);
            if (arg) |which| {
                if (which.isExactSymbol("caller")) {
                    return interpreter.callerEnv;
                } else if (which.isExactSymbol("evidence")) {
                    return interpreter.evidence;
                } else if (!which.isExactSymbol("self")) {
                    return interpreter.abort(Interpreter.Error.TypeError, which.getAttr(), "expected `self` or `parent`, got {}", .{which});
                }
            }
            return interpreter.env;
        }
    } },
    .{ "meta/replace-env", "replace the current environment with the given one; optionally accepts `'self` `'caller` or `'evidence` symbols indicating which environment to effect", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rargs = [2]SExpr{ undefined, undefined };
            const len = try interpreter.evalSmallList(args, 1, &rargs);
            const newEnv = rargs[0];
            try Interpreter.validateEnv(newEnv);
            if (len == 1 or rargs[1].isExactSymbol("self")) {
                interpreter.env = newEnv;
            } else {
                const which = rargs[1];
                if (which.isExactSymbol("caller")) {
                    interpreter.callerEnv = newEnv;
                } else if (which.isExactSymbol("evidence")) {
                    interpreter.evidence = newEnv;
                } else {
                    return interpreter.abort(Interpreter.Error.TypeError, which.getAttr(), "expected `self` or `parent`, got {}", .{which});
                }
            }
            return try SExpr.Nil(at);
        }
    } },

    .{ "meta/get-global-evidence", "get the global evidence environment frame", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            try interpreter.expect0(args);
            return interpreter.globalEvidence;
        }
    } },

    .{ "meta/set-global-evidence", "set the global evidence environment frame", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const newEv = (try interpreter.evalN(1, args))[0];
            Interpreter.validateFrame(newEv) catch |err| {
                return interpreter.abort(err, args.getAttr(), "bad frame: {}", .{newEv});
            };
            interpreter.globalEvidence = newEv;
            return newEv;
        }
    } },

    .{ "meta/ls", "shortcut for `(each (env/keys (meta/get-env)) (key . val) (print-ln key \" : \" (type/of val)))`; optionally accepts `'self` `'caller` or `'evidence` symbols indicating which environment to list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var env = interpreter.env;

            if (try interpreter.evalMaybe1(args)) |which| {
                if (which.isExactSymbol("caller")) {
                   env = interpreter.callerEnv;
                } else if (which.isExactSymbol("evidence")) {
                   env = interpreter.evidence;
                } else {
                    if (!which.isExactSymbol("self")) {
                        return interpreter.abort(Interpreter.Error.TypeError, args.getAttr(), "expected `self` or `parent`, got {}", .{which});
                    }
                }
            }


            const writer = std.io.getStdOut().writer();

            var envIt = env.iter();

            while (try envIt.next()) |frame| {
                var frameIt = frame.iter();
                while (try frameIt.next()) |pair| {
                    const xp = SExpr.castCons(pair) orelse return error.TypeError;
                    const key = xp.car;
                    const val = xp.cdr;
                    const typeName = try SExpr.Symbol(at, if (val.castExternData()) |ext| ext.typeNameSlice() else val.getTag().toSlice());
                    writer.print("{} : {s}\n", .{key, typeName}) catch |err| {
                        return interpreter.errorToException(at, err);
                    };
                }
            }

            writer.writeByte('\n') catch |err| {
                return interpreter.errorToException(at, err);
            };

            return try SExpr.Nil(at);
        }
    } },
};
