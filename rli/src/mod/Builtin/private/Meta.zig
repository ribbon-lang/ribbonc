const std = @import("std");

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Interpreter = Core.Interpreter;

pub const Doc =
    \\This module contains functions for converting data to syntax,
    \\and for direct access and manipulation of the execution environments.
    \\
    \\See [Syntax](#syntax) for more information on the syntax of `quote` and `quasiquote`.
    \\
;

pub const Env = .{
    .{ "quasiquote", "a quote accepting `unquote` and `unquote-splicing` in its body", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const esc = try interpreter.expect1(args);
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
            const arg = try interpreter.expect1(args);
            return interpreter.eval(arg);
        }
    } },
    .{ "quote", "makes a given input into its literal equivalent by skipping an evaluation step", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            return interpreter.expect1(args);
        }
    } },

    .{ "eval", "evaluate a given expression in the current env or an optional provided env", struct {
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

    .{ "gensym", "generate a unique symbol", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            try interpreter.expect0(args);
            return try SExpr.GenSymbol(at);
        }
    } },

    .{ "env-keys", "get the names of all bindings in the given env", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const env = try interpreter.eval1(args);
            try interpreter.validateListOrNil(at, env);
            const keys = try Interpreter.envKeys(env, interpreter.context.allocator);
            defer interpreter.context.allocator.free(keys);
            return try SExpr.List(at, keys);
        }
    } },
    .{ "env-lookup", "lookup a key symbol in an environment, returning the value it binds; prompts `fail` if the key is not found", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.eval2(args);
            const key = rargs[0];
            try interpreter.validateSymbol(at, key);
            const env = rargs[1];
            try interpreter.validateListOrNil(at, env);
            const entry = Interpreter.envLookup(key, env) catch |err| {
                return interpreter.abort(err, at, "bad env: {}", .{env});
            };
            if (entry) |v| {
                return v;
            } else {
                return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "env-pair", "lookup a key symbol in an environment, returning the pair it binds; prompts `fail` if the key is not found", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.eval2(args);
            const key = rargs[0];
            try interpreter.validateSymbol(at, key);
            const env = rargs[1];
            try interpreter.validateListOrNil(at, env);
            const entry = Interpreter.envLookupPair(key, env) catch |err| {
                return interpreter.abort(err, at, "bad env: {}", .{env});
            };
            if (entry) |pair| {
                try interpreter.validatePair(pair.getAttr(), pair);
                return pair;
            } else {
                return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "env-set!", "set the value associated with a name in an environment, returning the old value; prompts `fail` if the name is not bound", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.eval3(args);
            const key = rargs[0];
            try interpreter.validateSymbol(at, key);
            const value = rargs[1];
            const env = rargs[2];
            try interpreter.validateListOrNil(at, env);
            const entry = Interpreter.envLookupPair(key, env) catch |err| {
                return interpreter.abort(err, at, "bad env: {}", .{env});
            };
            if (entry) |pair| {
                const xp = try interpreter.castPair(at, pair);
                const oldValue = xp.cdr;
                xp.cdr = value;
                return oldValue;
            } else {
                return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "env-put!", "append a key-value pair to the top frame of an environment", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.eval3(args);
            const key = rargs[0];
            try interpreter.validateSymbol(at, key);
            const value = rargs[1];
            const env = rargs[2];
            try interpreter.validateListOrNil(at, env);
            try Interpreter.extendEnvFrame(at, key, value, env);
            return env;
        }
    } },
    .{ "env-copy", "copy a given environment", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const newEnv = try interpreter.eval1(args);
            const outEnv = Interpreter.copyEnv(at, newEnv);
            return outEnv;
        }
    } },
    .{ "env-new", "make a new environment from a simple a-list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const frame = try interpreter.eval1(args);
            Interpreter.validateFrame(frame) catch |err| {
                return interpreter.abort(err, at, "bad frame: {}", .{frame});
            };
            const nil = try SExpr.Nil(at);
            return try SExpr.Cons(at, frame, nil);
        }
    } },
    .{ "env-get-frame", "get the environment frame at the given offset depth; prompts `fail` if the depth is out of bounds", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.eval2(args);
            const env = rargs[0];
            const frameOffsetI = try interpreter.coerceNativeInt(at, rargs[1]);
            if (frameOffsetI < 0) {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a non-negative number for frame offset, got {}", .{frameOffsetI});
            }
            const frameOffset: usize = @intCast(frameOffsetI);
            const frame = Interpreter.getFrame(frameOffset, env) catch |err| {
                if (err == Interpreter.Error.EnvironmentUnderflow) {
                    return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
                }
                return interpreter.abort(err, at, "bad env: {}", .{env});
            };
            return frame;
        }
    } },
    .{ "env-push", "push a given environment frame into the current environment, returning the modified enviroment", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.eval2(args);
            var env = rargs[0];
            const frame = rargs[1];
            Interpreter.validateFrame(frame) catch |err| {
                return interpreter.abort(err, at, "bad frame: {}", .{frame});
            };
            Interpreter.pushFrame(frame, &env) catch |err| {
                return interpreter.abort(err, at, "bad env: {}", .{env});
            };
            return env;
        }
    } },
    .{ "env-pop", "pop an environment frame off the current environment, returning it and the modified environment as a pair `(frame . env)`; prompts `fail` if the environment is empty", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var env = try interpreter.eval1(args);
            const frame = Interpreter.popFrame(&env) catch |err| {
                if (err == Interpreter.Error.EnvironmentUnderflow) {
                    return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
                }
                return interpreter.abort(err, at, "bad env: {}", .{env});
            };
            return SExpr.Cons(at, frame, env);
        }
    } },

    .{ "swap-env", "replace the current environment with the given one, returning the old one; optionally accepts `'self` `'caller` or `'evidence` symbols indicating which environment to effect", struct {
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
    .{ "take-env", "take the current environment, leaving it empty; optionally accepts `'self` `'caller` or `'evidence` symbols indicating which environment to effect", struct {
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
    .{ "get-env", "take a copy of the current environment, leaving it in place; optionally accepts `'self` `'caller` or `'evidence` symbols indicating which environment to effect", struct {
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
    .{ "replace-env", "replace the current environment with the given one; optionally accepts `'self` `'caller` or `'evidence` symbols indicating which environment to effect", struct {
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

    .{ "get-global-evidence", "get the global evidence environment frame", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            try interpreter.expect0(args);
            return interpreter.globalEvidence;
        }
    } },

    .{ "set-global-evidence", "set the global evidence environment frame", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const newEv = try interpreter.eval1(args);
            Interpreter.validateFrame(newEv) catch |err| {
                return interpreter.abort(err, args.getAttr(), "bad frame: {}", .{newEv});
            };
            interpreter.globalEvidence = newEv;
            return newEv;
        }
    } },
};
