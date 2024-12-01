const std = @import("std");

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Eval = Core.Eval;

pub const Doc =
    \\This module contains functions for converting data to syntax,
    \\and for direct access and manipulation of the execution environments.
    \\
    \\See [Syntax](#syntax) for more information on the syntax of `quote` and `quasiquote`.
    \\
;

pub const Env = .{
    .{ "quasiquote", "a quote accepting `unquote` and `unquote-splicing` in its body", struct {
        pub fn fun(eval: *Eval, _: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const esc = try eval.expect1(args);
            const res = try impl(eval, esc);
            if (res.mode != .Value) {
                return eval.abort(Eval.Error.TypeError, esc.getAttr(), "expected a value, got {s}", .{res.mode});
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
        fn handleResult(eval: *Eval, list: *std.ArrayList(SExpr), res: ImplResult) Eval.Result!void {
            switch (res.mode) {
                .Splice => {
                    var it = try eval.argIterator(false, res.value);
                    while (try it.next()) |value| {
                        try list.append(value);
                    }
                },
                else => try list.append(res.value),
            }
        }
        fn impl(eval: *Eval, sexpr: SExpr) Eval.Result!ImplResult {
            if (sexpr.castCons()) |cons| {
                if (cons.car.isExactSymbol("unquote")) {
                    return Append(try unquote(eval, cons.attr, cons.cdr));
                } else if (cons.car.isExactSymbol("unquote-splicing")) {
                    return Splice(try unquote(eval, cons.attr, cons.cdr));
                } else if (cons.car.isExactSymbol("quote")) {
                    return Value(sexpr);
                }
            } else {
                return Value(sexpr);
            }
            var newList = std.ArrayList(SExpr).init(eval.context.allocator);
            defer newList.deinit();
            var it = sexpr;
            while (!it.isNil()) {
                var cons = it.castCons() orelse {
                    return Value(try SExpr.ListTail(sexpr.getAttr(), newList.items, it));
                };
                if (cons.car.isExactSymbol("unquote")) {
                    const tail = try unquote(eval, cons.attr, cons.cdr);
                    return Value(try SExpr.ListTail(sexpr.getAttr(), newList.items, tail));
                } else if (cons.car.isExactSymbol("unquote-splicing")) {
                    const tail = try unquote(eval, cons.attr, cons.cdr);
                    return Value(try SExpr.ListTail(sexpr.getAttr(), newList.items, tail));
                } else if (cons.car.isExactSymbol("quote")) {
                    try handleResult(eval, &newList, Value(it));
                    break;
                } else {
                    try handleResult(eval, &newList, try impl(eval, cons.car));
                }
                it = cons.cdr;
            }
            return Value(try SExpr.List(sexpr.getAttr(), newList.items));
        }
        fn unquote(eval: *Eval, _: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.expect1(args);
            return eval.resolve(arg);
        }
    } },
    .{ "quote", "makes a given input into its literal equivalent by skipping an evaluation step", struct {
        pub fn fun(eval: *Eval, _: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            return eval.expect1(args);
        }
    } },

    .{ "eval", "evaluate a given expression in the current env or an optional provided env", struct {
        pub fn fun(eval: *Eval, _: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var buf = [2]SExpr{ undefined, undefined };
            const len = try eval.resolveSmallList(args, 1, &buf);
            if (len == 1) {
                return try eval.resolve(buf[0]);
            } else {
                const oldEnv = eval.env;
                eval.env = buf[1];
                defer eval.env = oldEnv;
                return try eval.resolve(buf[0]);
            }
        }
    } },

    .{ "gensym", "generate a unique symbol", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            try eval.expect0(args);
            return try SExpr.GenSymbol(at);
        }
    } },

    .{ "env-keys", "get the names of all bindings in the given env", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const env = try eval.resolve1(args);
            try eval.validateListOrNil(at, env);
            const keys = try Eval.envKeys(env, eval.context.allocator);
            defer eval.context.allocator.free(keys);
            return try SExpr.List(at, keys);
        }
    } },
    .{ "env-lookup", "lookup a key symbol in an environment, returning the value it binds; prompts `fail` if the key is not found", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const key = rargs[0];
            try eval.validateSymbol(at, key);
            const env = rargs[1];
            try eval.validateListOrNil(at, env);
            const entry = Eval.envLookup(key, env) catch |err| {
                return eval.abort(err, at, "bad env: {}", .{env});
            };
            if (entry) |v| {
                return v;
            } else {
                return eval.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "env-pair", "lookup a key symbol in an environment, returning the pair it binds; prompts `fail` if the key is not found", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const key = rargs[0];
            try eval.validateSymbol(at, key);
            const env = rargs[1];
            try eval.validateListOrNil(at, env);
            const entry = Eval.envLookupPair(key, env) catch |err| {
                return eval.abort(err, at, "bad env: {}", .{env});
            };
            if (entry) |pair| {
                try eval.validatePair(pair.getAttr(), pair);
                return pair;
            } else {
                return eval.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "env-set!", "set the value associated with a name in an environment, returning the old value; prompts `fail` if the name is not bound", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve3(args);
            const key = rargs[0];
            try eval.validateSymbol(at, key);
            const value = rargs[1];
            const env = rargs[2];
            try eval.validateListOrNil(at, env);
            const entry = Eval.envLookupPair(key, env) catch |err| {
                return eval.abort(err, at, "bad env: {}", .{env});
            };
            if (entry) |pair| {
                const xp = try eval.castPair(at, pair);
                const oldValue = xp.cdr;
                xp.cdr = value;
                return oldValue;
            } else {
                return eval.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "env-put!", "append a key-value pair to the top frame of an environment", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve3(args);
            const key = rargs[0];
            try eval.validateSymbol(at, key);
            const value = rargs[1];
            const env = rargs[2];
            try eval.validateListOrNil(at, env);
            try Eval.extendEnvFrame(at, key, value, env);
            return env;
        }
    } },
    .{ "env-copy", "copy a given environment", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const newEnv = try eval.resolve1(args);
            const outEnv = Eval.copyEnv(at, newEnv);
            return outEnv;
        }
    } },
    .{ "env-new", "make a new environment from a simple a-list", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const frame = try eval.resolve1(args);
            Eval.validateFrame(frame) catch |err| {
                return eval.abort(err, at, "bad frame: {}", .{frame});
            };
            const nil = try SExpr.Nil(at);
            return try SExpr.Cons(at, frame, nil);
        }
    } },
    .{ "env-get-frame", "get the environment frame at the given offset depth; prompts `fail` if the depth is out of bounds", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const env = rargs[0];
            const frameOffsetI = try eval.coerceNativeInt(at, rargs[1]);
            if (frameOffsetI < 0) {
                return eval.abort(Eval.Error.TypeError, at, "expected a non-negative number for frame offset, got {}", .{frameOffsetI});
            }
            const frameOffset: usize = @intCast(frameOffsetI);
            const frame = Eval.getFrame(frameOffset, env) catch |err| {
                if (err == Eval.Error.EnvironmentUnderflow) {
                    return eval.nativePrompt(at, "fail", &[0]SExpr{});
                }
                return eval.abort(err, at, "bad env: {}", .{env});
            };
            return frame;
        }
    } },
    .{ "env-push", "push a given environment frame into the current environment, returning the modified enviroment", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            var env = rargs[0];
            const frame = rargs[1];
            Eval.validateFrame(frame) catch |err| {
                return eval.abort(err, at, "bad frame: {}", .{frame});
            };
            Eval.pushFrame(frame, &env) catch |err| {
                return eval.abort(err, at, "bad env: {}", .{env});
            };
            return env;
        }
    } },
    .{ "env-pop", "pop an environment frame off the current environment, returning it and the modified environment as a pair `(frame . env)`; prompts `fail` if the environment is empty", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var env = try eval.resolve1(args);
            const frame = Eval.popFrame(&env) catch |err| {
                if (err == Eval.Error.EnvironmentUnderflow) {
                    return eval.nativePrompt(at, "fail", &[0]SExpr{});
                }
                return eval.abort(err, at, "bad env: {}", .{env});
            };
            return SExpr.Cons(at, frame, env);
        }
    } },

    .{ "swap-env", "replace the current environment with the given one, returning the old one; optionally accepts `'self` `'caller` or `'evidence` symbols indicating which environment to effect", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = [2]SExpr{ undefined, undefined };
            const len = try eval.resolveSmallList(args, 1, &rargs);
            const newEnv = rargs[0];
            try Eval.validateEnv(newEnv);
            if (len == 1 or rargs[1].isExactSymbol("self")) {
                const outEnv = eval.env;
                eval.env = newEnv;
                return outEnv;
            } else {
                const which = rargs[1];
                if (which.isExactSymbol("caller")) {
                    const outEnv = eval.callerEnv;
                    eval.callerEnv = newEnv;
                    return outEnv;
                } if (which.isExactSymbol("evidence")) {
                    const outEnv = eval.evidence;
                    eval.evidence = newEnv;
                    return outEnv;
                } else {
                    return eval.abort(Eval.Error.TypeError, at, "expected `self` or `parent`, got {}", .{which});
                }
            }
        }
    } },
    .{ "take-env", "take the current environment, leaving it empty; optionally accepts `'self` `'caller` or `'evidence` symbols indicating which environment to effect", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolveMaybe1(args);
            const newEnv = try SExpr.Nil(at);
            if (arg) |which| {
                if (which.isExactSymbol("caller")) {
                    const outEnv = eval.callerEnv;
                    eval.callerEnv = newEnv;
                    return outEnv;
                } else if (which.isExactSymbol("evidence")) {
                    const outEnv = eval.evidence;
                    eval.evidence = newEnv;
                    return outEnv;
                } else if (!which.isExactSymbol("self")) {
                    return eval.abort(Eval.Error.TypeError, which.getAttr(), "expected `self` or `parent`, got {}", .{which});
                }
            }
            const outEnv = eval.env;
            eval.env = newEnv;
            return outEnv;
        }
    } },
    .{ "get-env", "take a copy of the current environment, leaving it in place; optionally accepts `'self` `'caller` or `'evidence` symbols indicating which environment to effect", struct {
        pub fn fun(eval: *Eval, _: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolveMaybe1(args);
            if (arg) |which| {
                if (which.isExactSymbol("caller")) {
                    return eval.callerEnv;
                } else if (which.isExactSymbol("evidence")) {
                    return eval.evidence;
                } else if (!which.isExactSymbol("self")) {
                    return eval.abort(Eval.Error.TypeError, which.getAttr(), "expected `self` or `parent`, got {}", .{which});
                }
            }
            return eval.env;
        }
    } },
    .{ "replace-env", "replace the current environment with the given one; optionally accepts `'self` `'caller` or `'evidence` symbols indicating which environment to effect", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = [2]SExpr{ undefined, undefined };
            const len = try eval.resolveSmallList(args, 1, &rargs);
            const newEnv = rargs[0];
            try Eval.validateEnv(newEnv);
            if (len == 1 or rargs[1].isExactSymbol("self")) {
                eval.env = newEnv;
            } else {
                const which = rargs[1];
                if (which.isExactSymbol("caller")) {
                    eval.callerEnv = newEnv;
                } else if (which.isExactSymbol("evidence")) {
                    eval.evidence = newEnv;
                } else {
                    return eval.abort(Eval.Error.TypeError, which.getAttr(), "expected `self` or `parent`, got {}", .{which});
                }
            }
            return try SExpr.Nil(at);
        }
    } },

    .{ "get-global-evidence", "get the global evidence environment frame", struct {
        pub fn fun(eval: *Eval, _: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            try eval.expect0(args);
            return eval.globalEvidence;
        }
    } },

    .{ "set-global-evidence", "set the global evidence environment frame", struct {
        pub fn fun(eval: *Eval, _: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const newEv = try eval.resolve1(args);
            Eval.validateFrame(newEv) catch |err| {
                return eval.abort(err, args.getAttr(), "bad frame: {}", .{newEv});
            };
            eval.globalEvidence = newEv;
            return newEv;
        }
    } },
};
