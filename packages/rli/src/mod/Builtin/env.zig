const std = @import("std");

const Rli = @import("../root.zig");
const Source = Rli.Source;
const SExpr = Rli.SExpr;
const Interpreter = Rli.Interpreter;

pub const Doc =
    \\This module contains functions manipulation of environments.
    \\
;

pub const Decls = .{
    .{ "env/keys", "get the names of all bindings in the given env", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const env = (try interpreter.evalN(1, args))[0];
            try interpreter.validateListOrNil(at, env);
            const keys = try Interpreter.envKeys(env, interpreter.context.allocator);
            defer interpreter.context.allocator.free(keys);
            return try SExpr.List(at, keys);
        }
    } },
    .{ "env/lookup-f", "lookup a key symbol in an environment, returning the value it binds; prompts `fail` if the key is not found", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.evalN(2, args);
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
                const envKeys = try Interpreter.envKeys(env, interpreter.context.allocator);
                defer interpreter.context.allocator.free(envKeys);
                return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "env/lookup", "lookup a key symbol in an environment, returning the value it binds; returns `nil` if the key is not found", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.evalN(2, args);
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
                return SExpr.Nil(at);
            }
        }
    } },
    .{ "env/pair", "lookup a key symbol in an environment, returning the pair it binds; prompts `fail` if the key is not found", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.evalN(2, args);
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
    .{ "env/set!", "set the value associated with a name in an environment, returning the old value; prompts `fail` if the name is not bound", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.evalN(3, args);
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
    .{ "env/put!", "append a key-value pair to the top frame of an environment", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.evalN(3, args);
            const key = rargs[0];
            try interpreter.validateSymbol(at, key);
            const value = rargs[1];
            const env = rargs[2];
            try interpreter.validateListOrNil(at, env);
            try Interpreter.extendEnvFrame(at, key, value, env);
            return env;
        }
    } },
    .{ "env/copy", "copy a given environment", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const newEnv = (try interpreter.evalN(1, args))[0];
            const outEnv = Interpreter.copyEnv(at, newEnv);
            return outEnv;
        }
    } },
    .{ "env/new", "make a new environment from a simple a-list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const frame = (try interpreter.evalN(1, args))[0];
            Interpreter.validateFrame(frame) catch |err| {
                return interpreter.abort(err, at, "bad frame: {}", .{frame});
            };
            const nil = try SExpr.Nil(at);
            return try SExpr.Cons(at, frame, nil);
        }
    } },
    .{ "env/get-frame", "get the environment frame at the given offset depth; prompts `fail` if the depth is out of bounds", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.evalN(2, args);
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
    .{ "env/push", "push a given environment frame into the current environment, returning the modified enviroment", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.evalN(2, args);
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
    .{ "env/pop", "pop an environment frame off the current environment, returning it and the modified environment as a pair `(frame . env)`; prompts `fail` if the environment is empty", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var env = (try interpreter.evalN(1, args))[0];
            const frame = Interpreter.popFrame(&env) catch |err| {
                if (err == Interpreter.Error.EnvironmentUnderflow) {
                    return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
                }
                return interpreter.abort(err, at, "bad env: {}", .{env});
            };
            return SExpr.Cons(at, frame, env);
        }
    } },
};
