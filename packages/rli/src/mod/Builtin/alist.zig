const std = @import("std");

const Rli = @import("../root.zig");
const Source = Rli.Source;
const SExpr = Rli.SExpr;
const Interpreter = Rli.Interpreter;

pub const Doc =
    \\This module contains functions for creating and manipulating association lists.
    \\
;

pub const Decls = .{
    .{ "alist/pair", "lookup a key symbol in an association list, returning the pair it binds; prompts `fail` if the key is not found", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalN(2, args);
            const key = rArgs[0];
            const alist = rArgs[1];
            try interpreter.validateListOrNil(at, alist);
            const pair = Interpreter.alistLookup(key, alist) catch |err| {
                return interpreter.abort(err, at, "expected an association list, got {}: `{}`", .{ alist.getTag(), alist });
            };
            if (pair) |p| {
                return p;
            } else {
                return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "alist/lookup-f", "lookup a key in an association list, returning its associated value; prompts `fail` if the key is not found", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalN(2, args);
            const key = rArgs[0];
            const alist = rArgs[1];
            try interpreter.validateListOrNil(at, alist);
            const pair = Interpreter.alistLookup(key, alist) catch |err| {
                return interpreter.abort(err, at, "expected an association list, got {}: `{}`", .{ alist.getTag(), alist });
            };
            if (pair) |p| {
                const xp = try interpreter.castPair(at, p);
                return xp.cdr;
            } else {
                return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "alist/lookup", "lookup a key in an association list, returning its associated value; returns `nil` if the key is not found", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalN(2, args);
            const key = rArgs[0];
            const alist = rArgs[1];
            try interpreter.validateListOrNil(at, alist);
            const pair = Interpreter.alistLookup(key, alist) catch |err| {
                return interpreter.abort(err, at, "expected an association list, got {}: `{}`", .{ alist.getTag(), alist });
            };
            if (pair) |p| {
                const xp = try interpreter.castPair(at, p);
                return xp.cdr;
            } else {
                return try SExpr.Nil(at);
            }
        }
    } },
    .{ "alist/member?", "check if a key is present in an association list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalN(2, args);
            const key = rArgs[0];
            const alist = rArgs[1];
            try interpreter.validateListOrNil(at, alist);
            const pair = Interpreter.alistLookup(key, alist) catch |err| {
                return interpreter.abort(err, at, "expected an association list, got {}: `{}`", .{ alist.getTag(), alist });
            };
            return SExpr.Bool(at, pair != null);
        }
    } },
    .{ "alist/append", "append a key-value pair to an association list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalN(3, args);
            const key = rArgs[0];
            const value = rArgs[1];
            const alist = rArgs[2];
            try interpreter.validateListOrNil(at, alist);
            return SExpr.Cons(at, try SExpr.Cons(at, key, value), alist);
        }
    } },
    .{ "alist/set!", "set the value of an existing key-value pair in an association list, returning the old value; prompts `fail` if the key is not found", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalN(3, args);
            const key = rArgs[0];
            const value = rArgs[1];
            const alist = rArgs[2];
            try interpreter.validateListOrNil(at, alist);
            const pair = Interpreter.alistLookup(key, alist) catch |err| {
                return interpreter.abort(err, at, "expected an association list, got {}: `{}`", .{ alist.getTag(), alist });
            };
            if (pair) |p| {
                const xp = try interpreter.castPair(at, p);
                const oldValue = xp.cdr;
                xp.cdr = value;
                return oldValue;
            } else {
                return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "alist/each", "calls a function with each key-value pair in an association list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalN(2, args);
            const alist = rArgs[0];
            const callback = rArgs[1];
            try interpreter.validateCallable(at, callback);
            var iter = try interpreter.argIterator(false, alist);
            while (try iter.next()) |pair| {
                const xp = try interpreter.castPair(at, pair);
                _ = try interpreter.nativeInvoke(at, callback, &[_]SExpr{ xp.car, xp.cdr });
            }
            return try SExpr.Nil(at);
        }
    } },
    .{ "alist/keys", "get the keys of a given association list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const frame = (try interpreter.evalN(1, args))[0];
            const keys = Interpreter.alistKeys(frame) catch |err| {
                return interpreter.abort(err, at, "expected an association list, got {}: `{}`", .{ frame.getTag(), frame });
            };
            defer interpreter.context.allocator.free(keys);
            return try SExpr.List(at, keys);
        }
    } },
    .{ "alist/remove", "remove a key-value pair from an association list, returning a new list; prompts `fail` if the key is not found", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalN(2, args);
            const key = rArgs[0];
            const alist = rArgs[1];
            try interpreter.validateListOrNil(at, alist);
            if (try Interpreter.alistRemove(key, alist)) |newList| {
                return newList;
            } else {
                return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
};
