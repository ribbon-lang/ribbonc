const std = @import("std");

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Eval = Core.Eval;

pub const Doc =
    \\This module contains functions for creating and manipulating association lists.
    \\
;

pub const Env = .{
    .{ "alist-pair", "lookup a key symbol in an association list, returning the pair it binds; prompts `fail` if the key is not found", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const key = rargs[0];
            try eval.validateSymbol(at, key);
            const alist = rargs[1];
            try eval.validateListOrNil(at, alist);
            const pair = Eval.frameLookup(key, alist) catch |err| {
                return eval.abort(err, at, "expected an association list, got {}: `{}`", .{ alist.getTag(), alist });
            };
            if (pair) |p| {
                return p;
            } else {
                return eval.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "alist-lookup", "lookup a key symbol in an association list, returning its associated value; prompts `fail` if the key is not found", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const key = rargs[0];
            try eval.validateSymbol(at, key);
            const alist = rargs[1];
            try eval.validateListOrNil(at, alist);
            const pair = Eval.frameLookup(key, alist) catch |err| {
                return eval.abort(err, at, "expected an association list, got {}: `{}`", .{ alist.getTag(), alist });
            };
            if (pair) |p| {
                const xp = try eval.castPair(at, p);
                return xp.cdr;
            } else {
                return eval.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "alist-member?", "check if a key symbol is present in an association list", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const key = rargs[0];
            try eval.validateSymbol(at, key);
            const alist = rargs[1];
            try eval.validateListOrNil(at, alist);
            const pair = Eval.frameLookup(key, alist) catch |err| {
                return eval.abort(err, at, "expected an association list, got {}: `{}`", .{ alist.getTag(), alist });
            };
            return SExpr.Bool(at, pair != null);
        }
    } },
    .{ "alist-put", "append a key-value pair to an association list", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve3(args);
            const key = rargs[0];
            try eval.validateSymbol(at, key);
            const value = rargs[1];
            const alist = rargs[2];
            try eval.validateListOrNil(at, alist);
            return SExpr.Cons(at, try SExpr.Cons(at, key, value), alist);
        }
    } },
    .{ "alist-set!", "set the value of an existing key-value pair in an association list, returning the old value; prompts `fail` if the key is not found", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve3(args);
            const key = rargs[0];
            try eval.validateSymbol(at, key);
            const value = rargs[1];
            const alist = rargs[2];
            try eval.validateListOrNil(at, alist);
            const pair = Eval.frameLookup(key, alist) catch |err| {
                return eval.abort(err, at, "expected an association list, got {}: `{}`", .{ alist.getTag(), alist });
            };
            if (pair) |p| {
                const xp = try eval.castPair(at, p);
                const oldValue = xp.cdr;
                xp.cdr = value;
                return oldValue;
            } else {
                return eval.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "alist-each", "calls a function with each key-value pair in an association list", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const alist = rargs[0];
            const callback = rargs[1];
            try eval.validateCallable(at, callback);
            var iter = try eval.argIterator(false, alist);
            while (try iter.next()) |pair| {
                const xp = try eval.castPair(at, pair);
                _ = try eval.nativeInvoke(at, callback, &[_]SExpr{ xp.car, xp.cdr });
            }
            return try SExpr.Nil(at);
        }
    } },
    .{ "alist-keys", "get the keys of a given association list", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const frame = try eval.resolve1(args);
            const keys = Eval.frameKeys(frame) catch |err| {
                return eval.abort(err, at, "expected an association list, got {}: `{}`", .{ frame.getTag(), frame });
            };
            defer eval.context.allocator.free(keys);
            return try SExpr.List(at, keys);
        }
    } },
};
