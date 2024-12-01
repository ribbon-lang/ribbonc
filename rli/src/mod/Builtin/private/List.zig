const std = @import("std");

const MiscUtils = @import("Utils").Misc;

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Eval = Core.Eval;

pub const Doc =
    \\This module contains functions for creating and manipulating lists and pairs.
    \\
;

pub const Env = .{
    .{ "nil", "the empty list constant", struct {
        pub fn init(at: *const Source.Attr) Eval.Result!SExpr {
            return try SExpr.Nil(at);
        }
    } },
    .{ "cons", "join a head and tail into a pair", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const eargs = try eval.resolve2(args);
            return try SExpr.Cons(at, eargs[0], eargs[1]);
        }
    } },
    .{ "car", "get the head of a pair", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const list = try eval.resolve1(args);
            return (try eval.castPair(at, list)).car;
        }
    } },
    .{ "set-car!", "set the head of a pair; returns the old value", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const eargs = try eval.resolve2(args);
            const list = try eval.castPair(at, eargs[0]);
            const newCar = eargs[1];
            const oldCar = list.car;
            list.car = newCar;
            return oldCar;
        }
    } },
    .{ "cdr", "get the tail of a pair", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const list = try eval.resolve1(args);
            return (try eval.castPair(at, list)).cdr;
        }
    } },
    .{ "set-cdr!", "set the tail of a pair; returns the old value", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const eargs = try eval.resolve2(args);
            const list = try eval.castPair(at, eargs[0]);
            const newCdr = eargs[1];
            const oldCdr = list.cdr;
            list.cdr = newCdr;
            return oldCdr;
        }
    } },
    .{ "list", "create a new list, with any number of values", struct {
        pub fn fun(eval: *Eval, _: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            return eval.resolveListRecursive(args);
        }
    } },
    .{ "length", "get the length of a list", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const list = try eval.resolve1(args);
            try eval.validateListOrNil(at, list);
            var iter = try eval.argIterator(false, list);
            var len: usize = 0;
            while (try iter.next()) |_| {
                len += 1;
            }
            if (len > @as(usize, std.math.maxInt(i64))) {
                return eval.abort(Eval.Error.RangeError, at, "list is too long to get its length: {}", .{len});
            }
            return SExpr.Int(at, @intCast(len));
        }
    } },
    .{ "map", "apply a function to each element of a list, returning a new list of the results", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const list = rargs[0];
            const func = rargs[1];
            try eval.validateListOrNil(at, list);
            try eval.validateCallable(at, func);
            var iter = try eval.argIterator(false, list);
            var out = std.ArrayList(SExpr).init(eval.context.allocator);
            defer out.deinit();
            while (try iter.next()) |arg| {
                const result = try eval.nativeInvoke(at, func, &[_]SExpr{arg});
                try out.append(result);
            }
            return try SExpr.List(at, out.items);
        }
    } },
    .{ "each", "apply a function to each element of a list", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const list = rargs[0];
            const func = rargs[1];
            try eval.validateListOrNil(at, list);
            try eval.validateCallable(at, func);
            var iter = try eval.argIterator(false, list);
            while (try iter.next()) |arg| {
                _ = try eval.nativeInvoke(at, func, &[_]SExpr{arg});
            }
            return try SExpr.Nil(at);
        }
    } },
    .{ "element", "determine if a given value is contained in a list", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const list = rargs[0];
            const value = rargs[1];
            try eval.validateListOrNil(at, list);
            var iter = try eval.argIterator(false, list);
            while (try iter.next()) |arg| {
                if (MiscUtils.equal(arg, value)) {
                    return SExpr.Bool(at, true);
                }
            }
            return SExpr.Bool(at, false);
        }
    } },
};
