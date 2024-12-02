const std = @import("std");

const MiscUtils = @import("Utils").Misc;

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Interpreter = Core.Interpreter;

pub const Doc =
    \\This module contains functions for creating and manipulating lists and pairs.
    \\
;

pub const Env = .{
    .{ "nil", "the empty list constant", struct {
        pub fn init(at: *const Source.Attr) Interpreter.Result!SExpr {
            return try SExpr.Nil(at);
        }
    } },
    .{ "cons", "join a head and tail into a pair", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.eval2(args);
            return try SExpr.Cons(at, rArgs[0], rArgs[1]);
        }
    } },
    .{ "car", "get the head of a pair", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const list = try interpreter.eval1(args);
            return (try interpreter.castPair(at, list)).car;
        }
    } },
    .{ "set-car!", "set the head of a pair; returns the old value", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.eval2(args);
            const list = try interpreter.castPair(at, rArgs[0]);
            const newCar = rArgs[1];
            const oldCar = list.car;
            list.car = newCar;
            return oldCar;
        }
    } },
    .{ "cdr", "get the tail of a pair", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const list = try interpreter.eval1(args);
            return (try interpreter.castPair(at, list)).cdr;
        }
    } },
    .{ "set-cdr!", "set the tail of a pair; returns the old value", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.eval2(args);
            const list = try interpreter.castPair(at, rArgs[0]);
            const newCdr = rArgs[1];
            const oldCdr = list.cdr;
            list.cdr = newCdr;
            return oldCdr;
        }
    } },
    .{ "list", "create a new list, with any number of values", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            return interpreter.evalListRecursive(args);
        }
    } },
    .{ "list-length", "get the length of a list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const list = try interpreter.eval1(args);
            try interpreter.validateListOrNil(at, list);
            var iter = try interpreter.argIterator(false, list);
            var len: usize = 0;
            while (try iter.next()) |_| {
                len += 1;
            }
            if (len > @as(usize, std.math.maxInt(i64))) {
                return interpreter.abort(Interpreter.Error.RangeError, at, "list is too long to get its length: {}", .{len});
            }
            return SExpr.Int(at, @intCast(len));
        }
    } },
    .{ "list-map", "apply a function to each element of a list, returning a new list of the results", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.eval2(args);
            const list = rArgs[0];
            const func = rArgs[1];
            try interpreter.validateListOrNil(at, list);
            try interpreter.validateCallable(at, func);
            var iter = try interpreter.argIterator(false, list);
            var out = std.ArrayList(SExpr).init(interpreter.context.allocator);
            defer out.deinit();
            while (try iter.next()) |arg| {
                const result = try interpreter.nativeInvoke(at, func, &[_]SExpr{arg});
                try out.append(result);
            }
            return try SExpr.List(at, out.items);
        }
    } },
    .{ "list-each", "apply a function to each element of a list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.eval2(args);
            const list = rArgs[0];
            const func = rArgs[1];
            try interpreter.validateListOrNil(at, list);
            try interpreter.validateCallable(at, func);
            var iter = try interpreter.argIterator(false, list);
            while (try iter.next()) |arg| {
                _ = try interpreter.nativeInvoke(at, func, &[_]SExpr{arg});
            }
            return try SExpr.Nil(at);
        }
    } },
    .{ "list-member?", "determine if a given value is contained in a list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.eval2(args);
            const list = rArgs[0];
            const value = rArgs[1];
            try interpreter.validateListOrNil(at, list);
            var iter = try interpreter.argIterator(false, list);
            while (try iter.next()) |arg| {
                if (MiscUtils.equal(arg, value)) {
                    return SExpr.Bool(at, true);
                }
            }
            return SExpr.Bool(at, false);
        }
    } },
};
