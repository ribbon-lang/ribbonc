const std = @import("std");

const MiscUtils = @import("Utils").Misc;

const Rli = @import("../root.zig");
const Source = Rli.Source;
const SExpr = Rli.SExpr;
const Interpreter = Rli.Interpreter;

pub const Doc =
    \\This module contains functions and primitives for logical operations,
    \\such as comparison and boolean algebra.
    \\
    \\Additionally, there is the `truthy?` function, which converts
    \\any value into a boolean, as well as the constants `true` and `false.`
    \\
;

fn eql(interpreter: *Interpreter, args: SExpr) Interpreter.Result!bool {
    var rargs = try interpreter.argIterator(true, args);

    const a = try rargs.atLeast();

    while (try rargs.next()) |b| {
        if (!MiscUtils.equal(a, b)) {
            return false;
        }
    }

    return true;
}

fn eqlAddress(interpreter: *Interpreter, args: SExpr) Interpreter.Result!bool {
    var rargs = try interpreter.argIterator(true, args);

    const a = try rargs.atLeast();

    while (try rargs.next()) |b| {
        if (!MiscUtils.equalAddress(a, b)) {
            return false;
        }
    }

    return true;
}

pub const Decls = .{
    .{ .{ "eq?", "==" }, "determine if any number of values are equal; uses structural comparison", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            return try SExpr.Bool(at, try eql(interpreter, args));
        }
    } },
    .{ .{ "not-eq?", "/=" }, "determine if any number of values are not equal; uses structural comparison", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            return try SExpr.Bool(at, !try eql(interpreter, args));
        }
    } },
    .{ .{ "less?", "<" }, "determine if any number of values are in order of least to greatest; uses structural comparison", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rargs = try interpreter.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!MiscUtils.less(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },
    .{ .{ "greater?", ">" }, "determine if any number of values are in order of greatest to least; uses structural comparison", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rargs = try interpreter.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!MiscUtils.greater(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },
    .{ .{ "less-or-equal?", "<=" }, "determine if any number of values are in order from least to greatest, allowing adjacent values to be equal; uses structural comparison", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rargs = try interpreter.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!MiscUtils.lessOrEqual(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },
    .{ .{ "greater-or-equal?", ">=" }, "determine if any number of values are in order from greatest to least, allowing adjacent values to be equal; uses structural comparison", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rargs = try interpreter.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!MiscUtils.greaterOrEqual(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },

    .{ .{ "eq-addr?", "==*" }, "determine if any number of values are equal; uses address comparisons for object types", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            return try SExpr.Bool(at, try eqlAddress(interpreter, args));
        }
    } },
    .{ .{ "not-eq-addr?", "/=*" }, "determine if any number of values are not equal; uses address comparisons for object types", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            return try SExpr.Bool(at, !try eqlAddress(interpreter, args));
        }
    } },
    .{ .{ "less-addr?", "<*" }, "determine if any number of values are in order of least to greatest; uses address comparisons for object types", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rargs = try interpreter.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!MiscUtils.lessAddress(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },
    .{ .{ "greater-addr?", ">*" }, "determine if any number of values are in order of greatest to least; uses address comparisons for object types", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rargs = try interpreter.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!MiscUtils.greaterAddress(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },
    .{ .{ "less-or-equal-addr?", "<=*" }, "determine if any number of values are in order from least to greatest, allowing adjacent values to be equal; uses address comparisons for object types", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rargs = try interpreter.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!MiscUtils.lessOrEqualAddress(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },
    .{ .{ "greater-or-equal-addr?", ">=*" }, "determine if any number of values are in order from greatest to least, allowing adjacent values to be equal; uses address comparisons for object types", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rargs = try interpreter.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!MiscUtils.greaterOrEqualAddress(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },

    .{ .{ "not", "!" }, "logical not, performs truthy conversion", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, !arg.coerceNativeBool());
        }
    } },
    .{ .{ "and", "&&" }, "logical and accepting any number of values, short circuiting. performs truthy conversion for tests and returns the first failing value", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rargs = try interpreter.argIterator(true, args);
            var a = try rargs.atLeast();
            if (!a.coerceNativeBool()) {
                return a;
            }
            while (try rargs.next()) |b| {
                if (!b.coerceNativeBool()) {
                    return b;
                }
                a = b;
            }
            return a;
        }
    } },
    .{ .{ "or", "||" }, "logical or accepting any number of values, short circuiting. performs truthy conversion for tests and returns the first succeeding value", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rargs = try interpreter.argIterator(true, args);
            var a = try rargs.atLeast();
            if (a.coerceNativeBool()) {
                return a;
            }
            while (try rargs.next()) |b| {
                if (b.coerceNativeBool()) {
                    return b;
                }
                a = b;
            }
            return a;
        }
    } },
    .{ "truthy?", "performs truthy conversion", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.coerceNativeBool());
        }
    } },
    .{ "true", "boolean constant", struct {
        pub fn init(at: *const Source.Attr) Interpreter.Result!SExpr {
            return try SExpr.Bool(at, true);
        }
    } },
    .{ "false", "boolean constant", struct {
        pub fn init(at: *const Source.Attr) Interpreter.Result!SExpr {
            return try SExpr.Bool(at, false);
        }
    } },
};
