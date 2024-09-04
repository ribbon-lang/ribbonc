const std = @import("std");

const Support = @import("Support");

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Eval = Core.Eval;

pub const Doc =
    \\This module contains functions and primitives for logical operations,
    \\such as comparison and boolean algebra.
    \\
    \\Additionally, there is the `truthy?` function, which converts
    \\any value into a boolean, as well as the constants `true` and `false.`
    \\
;

fn eql(eval: *Eval, args: SExpr) Eval.Result!bool {
    var rargs = try eval.argIterator(true, args);

    const a = try rargs.atLeast();

    while (try rargs.next()) |b| {
        if (!Support.equal(a, b)) {
            return false;
        }
    }

    return true;
}

fn eqlAddress(eval: *Eval, args: SExpr) Eval.Result!bool {
    var rargs = try eval.argIterator(true, args);

    const a = try rargs.atLeast();

    while (try rargs.next()) |b| {
        if (!Support.equalAddress(a, b)) {
            return false;
        }
    }

    return true;
}

pub const Env = .{
    .{ .{ "eq?", "==" }, "determine if any number of values are equal; uses structural comparison", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            return try SExpr.Bool(at, try eql(eval, args));
        }
    } },
    .{ .{ "not-eq?", "/=" }, "determine if any number of values are not equal; uses structural comparison", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            return try SExpr.Bool(at, !try eql(eval, args));
        }
    } },
    .{ .{ "less?", "<" }, "determine if any number of values are in order of least to greatest; uses structural comparison", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!Support.less(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },
    .{ .{ "greater?", ">" }, "determine if any number of values are in order of greatest to least; uses structural comparison", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!Support.greater(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },
    .{ .{ "less-or-equal?", "<=" }, "determine if any number of values are in order from least to greatest, allowing adjacent values to be equal; uses structural comparison", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!Support.lessOrEqual(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },
    .{ .{ "greater-or-equal?", ">=" }, "determine if any number of values are in order from greatest to least, allowing adjacent values to be equal; uses structural comparison", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!Support.greaterOrEqual(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },

    .{ .{ "eq-addr?", "==*" }, "determine if any number of values are equal; uses address comparisons for object types", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            return try SExpr.Bool(at, try eqlAddress(eval, args));
        }
    } },
    .{ .{ "not-eq-addr?", "/=*" }, "determine if any number of values are not equal; uses address comparisons for object types", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            return try SExpr.Bool(at, !try eqlAddress(eval, args));
        }
    } },
    .{ .{ "less-addr?", "<*" }, "determine if any number of values are in order of least to greatest; uses address comparisons for object types", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!Support.lessAddress(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },
    .{ .{ "greater-addr?", ">*" }, "determine if any number of values are in order of greatest to least; uses address comparisons for object types", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!Support.greaterAddress(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },
    .{ .{ "less-or-equal-addr?", "<=*" }, "determine if any number of values are in order from least to greatest, allowing adjacent values to be equal; uses address comparisons for object types", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!Support.lessOrEqualAddress(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },
    .{ .{ "greater-or-equal-addr?", ">=*" }, "determine if any number of values are in order from greatest to least, allowing adjacent values to be equal; uses address comparisons for object types", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            var a = try rargs.atLeast();
            while (try rargs.next()) |b| {
                if (!Support.greaterOrEqualAddress(a, b)) {
                    return try SExpr.Bool(at, false);
                }
                a = b;
            }
            return try SExpr.Bool(at, true);
        }
    } },

    .{ .{ "not", "!" }, "logical not, performs truthy conversion", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            return try SExpr.Bool(at, !arg.coerceNativeBool());
        }
    } },
    .{ .{ "and", "&&" }, "logical and accepting any number of values, short circuiting. performs truthy conversion for tests and returns the first failing value", struct {
        pub fn fun(eval: *Eval, _: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
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
        pub fn fun(eval: *Eval, _: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
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
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            return try SExpr.Bool(at, arg.coerceNativeBool());
        }
    } },
    .{ "true", "boolean constant", struct {
        pub fn init(at: *const Source.Attr) Eval.Result!SExpr {
            return try SExpr.Bool(at, true);
        }
    } },
    .{ "false", "boolean constant", struct {
        pub fn init(at: *const Source.Attr) Eval.Result!SExpr {
            return try SExpr.Bool(at, false);
        }
    } },
};
