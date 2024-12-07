const std = @import("std");

const Rli = @import("../root.zig");
const Source = Rli.Source;
const SExpr = Rli.SExpr;
const Interpreter = Rli.Interpreter;

pub const Doc =
    \\This module provides basic arithmetic functions, constants, and predicates.
    \\
;

pub const Decls = .{
    .{ .{ "add", "+" }, "integer/floating point addition on any number of values", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rArgs = try interpreter.argIterator(true, args);
            var a = try rArgs.atLeast();
            try interpreter.validateNumber(at, a);
            if (!rArgs.hasNext()) {
                switch (a.getTag()) {
                    .Int => return try SExpr.Int(at, @as(i64, @intCast(@abs(a.forceInt())))),
                    .Float => return try SExpr.Float(at, @abs(a.forceFloat())),
                    else => unreachable,
                }
            }
            while (try rArgs.next()) |b| {
                try interpreter.validateNumber(at, b);
                a = try castedBinOp(.add, interpreter, at, a, b);
            }
            return a;
        }
    } },
    .{ .{ "sub", "-" }, "integer/floating point subtraction on any number of values", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rArgs = try interpreter.argIterator(true, args);
            var a = try rArgs.atLeast();
            try interpreter.validateNumber(at, a);
            if (!rArgs.hasNext()) {
                switch (a.getTag()) {
                    .Int => return try SExpr.Int(at, -a.forceInt()),
                    .Float => return try SExpr.Float(at, -a.forceFloat()),
                    else => unreachable,
                }
            }
            while (try rArgs.next()) |b| {
                try interpreter.validateNumber(at, b);
                a = try castedBinOp(.sub, interpreter, at, a, b);
            }
            return a;
        }
    } },
    .{ .{ "mul", "*" }, "integer/floating point multiplication on any number of values", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rArgs = try interpreter.argIterator(true, args);
            var a = try rArgs.atLeast();
            try interpreter.validateNumber(at, a);
            while (try rArgs.next()) |b| {
                try interpreter.validateNumber(at, b);
                a = try castedBinOp(.mul, interpreter, at, a, b);
            }
            return a;
        }
    } },
    .{ .{ "div", "/" }, "integer/floating point division on any number of values", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rArgs = try interpreter.argIterator(true, args);
            var a = try rArgs.atLeast();
            try interpreter.validateNumber(at, a);

            while (try rArgs.next()) |b| {
                try interpreter.validateNumber(at, b);
                a = try castedBinOp(.div, interpreter, at, a, b);
            }
            return a;
        }
    } },
    .{ .{ "mod", "%" }, "integer/floating point remainder division on any number of values", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rArgs = try interpreter.argIterator(true, args);
            var a = try rArgs.atLeast();
            try interpreter.validateNumber(at, a);

            while (try rArgs.next()) |b| {
                try interpreter.validateNumber(at, b);
                a = try castedBinOp(.mod, interpreter, at, a, b);
            }
            return a;
        }
    } },
    .{ .{ "pow", "^" }, "integer/floating point exponentiation on any number of values", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rArgs = try interpreter.argIterator(true, args);
            var a = try rArgs.atLeast();
            try interpreter.validateNumber(at, a);
            while (try rArgs.next()) |b| {
                try interpreter.validateNumber(at, b);
                a = try castedBinOp(.pow, interpreter, at, a, b);
            }
            return a;
        }
    } },

    .{ "nan?", "check if input is not a number", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, if (arg.isFloat()) std.math.isNan(arg.forceFloat()) else !arg.isInt());
        }
    } },
    .{ "inf?", "check if input is a floating point infinity", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, if (arg.isFloat()) std.math.isInf(arg.forceFloat()) else false);
        }
    } },
    .{ "-inf?", "check if input is a negative floating point infinity", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, if (arg.isFloat()) std.math.isNegativeInf(arg.forceFloat()) else false);
        }
    } },
    .{ "+inf?", "check if input is a positive floating point infinity", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, if (arg.isFloat()) std.math.isPositiveInf(arg.forceFloat()) else false);
        }
    } },
    .{ "odd?", "check if input is odd", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            const i = arg.coerceNativeInt() orelse return interpreter.abort(error.TypeError, at, "expected an integer, got {}: `{}`", .{ arg.getTag(), arg });
            return try SExpr.Bool(at, @rem(i, 2) != 0);
        }
    } },
    .{ "even?", "check if input is even", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            const i = arg.coerceNativeInt() orelse return interpreter.abort(error.TypeError, at, "expected an integer, got {}: `{}`", .{ arg.getTag(), arg });
            return try SExpr.Bool(at, @rem(i, 2) == 0);
        }
    } },

    .{ "inf", "floating point infinity constant", struct {
        pub fn init(at: *const Source.Attr) Interpreter.Result!SExpr {
            return try SExpr.Float(at, std.math.inf(f64));
        }
    } },
    .{ "nan", "floating point not a number constant", struct {
        pub fn init(at: *const Source.Attr) Interpreter.Result!SExpr {
            return try SExpr.Float(at, std.math.nan(f64));
        }
    } },
    .{ "max-int", "the maximum possible integer constant", struct {
        pub fn init(at: *const Source.Attr) Interpreter.Result!SExpr {
            return try SExpr.Int(at, std.math.maxInt(i64));
        }
    } },
    .{ "min-int", "the minimum possible integer constant", struct {
        pub fn init(at: *const Source.Attr) Interpreter.Result!SExpr {
            return try SExpr.Int(at, std.math.minInt(i64));
        }
    } },
    .{ "epsilon", "the minimum difference between two floating point numbers constant", struct {
        pub fn init(at: *const Source.Attr) Interpreter.Result!SExpr {
            return try SExpr.Float(at, std.math.floatEps(f64));
        }
    } },

    .{ "floor", "round a floating point number down", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            if (arg.isInt()) {
                return try SExpr.Float(at, @floatFromInt(arg.forceInt()));
            } else if (arg.isFloat()) {
                return try SExpr.Float(at, @floor(arg.forceFloat()));
            } else {
                return interpreter.abort(Interpreter.EvaluationError.TypeError, at, "expected an integer or a float, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "ceil", "round a floating point number up", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            if (arg.isInt()) {
                return try SExpr.Float(at, @floatFromInt(arg.forceInt()));
            } else if (arg.isFloat()) {
                return try SExpr.Float(at, @ceil(arg.forceFloat()));
            } else {
                return interpreter.abort(Interpreter.EvaluationError.TypeError, at, "expected an integer or a float, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "round", "round a floating point number", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            if (arg.isInt()) {
                return try SExpr.Float(at, @floatFromInt(arg.forceInt()));
            } else if (arg.isFloat()) {
                return try SExpr.Float(at, @round(arg.forceFloat()));
            } else {
                return interpreter.abort(Interpreter.EvaluationError.TypeError, at, "expected an integer or a float, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "frac", "take the fractional part of a floating point number", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            if (arg.isInt()) {
                return try SExpr.Float(at, @floatFromInt(arg.forceInt()));
            } else if (arg.isFloat()) {
                return try SExpr.Float(at, arg.forceFloat() - @trunc(arg.forceFloat()));
            } else {
                return interpreter.abort(Interpreter.EvaluationError.TypeError, at, "expected an integer or a float, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
};

pub fn castedBinOp(comptime op: @Type(.enum_literal), interpreter: *Interpreter, at: *const Source.Attr, a: SExpr, b: SExpr) !SExpr {
    const opFn = switch (op) {
        .add => struct { fn fun (_: *Interpreter, _: *const Source.Attr, x: anytype, y: @TypeOf(x)) !@TypeOf(y) { return x + y; } },
        .sub => struct { fn fun (_: *Interpreter, _: *const Source.Attr, x: anytype, y: @TypeOf(x)) !@TypeOf(y) { return x - y; } },
        .mul => struct { fn fun (_: *Interpreter, _: *const Source.Attr, x: anytype, y: @TypeOf(x)) !@TypeOf(y) { return x * y; } },
        .div => struct { fn fun (i: *Interpreter, att: *const Source.Attr, x: anytype, y: @TypeOf(x)) !@TypeOf(y) { if (y == 0) return i.abort(error.RangeError, att, "division by zero", .{}); return switch (@TypeOf(x)) { i64 => @divFloor(x, y), f64 => x / y, else => unreachable }; } },
        .mod => struct { fn fun (i: *Interpreter, att: *const Source.Attr, x: anytype, y: @TypeOf(x)) !@TypeOf(y) { if (y == 0) return i.abort(error.RangeError, att, "division by zero", .{}); return @mod(x, y); } },
        .rem => struct { fn fun (i: *Interpreter, att: *const Source.Attr, x: anytype, y: @TypeOf(x)) !@TypeOf(y) { if (y == 0) return i.abort(error.RangeError, att, "division by zero", .{}); return @rem(x, y); } },
        .pow => struct { fn fun (i: *Interpreter, att: *const Source.Attr, x: anytype, y: @TypeOf(x)) !@TypeOf(y) { if (y == 0) return i.abort(error.RangeError, att, "division by zero", .{}); return std.math.pow(@TypeOf(x), x, y); } },
        else => @compileError("unsupported binary operation " ++ @typeName(op)),
    }.fun;
    if (a.isInt() and b.isInt()) {
        return SExpr.Int(at, try opFn(interpreter, at, a.forceInt(), b.forceInt()));
    } else if (a.isFloat() or b.isFloat()) {
        return SExpr.Float(at, try opFn(interpreter, at, a.forceFloat(), b.forceFloat()));
    } else if (a.isInt() and b.isFloat()) {
        return SExpr.Float(at, try opFn(interpreter, at, @as(f64, @floatFromInt(a.forceInt())),  b.forceFloat()));
    } else if (a.isFloat() and b.isInt()) {
        return SExpr.Float(at, try opFn(interpreter, at, a.forceFloat(), @as(f64, @floatFromInt(b.forceInt()))));
    } else {
        return interpreter.abort(Interpreter.EvaluationError.TypeError, at, "expected an integer or a float, got {}: `{}` & {}: `{}`", .{a.getTag(), a, b.getTag(), b});
    }
}
