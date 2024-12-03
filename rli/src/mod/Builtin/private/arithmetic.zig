const std = @import("std");

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Interpreter = Core.Interpreter;

pub const Doc =
    \\This module provides basic arithmetic functions, constants, and predicates.
    \\
;

fn checkIsIntOrFloat(interpreter: *Interpreter, at: *const Source.Attr, index: usize, a: SExpr) Interpreter.Result!void {
    if (!(a.isInt() or a.isFloat()))
        return interpreter.abort(Interpreter.Error.TypeError, at, "expected an integer or a float for argument {}, got {}: `{}`", .{ index + 1, a.getTag(), a });
}

pub const Env = .{
    .{ .{ "add", "+" }, "integer/floating point addition on any number of values", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rArgs = try interpreter.argIterator(true, args);
            var a = try rArgs.atLeast();
            try checkIsIntOrFloat(interpreter, at, 0, a);
            if (!rArgs.hasNext()) {
                switch (a.getTag()) {
                    .Int => return try SExpr.Int(at, @as(i64, @intCast(@abs(a.forceInt())))),
                    .Float => return try SExpr.Float(at, @abs(a.forceFloat())),
                    else => unreachable,
                }
            }
            while (try rArgs.nextWithIndex()) |next| {
                const b = next[0];
                const i = next[1];
                try checkIsIntOrFloat(interpreter, at, i, b);
                if (a.isInt() and b.isInt()) {
                    a = try SExpr.Int(at, a.forceInt() + b.forceInt());
                } else if (a.isFloat() or b.isFloat()) {
                    a = try SExpr.Float(at, a.forceFloat() + b.forceFloat());
                } else if (a.isInt() and b.isFloat()) {
                    a = try SExpr.Float(at, @as(f64, @floatFromInt(a.forceInt())) + b.forceFloat());
                } else if (a.isFloat() and b.isInt()) {
                    a = try SExpr.Float(at, a.forceFloat() + @as(f64, @floatFromInt(b.forceInt())));
                } else unreachable;
            }
            return a;
        }
    } },
    .{ .{ "sub", "-" }, "integer/floating point subtraction on any number of values", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rArgs = try interpreter.argIterator(true, args);
            var a = try rArgs.atLeast();
            try checkIsIntOrFloat(interpreter, at, 0, a);
            if (!rArgs.hasNext()) {
                switch (a.getTag()) {
                    .Int => return try SExpr.Int(at, -a.forceInt()),
                    .Float => return try SExpr.Float(at, -a.forceFloat()),
                    else => unreachable,
                }
            }
            while (try rArgs.nextWithIndex()) |next| {
                const b = next[0];
                const i = next[1];
                try checkIsIntOrFloat(interpreter, at, i, b);
                if (a.isInt() and b.isInt()) {
                    a = try SExpr.Int(at, a.forceInt() - b.forceInt());
                } else if (a.isFloat() or b.isFloat()) {
                    a = try SExpr.Float(at, a.forceFloat() - b.forceFloat());
                } else if (a.isInt() and b.isFloat()) {
                    a = try SExpr.Float(at, @as(f64, @floatFromInt(a.forceInt())) - b.forceFloat());
                } else if (a.isFloat() and b.isInt()) {
                    a = try SExpr.Float(at, a.forceFloat() - @as(f64, @floatFromInt(b.forceInt())));
                } else unreachable;
            }
            return a;
        }
    } },
    .{ .{ "mul", "*" }, "integer/floating point multiplication on any number of values", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rArgs = try interpreter.argIterator(true, args);
            var a = try rArgs.atLeast();
            try checkIsIntOrFloat(interpreter, at, 0, a);
            while (try rArgs.nextWithIndex()) |next| {
                const b = next[0];
                const i = next[1];
                try checkIsIntOrFloat(interpreter, at, i, b);
                if (a.isInt() and b.isInt()) {
                    a = try SExpr.Int(at, a.forceInt() * b.forceInt());
                } else if (a.isFloat() or b.isFloat()) {
                    a = try SExpr.Float(at, a.forceFloat() * b.forceFloat());
                } else if (a.isInt() and b.isFloat()) {
                    a = try SExpr.Float(at, @as(f64, @floatFromInt(a.forceInt())) * b.forceFloat());
                } else if (a.isFloat() and b.isInt()) {
                    a = try SExpr.Float(at, a.forceFloat() * @as(f64, @floatFromInt(b.forceInt())));
                } else unreachable;
            }
            return a;
        }
    } },
    .{ .{ "div", "/" }, "integer/floating point division on any number of values", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rArgs = try interpreter.argIterator(true, args);
            var a = try rArgs.atLeast();
            try checkIsIntOrFloat(interpreter, at, 0, a);
            while (try rArgs.nextWithIndex()) |next| {
                const b = next[0];
                const i = next[1];
                try checkIsIntOrFloat(interpreter, at, i, b);
                if (a.isInt() and b.isInt()) {
                    const ib = b.forceInt();
                    if (ib == 0) {
                        return interpreter.abort(Interpreter.Error.DivisionByZero, b.getAttr(), "integer division by zero", .{});
                    }
                    a = try SExpr.Int(at, @divFloor(a.forceInt(), ib));
                } else if (a.isFloat() or b.isFloat()) {
                    const fb = b.forceFloat();
                    if (fb == 0.0) {
                        return interpreter.abort(Interpreter.Error.DivisionByZero, b.getAttr(), "float division by zero", .{});
                    }
                    a = try SExpr.Float(at, a.forceFloat() / fb);
                } else if (a.isInt() and b.isFloat()) {
                    const fb = b.forceFloat();
                    if (fb == 0.0) {
                        return interpreter.abort(Interpreter.Error.DivisionByZero, b.getAttr(), "float division by zero", .{});
                    }
                    a = try SExpr.Float(at, @as(f64, @floatFromInt(a.forceInt())) / fb);
                } else if (a.isFloat() and b.isInt()) {
                    const fb = @as(f64, @floatFromInt(b.forceInt()));
                    if (fb == 0.0) {
                        return interpreter.abort(Interpreter.Error.DivisionByZero, b.getAttr(), "float division by zero", .{});
                    }
                    a = try SExpr.Float(at, a.forceFloat() / fb);
                } else unreachable;
            }
            return a;
        }
    } },
    .{ .{ "mod", "%" }, "integer/floating point remainder division on any number of values", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rArgs = try interpreter.argIterator(true, args);
            var a = try rArgs.atLeast();
            try checkIsIntOrFloat(interpreter, at, 0, a);
            while (try rArgs.nextWithIndex()) |next| {
                const b = next[0];
                const i = next[1];
                try checkIsIntOrFloat(interpreter, at, i, b);
                if (a.isInt() and b.isInt()) {
                    const ib = b.forceInt();
                    if (ib == 0) {
                        return interpreter.abort(Interpreter.Error.DivisionByZero, b.getAttr(), "integer modulo by zero", .{});
                    }
                    a = try SExpr.Int(at, @mod(a.forceInt(), ib));
                } else if (a.isFloat() or b.isFloat()) {
                    const fb = b.forceFloat();
                    if (fb == 0.0) {
                        return interpreter.abort(Interpreter.Error.DivisionByZero, b.getAttr(), "float modulo by zero", .{});
                    }
                    a = try SExpr.Float(at, @mod(a.forceFloat(), fb));
                } else if (a.isInt() and b.isFloat()) {
                    const fb = b.forceFloat();
                    if (fb == 0.0) {
                        return interpreter.abort(Interpreter.Error.DivisionByZero, b.getAttr(), "float modulo by zero", .{});
                    }
                    a = try SExpr.Float(at, @mod(@as(f64, @floatFromInt(a.forceInt())), fb));
                } else if (a.isFloat() and b.isInt()) {
                    const fb = @as(f64, @floatFromInt(b.forceInt()));
                    if (fb == 0.0) {
                        return interpreter.abort(Interpreter.Error.DivisionByZero, b.getAttr(), "float modulo by zero", .{});
                    }
                    a = try SExpr.Float(at, @mod(a.forceFloat(), fb));
                } else unreachable;
            }
            return a;
        }
    } },
    .{ .{ "pow", "^" }, "integer/floating point exponentiation on any number of values", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rArgs = try interpreter.argIterator(true, args);
            var a = try rArgs.atLeast();
            try checkIsIntOrFloat(interpreter, at, 0, a);
            while (try rArgs.nextWithIndex()) |next| {
                const b = next[0];
                const i = next[1];
                try checkIsIntOrFloat(interpreter, at, i, b);
                if (a.isInt() and b.isInt()) {
                    a = try SExpr.Int(at, std.math.pow(i64, a.forceInt(), b.forceInt()));
                } else if (a.isFloat() or b.isFloat()) {
                    a = try SExpr.Float(at, std.math.pow(f64, a.forceFloat(), b.forceFloat()));
                } else if (a.isInt() and b.isFloat()) {
                    a = try SExpr.Float(at, std.math.pow(f64, @as(f64, @floatFromInt(a.forceInt())), b.forceFloat()));
                } else if (a.isFloat() and b.isInt()) {
                    a = try SExpr.Float(at, std.math.pow(f64, a.forceFloat(), @as(f64, @floatFromInt(b.forceInt()))));
                } else unreachable;
            }
            return a;
        }
    } },

    .{ "nan?", "check if input is not a number", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            return try SExpr.Bool(at, if (arg.isFloat()) std.math.isNan(arg.forceFloat()) else !arg.isInt());
        }
    } },
    .{ "inf?", "check if input is a floating point infinity", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            return try SExpr.Bool(at, if (arg.isFloat()) std.math.isInf(arg.forceFloat()) else false);
        }
    } },
    .{ "-inf?", "check if input is a negative floating point infinity", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            return try SExpr.Bool(at, if (arg.isFloat()) std.math.isNegativeInf(arg.forceFloat()) else false);
        }
    } },
    .{ "+inf?", "check if input is a positive floating point infinity", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            return try SExpr.Bool(at, if (arg.isFloat()) std.math.isPositiveInf(arg.forceFloat()) else false);
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
            const arg = try interpreter.eval1(args);
            if (arg.isInt()) {
                return try SExpr.Float(at, @floatFromInt(arg.forceInt()));
            } else if (arg.isFloat()) {
                return try SExpr.Float(at, @floor(arg.forceFloat()));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected an integer or a float, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "ceil", "round a floating point number up", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.isInt()) {
                return try SExpr.Float(at, @floatFromInt(arg.forceInt()));
            } else if (arg.isFloat()) {
                return try SExpr.Float(at, @ceil(arg.forceFloat()));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected an integer or a float, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "round", "round a floating point number", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.isInt()) {
                return try SExpr.Float(at, @floatFromInt(arg.forceInt()));
            } else if (arg.isFloat()) {
                return try SExpr.Float(at, @round(arg.forceFloat()));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected an integer or a float, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "frac", "take the fractional part of a floating point number", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.isInt()) {
                return try SExpr.Float(at, @floatFromInt(arg.forceInt()));
            } else if (arg.isFloat()) {
                return try SExpr.Float(at, arg.forceFloat() - @trunc(arg.forceFloat()));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected an integer or a float, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
};
