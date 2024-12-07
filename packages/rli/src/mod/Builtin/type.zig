const std = @import("std");

const Rli = @import("../root.zig");
const Source = Rli.Source;
const SExpr = Rli.SExpr;
const Interpreter = Rli.Interpreter;

pub const Doc =
    \\This module provides facilities for the inspection of value types.
    \\
;

pub const Decls = .{
    // TODO: this should just be type-of
    .{ "type/of", "get a symbol representing the type of a value", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Symbol(at, if (arg.castExternData()) |ext| ext.typeNameSlice() else arg.getTag().toSlice());
        }
    } },

    .{ "nil?", "determine if a value is the empty list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.isNil());
        }
    } },
    .{ "atom?", "determine if a value is an atom", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, !arg.isCons());
        }
    } },
    .{ "pair?", "determine if a value is a cons pair", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.isCons());
        }
    } },
    .{ "list?", "determine if a value is a cons pair or nil", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.isCons() or arg.isNil());
        }
    } },
    .{ "bool?", "determine if a value is a boolean", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.isBool());
        }
    } },
    .{ "int?", "determine if a value is an integer", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.isInt());
        }
    } },
    .{ "char?", "determine if a value is a character", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.isChar());
        }
    } },
    .{ "float?", "determine if a value is a floating point number", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.isFloat());
        }
    } },
    .{ "string?", "determine if a value is a string", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.isString());
        }
    } },
    .{ "symbol?", "determine if a value is a symbol", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.isSymbol());
        }
    } },
    .{ "function?", "determine if a value is a function", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.isFunction());
        }
    } },
    .{ "lambda?", "determine if a value is a lambda", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.isLambda());
        }
    } },
    .{ "macro?", "determine if a value is a macro", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.isMacro());
        }
    } },
    .{ "extern-data?", "determine if a value is external data such as a file", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const eargs = try interpreter.evalListInRange(args, 1, 2);
            if (eargs.len == 1) {
                return try SExpr.Bool(at, eargs[0].isExternData());
            } else {
                return try SExpr.Bool(at, eargs[0].isNamedExternData(eargs[1].castSymbolSlice() orelse {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "expected an extern data type name symbol, got {}", .{eargs[1].getTag()});
                }));
            }
        }
    } },
    .{ "extern-function?", "determine if a value is an external function", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.isExternFunction());
        }
    } },
    .{ "builtin?", "determine if a value is a builtin function", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.isBuiltin());
        }
    } },
    .{ "callable?", "determine if a value is callable", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try SExpr.Bool(at, arg.isCallable());
        }
    } },
};
