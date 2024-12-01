const std = @import("std");

const MiscUtils = @import("Utils").Misc;

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Eval = Core.Eval;
const Parser = Core.Parser;

pub const Doc =
    \\This module provides functions for converting between specific types,
    \\as well as converting between arbitrary values and strings.
    \\
;

pub const Env = .{
    .{ "bool<-int", "convert an integer to a boolean", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.isInt()) {
                return try SExpr.Bool(at, arg.forceInt() != 0);
            } else if (arg.isBool()) {
                return arg;
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected an integer, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "int<-bool", "convert a boolean to an integer", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.isBool()) {
                return try SExpr.Int(at, @intFromBool(arg.forceBool()));
            } else if (arg.isInt()) {
                return arg;
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a bool, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "int<-char", "convert a character to an integer", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.isChar()) {
                return try SExpr.Int(at, arg.forceChar());
            } else if (arg.isInt()) {
                return arg;
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a char, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "char<-int", "convert an integer to a character", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.isInt()) {
                const i = arg.forceInt();
                if (i >= 0 and i <= 0x10FFFF) {
                    return try SExpr.Char(at, @as(u21, @intCast(arg.forceInt())));
                } else {
                    return eval.abort(Eval.Error.TypeError, at, "expected an integer in range 0 to 0x10FFFF, got 0x{b:0>16}", .{i});
                }
            } else if (arg.isChar()) {
                return arg;
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected an integer, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "int<-float", "convert a float to an integer", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.isFloat()) {
                return try SExpr.Int(at, @intFromFloat(arg.forceFloat()));
            } else if (arg.isInt()) {
                return arg;
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a float, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "float<-int", "convert an integer to a float", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.isInt()) {
                return try SExpr.Float(at, @floatFromInt(arg.forceInt()));
            } else if (arg.isFloat()) {
                return arg;
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected an integer, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "string<-symbol", "convert a symbol to a string", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.isSymbol()) {
                return try SExpr.String(at, arg.forceSymbol().toSlice());
            } else if (arg.isString()) {
                return arg;
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a symbol, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "symbol<-string", "convert a string to a symbol", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.isString()) {
                return try SExpr.Symbol(at, arg.forceString().toSlice());
            } else if (arg.isSymbol()) {
                return arg;
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a string, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "stringify", "convert any value to a string representation; optionally accepts two parameters where the first may be a symbol of the set `Display`, `Attr`, `Dotted`, or `Source`, indicating the way in which to format the second value", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = [2]SExpr{ undefined, undefined };
            const len = try eval.resolveSmallList(args, 1, &rargs);
            const str =
                if (len == 2)
            two: {
                const fmt = try eval.castSymbolSlice(at, rargs[0]);
                break :two if (std.mem.eql(u8, fmt, "Display"))
                    try std.fmt.allocPrint(eval.context.allocator, "{display}", .{rargs[1]})
                else if (std.mem.eql(u8, fmt, "Attr"))
                    try std.fmt.allocPrint(eval.context.allocator, "{attr}", .{rargs[1]})
                else if (std.mem.eql(u8, fmt, "Dotted"))
                    try std.fmt.allocPrint(eval.context.allocator, "{.}", .{rargs[1]})
                else if (std.mem.eql(u8, fmt, "Source"))
                    try std.fmt.allocPrint(eval.context.allocator, "{}", .{rargs[1]})
                else
                    return eval.abort(Eval.Error.TypeError, at, "expected format symbol to be either Display, Dotted, or Source, got {s}", .{fmt});
            } else try std.fmt.allocPrint(eval.context.allocator, "{}", .{rargs[0]});
            return try SExpr.StringPreallocated(at, str);
        }
    } },
    .{ "unstringify", "convert a string representation to a value", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            const str = try eval.castStringSlice(at, arg);
            const attr = arg.getAttr();
            const parser = try Parser.init(eval.context);
            defer parser.deinit();
            const pos =
                if (attr.range) |r| (if (r.start) |s| s else null) else null;
            if (pos != null) {
                try parser.setFileName(attr.filename);
            } else {
                try parser.setFileName("unstringify");
            }
            parser.setInput(str, pos);
            const out = parser.scanSExprP() catch |err| {
                if (Parser.isParseError(err)) {
                    const eat = try parser.mkAttr(parser.pos, parser.pos);
                    return eval.nativePrompt(at, "exception", &[_]SExpr{try SExpr.Symbol(eat, @errorName(err))});
                } else {
                    const nerr = Eval.asResult(err).?;
                    if (Eval.asError(nerr)) |eerr| {
                        if (eval.errDiagnosticFilled()) {
                            return nerr;
                        } else {
                            return eval.abort(eerr, at, "error in parser call", .{});
                        }
                    } else {
                        return Eval.asSignal(nerr).?;
                    }
                }
            } orelse {
                const eat = try parser.mkAttr(parser.pos, parser.pos);
                return eval.nativePrompt(at, "exception", &[_]SExpr{try SExpr.Symbol(eat, "UnstringifyEmpty")});
            };
            if (parser.notEof()) {
                const eat = try parser.mkAttr(parser.pos, parser.pos);
                return eval.nativePrompt(at, "exception", &[_]SExpr{try SExpr.Symbol(eat, "ExpectedEof")});
            }
            return out;
        }
    } },
};
