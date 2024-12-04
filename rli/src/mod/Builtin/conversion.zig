const std = @import("std");

const MiscUtils = @import("Utils").Misc;
const TextUtils = @import("Utils").Text;

const Rli = @import("../root.zig");
const Source = Rli.Source;
const SExpr = Rli.SExpr;
const Interpreter = Rli.Interpreter;
const Parser = Rli.Parser;

pub const Doc =
    \\This module provides functions for converting between specific types,
    \\as well as converting between arbitrary values and strings.
    \\
;

pub const Decls = .{
    .{ "bool<-int", "convert an integer to a boolean", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.isInt()) {
                return try SExpr.Bool(at, arg.forceInt() != 0);
            } else if (arg.isBool()) {
                return arg;
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected an integer, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "int<-bool", "convert a boolean to an integer", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.isBool()) {
                return try SExpr.Int(at, @intFromBool(arg.forceBool()));
            } else if (arg.isInt()) {
                return arg;
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a bool, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "int<-char", "convert a character to an integer", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.isChar()) {
                return try SExpr.Int(at, arg.forceChar());
            } else if (arg.isInt()) {
                return arg;
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a char, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "char<-int", "convert an integer to a character", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.isInt()) {
                const i = arg.forceInt();
                if (i >= 0 and i <= 0x10FFFF) {
                    return try SExpr.Char(at, @as(u21, @intCast(arg.forceInt())));
                } else {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "expected an integer in range 0 to 0x10FFFF, got 0x{b:0>16}", .{i});
                }
            } else if (arg.isChar()) {
                return arg;
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected an integer, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "int<-float", "convert a float to an integer", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.isFloat()) {
                return try SExpr.Int(at, @intFromFloat(arg.forceFloat()));
            } else if (arg.isInt()) {
                return arg;
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a float, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "float<-int", "convert an integer to a float", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.isInt()) {
                return try SExpr.Float(at, @floatFromInt(arg.forceInt()));
            } else if (arg.isFloat()) {
                return arg;
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected an integer, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "string<-symbol", "convert a symbol to a string", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.isSymbol()) {
                return try SExpr.String(at, arg.forceSymbol().toSlice());
            } else if (arg.isString()) {
                return arg;
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a symbol, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "symbol<-string", "convert a string to a symbol", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.isString()) {
                return try SExpr.Symbol(at, arg.forceString().toSlice());
            } else if (arg.isSymbol()) {
                return arg;
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a string, got {}", .{arg.getTag()});
            }
        }
    } },
    .{ "list<-string", "convert a string to a list of characters", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            const str = try interpreter.castStringSlice(at, arg);
            var listBuf = std.ArrayList(SExpr).init(interpreter.context.allocator);
            defer listBuf.deinit();
            var i: usize = 0;
            while (i < str.len) {
                const dec = TextUtils.decode1(str[i..]) catch {
                    return interpreter.abort(Interpreter.Error.BadEncoding, at, "bad utf8 string", .{});
                };
                const char = try SExpr.Char(at, dec.ch);
                try listBuf.append(char);
                i += dec.len;
            }
            return try SExpr.List(at, listBuf.items);
        }
    } },
    .{ "string<-list", "convert a list of characters to a string", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const list = try interpreter.eval1(args);
            var mem = std.ArrayList(u8).init(interpreter.context.allocator);
            defer mem.deinit();
            var chars = try interpreter.argIterator(false, list);
            while (try chars.next()) |ch| {
                const char = try interpreter.coerceNativeChar(at, ch);
                var charBuf = [1]u8{0} ** 4;
                const charSize = TextUtils.encode(char, &charBuf) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad char {}", .{char});
                };
                try mem.appendSlice(charBuf[0..charSize]);
            }
            const newStr = try mem.toOwnedSlice();
            return try SExpr.StringPreallocatedUnchecked(at, newStr);
        }
    } },
    .{ "stringify", "convert any value to a string representation; optionally accepts two parameters where the first may be a symbol of the set `Display`, `Attr`, `Dotted`, or `Source`, indicating the way in which to format the second value", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rargs = [2]SExpr{ undefined, undefined };
            const len = try interpreter.evalSmallList(args, 1, &rargs);
            const str =
                if (len == 2)
            two: {
                const fmt = try interpreter.castSymbolSlice(at, rargs[0]);
                break :two if (std.mem.eql(u8, fmt, "Display"))
                    try std.fmt.allocPrint(interpreter.context.allocator, "{display}", .{rargs[1]})
                else if (std.mem.eql(u8, fmt, "Attr"))
                    try std.fmt.allocPrint(interpreter.context.allocator, "{attr}", .{rargs[1]})
                else if (std.mem.eql(u8, fmt, "Dotted"))
                    try std.fmt.allocPrint(interpreter.context.allocator, "{.}", .{rargs[1]})
                else if (std.mem.eql(u8, fmt, "Source"))
                    try std.fmt.allocPrint(interpreter.context.allocator, "{}", .{rargs[1]})
                else
                    return interpreter.abort(Interpreter.Error.TypeError, at, "expected format symbol to be either Display, Dotted, or Source, got {s}", .{fmt});
            } else try std.fmt.allocPrint(interpreter.context.allocator, "{}", .{rargs[0]});
            return try SExpr.StringPreallocated(at, str);
        }
    } },
    .{ "unstringify", "convert a string representation to a value", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            const str = try interpreter.castStringSlice(at, arg);
            const attr = arg.getAttr();
            const parser = try Parser.init(interpreter.context);
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
                    return interpreter.nativePrompt(at, "exception", &[_]SExpr{try SExpr.Symbol(eat, @errorName(err))});
                } else {
                    const nerr = Interpreter.asResult(err).?;
                    if (Interpreter.asError(nerr)) |eerr| {
                        if (interpreter.errDiagnosticFilled()) {
                            return nerr;
                        } else {
                            return interpreter.abort(eerr, at, "error in parser call", .{});
                        }
                    } else {
                        return Interpreter.asSignal(nerr).?;
                    }
                }
            } orelse {
                const eat = try parser.mkAttr(parser.pos, parser.pos);
                return interpreter.nativePrompt(at, "exception", &[_]SExpr{try SExpr.Symbol(eat, "UnstringifyEmpty")});
            };
            if (parser.notEof()) {
                const eat = try parser.mkAttr(parser.pos, parser.pos);
                return interpreter.nativePrompt(at, "exception", &[_]SExpr{try SExpr.Symbol(eat, "ExpectedEof")});
            }
            return out;
        }
    } },
};
