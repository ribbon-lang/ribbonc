const std = @import("std");

const TextUtils = @import("Utils").Text;

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Eval = Core.Eval;

pub const Doc =
    \\This module provides functions for working with symbols, paralleling the [`String` module](#string).
    \\
;

pub const Env = .{
    .{ "empty-symbol?", "check if a value is the empty symbol", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            return try SExpr.Bool(at, if (arg.castSymbolSlice()) |str| str.len == 0 else false);
        }
    } },
    .{ "symbol-length", "get the number of characters in a symbol", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            const str = try eval.castSymbolSlice(at, arg);
            const len = TextUtils.codepointCount(str) catch {
                return eval.abort(Eval.Error.BadEncoding, at, "bad utf8 symbol", .{});
            };
            if (len > std.math.maxInt(i64)) {
                return eval.abort(Eval.Error.RangeError, at, "symbol is too long to take its length", .{});
            }
            return try SExpr.Int(at, @intCast(len));
        }
    } },
    .{ "symbol-find", "within a given symbol, find the character index of another symbol, or a character; returns nil if not found", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const haystack = try eval.castSymbolSlice(at, rargs[0]);
            var needleBuf = [4]u8{ 0, 0, 0, 0 };
            const needle =
                if (rargs[1].castSymbolSlice()) |s| s
                else if (rargs[1].coerceNativeChar()) |c| needleBuf[0..(TextUtils.encode(c, &needleBuf)
                    catch return eval.abort(Eval.Error.TypeError, at, "bad char {}", .{c}))]
                else {
                    return eval.abort(Eval.Error.TypeError, at, "expected a symbol or char for symbol-intercalate separator, got {}: `{}`", .{ rargs[1].getTag(), rargs[1] });
                };
            const pos = TextUtils.findStrCodepointIndex(haystack, needle) catch {
                return eval.abort(Eval.Error.BadEncoding, at, "bad utf8 symbol", .{});
            } orelse {
                return try SExpr.Nil(at);
            };
            if (pos > std.math.maxInt(i64)) {
                return eval.abort(Eval.Error.RangeError, at, "symbol-find result is too large to fit in an integer", .{});
            }
            return try SExpr.Int(at, @intCast(pos));
        }
    } },
    .{ "symbol-concat", "given any number of symbols or characters, returns a new symbol with all of them concatenated in order", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            var newStr = std.ArrayList(u8).init(eval.context.allocator);
            while (try rargs.next()) |arg| {
                if (arg.castSymbolSlice()) |str| {
                    try newStr.appendSlice(str);
                } else if (arg.coerceNativeChar()) |char| {
                    var charBuf = [1]u8{0} ** 4;
                    const charSize = TextUtils.encode(char, &charBuf) catch {
                        return eval.abort(Eval.Error.TypeError, at, "bad char {}", .{char});
                    };
                    try newStr.appendSlice(charBuf[0..charSize]);
                } else {
                    return eval.abort(Eval.Error.TypeError, at, "expected a symbol or char, got {}", .{arg.getTag()});
                }
            }
            return try SExpr.Symbol(at, try newStr.toOwnedSlice());
        }
    } },
    .{ "symbol-intercalate", "given a symbol or a char, and any number of subsequent symbols or chars, returns a new symbol with all of the subsequent values concatenated in order with the first value in between concatenations", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            var newStr = std.ArrayList(u8).init(eval.context.allocator);
            var sepBuf = [4]u8{ 0, 0, 0, 0 };
            const sep = try rargs.atLeast();
            const sepStr =
                if (sep.castSymbolSlice()) |s| s
                else if (sep.coerceNativeChar()) |c| sepBuf[0..(TextUtils.encode(c, &sepBuf)
                    catch return eval.abort(Eval.Error.TypeError, at, "bad char {}", .{c}))]
                else {
                    return eval.abort(Eval.Error.TypeError, at, "expected a symbol or char for symbol-intercalate separator, got {}: `{}`", .{ sep.getTag(), sep });
                };
            if (!rargs.hasNext()) {
                try rargs.assertDone();
                return try SExpr.String(at, "");
            }
            var charBuf = [4]u8{ 0, 0, 0, 0 };
            const fst = try rargs.atLeast();
            const fstStr =
                if (fst.castSymbolSlice()) |s| s
                else if (fst.coerceNativeChar()) |c| charBuf[0..(TextUtils.encode(c, &charBuf)
                    catch return eval.abort(Eval.Error.TypeError, at, "bad char {}", .{c}))]
                else {
                    return eval.abort(Eval.Error.TypeError, at, "expected a symbol or char for symbol-intercalate argument, got {}: `{}`", .{ fst.getTag(), fst });
                };
            try newStr.appendSlice(fstStr);
            while (try rargs.next()) |arg| {
                const str =
                    if (arg.castSymbolSlice()) |s| s
                    else if (arg.coerceNativeChar()) |c| charBuf[0..(TextUtils.encode(c, &charBuf)
                        catch return eval.abort(Eval.Error.TypeError, at, "bad char {}", .{c}))]
                    else {
                        return eval.abort(Eval.Error.TypeError, at, "expected a symbol or char for symbol-intercalate argument, got {}: `{}`", .{ arg.getTag(), fst });
                    };
                try newStr.appendSlice(sepStr);
                try newStr.appendSlice(str);
            }
            return try SExpr.Symbol(at, try newStr.toOwnedSlice());
        }
    } },
    .{ "subsymbol", "given a symbol and two character indices, returns a new symbol containing the designated section; returns nil if out of range", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = [3]SExpr{ undefined, undefined, undefined };
            const len = try eval.resolveSmallList(args, 2, &rargs);
            const str = try eval.castSymbolSlice(at, rargs[0]);
            const start = try eval.coerceNativeInt(at, rargs[1]);
            if (start < 0) {
                return eval.abort(Eval.Error.RangeError, at, "expected a non-negative integer, got {}", .{start});
            }
            const end: i64 =
                if (len == 3)
                    try eval.coerceNativeInt(at, rargs[2])
                else if (str.len < std.math.maxInt(i64))
                    @intCast(str.len)
                else
                    return eval.abort(Eval.Error.RangeError, at, "subsymbol is too long to take its length", .{});
            if (end < 0) {
                return eval.abort(Eval.Error.RangeError, at, "expected a non-negative integer, got {}", .{end});
            }
            if (end < start) {
                return eval.abort(Eval.Error.RangeError, at, "subsymbol end index is less than start index", .{});
            }
            const startOffset = TextUtils.nthCodepointOffset(@intCast(start), str) catch {
                return eval.abort(Eval.Error.BadEncoding, at, "bad utf8 symbol", .{});
            } orelse {
                return try SExpr.Nil(at);
            };
            const endOffset = TextUtils.nthCodepointOffset(@intCast(end), str) catch {
                return eval.abort(Eval.Error.BadEncoding, at, "bad utf8 symbol", .{});
            } orelse {
                return try SExpr.Nil(at);
            };
            const newStr = str[startOffset..endOffset];
            return SExpr.Symbol(at, newStr);
        }
    } },
};
