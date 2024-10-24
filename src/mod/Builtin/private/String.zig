const std = @import("std");

const TextUtils = @import("ZigUtils").Text;

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Eval = Core.Eval;

pub const Doc =
    \\This module provides functions for working with strings,
    \\as well as conversions between strings and lists of chars.
    \\
    \\The functions in this module are designed to be utf8-safe, and will
    \\generally cause a compilation error if used improperly.
    \\
    \\Most functions come in a codepoint-indexed and byte-index variant.
    \\
    \\> [!Caution]
    \\> Special care must be take in particular with the byte-indexed
    \\> functions to avoid causing errors, as they validate that their operation is
    \\> boundary-aligned.
    \\
;

pub const Env = .{
    .{ "empty-string?", "check if a value is the empty string", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            return try SExpr.Bool(at, if (arg.castStringSlice()) |str| str.len == 0 else false);
        }
    } },
    .{ "string-length", "get the number of characters in a string", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            const str = try eval.castStringSlice(at, arg);
            const len = TextUtils.codepointCount(str) catch {
                return eval.abort(Eval.Error.BadEncoding, at, "bad utf8 string", .{});
            };
            if (len > std.math.maxInt(i64)) {
                return eval.abort(Eval.Error.RangeError, at, "string is too long to take its length", .{});
            }
            return try SExpr.Int(at, @intCast(len));
        }
    } },
    .{ "list<-string", "convert a string to a list of characters", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            const str = try eval.castStringSlice(at, arg);
            var listBuf = std.ArrayList(SExpr).init(eval.context.allocator);
            defer listBuf.deinit();
            var i: usize = 0;
            while (i < str.len) {
                const dec = TextUtils.decode1(str[i..]) catch {
                    return eval.abort(Eval.Error.BadEncoding, at, "bad utf8 string", .{});
                };
                const char = try SExpr.Char(at, dec.ch);
                try listBuf.append(char);
                i += dec.len;
            }
            return try SExpr.List(at, listBuf.items);
        }
    } },
    .{ "string<-list", "convert a list of characters to a string", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const list = try eval.resolve1(args);
            var mem = std.ArrayList(u8).init(eval.context.allocator);
            defer mem.deinit();
            var chars = try eval.argIterator(false, list);
            while (try chars.next()) |ch| {
                const char = try eval.coerceNativeChar(at, ch);
                var charBuf = [1]u8{0} ** 4;
                const charSize = TextUtils.encode(char, &charBuf) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad char {}", .{char});
                };
                try mem.appendSlice(charBuf[0..charSize]);
            }
            const newStr = try mem.toOwnedSlice();
            return try SExpr.StringPreallocatedUnchecked(at, newStr);
        }
    } },
    .{ "string-find", "within a given string, find the character index of another string, or a character; returns nil if not found", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const haystack = try eval.castStringSlice(at, rargs[0]);
            var needleBuf = [4]u8{ 0, 0, 0, 0 };
            const needle =
                if (rargs[1].castStringSlice()) |s| s
                else if (rargs[1].coerceNativeChar()) |c| needleBuf[0..(TextUtils.encode(c, &needleBuf)
                    catch return eval.abort(Eval.Error.TypeError, at, "bad char {}", .{c}))]
                else {
                    return eval.abort(Eval.Error.TypeError, at, "expected a string or char for string-intercalate separator, got {}: `{}`", .{ rargs[1].getTag(), rargs[1] });
                };
            const pos = TextUtils.findStrCodepointIndex(haystack, needle) catch {
                return eval.abort(Eval.Error.BadEncoding, at, "bad utf8 string", .{});
            } orelse {
                return try SExpr.Nil(at);
            };
            if (pos > std.math.maxInt(i64)) {
                return eval.abort(Eval.Error.RangeError, at, "string-find result is too large to fit in an integer", .{});
            }
            return try SExpr.Int(at, @intCast(pos));
        }
    } },
    .{ "string-find-byte-offset", "within a given string, find the byte index of another string; returns nil if not found", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const haystack = try eval.castStringSlice(at, rargs[0]);
            var needleBuf = [4]u8{ 0, 0, 0, 0 };
            const needle =
                if (rargs[1].castStringSlice()) |s| s
                else if (rargs[1].coerceNativeChar()) |c| needleBuf[0..(TextUtils.encode(c, &needleBuf)
                    catch return eval.abort(Eval.Error.TypeError, at, "bad char {}", .{c}))]
                else {
                    return eval.abort(Eval.Error.TypeError, at, "expected a string or char for string-intercalate separator, got {}: `{}`", .{ rargs[1].getTag(), rargs[1] });
                };
            const pos = TextUtils.findStr(haystack, needle) orelse {
                return try SExpr.Nil(at);
            };
            if (pos > std.math.maxInt(i64)) {
                return eval.abort(Eval.Error.RangeError, at, "string-find-byte-offset result is too large to fit in an integer", .{});
            }
            return try SExpr.Int(at, @intCast(pos));
        }
    } },
    .{ "nth-char", "get the character at the given character index; returns nil if out of range", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const n = try eval.coerceNativeInt(at, rargs[0]);
            if (n < 0) {
                return eval.abort(Eval.Error.RangeError, at, "expected a non-negative integer, got {}", .{n});
            }
            const str = try eval.castStringSlice(at, rargs[1]);
            const char = TextUtils.nthCodepoint(@intCast(n), str) catch {
                return eval.abort(Eval.Error.BadEncoding, at, "bad utf8 string", .{});
            } orelse {
                return try SExpr.Nil(at);
            };
            return try SExpr.Char(at, char);
        }
    } },
    .{ "index<-byte-offset", "given a string, convert a byte index within it to a character index; returns nil if out of range", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const str = try eval.castStringSlice(at, rargs[0]);
            const offset = try eval.coerceNativeInt(at, rargs[1]);
            if (offset < 0) {
                return eval.abort(Eval.Error.RangeError, at, "expected a non-negative integer, got {}", .{offset});
            }
            const index = TextUtils.offsetToCodepointIndex(str, @intCast(offset)) catch {
                return try SExpr.Nil(at);
            };
            if (index > std.math.maxInt(i64)) {
                return eval.abort(Eval.Error.RangeError, at, "index<-byte-offset result is too large to fit in an integer", .{});
            }
            return try SExpr.Int(at, @intCast(index));
        }
    } },
    .{ "byte-offset<-index", "given a string, convert a character index within it to a byte index; returns nil if out of range or mis-aligned ", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const str = try eval.castStringSlice(at, rargs[0]);
            const n = try eval.coerceNativeInt(at, rargs[1]);
            if (n < 0) {
                return eval.abort(Eval.Error.RangeError, at, "expected a non-negative integer, got {}", .{n});
            }
            const index = TextUtils.nthCodepointOffset(@intCast(n), str) catch {
                return try SExpr.Nil(at);
            } orelse {
                return try SExpr.Nil(at);
            };
            if (index > std.math.maxInt(i64)) {
                return eval.abort(Eval.Error.RangeError, at, "nth-char-offset result is too large to fit in an integer", .{});
            }
            return try SExpr.Int(at, @intCast(index));
        }
    } },
    .{ "string-concat", "given any number of strings or characters, returns a new string with all of them concatenated in order", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            var newStr = std.ArrayList(u8).init(eval.context.allocator);
            while (try rargs.next()) |arg| {
                if (arg.castStringSlice()) |str| {
                    try newStr.appendSlice(str);
                } else if (arg.coerceNativeChar()) |char| {
                    var charBuf = [1]u8{0} ** 4;
                    const charSize = TextUtils.encode(char, &charBuf) catch {
                        return eval.abort(Eval.Error.TypeError, at, "bad char {}", .{char});
                    };
                    try newStr.appendSlice(charBuf[0..charSize]);
                } else {
                    return eval.abort(Eval.Error.TypeError, at, "expected a string or char, got {}", .{arg.getTag()});
                }
            }
            return try SExpr.StringPreallocatedUnchecked(at, try newStr.toOwnedSlice());
        }
    } },
    .{ "string-intercalate", "given a string or a char, and any number of subsequent strings or chars, returns a new string with all of the subsequent values concatenated in order with the first value in between concatenations", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            var newStr = std.ArrayList(u8).init(eval.context.allocator);
            var sepBuf = [4]u8{ 0, 0, 0, 0 };
            const sep = try rargs.atLeast();
            const sepStr =
                if (sep.castStringSlice()) |s| s
                else if (sep.coerceNativeChar()) |c| sepBuf[0..(TextUtils.encode(c, &sepBuf)
                    catch return eval.abort(Eval.Error.TypeError, at, "bad char {}", .{c}))]
                else {
                    return eval.abort(Eval.Error.TypeError, at, "expected a string or char for string-intercalate separator, got {}: `{}`", .{ sep.getTag(), sep });
                };
            if (!rargs.hasNext()) {
                try rargs.assertDone();
                return try SExpr.String(at, "");
            }
            var charBuf = [4]u8{ 0, 0, 0, 0 };
            const fst = try rargs.atLeast();
            const fstStr =
                if (fst.castStringSlice()) |s| s
                else if (fst.coerceNativeChar()) |c| charBuf[0..(TextUtils.encode(c, &charBuf)
                    catch return eval.abort(Eval.Error.TypeError, at, "bad char {}", .{c}))]
                else {
                    return eval.abort(Eval.Error.TypeError, at, "expected a string or char for string-intercalate argument, got {}: `{}`", .{ fst.getTag(), fst });
                };
            try newStr.appendSlice(fstStr);
            while (try rargs.next()) |arg| {
                const str =
                    if (arg.castStringSlice()) |s| s
                    else if (arg.coerceNativeChar()) |c| charBuf[0..(TextUtils.encode(c, &charBuf)
                        catch return eval.abort(Eval.Error.TypeError, at, "bad char {}", .{c}))]
                    else {
                        return eval.abort(Eval.Error.TypeError, at, "expected a string or char for string-intercalate argument, got {}: `{}`", .{ arg.getTag(), fst });
                    };
                try newStr.appendSlice(sepStr);
                try newStr.appendSlice(str);
            }
            return try SExpr.StringPreallocatedUnchecked(at, try newStr.toOwnedSlice());
        }
    } },
    .{ "substring", "given a string and two character indices, returns a new string containing the designated section; returns nil if out of range", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = [3]SExpr{ undefined, undefined, undefined };
            const len = try eval.resolveSmallList(args, 2, &rargs);
            const str = try eval.castStringSlice(at, rargs[0]);
            const start = try eval.coerceNativeInt(at, rargs[1]);
            if (start < 0) {
                return eval.abort(Eval.Error.RangeError, at, "expected a non-negative integer, got {}", .{start});
            }
            const end: i64 =
                if (len == 3) try eval.coerceNativeInt(at, rargs[2])
                else if (str.len < std.math.maxInt(i64)) @intCast(str.len)
                else return eval.abort(Eval.Error.RangeError, at, "substring is too long to take its length", .{});
            if (end < 0) {
                return eval.abort(Eval.Error.RangeError, at, "expected a non-negative integer, got {}", .{end});
            }
            if (end < start) {
                return eval.abort(Eval.Error.RangeError, at, "substring end index is less than start index", .{});
            }
            const startOffset = TextUtils.nthCodepointOffset(@intCast(start), str) catch {
                return eval.abort(Eval.Error.BadEncoding, at, "bad utf8 string", .{});
            } orelse {
                return try SExpr.Nil(at);
            };
            const endOffset = TextUtils.nthCodepointOffset(@intCast(end), str) catch {
                return eval.abort(Eval.Error.BadEncoding, at, "bad utf8 string", .{});
            } orelse {
                return try SExpr.Nil(at);
            };
            const newStr = str[startOffset..endOffset];
            return SExpr.StringPreallocatedUnchecked(at, newStr);
        }
    } },
    .{ "substring-byte-offset", "given a string and two byte indices, returns a new string containing the designated section; ; returns nil if out of range or mis-aligned", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolveListInRange(args, 2, 3);
            const str = try eval.castStringSlice(at, rargs[0]);
            const start = try eval.coerceNativeInt(at, rargs[1]);
            if (start < 0) {
                return eval.abort(Eval.Error.RangeError, at, "expected a non-negative integer, got {}", .{start});
            }
            if (start > str.len) {
                return SExpr.Nil(at);
            }
            const end: i64 = if (rargs.len == 3)
                try eval.coerceNativeInt(at, rargs[2])
            else if (str.len < std.math.maxInt(i64))
                @intCast(str.len)
            else
                return eval.abort(Eval.Error.RangeError, at, "string is too long to take its length", .{});
            if (end < 0) {
                return eval.abort(Eval.Error.RangeError, at, "expected a non-negative integer, got {}", .{end});
            }
            if (end < start) {
                return eval.abort(Eval.Error.RangeError, at, "substring end index is less than start index", .{});
            }
            if (end > str.len) {
                return SExpr.Nil(at);
            }
            const newStr = str[@intCast(start)..@intCast(end)];
            if (!TextUtils.isValidStr(newStr)) {
                return eval.abort(Eval.Error.BadEncoding, at, "bad utf8 substring", .{});
            }
            return SExpr.StringPreallocatedUnchecked(at, newStr);
        }
    } },
    .{ "format", "stringify all arguments with `'Display`, then concatenate", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            var out = std.ArrayList(u8).init(eval.context.allocator);
            defer out.deinit();
            const writer = out.writer();
            while (try rargs.next()) |next| {
                try writer.print("{display}", .{next});
            }
            return SExpr.StringPreallocatedUnchecked(at, try out.toOwnedSlice());
        }
    } },
};
