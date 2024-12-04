const std = @import("std");

const TextUtils = @import("Utils").Text;

const Rli = @import("../root.zig");
const Source = Rli.Source;
const SExpr = Rli.SExpr;
const Interpreter = Rli.Interpreter;

pub const Doc =
    \\This module provides predicate and conversion functions,
    \\to enable working with utf8 text and utf32 codepoints.
    \\
    \\All functions here are overloaded to work both with
    \\single characters, as well as strings.
    \\
;

pub const Decls = .{
    .{ "text/category", "given a char, gives a symbol representing the unicode general category", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            const char = try interpreter.coerceNativeChar(at, arg);
            return try SExpr.Symbol(at, @tagName(TextUtils.generalCategory(char)));
        }
    } },
    .{ "text/describe-category", "given a unicode character category symbol, returns a string explaining the value", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            const sym = try interpreter.castSymbolSlice(at, arg);
            const cat = TextUtils.generalCategoryFromName(sym) orelse {
                return interpreter.abort(Interpreter.Error.TypeError, at, "unknown unicode general category `{s}`", .{sym});
            };
            return try SExpr.StringPreallocatedUnchecked(at, TextUtils.describeGeneralCategory(cat));
        }
    } },
    .{ "text/control?", "given a string or char, checks if all characters are control characters", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isControlStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isControl(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/letter?", "given a string or char, checks if all characters are letter characters", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isLetterStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isLetter(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/mark?", "given a string or char, checks if all characters are mark characters", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isMarkStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isMark(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/number?", "given a string or char, checks if all characters are number characters", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isNumberStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isNumber(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/punctuation?", "given a string or char, checks if all characters are punctuation characters", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isPunctuationStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isPunctuation(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/separator?", "given a string or char, checks if all characters are separator characters", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isSeparatorStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isSeparator(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/symbol?", "given a string or char, checks if all characters are symbol characters", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isSymbolStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isSymbol(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/math?", "given a string or char, checks if all characters are math characters", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isMathStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isMath(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/alphabetic?", "given a string or char, checks if all characters are alphabetic characters", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isAlphabeticStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isAlphabetic(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/id-start?", "given a string or char, checks if all characters are id-start characters char", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isIdStartStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isIdStart(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/id-continue?", "given a string or char, checks if all characters are id-continue characters char", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isIdContinueStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isIdContinue(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/xid-start?", "given a string or char, checks if all characters are xid-start characters char", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isXidStartStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isXidStart(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/xid-continue?", "given a string or char, checks if all characters are xid-continue characters char", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isXidContinueStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isXidContinue(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/space?", "given a string or char, checks if all characters are space characters", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isSpaceStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isSpace(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/hex-digit?", "given a string or char, checks if all characters are hexadecimal digit characters char", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isHexDigitStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isHexDigit(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/diacritic?", "given a string or char, checks if all characters are diacritic characters", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isDiacriticStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isDiacritic(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/numeric?", "given a string or char, checks if all characters are numeric characters", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isNumericStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isNumeric(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/digit?", "given a string or char, checks if all characters are digit characters", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isDigitStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isDigit(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/decimal?", "given a string or char, checks if all characters are decimal digit characters char", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isDecimalStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isDecimal(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/hex?", "given a string or char, checks if all characters are hexadecimal digit characters char", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isHexDigitStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isHexDigit(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },

    .{ "text/lowercase?", "given a string or char, checks if all characters are lowercase", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            return try SExpr.Bool(at, if (arg.castStringSlice()) |str| TextUtils.isLowerStr(str) else if (arg.coerceNativeChar()) |char| TextUtils.isLower(char) else false);
        }
    } },
    .{ "text/uppercase?", "given a string or char, checks if all characters are uppercase", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            return try SExpr.Bool(at, if (arg.castStringSlice()) |str| TextUtils.isUpperStr(str) else if (arg.coerceNativeChar()) |char| TextUtils.isUpper(char) else false);
        }
    } },
    .{ "text/lowercase", "given a string or char, returns a new copy with all of the characters converted to lowercase", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                const newStr = TextUtils.toLowerStr(interpreter.context.allocator, str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                };
                return try SExpr.StringPreallocatedUnchecked(at, newStr);
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Char(at, TextUtils.toLower(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/uppercase", "given a string or char, returns a new copy with all of the characters converted to uppercase", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                const newStr = TextUtils.toUpperStr(interpreter.context.allocator, str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                };
                return try SExpr.StringPreallocatedUnchecked(at, newStr);
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Char(at, TextUtils.toUpper(char));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/casefold", "given a string or a char, returns a new copy with all characters converted with unicode case folding; note that is may require converting chars to strings", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                const newStr = TextUtils.caseFoldStr(interpreter.context.allocator, str) catch {
                    return interpreter.abort(Interpreter.Error.BadEncoding, at, "bad utf8 string", .{});
                };
                return try SExpr.StringPreallocatedUnchecked(at, newStr);
            } else if (arg.coerceNativeChar()) |char| {
                const newChars = TextUtils.caseFold(char);
                if (newChars.len == 1) {
                    return try SExpr.Char(at, newChars[0]);
                } else {
                    var newStr = std.ArrayList(u8).init(interpreter.context.allocator);
                    defer newStr.deinit();
                    var byteBuf = [4]u8{ 0, 0, 0, 0 };
                    for (newChars) |ch| {
                        const len = TextUtils.encode(ch, &byteBuf) catch {
                            return interpreter.abort(Interpreter.Error.BadEncoding, at, "bad utf32 char", .{});
                        };
                        try newStr.appendSlice(byteBuf[0..len]);
                    }
                    return try SExpr.StringPreallocatedUnchecked(at, try newStr.toOwnedSlice());
                }
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/byte-count", "given a string or a char, returns the number of bytes required to represent it as text/8", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                const len = str.len;
                if (len > std.math.maxInt(i64)) {
                    return interpreter.abort(Interpreter.Error.RangeError, at, "string is too long to take its byte count", .{});
                }
                return try SExpr.Int(at, @intCast(len));
            } else if (arg.coerceNativeChar()) |char| {
                const len = TextUtils.sequenceLength(char) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf32 char", .{});
                };
                return try SExpr.Int(at, @intCast(len));
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/display-width", "given a string or a char, returns the width of the value in visual columns (approximate)", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            if (arg.castStringSlice()) |str| {
                const width = TextUtils.displayWidthStr(str) catch {
                    return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                };
                return try SExpr.Int(at, width);
            } else if (arg.coerceNativeChar()) |char| {
                const width = TextUtils.displayWidth(char);
                return try SExpr.Int(at, width);
            } else {
                return interpreter.abort(Interpreter.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "text/case-insensitive-eq?", "compare two strings or chars using unicode case folding to ignore case", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const eargs = try interpreter.evalListInRange(args, 2, 2);
            if (eargs[0].castStringSlice()) |str1| {
                if (eargs[1].castStringSlice()) |str2| {
                    return try SExpr.Bool(at, TextUtils.caseInsensitiveCompareStr(str1, str2) catch {
                        return interpreter.abort(Interpreter.Error.TypeError, at, "bad utf8 string", .{});
                    });
                }
            } else if (eargs[0].coerceNativeChar()) |char1| {
                if (eargs[1].coerceNativeChar()) |char2| {
                    return try SExpr.Bool(at, TextUtils.caseInsensitiveCompare(char1, char2));
                }
            }
            return interpreter.abort(Interpreter.Error.TypeError, at, "expected two Strings or Two chars, got {}: `{}`, and {}: `{}`", .{ eargs[0].getTag(), eargs[0], eargs[1].getTag(), eargs[1] });
        }
    } },
};
