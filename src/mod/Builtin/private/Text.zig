const std = @import("std");

const TextUtils = @import("ZigUtils").Text;

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Eval = Core.Eval;

pub const Doc =
    \\This module provides predicate and conversion functions,
    \\to enable working with utf8 text and utf32 codepoints.
    \\
    \\All functions here are overloaded to work both with
    \\single characters, as well as strings.
    \\
;

pub const Env = .{
    .{ "utf-category", "given a char, gives a symbol representing the unicode general category", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            const char = try eval.coerceNativeChar(at, arg);
            return try SExpr.Symbol(at, @tagName(TextUtils.generalCategory(char)));
        }
    } },
    .{ "utf-describe-category", "given a unicode character category symbol, returns a string explaining the value", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            const sym = try eval.castSymbolSlice(at, arg);
            const cat = TextUtils.generalCategoryFromName(sym) orelse {
                return eval.abort(Eval.Error.TypeError, at, "unknown unicode general category `{s}`", .{sym});
            };
            return try SExpr.StringPreallocatedUnchecked(at, TextUtils.describeGeneralCategory(cat));
        }
    } },
    .{ "utf-control?", "given a string or char, checks if all characters are control characters", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isControlStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isControl(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-letter?", "given a string or char, checks if all characters are letter characters", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isLetterStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isLetter(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-mark?", "given a string or char, checks if all characters are mark characters", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isMarkStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isMark(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-number?", "given a string or char, checks if all characters are number characters", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isNumberStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isNumber(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-punctuation?", "given a string or char, checks if all characters are punctuation characters", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isPunctuationStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isPunctuation(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-separator?", "given a string or char, checks if all characters are separator characters", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isSeparatorStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isSeparator(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-symbol?", "given a string or char, checks if all characters are symbol characters", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isSymbolStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isSymbol(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-math?", "given a string or char, checks if all characters are math characters", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isMathStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isMath(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-alphabetic?", "given a string or char, checks if all characters are alphabetic characters", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isAlphabeticStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isAlphabetic(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-id-start?", "given a string or char, checks if all characters are id-start characters char", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isIdStartStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isIdStart(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-id-continue?", "given a string or char, checks if all characters are id-continue characters char", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isIdContinueStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isIdContinue(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-xid-start?", "given a string or char, checks if all characters are xid-start characters char", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isXidStartStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isXidStart(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-xid-continue?", "given a string or char, checks if all characters are xid-continue characters char", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isXidContinueStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isXidContinue(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-space?", "given a string or char, checks if all characters are space characters", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isSpaceStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isSpace(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-hex-digit?", "given a string or char, checks if all characters are hexadecimal digit characters char", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isHexDigitStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isHexDigit(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-diacritic?", "given a string or char, checks if all characters are diacritic characters", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isDiacriticStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isDiacritic(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-numeric?", "given a string or char, checks if all characters are numeric characters", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isNumericStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isNumeric(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-digit?", "given a string or char, checks if all characters are digit characters", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isDigitStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isDigit(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-decimal?", "given a string or char, checks if all characters are decimal digit characters char", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isDecimalStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isDecimal(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-hex?", "given a string or char, checks if all characters are hexadecimal digit characters char", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                return try SExpr.Bool(at, TextUtils.isHexDigitStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                });
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Bool(at, TextUtils.isHexDigit(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },

    .{ "utf-lowercase?", "given a string or char, checks if all characters are lowercase", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            return try SExpr.Bool(at, if (arg.castStringSlice()) |str| TextUtils.isLowerStr(str) else if (arg.coerceNativeChar()) |char| TextUtils.isLower(char) else false);
        }
    } },
    .{ "utf-uppercase?", "given a string or char, checks if all characters are uppercase", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            return try SExpr.Bool(at, if (arg.castStringSlice()) |str| TextUtils.isUpperStr(str) else if (arg.coerceNativeChar()) |char| TextUtils.isUpper(char) else false);
        }
    } },
    .{ "utf-lowercase", "given a string or char, returns a new copy with all of the characters converted to lowercase", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                const newStr = TextUtils.toLowerStr(eval.context.allocator, str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                };
                return try SExpr.StringPreallocatedUnchecked(at, newStr);
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Char(at, TextUtils.toLower(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-uppercase", "given a string or char, returns a new copy with all of the characters converted to uppercase", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                const newStr = TextUtils.toUpperStr(eval.context.allocator, str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                };
                return try SExpr.StringPreallocatedUnchecked(at, newStr);
            } else if (arg.coerceNativeChar()) |char| {
                return try SExpr.Char(at, TextUtils.toUpper(char));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-casefold", "given a string or a char, returns a new copy with all characters converted with unicode case folding; note that is may require converting chars to strings", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                const newStr = TextUtils.caseFoldStr(eval.context.allocator, str) catch {
                    return eval.abort(Eval.Error.BadEncoding, at, "bad utf8 string", .{});
                };
                return try SExpr.StringPreallocatedUnchecked(at, newStr);
            } else if (arg.coerceNativeChar()) |char| {
                const newChars = TextUtils.caseFold(char);
                if (newChars.len == 1) {
                    return try SExpr.Char(at, newChars[0]);
                } else {
                    var newStr = std.ArrayList(u8).init(eval.context.allocator);
                    defer newStr.deinit();
                    var byteBuf = [4]u8{ 0, 0, 0, 0 };
                    for (newChars) |ch| {
                        const len = TextUtils.encode(ch, &byteBuf) catch {
                            return eval.abort(Eval.Error.BadEncoding, at, "bad utf32 char", .{});
                        };
                        try newStr.appendSlice(byteBuf[0..len]);
                    }
                    return try SExpr.StringPreallocatedUnchecked(at, try newStr.toOwnedSlice());
                }
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-byte-count", "given a string or a char, returns the number of bytes required to represent it as utf-8", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                const len = str.len;
                if (len > std.math.maxInt(i64)) {
                    return eval.abort(Eval.Error.RangeError, at, "string is too long to take its byte count", .{});
                }
                return try SExpr.Int(at, @intCast(len));
            } else if (arg.coerceNativeChar()) |char| {
                const len = TextUtils.sequenceLength(char) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf32 char", .{});
                };
                return try SExpr.Int(at, @intCast(len));
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-display-width", "given a string or a char, returns the width of the value in visual columns (approximate)", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            if (arg.castStringSlice()) |str| {
                const width = TextUtils.displayWidthStr(str) catch {
                    return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                };
                return try SExpr.Int(at, width);
            } else if (arg.coerceNativeChar()) |char| {
                const width = TextUtils.displayWidth(char);
                return try SExpr.Int(at, width);
            } else {
                return eval.abort(Eval.Error.TypeError, at, "expected a String or a Char, got {}: `{}`", .{ arg.getTag(), arg });
            }
        }
    } },
    .{ "utf-case-insensitive-eq?", "compare two strings or chars using unicode case folding to ignore case", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const eargs = try eval.resolveListInRange(args, 2, 2);
            if (eargs[0].castStringSlice()) |str1| {
                if (eargs[1].castStringSlice()) |str2| {
                    return try SExpr.Bool(at, TextUtils.caseInsensitiveCompareStr(str1, str2) catch {
                        return eval.abort(Eval.Error.TypeError, at, "bad utf8 string", .{});
                    });
                }
            } else if (eargs[0].coerceNativeChar()) |char1| {
                if (eargs[1].coerceNativeChar()) |char2| {
                    return try SExpr.Bool(at, TextUtils.caseInsensitiveCompare(char1, char2));
                }
            }
            return eval.abort(Eval.Error.TypeError, at, "expected two Strings or Two chars, got {}: `{}`, and {}: `{}`", .{ eargs[0].getTag(), eargs[0], eargs[1].getTag(), eargs[1] });
        }
    } },
};
