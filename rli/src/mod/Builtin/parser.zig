const std = @import("std");

const Extern = @import("Utils").Extern;
const MiscUtils = @import("Utils").Misc;
const attr = @import("attr.zig");

const Rli = @import("../root.zig");
const Source = Rli.Source;
const SExpr = Rli.SExpr;
const Interpreter = Rli.Interpreter;
const Parser = Rli.Parser;

pub const Doc =
    \\This module provides access to Ribbon's parser from within the language.
    \\
    \\> ##### Example
    \\> ```lisp
    \\> (def p (parser/new))
    \\> (def first-ln "(print-ln \"hello world\")")
    \\> (def line-len (string-length first-ln))
    \\> (parser/filename! p "foo")
    \\> (parser/input! p
    \\>     (string-intercalate "\n"
    \\>         first-ln
    \\>         "(print-ln \"goodbye world\")"
    \\>         "(+ 1 2)")
    \\>     '((2 . 1) . 0))
    \\> (def res1 (parser/parse-sexpr! p))
    \\> (assert-eq
    \\>     (cdr (attr-range (attr-of res1)))
    \\>     (cons (cons 2 (+ 1 line-len)) line-len))
    \\> (interpreter res1)
    \\> (interpreter (parse-sexpr! p))
    \\> (assert-eq (interpreter (parse-sexpr! p)) 3)
    \\> (assert (parser/eof? p))
    \\> ```
;

pub const Decls = .{
    .{ "parser/new", "create a parser object", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            try interpreter.expect0(args);
            const parser = try Parser.init(interpreter.context);
            return try ExternParser(at, parser);
        }
    } },

    .{ "parser/filename!", "set the file name of the given parser; returns old value", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.eval2(args);
            const parser = try interpreter.castExternDataPtr(Parser, at, rargs[0]);
            const fileName = try interpreter.castStringSlice(at, rargs[1]);
            const oldFileName = parser.fileName;
            parser.fileName = fileName;
            return try SExpr.StringPreallocated(at, oldFileName);
        }
    } },
    .{ "parser/filename", "get the file name of the given parser", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            const parser = try interpreter.castExternDataPtr(Parser, at, arg);
            return try SExpr.StringPreallocated(at, parser.fileName);
        }
    } },

    .{ "parser/input!", "set the input of the given parser, optionally providing a position offset; returns old values as a pair", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var eargs = [3]SExpr{ undefined, undefined, undefined };
            const len = try interpreter.evalSmallList(args, 2, &eargs);
            const parser = try interpreter.castExternDataPtr(Parser, at, eargs[0]);
            const input = try interpreter.castStringSlice(at, eargs[1]);
            const offset = if (len == 3) try attr.convertSExprToPos(interpreter, at, eargs[2]) else null;
            const oldInput = parser.input;
            const oldOffset = try attr.convertPosToSExpr(at, parser.posOffset);
            parser.setInput(input, offset);
            return try SExpr.Cons(at, try SExpr.String(at, oldInput), oldOffset);
        }
    } },
    .{ "parser/input", "get the input of the given parser", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            const parser = try interpreter.castExternDataPtr(Parser, at, arg);
            return try SExpr.String(at, parser.input);
        }
    } },

    .{ "parser/parse-sexpr!", "parse an S-expression from the given parser's input; prompts `exception` with an error symbol if an error is encountered; prompts `fail` if there was nothing to parse", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            const parser = try interpreter.castExternDataPtr(Parser, at, arg);
            return parser.scanSExprP() catch |err| {
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
                return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
            };
        }
    } },

    .{ "parser/eof?", "determine whether a parser is at the end of its input", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = try interpreter.eval1(args);
            const parser = try interpreter.castExternDataPtr(Parser, at, arg);
            return try SExpr.Bool(at, parser.isEof());
        }
    } },
};

pub fn ExternParser(at: *const Source.Attr, parser: *Rli.Parser) !SExpr {
    const ParserVTable = SExpr.Types.ExternData.VTable(Rli.Parser){
        .compare = struct {
            fn fun(self: *const Rli.Parser, other: *const Rli.Parser) callconv(.C) MiscUtils.Ordering {
                return MiscUtils.compare(self.*, other.*);
            }
        }.fun,
        .hashWith = struct {
            fn fun(self: *const Rli.Parser, hasher: *Extern.Hasher) callconv(.C) void {
                MiscUtils.hashWith(hasher, @intFromPtr(self));
            }
        }.fun,
        .finalizer = struct {
            fn fun(self: *Rli.Parser) callconv(.C) void {
                self.deinit();
            }
        }.fun,
    };

    return SExpr.ExternData(Rli.Parser, at, parser, &ParserVTable);
}
