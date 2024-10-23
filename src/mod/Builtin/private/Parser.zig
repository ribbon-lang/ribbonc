const std = @import("std");

const Extern = @import("Extern");
const Support = @import("ZigUtils").Misc;
const BuiltinSource = @import("Builtin:Source");

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Eval = Core.Eval;
const Parser = Core.Parser;

pub const Doc =
    \\This module provides access to Ribbon's parser from within the language.
    \\
    \\> ##### Example
    \\> ```lisp
    \\> (def-var p (parser-new))
    \\> (def-var first-ln "(print-ln \"hello world\")")
    \\> (def-var line-len (string-length first-ln))
    \\> (parser-filename! p "foo")
    \\> (parser-input! p
    \\>     (string-intercalate "\n"
    \\>         first-ln
    \\>         "(print-ln \"goodbye world\")"
    \\>         "(+ 1 2)")
    \\>     '((2 . 1) . 0))
    \\> (def-var res1 (parse-sexpr! p))
    \\> (assert-eq
    \\>     (cdr (attr-range (attr-of res1)))
    \\>     (cons (cons 2 (+ 1 line-len)) line-len))
    \\> (eval res1)
    \\> (eval (parse-sexpr! p))
    \\> (assert-eq (eval (parse-sexpr! p)) 3)
    \\> (assert (parser-eof? p))
    \\> ```
;

pub const Env = .{
    .{ "parser-new", "create a parser object", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            try eval.expect0(args);
            const parser = try Parser.init(eval.context);
            return try ExternParser(at, parser);
        }
    } },

    .{ "parser-filename!", "set the file name of the given parser; returns old value", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolve2(args);
            const parser = try eval.castExternDataPtr(Parser, at, rargs[0]);
            const fileName = try eval.castStringSlice(at, rargs[1]);
            const oldFileName = parser.fileName;
            parser.fileName = fileName;
            return try SExpr.StringPreallocated(at, oldFileName);
        }
    } },
    .{ "parser-filename", "get the file name of the given parser", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            const parser = try eval.castExternDataPtr(Parser, at, arg);
            return try SExpr.StringPreallocated(at, parser.fileName);
        }
    } },

    .{ "parser-input!", "set the input of the given parser, optionally providing a position offset; returns old values as a pair", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var eargs = [3]SExpr{ undefined, undefined, undefined };
            const len = try eval.resolveSmallList(args, 2, &eargs);
            const parser = try eval.castExternDataPtr(Parser, at, eargs[0]);
            const input = try eval.castStringSlice(at, eargs[1]);
            const offset = if (len == 3) try BuiltinSource.convertSExprToPos(eval, at, eargs[2]) else null;
            const oldInput = parser.input;
            const oldOffset = try BuiltinSource.convertPosToSExpr(at, parser.posOffset);
            parser.setInput(input, offset);
            return try SExpr.Cons(at, try SExpr.String(at, oldInput), oldOffset);
        }
    } },
    .{ "parser-input", "get the input of the given parser", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            const parser = try eval.castExternDataPtr(Parser, at, arg);
            return try SExpr.String(at, parser.input);
        }
    } },

    .{ "parse-sexpr!", "parse an S-expression from the given parser's input; prompts `exception` with an error symbol if an error is encountered; prompts `fail` if there was nothing to parse", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            const parser = try eval.castExternDataPtr(Parser, at, arg);
            return parser.scanSExprP() catch |err| {
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
                return eval.nativePrompt(at, "fail", &[0]SExpr{});
            };
        }
    } },

    .{ "parser-eof?", "determine whether a parser is at the end of its input", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const arg = try eval.resolve1(args);
            const parser = try eval.castExternDataPtr(Parser, at, arg);
            return try SExpr.Bool(at, parser.isEof());
        }
    } },
};

pub fn ExternParser(attr: *const Source.Attr, parser: *Core.Parser) !SExpr {
    const ParserVTable = SExpr.Types.ExternData.VTable(Core.Parser){
        .compare = struct {
            fn fun(self: *const Core.Parser, other: *const Core.Parser) callconv(.C) Support.Ordering {
                return Support.compare(self.*, other.*);
            }
        }.fun,
        .hashWith = struct {
            fn fun(self: *const Core.Parser, hasher: *Extern.Hasher) callconv(.C) void {
                Support.hashWith(hasher, @intFromPtr(self));
            }
        }.fun,
        .finalizer = struct {
            fn fun(self: *Core.Parser) callconv(.C) void {
                self.deinit();
            }
        }.fun,
    };

    return SExpr.ExternData(Core.Parser, attr, parser, &ParserVTable);
}
