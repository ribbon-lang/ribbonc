const std = @import("std");

const MiscUtils = @import("ZigUtils").Misc;

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Eval = Core.Eval;

pub const Doc =
    \\This module provides special forms for controlling the flow of execution.
    \\
    \\`if` is a one or two option conditional branch.
    \\> ##### Example
    \\> ```lisp
    \\> (if x (conditional-action))
    \\> (if y (conditional-action) (else-action))
    \\> ```
    \\
    \\`cond` is a multi-option conditional branch.
    \\> ##### Example
    \\> ```lisp
    \\> (cond
    \\>   (x (conditional-action1))
    \\>   (y (conditional-action2))
    \\>   (else (default-action)))
    \\> ```
    \\
    \\`match` uses lambda lists to perform structural pattern matching on an input.
    \\> ##### Example
    \\> ```lisp
    \\> (match x
    \\>   ((0) (conditional-action0))
    \\>   ((x) (conditional-action1 x))
    \\>   ((x y) (conditional-action2 x y))
    \\>   ((x y . z) (conditional-action3 x y z))
    \\>   (else (default-action)))
    \\> ```
    \\For more information on the syntax of lambda lists, see the [`LambdaList` module](#lambdalist).
    \\
    \\`begin` allows for sequencing expressions.
    \\> ##### Example
    \\> ```lisp
    \\> (begin
    \\>   (action1)
    \\>   (action2))
    \\> ```
;

pub const Env = .{
    .{ "if", "two-option conditional branch", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var buf = [3]SExpr{ undefined, undefined, undefined };
            const len = try eval.expectSmallList(args, 2, &buf);
            const cond = buf[0];
            const then = buf[1];
            const els: ?SExpr = if (len == 3) buf[2] else null;
            const condval = try eval.resolve(cond);
            if (condval.coerceNativeBool()) {
                return try eval.resolve(then);
            } else if (els) |elsx| {
                return try eval.resolve(elsx);
            } else {
                return try SExpr.Nil(at);
            }
        }
    } },
    .{ "when", "single-option conditional branch, taken if the condition is true", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolveAtLeast1(args);
            const cond = rargs.head;
            const then = rargs.tail;
            if (cond.coerceNativeBool()) {
                return try eval.runProgram(then);
            } else {
                return try SExpr.Nil(at);
            }
        }
    } },
    .{ "unless", "single-option conditional branch, taken if the condition is false", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolveAtLeast1(args);
            const cond = rargs.head;
            const then = rargs.tail;
            if (!cond.coerceNativeBool()) {
                return try eval.runProgram(then);
            } else {
                return try SExpr.Nil(at);
            }
        }
    } },
    .{ "cond", "multi-option conditional branch", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var eargs = try eval.argIterator(false, args);
            while (try eargs.next()) |seg| {
                const buf = try eval.expectAtLeast1(seg);
                const cond = buf.head;
                const then = buf.tail;
                const condval = if (cond.isExactSymbol("else")) {
                    if (eargs.hasNext()) {
                        return eval.abort(Eval.Error.TooManyArguments, at, "expected else to be the last cond clause", .{});
                    }
                    return try eval.runProgram(then);
                } else try eval.resolve(cond);
                if (condval.coerceNativeBool()) {
                    return try eval.runProgram(then);
                }
            }
            return try SExpr.Nil(at);
        }
    } },
    .{ "match", "lambda list based matching on any inputt", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const res = try eval.resolveAtLeast1(args);
            const scrutinee = res.head;
            const cases = res.tail;
            var eargs = try eval.argIterator(false, cases);
            while (try eargs.next()) |x| {
                const case = try eval.expectAtLeast1(x);
                const llist = case.head;
                const then = case.tail;
                if (llist.isExactSymbol("else")) {
                    if (eargs.hasNext()) {
                        return eval.abort(Eval.Error.TooManyArguments, at, "expected else to be the last match case", .{});
                    }
                    return try eval.runProgram(then);
                }
                switch (try Eval.LambdaListLite.run(eval, llist.getAttr(), llist, scrutinee)) {
                    .Okay => |frame| {
                        try Eval.pushFrame(frame, &eval.env);
                        defer _ = Eval.popFrame(&eval.env) catch unreachable;
                        return try eval.runProgram(then);
                    },
                    else => {},
                }
            }
            return SExpr.Nil(at);
        }
    } },
    .{ "begin", "allows sequencing expressions", struct {
        pub fn fun(eval: *Eval, _: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            return try eval.runProgram(args);
        }
    } },
    .{ "panic", "runs `format` on the values provided and then triggers a panic with the resulting string", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            var out = std.ArrayList(u8).init(eval.context.allocator);
            defer out.deinit();
            const writer = out.writer();
            while (try rargs.next()) |next| {
                try writer.print("{display}", .{next});
            }
            return eval.abort(Eval.Error.Panic, at, "{s}", .{try out.toOwnedSlice()});
        }
    } },
    .{ "panic-at", "uses the first argument as the source attribution for the panic; runs `format` on subsequent values provided and then triggers a panic with the resulting string", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var rargs = try eval.argIterator(true, args);
            const fst = try rargs.atLeast();
            const eat = try eval.castExternDataPtr(Source.Attr, at, fst);
            var out = std.ArrayList(u8).init(eval.context.allocator);
            defer out.deinit();
            const writer = out.writer();
            while (try rargs.next()) |next| {
                try writer.print("{display}", .{next});
            }
            return eval.abort(Eval.Error.Panic, eat, "{s}", .{try out.toOwnedSlice()});
        }
    } },
    .{ "throw", "prompts `exception` with the value provided; this is a shortcut for `(prompt exception arg)`", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const msg = try eval.resolve1(args);
            return eval.nativePrompt(at, "exception", &[1]SExpr{msg});
        }
    } },
    .{ "stop", "prompts `fail`; this is a shortcut for `(prompt fail)`", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, _: SExpr) Eval.Result!SExpr {
            return eval.nativePrompt(at, "fail", &[0]SExpr{});
        }
    } },
    .{ "assert", "asserts that a condition is true; if it is not, triggers a panic with the subsequent arguments, or with the condition itself if none were provided", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs = try eval.resolveAtLeast1(args);
            const cond = rargs.head;
            if (cond.coerceNativeBool()) {
                return try SExpr.Nil(at);
            } else {
                var it = try eval.argIterator(true, rargs.tail);
                if (!it.hasNext()) {
                    try it.assertDone();
                    return eval.abort(Eval.Error.Panic, at, "assert failed: {display}", .{args});
                } else {
                    var out = std.ArrayList(u8).init(eval.context.allocator);
                    defer out.deinit();
                    const writer = out.writer();
                    while (try it.next()) |next| {
                        try writer.print("{display}", .{next});
                    }
                    return eval.abort(Eval.Error.Panic, at, "assert failed: {s}", .{try out.toOwnedSlice()});
                }
            }
        }
    } },
    .{ "assert-eq", "asserts that the first two values provided are equal, using structural equality on objects; if they are not, triggers a panic with any subsequent values provided, or with the condition if none were", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const eargs = try eval.expectAtLeast2(args);
            const a = try eval.resolve(eargs.head[0]);
            const b = try eval.resolve(eargs.head[1]);
            if (MiscUtils.equal(a, b)) {
                return try SExpr.Nil(at);
            } else {
                var it = try eval.argIterator(true, eargs.tail);
                if (!it.hasNext()) {
                    try it.assertDone();
                    return eval.abort(Eval.Error.Panic, at,
                        "assert-eq failed:\n\ta: {} = {}\n\tb: {} = {}",
                        .{eargs.head[0], a, eargs.head[1], b});
                } else {
                    var out = std.ArrayList(u8).init(eval.context.allocator);
                    defer out.deinit();
                    const writer = out.writer();
                    while (try it.next()) |next| {
                        try writer.print("{display}", .{next});
                    }
                    return eval.abort(Eval.Error.Panic, at, "assert-eq failed: {s}", .{try out.toOwnedSlice()});
                }
            }
        }
    } },
    .{ "assert-eq-addr", "asserts that the first two values provided are equal, using address equality on objects; if they are not, triggers a panic with any subsequent values provided, or with the condition if none were", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const eargs = try eval.expectAtLeast2(args);
            const a = try eval.resolve(eargs.head[0]);
            const b = try eval.resolve(eargs.head[1]);
            if (MiscUtils.equalAddress(a, b)) {
                return try SExpr.Nil(at);
            } else {
                var it = try eval.argIterator(true, eargs.tail);
                if (!it.hasNext()) {
                    try it.assertDone();
                    return eval.abort(Eval.Error.Panic, at,
                        "assert-eq-addr failed:\n\ta: {} = {}\n\tb: {} = {}",
                        .{eargs.head[0], a, eargs.head[1], b});
                } else {
                    var out = std.ArrayList(u8).init(eval.context.allocator);
                    defer out.deinit();
                    const writer = out.writer();
                    while (try it.next()) |next| {
                        try writer.print("{display}", .{next});
                    }
                    return eval.abort(Eval.Error.Panic, at, "assert-eq-addr failed: {s}", .{try out.toOwnedSlice()});
                }
            }
        }
    } },
    .{ "assert-at", "asserts that a condition is true; if it is not, triggers a panic with the subsequent arguments, or with the condition itself if none were provided", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs0 = try eval.resolveAtLeast1(args);
            const eat = try eval.castExternDataPtr(Source.Attr, at, rargs0.head);
            const eCondInput = try eval.expectAtLeast1(rargs0.tail);
            const cond = try eval.resolve(eCondInput.head);
            const tail = eCondInput.tail;
            if (cond.coerceNativeBool()) {
                return try SExpr.Nil(at);
            } else {
                var it = try eval.argIterator(true, tail);
                if (!it.hasNext()) {
                    try it.assertDone();
                    return eval.abort(Eval.Error.Panic, eat, "assert failed: {}", .{eCondInput.head});
                } else {
                    var out = std.ArrayList(u8).init(eval.context.allocator);
                    defer out.deinit();
                    const writer = out.writer();
                    while (try it.next()) |next| {
                        try writer.print("{display}", .{next});
                    }
                    return eval.abort(Eval.Error.Panic, eat, "assert failed: {s}", .{try out.toOwnedSlice()});
                }
            }
        }
    } },
    .{ "assert-eq-at", "asserts that the first two values provided are equal, using structural equality on objects; if they are not, triggers a panic with any subsequent values provided, or with the equality inputs if none were", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs0 = try eval.resolveAtLeast1(args);
            const eat = try eval.castExternDataPtr(Source.Attr, at, rargs0.head);
            const eEqInputs = try eval.expectAtLeast2(rargs0.tail);
            const rEqInputs = [2]SExpr{ try eval.resolve(eEqInputs.head[0]), try eval.resolve(eEqInputs.head[1]) };
            const tail = eEqInputs.tail;
            if (MiscUtils.equal(rEqInputs[0], rEqInputs[1])) {
                return try SExpr.Nil(at);
            } else {
                var it = try eval.argIterator(true, tail);
                if (!it.hasNext()) {
                    try it.assertDone();
                    return eval.abort(Eval.Error.Panic, eat, "assert-eq failed: ({} {})", .{ eEqInputs.head[0], eEqInputs.head[1] });
                } else {
                    var out = std.ArrayList(u8).init(eval.context.allocator);
                    defer out.deinit();
                    const writer = out.writer();
                    while (try it.next()) |next| {
                        try writer.print("{display}", .{next});
                    }
                    return eval.abort(Eval.Error.Panic, eat, "assert-eq failed: {s}", .{try out.toOwnedSlice()});
                }
            }
        }
    } },
    .{ "assert-eq-addr-at", "asserts, using the location provided as the first argument, that the next two values provided are equal, using address equality on objects; if they are not, triggers a panic with any subsequent values provided, or with the equality inputs if none were", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const rargs0 = try eval.resolveAtLeast1(args);
            const eat = try eval.castExternDataPtr(Source.Attr, at, rargs0.head);
            const eEqInputs = try eval.expectAtLeast2(rargs0.tail);
            const rEqInputs = [2]SExpr{ try eval.resolve(eEqInputs.head[0]), try eval.resolve(eEqInputs.head[1]) };
            const tail = eEqInputs.tail;
            if (MiscUtils.equalAddress(rEqInputs[0], rEqInputs[1])) {
                return try SExpr.Nil(at);
            } else {
                var it = try eval.argIterator(true, tail);
                if (!it.hasNext()) {
                    try it.assertDone();
                    return eval.abort(Eval.Error.Panic, eat, "assert-eq-addr failed: ({} {})", .{ eEqInputs.head[0], eEqInputs.head[1] });
                } else {
                    var out = std.ArrayList(u8).init(eval.context.allocator);
                    defer out.deinit();
                    const writer = out.writer();
                    while (try it.next()) |next| {
                        try writer.print("{display}", .{next});
                    }
                    return eval.abort(Eval.Error.Panic, eat, "assert-eq-addr failed: {s}", .{try out.toOwnedSlice()});
                }
            }
        }
    } },
    .{ "e-assert", "asserts that a condition is true; if it is not, prompts `exception` with the second value provided or with the symbol `AssertionFailed` if one is not", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var buf = [2]SExpr{ undefined, undefined };
            const len = try eval.resolveSmallList(args, 1, &buf);
            const cond = buf[0];
            const msg =
                if (len == 2) buf[1] else try SExpr.Symbol(at, "AssertionFailed");
            if (cond.coerceNativeBool()) {
                return try SExpr.Nil(at);
            } else {
                return eval.nativePrompt(at, "exception", &[1]SExpr{msg});
            }
        }
    } },
    .{ "e-assert-eq", "asserts that the first two values provided are equal, using structural equality on objects; if they are not, prompts `exception` with any subsequent values provided, or with the condition if none were", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var buf = [3]SExpr{ undefined, undefined, undefined };
            const len = try eval.resolveSmallList(args, 2, &buf);
            const a = buf[0];
            const b = buf[1];
            const msg =
                if (len == 3) buf[2] else try SExpr.Symbol(at, "AssertionFailed");
            if (MiscUtils.equal(a, b)) {
                return try SExpr.Nil(at);
            } else {
                return eval.nativePrompt(at, "exception", &[1]SExpr{msg});
            }
        }
    } },
    .{ "e-assert-eq-addr", "asserts that the first two values provided are equal, using address equality on objects; if they are not, prompts `exception` with any subsequent values provided, or with the condition if none were", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            var buf = [3]SExpr{ undefined, undefined, undefined };
            const len = try eval.resolveSmallList(args, 2, &buf);
            const a = buf[0];
            const b = buf[1];
            const msg =
                if (len == 3) buf[2] else try SExpr.Symbol(at, "AssertionFailed");
            if (MiscUtils.equalAddress(a, b)) {
                return try SExpr.Nil(at);
            } else {
                return eval.nativePrompt(at, "exception", &[1]SExpr{msg});
            }
        }
    } },
    .{ "f-assert", "asserts that a condition is true; if it is not, prompts `fail`", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const cond = try eval.resolve1(args);
            if (cond.coerceNativeBool()) {
                return try SExpr.Nil(at);
            } else {
                return eval.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "f-assert-eq", "asserts that the two values provided are equal, using structural equality on objects; if they are not, prompts `fail`", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const buf = try eval.resolve2(args);
            const a = buf[0];
            const b = buf[1];
            if (MiscUtils.equal(a, b)) {
                return try SExpr.Nil(at);
            } else {
                return eval.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "f-assert-eq-addr", "asserts that the two values provided are equal, using address equality on objects; if they are not, prompts `fail`", struct {
        pub fn fun(eval: *Eval, at: *const Source.Attr, args: SExpr) Eval.Result!SExpr {
            const buf = try eval.resolve2(args);
            const a = buf[0];
            const b = buf[1];
            if (MiscUtils.equalAddress(a, b)) {
                return try SExpr.Nil(at);
            } else {
                return eval.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
};
