const std = @import("std");

const MiscUtils = @import("Utils").Misc;

const Core = @import("Core");
const Source = Core.Source;
const SExpr = Core.SExpr;
const Interpreter = Core.Interpreter;

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
    \\For more information on the syntax of lambda lists, see the [`Pattern` module](#pattern).
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
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var buf = [3]SExpr{ undefined, undefined, undefined };
            const len = try interpreter.expectSmallList(args, 2, &buf);
            const cond = buf[0];
            const then = buf[1];
            const els: ?SExpr = if (len == 3) buf[2] else null;
            const condval = try interpreter.eval(cond);
            if (condval.coerceNativeBool()) {
                return try interpreter.eval(then);
            } else if (els) |elsx| {
                return try interpreter.eval(elsx);
            } else {
                return try SExpr.Nil(at);
            }
        }
    } },
    .{ "when", "single-option conditional branch, taken if the condition is true", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.evalAtLeast1(args);
            const cond = rargs.head;
            const then = rargs.tail;
            if (cond.coerceNativeBool()) {
                return try interpreter.runProgram(then);
            } else {
                return try SExpr.Nil(at);
            }
        }
    } },
    .{ "unless", "single-option conditional branch, taken if the condition is false", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.evalAtLeast1(args);
            const cond = rargs.head;
            const then = rargs.tail;
            if (!cond.coerceNativeBool()) {
                return try interpreter.runProgram(then);
            } else {
                return try SExpr.Nil(at);
            }
        }
    } },
    .{ "cond", "multi-option conditional branch", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var eargs = try interpreter.argIterator(false, args);
            while (try eargs.next()) |seg| {
                const buf = try interpreter.expectAtLeast1(seg);
                const cond = buf.head;
                const then = buf.tail;
                const condval = if (cond.isExactSymbol("else")) {
                    if (eargs.hasNext()) {
                        return interpreter.abort(Interpreter.Error.TooManyArguments, at, "expected else to be the last cond clause", .{});
                    }
                    return try interpreter.runProgram(then);
                } else try interpreter.eval(cond);
                if (condval.coerceNativeBool()) {
                    return try interpreter.runProgram(then);
                }
            }
            return try SExpr.Nil(at);
        }
    } },
    .{ "match", "pattern based matching on any inputt", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const res = try interpreter.evalAtLeast1(args);
            const scrutinee = res.head;
            const cases = res.tail;
            var eargs = try interpreter.argIterator(false, cases);
            while (try eargs.next()) |x| {
                const case = try interpreter.expectAtLeast1(x);
                const llist = case.head;
                const then = case.tail;
                if (llist.isExactSymbol("else")) {
                    if (eargs.hasNext()) {
                        return interpreter.abort(Interpreter.Error.TooManyArguments, at, "expected else to be the last match case", .{});
                    }
                    return try interpreter.runProgram(then);
                }
                switch (try Interpreter.PatternLite.run(interpreter, llist.getAttr(), llist, scrutinee)) {
                    .Okay => |frame| {
                        try Interpreter.pushFrame(frame, &interpreter.env);
                        defer _ = Interpreter.popFrame(&interpreter.env) catch unreachable;
                        return try interpreter.runProgram(then);
                    },
                    else => {},
                }
            }
            return SExpr.Nil(at);
        }
    } },
    .{ "begin", "allows sequencing expressions", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            return try interpreter.runProgram(args);
        }
    } },
    .{ "panic", "runs `format` on the values provided and then triggers a panic with the resulting string", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rargs = try interpreter.argIterator(true, args);
            var out = std.ArrayList(u8).init(interpreter.context.allocator);
            defer out.deinit();
            const writer = out.writer();
            var i: usize = 0;
            while (try rargs.next()) |next| {
                try writer.print("{display}", .{next});
                i += 1;
            }
            return interpreter.abort(Interpreter.Error.Panic, at, "{s}", .{try out.toOwnedSlice()});
        }
    } },
    .{ "panic-at", "uses the first argument as the source attribution for the panic; runs `format` on subsequent values provided and then triggers a panic with the resulting string", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var rargs = try interpreter.argIterator(true, args);
            const fst = try rargs.atLeast();
            const eat = try interpreter.castExternDataPtr(Source.Attr, at, fst);
            var out = std.ArrayList(u8).init(interpreter.context.allocator);
            defer out.deinit();
            const writer = out.writer();
            while (try rargs.next()) |next| {
                try writer.print("{display}", .{next});
            }
            return interpreter.abort(Interpreter.Error.Panic, eat, "{s}", .{try out.toOwnedSlice()});
        }
    } },
    .{ "throw", "prompts `exception` with the value provided; this is a shortcut for `(prompt exception arg)`", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const msg = try interpreter.eval1(args);
            return interpreter.nativePrompt(at, "exception", &[1]SExpr{msg});
        }
    } },
    .{ "stop", "prompts `fail`; this is a shortcut for `(prompt fail)`", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, _: SExpr) Interpreter.Result!SExpr {
            return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
        }
    } },
    .{ "assert", "asserts that a condition is true; if it is not, triggers a panic with the subsequent arguments, or with the condition itself if none were provided", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs = try interpreter.evalAtLeast1(args);
            const cond = rargs.head;
            if (cond.coerceNativeBool()) {
                return try SExpr.Nil(at);
            } else {
                var it = try interpreter.argIterator(true, rargs.tail);
                if (!it.hasNext()) {
                    try it.assertDone();
                    return interpreter.abort(Interpreter.Error.Panic, at, "assert failed: {display}", .{args});
                } else {
                    var out = std.ArrayList(u8).init(interpreter.context.allocator);
                    defer out.deinit();
                    const writer = out.writer();
                    while (try it.next()) |next| {
                        try writer.print("{display}", .{next});
                    }
                    return interpreter.abort(Interpreter.Error.Panic, at, "assert failed: {s}", .{try out.toOwnedSlice()});
                }
            }
        }
    } },
    .{ "assert-eq", "asserts that the first two values provided are equal, using structural equality on objects; if they are not, triggers a panic with any subsequent values provided, or with the condition if none were", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const eargs = try interpreter.expectAtLeast2(args);
            const a = try interpreter.eval(eargs.head[0]);
            const b = try interpreter.eval(eargs.head[1]);
            if (MiscUtils.equal(a, b)) {
                return try SExpr.Nil(at);
            } else {
                var it = try interpreter.argIterator(true, eargs.tail);
                if (!it.hasNext()) {
                    try it.assertDone();
                    return interpreter.abort(Interpreter.Error.Panic, at,
                        "assert-eq failed:\n\ta: {} = {}\n\tb: {} = {}",
                        .{eargs.head[0], a, eargs.head[1], b});
                } else {
                    var out = std.ArrayList(u8).init(interpreter.context.allocator);
                    defer out.deinit();
                    const writer = out.writer();
                    while (try it.next()) |next| {
                        try writer.print("{display}", .{next});
                    }
                    return interpreter.abort(Interpreter.Error.Panic, at, "assert-eq failed: {s}", .{try out.toOwnedSlice()});
                }
            }
        }
    } },
    .{ "assert-eq-addr", "asserts that the first two values provided are equal, using address equality on objects; if they are not, triggers a panic with any subsequent values provided, or with the condition if none were", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const eargs = try interpreter.expectAtLeast2(args);
            const a = try interpreter.eval(eargs.head[0]);
            const b = try interpreter.eval(eargs.head[1]);
            if (MiscUtils.equalAddress(a, b)) {
                return try SExpr.Nil(at);
            } else {
                var it = try interpreter.argIterator(true, eargs.tail);
                if (!it.hasNext()) {
                    try it.assertDone();
                    return interpreter.abort(Interpreter.Error.Panic, at,
                        "assert-eq-addr failed:\n\ta: {} = {}\n\tb: {} = {}",
                        .{eargs.head[0], a, eargs.head[1], b});
                } else {
                    var out = std.ArrayList(u8).init(interpreter.context.allocator);
                    defer out.deinit();
                    const writer = out.writer();
                    while (try it.next()) |next| {
                        try writer.print("{display}", .{next});
                    }
                    return interpreter.abort(Interpreter.Error.Panic, at, "assert-eq-addr failed: {s}", .{try out.toOwnedSlice()});
                }
            }
        }
    } },
    .{ "assert-at", "asserts that a condition is true; if it is not, triggers a panic with the subsequent arguments, or with the condition itself if none were provided", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs0 = try interpreter.evalAtLeast1(args);
            const eat = try interpreter.castExternDataPtr(Source.Attr, at, rargs0.head);
            const eCondInput = try interpreter.expectAtLeast1(rargs0.tail);
            const cond = try interpreter.eval(eCondInput.head);
            const tail = eCondInput.tail;
            if (cond.coerceNativeBool()) {
                return try SExpr.Nil(at);
            } else {
                var it = try interpreter.argIterator(true, tail);
                if (!it.hasNext()) {
                    try it.assertDone();
                    return interpreter.abort(Interpreter.Error.Panic, eat, "assert failed: {}", .{eCondInput.head});
                } else {
                    var out = std.ArrayList(u8).init(interpreter.context.allocator);
                    defer out.deinit();
                    const writer = out.writer();
                    while (try it.next()) |next| {
                        try writer.print("{display}", .{next});
                    }
                    return interpreter.abort(Interpreter.Error.Panic, eat, "assert failed: {s}", .{try out.toOwnedSlice()});
                }
            }
        }
    } },
    .{ "assert-eq-at", "asserts that the first two values provided are equal, using structural equality on objects; if they are not, triggers a panic with any subsequent values provided, or with the equality inputs if none were", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs0 = try interpreter.evalAtLeast1(args);
            const eat = try interpreter.castExternDataPtr(Source.Attr, at, rargs0.head);
            const eEqInputs = try interpreter.expectAtLeast2(rargs0.tail);
            const rEqInputs = [2]SExpr{ try interpreter.eval(eEqInputs.head[0]), try interpreter.eval(eEqInputs.head[1]) };
            const tail = eEqInputs.tail;
            if (MiscUtils.equal(rEqInputs[0], rEqInputs[1])) {
                return try SExpr.Nil(at);
            } else {
                var it = try interpreter.argIterator(true, tail);
                if (!it.hasNext()) {
                    try it.assertDone();
                    return interpreter.abort(Interpreter.Error.Panic, eat, "assert-eq failed: ({} {})", .{ eEqInputs.head[0], eEqInputs.head[1] });
                } else {
                    var out = std.ArrayList(u8).init(interpreter.context.allocator);
                    defer out.deinit();
                    const writer = out.writer();
                    while (try it.next()) |next| {
                        try writer.print("{display}", .{next});
                    }
                    return interpreter.abort(Interpreter.Error.Panic, eat, "assert-eq failed: {s}", .{try out.toOwnedSlice()});
                }
            }
        }
    } },
    .{ "assert-eq-addr-at", "asserts, using the location provided as the first argument, that the next two values provided are equal, using address equality on objects; if they are not, triggers a panic with any subsequent values provided, or with the equality inputs if none were", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rargs0 = try interpreter.evalAtLeast1(args);
            const eat = try interpreter.castExternDataPtr(Source.Attr, at, rargs0.head);
            const eEqInputs = try interpreter.expectAtLeast2(rargs0.tail);
            const rEqInputs = [2]SExpr{ try interpreter.eval(eEqInputs.head[0]), try interpreter.eval(eEqInputs.head[1]) };
            const tail = eEqInputs.tail;
            if (MiscUtils.equalAddress(rEqInputs[0], rEqInputs[1])) {
                return try SExpr.Nil(at);
            } else {
                var it = try interpreter.argIterator(true, tail);
                if (!it.hasNext()) {
                    try it.assertDone();
                    return interpreter.abort(Interpreter.Error.Panic, eat, "assert-eq-addr failed: ({} {})", .{ eEqInputs.head[0], eEqInputs.head[1] });
                } else {
                    var out = std.ArrayList(u8).init(interpreter.context.allocator);
                    defer out.deinit();
                    const writer = out.writer();
                    while (try it.next()) |next| {
                        try writer.print("{display}", .{next});
                    }
                    return interpreter.abort(Interpreter.Error.Panic, eat, "assert-eq-addr failed: {s}", .{try out.toOwnedSlice()});
                }
            }
        }
    } },
    .{ "e-assert", "asserts that a condition is true; if it is not, prompts `exception` with the second value provided or with the symbol `AssertionFailed` if one is not", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var buf = [2]SExpr{ undefined, undefined };
            const len = try interpreter.evalSmallList(args, 1, &buf);
            const cond = buf[0];
            const msg =
                if (len == 2) buf[1] else try SExpr.Symbol(at, "AssertionFailed");
            if (cond.coerceNativeBool()) {
                return try SExpr.Nil(at);
            } else {
                return interpreter.nativePrompt(at, "exception", &[1]SExpr{msg});
            }
        }
    } },
    .{ "e-assert-eq", "asserts that the first two values provided are equal, using structural equality on objects; if they are not, prompts `exception` with any subsequent values provided, or with the condition if none were", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var buf = [3]SExpr{ undefined, undefined, undefined };
            const len = try interpreter.evalSmallList(args, 2, &buf);
            const a = buf[0];
            const b = buf[1];
            const msg =
                if (len == 3) buf[2] else try SExpr.Symbol(at, "AssertionFailed");
            if (MiscUtils.equal(a, b)) {
                return try SExpr.Nil(at);
            } else {
                return interpreter.nativePrompt(at, "exception", &[1]SExpr{msg});
            }
        }
    } },
    .{ "e-assert-eq-addr", "asserts that the first two values provided are equal, using address equality on objects; if they are not, prompts `exception` with any subsequent values provided, or with the condition if none were", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var buf = [3]SExpr{ undefined, undefined, undefined };
            const len = try interpreter.evalSmallList(args, 2, &buf);
            const a = buf[0];
            const b = buf[1];
            const msg =
                if (len == 3) buf[2] else try SExpr.Symbol(at, "AssertionFailed");
            if (MiscUtils.equalAddress(a, b)) {
                return try SExpr.Nil(at);
            } else {
                return interpreter.nativePrompt(at, "exception", &[1]SExpr{msg});
            }
        }
    } },
    .{ "f-assert", "asserts that a condition is true; if it is not, prompts `fail`", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const cond = try interpreter.eval1(args);
            if (cond.coerceNativeBool()) {
                return try SExpr.Nil(at);
            } else {
                return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "f-assert-eq", "asserts that the two values provided are equal, using structural equality on objects; if they are not, prompts `fail`", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const buf = try interpreter.eval2(args);
            const a = buf[0];
            const b = buf[1];
            if (MiscUtils.equal(a, b)) {
                return try SExpr.Nil(at);
            } else {
                return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
    .{ "f-assert-eq-addr", "asserts that the two values provided are equal, using address equality on objects; if they are not, prompts `fail`", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const buf = try interpreter.eval2(args);
            const a = buf[0];
            const b = buf[1];
            if (MiscUtils.equalAddress(a, b)) {
                return try SExpr.Nil(at);
            } else {
                return interpreter.nativePrompt(at, "fail", &[0]SExpr{});
            }
        }
    } },
};
