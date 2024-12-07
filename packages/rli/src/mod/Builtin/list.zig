const std = @import("std");

const MiscUtils = @import("Utils").Misc;

const Rli = @import("../root.zig");
const Source = Rli.Source;
const SExpr = Rli.SExpr;
const Interpreter = Rli.Interpreter;

pub const Doc =
    \\This module contains functions for creating and manipulating lists.
    \\
;

pub const Decls = .{
    .{ "nil", "the empty list constant", struct {
        pub fn init(at: *const Source.Attr) Interpreter.Result!SExpr {
            return try SExpr.Nil(at);
        }
    } },
    .{ "list", "create a new list, with any number of values", struct {
        pub fn fun(interpreter: *Interpreter, _: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            return interpreter.evalListRecursive(args);
        }
    } },
    .{ "list/length", "get the length of a list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const list = (try interpreter.evalN(1, args))[0];
            try interpreter.validateListOrNil(at, list);
            var iter = try interpreter.argIterator(false, list);
            var len: usize = 0;
            while (try iter.next()) |_| {
                len += 1;
            }
            if (len > @as(usize, std.math.maxInt(i64))) {
                return interpreter.abort(Interpreter.Error.RangeError, at, "list is too long to get its length: {}", .{len});
            }
            return SExpr.Int(at, @intCast(len));
        }
    } },
    .{ "list/map", "apply a function to each element of a list, returning a new list of the results", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalN(2, args);
            const list = rArgs[0];
            const func = rArgs[1];
            try interpreter.validateListOrNil(at, list);
            try interpreter.validateCallable(at, func);
            var iter = try interpreter.argIterator(false, list);
            var out = std.ArrayList(SExpr).init(interpreter.context.allocator);
            defer out.deinit();
            while (try iter.next()) |arg| {
                const result = try interpreter.nativeInvoke(at, func, &[_]SExpr{arg});
                try out.append(result);
            }
            return try SExpr.List(at, out.items);
        }
    } },
    .{ "list/each", "apply a function to each element of a list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalN(2, args);
            const list = rArgs[0];
            const func = rArgs[1];
            try interpreter.validateListOrNil(at, list);
            try interpreter.validateCallable(at, func);
            var iter = try interpreter.argIterator(false, list);
            while (try iter.next()) |arg| {
                _ = try interpreter.nativeInvoke(at, func, &[_]SExpr{arg});
            }
            return try SExpr.Nil(at);
        }
    } },
    .{ "list/foldl", "apply a function `(acc x) -> acc` to each element of a list of `x` and a provided accumulator, returning the final accumulator value", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalN(3, args);
            const list = rArgs[0];
            const init = rArgs[1];
            const func = rArgs[2];
            try interpreter.validateListOrNil(at, list);
            try interpreter.validateCallable(at, func);
            var iter = try interpreter.argIterator(false, list);
            var acc = init;
            while (try iter.next()) |arg| {
                acc = try interpreter.nativeInvoke(at, func, &[_]SExpr{acc, arg});
            }
            return acc;
        }
    } },
    .{ "list/foldr", "apply a function `(acc x) -> acc` to each element of a list of `x` and a provided accumulator, returning the final accumulator value", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalN(3, args);
            const list = rArgs[0].to([]SExpr) catch |err| {
                return interpreter.abort(err, at, "expected list at argument 0, got: {}", .{rArgs[0]});
            };
            const init = rArgs[1];
            const func = rArgs[2];

            try interpreter.validateCallable(at, func);

            var acc = init;
            for (0..list.len) |i| {
                const arg = list[list.len - i - 1];
                acc = try interpreter.nativeInvoke(at, func, &[_]SExpr{acc, arg});
            }
            return acc;
        }
    } },
    .{ "list/filter", "apply a function `(x) -> bool` to each element of a list, and append them into a new list if the returned boolean is true", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalN(2, args);
            const list = rArgs[0];
            const func = rArgs[1];
            try interpreter.validateListOrNil(at, list);
            try interpreter.validateCallable(at, func);
            var iter = try interpreter.argIterator(false, list);
            var out = std.ArrayList(SExpr).init(interpreter.context.allocator);
            defer out.deinit();
            while (try iter.next()) |arg| {
                const result = try interpreter.nativeInvoke(at, func, &[_]SExpr{arg});
                if (result.coerceNativeBool()) {
                    try out.append(arg);
                }
            }
            return try SExpr.List(at, out.items);
        }
    } },
    .{ "list/concat", "concatenate any number of lists into a new list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var out = std.ArrayList(SExpr).init(interpreter.context.allocator);
            defer out.deinit();
            var eArgs = try interpreter.argIterator(true, args);
            while (try eArgs.next()) |arg| {
                if (arg.isList()) {
                    var iter = try interpreter.argIterator(false, arg);
                    while (try iter.next()) |elem| {
                        try out.append(elem);
                    }
                } else {
                    try out.append(arg);
                }
            }
            return try SExpr.List(at, out.items);
        }
    } },
    .{ "list/intercalate", "given a list, and any number of subsequent lists, returns a new list with all of the subsequent values concatenated in order with the first value in between concatenations", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            var eArgs = try interpreter.evalList(args);

            var out = std.ArrayList(SExpr).init(interpreter.context.allocator);
            defer out.deinit();

            const buf = &.{eArgs[0]};
            const sep = if (eArgs[0].isList()) eArgs[0].to([]SExpr) catch |err| {
                return interpreter.abort(err, at, "expected list at argument 0, got: {}", .{eArgs[0]});
            } else buf;

            for (eArgs[1..], 1..) |eArg, i| {
                if (eArg.isList()) {
                    try out.appendSlice(eArg.to([]SExpr) catch |err| {
                        return interpreter.abort(err, at, "expected list at argument {}, got: {}", .{i, eArg});
                    });
                } else {
                    try out.append(eArg);
                }

                if (i < eArgs.len - 1) {
                    try out.appendSlice(sep);
                }
            }

            return try SExpr.List(at, out.items);
        }
    } },
    .{ "list/member?", "determine if a given value is contained in a list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalN(2, args);
            const value = rArgs[0];
            const list = rArgs[1];
            try interpreter.validateListOrNil(at, list);
            var iter = try interpreter.argIterator(false, list);
            while (try iter.next()) |arg| {
                if (MiscUtils.equal(arg, value)) {
                    return SExpr.Bool(at, true);
                }
            }
            return SExpr.Bool(at, false);
        }
    } },
    .{ "list/zip", "iterate through any number of lists and join their adjacent elements into lists; stops at the end of the shortest list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalListOfT([]SExpr, args);
            if (rArgs.len == 0) return SExpr.Nil(at);

            var out = std.ArrayList(SExpr).init(interpreter.context.allocator);
            loop: for (0..std.math.maxInt(i64)) |i| {
                var elem = std.ArrayList(SExpr).init(interpreter.context.allocator);
                for (rArgs) |rArg| {
                    if (rArg.len > i) {
                        try elem.append(rArg[i]);
                    } else break :loop;
                }
                try out.append(try SExpr.List(at, elem.items));
            }
            return SExpr.List(at, out.items);
        }
    } },
    .{ "list/enumerate", "iterate through any number of lists and join their adjacent elements into lists that start with the index; stops at the end of the shortest list", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const rArgs = try interpreter.evalListOfT([]SExpr, args);
            if (rArgs.len == 0) return SExpr.Nil(at);

            var out = std.ArrayList(SExpr).init(interpreter.context.allocator);
            loop: for (0..std.math.maxInt(i64)) |i| {
                var elem = std.ArrayList(SExpr).init(interpreter.context.allocator);
                try elem.append(try SExpr.Int(at, @intCast(i)));
                for (rArgs) |rArg| {
                    if (rArg.len > i) {
                        try elem.append(rArg[i]);
                    } else break :loop;
                }
                try out.append(try SExpr.List(at, elem.items));
            }
            return SExpr.List(at, out.items);
        }
    } },
};
