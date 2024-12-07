const std = @import("std");

const Extern = @import("Utils").Extern;
const MiscUtils = @import("Utils").Misc;
const Rli = @import("../root.zig");
const Source = Rli.Source;
const SExpr = Rli.SExpr;
const Interpreter = Rli.Interpreter;

pub const Doc =
    \\This module provides functions for the creation, access and transformation
    \\of source-attribution primitives.
    \\
;

pub const Decls = .{
    .{ "attr/here", "create a source attribution referencing the call location", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            try interpreter.expect0(args);
            return ExternAttr(at, at);
        }
    } },
    .{ "attr/filename", "get the filename stored in an Attr", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            const attr: *const Source.Attr = try interpreter.castExternDataPtr(Source.Attr, at, arg);
            return try SExpr.StringPreallocated(at, attr.filename);
        }
    } },
    .{ "attr/comments", "get the comments stored in an Attr", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            const attr: *const Source.Attr = try interpreter.castExternDataPtr(Source.Attr, at, arg);

            return try convertListToSExpr(interpreter, at, attr.comments, convertCommentToSExpr);
        }
    } },
    .{ "attr/range", "get the range stored in an Attr", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            const attr: *const Source.Attr = try interpreter.castExternDataPtr(Source.Attr, at, arg);
            return try convertRangeToSExpr(at, attr.range);
        }
    } },
    .{ "attr/new", "create a new Attr from a filename string and a range object", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const eArgs = try interpreter.evalN(3, args);
            const filename = try interpreter.castStringSlice(at, eArgs[0]);
            const range = try convertSExprToRange(interpreter, at, eArgs[1]);
            const comments = try convertSExprToList(Source.Comment, interpreter, at, eArgs[2], convertSExprToComment);
            const attr = try interpreter.context.new(Source.Attr{
                .context = interpreter.context,
                .filename = filename,
                .range = range,
                .comments = comments,
            });
            return try ExternAttr(at, attr);
        }
    } },
    .{ "attr/of", "extract the Attr from a value", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const arg = (try interpreter.evalN(1, args))[0];
            return try ExternAttr(at, arg.getAttr());
        }
    } },
    .{ "attr/of-name", "extract the Attr from a binding in the environment", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const key = (try interpreter.evalN(1, args))[0];
            try interpreter.validateSymbol(at, key);
            const env = interpreter.env;
            const entry = Interpreter.envLookupPair(key, env) catch |err| {
                return interpreter.abort(err, at, "bad env: {}", .{env});
            };
            if (entry) |v| {
                return try ExternAttr(at, v.getAttr());
            } else {
                return SExpr.Nil(at);
            }
        }
    } },
    .{ "attr/set!", "set the Attr of a value; returns the value", struct {
        pub fn fun(interpreter: *Interpreter, at: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
            const eArgs = try interpreter.evalN(2, args);
            const attr: *const Source.Attr = try interpreter.castExternDataPtr(Source.Attr, at, eArgs[0]);
            eArgs[1].setAttr(attr);
            return eArgs[1];
        }
    } },
};

pub fn convertRangeToSExpr(at: *const Source.Attr, value: ?Source.Range) !SExpr {
    if (value) |range| {
        const start = try convertPosToSExpr(at, range.start);
        const end = try convertPosToSExpr(at, range.end);
        return try SExpr.Cons(at, start, end);
    } else {
        return try SExpr.Nil(at);
    }
}

pub fn convertPosToSExpr(at: *const Source.Attr, value: ?Source.Pos) !SExpr {
    if (value) |pos| {
        return try SExpr.Cons(
            at,
            try SExpr.Cons(
                at,
                try SExpr.Int(at, pos.line),
                try SExpr.Int(at, pos.column),
            ),
            try SExpr.Int(at, pos.offset),
        );
    } else {
        return try SExpr.Nil(at);
    }
}

pub fn convertSExprToRange(interpreter: *Interpreter, at: *const Source.Attr, value: SExpr) !?Source.Range {
    const xp1 = value.castCons() orelse {
        if (value.isNil()) {
            return null;
        } else {
            return interpreter.abort(Interpreter.Error.TypeError, at, "expected a source range of the form `(start . end)`, got {}", .{value.getTag()});
        }
    };

    const start = try convertSExprToPos(interpreter, at, xp1.car);
    const end = try convertSExprToPos(interpreter, at, xp1.cdr);

    return Source.Range{
        .start = start,
        .end = end,
    };
}

pub fn convertSExprToPos(interpreter: *Interpreter, at: *const Source.Attr, value: SExpr) !?Source.Pos {
    const xp1 = value.castCons() orelse {
        if (value.isNil()) {
            return null;
        } else {
            return interpreter.abort(Interpreter.Error.TypeError, at, "expected a source position of the form `((line . column) . offset)`, got {}", .{value.getTag()});
        }
    };

    const xp2 = xp1.car.castCons() orelse {
        return interpreter.abort(Interpreter.Error.TypeError, at, "expected a source position line and column pair of the form `(line . column)`, got {}: `{}`", .{ xp1.car.getTag(), xp1.car });
    };

    const line = try interpreter.castInt(at, xp2.car);

    const column = try interpreter.castInt(at, xp2.cdr);

    const offset = try interpreter.castInt(at, xp1.cdr);

    if (line < 0) {
        return interpreter.abort(Interpreter.Error.RangeError, at, "line must be non-negative, got {}", .{line});
    }

    if (column < 0) {
        return interpreter.abort(Interpreter.Error.RangeError, at, "column must be non-negative, got {}", .{column});
    }

    if (offset < 0) {
        return interpreter.abort(Interpreter.Error.RangeError, at, "offset must be non-negative, got {}", .{offset});
    }

    return Source.Pos{
        .line = @intCast(line),
        .column = @intCast(column),
        .offset = @intCast(offset),
    };
}

pub fn convertListToSExpr(interpreter: *Interpreter, at: *const Source.Attr, value: anytype, convertElement: fn (*Interpreter, *const Source.Attr, @typeInfo(@TypeOf(value)).pointer.child) Interpreter.Result!SExpr) Interpreter.Result!SExpr {
    var tail = try SExpr.Nil(interpreter.context.attr);

    for (value) |element| {
        const v = try convertElement(interpreter, at, element);
        tail = try SExpr.Cons(interpreter.context.attr, v, tail);
    }

    return tail;
}

pub fn convertSExprToList(comptime T: type, interpreter: *Interpreter, at: *const Source.Attr, value: SExpr, convertElement: fn (*Interpreter, *const Source.Attr, SExpr) Interpreter.Result!T) ![]T {
    var result = std.ArrayList(T).init(interpreter.context.allocator);

    var it = value.iter();

    while (try it.next()) |element| {
        const v = try convertElement(interpreter, at, element);
        try result.append(v);
    }

    return result.toOwnedSlice();
}

pub fn convertSExprToComment(interpreter: *Interpreter, at: *const Source.Attr, value: SExpr) !Source.Comment {
    const pair = try interpreter.castPair(at, value);
    const kind = try convertSExprToCommentKind(interpreter, at, pair.car);
    const text = try interpreter.castStringSlice(at, pair.cdr);

    return Source.Comment { .kind = kind, .text = text };
}

pub fn convertCommentToSExpr(_: *Interpreter, at: *const Source.Attr, value: Source.Comment) !SExpr {
    return try SExpr.Cons(at, try SExpr.Symbol(at, @tagName(value.kind)), try SExpr.StringPreallocated(at, value.text));
}

pub fn convertSExprToCommentKind(interpreter: *Interpreter, at: *const Source.Attr, value: SExpr) !Source.Comment.Kind {
    const text = try interpreter.castStringSlice(at, value);

    const kindNames = comptime std.meta.fieldNames(Source.Comment.Kind);
    inline for (comptime kindNames) |name| {
        if (std.mem.eql(u8, text, name)) {
            return @field(Source.Comment.Kind, name);
        }
    }

    return interpreter.abort(Interpreter.Error.TypeError, at, "expected a comment kind (one of {s}), got {s}", .{kindNames, text});
}

pub fn ExternAttr(at: *const Source.Attr, value: *const Source.Attr) !SExpr {
    const ParserVTable = SExpr.Types.ExternData.VTable(Source.Attr){
        .compare = struct {
            fn fun(self: *const Source.Attr, other: *const Source.Attr) callconv(.C) MiscUtils.Ordering {
                return MiscUtils.compare(self.*, other.*);
            }
        }.fun,
        .format = struct {
            fn fun(self: *const Source.Attr, f: Extern.Writer) callconv(.C) bool {
                return f.print("{}", .{self.*});
            }
        }.fun,
        .hashWith = struct {
            fn fun(self: *const Source.Attr, hasher: *Extern.Hasher) callconv(.C) void {
                MiscUtils.hashWith(hasher, self.*);
            }
        }.fun,
    };

    return try SExpr.ExternData(Source.Attr, at, @constCast(value), &ParserVTable);
}
