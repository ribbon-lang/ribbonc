const std = @import("std");
const MiscUtils = @import("Utils").Misc;

const Rml = @import("root.zig");
const Error = Rml.Error;
const Ordering = Rml.Ordering;
const OOM = Rml.OOM;
const const_ptr = Rml.const_ptr;
const ptr = Rml.ptr;
const Obj = Rml.Obj;
const Object = Rml.Object;
const Procedure = Rml.Procedure;
const Char = Rml.Char;
const String = Rml.String;
const Array = Rml.Array;
const Symbol = Rml.Symbol;
const getHeader = Rml.getHeader;
const getOrigin = Rml.getOrigin;
const getObj = Rml.getObj;
const getRml = Rml.getRml;
const forceObj = Rml.forceObj;
const coerceBool = Rml.coerceBool;

pub const Alias = struct {
    sym: Obj(Symbol),
    sub: Object,

    pub fn deinit(self: Alias) void {
        self.sub.deinit();
    }
};


pub const Pattern = union(enum) {
    // _                    ;wildcard
    wildcard: void,

    // x y z                ;variable
    symbol: Obj(Symbol),

    // () [] {}             ;interchangeable block syntax
    // ~() ~[] ~{}          ;literal block syntax
    block: Obj(Rml.Block),

    // nil true 1 'c' "foo" ;literal
    value_literal: Object,

    // *(foo?) *(@foo x y)  ;procedural literal syntax
    procedure: Object,

    // 'foo '(foo)          ;value-wise quotation
    // `foo `(foo)          ;pattern-wise quotation
    // ,foo ,@foo           ;unquote, unquote-splicing
    quote: Obj(Rml.Quote),

    // (as symbol patt)     ;aliasing ;outer block is not-a-block
    alias: Alias,

    // x y z                ;bare sequence
    sequence: Obj(Array),

    // (? patt)             ;optional ;outer block is not-a-block
    optional: Object,

    // (* patt)             ;zero or more ;outer block is not-a-block
    zero_or_more: Object,

    // (+ patt)             ;one or more ;outer block is not-a-block
    one_or_more: Object,

    // (| patt patt)        ;alternation ;outer block is not-a-block
    alternation: Obj(Array),

    pub fn onDeinit (self: ptr(Pattern)) void {
        switch (self.*) {
            .wildcard => {},
            .symbol => |sym| sym.deinit(),
            .block => |block| block.deinit(),
            .value_literal => |value_literal| value_literal.deinit(),
            .procedure => |procedure| procedure.deinit(),
            .quote => |quote| quote.deinit(),
            .alias => |alias| alias.deinit(),
            .sequence => |sequence| sequence.deinit(),
            .optional => |optional| optional.deinit(),
            .zero_or_more => |zero_or_more| zero_or_more.deinit(),
            .one_or_more => |one_or_more| one_or_more.deinit(),
            .alternation => |alternation| alternation.deinit(),
        }
    }

    pub fn run(self: ptr(Pattern), interpreter: ptr(Rml.Interpreter), diag: ?*?Rml.Diagnostic, input: Object) Rml.Result! ?Obj(Table) {
        var offset: usize = 0;
        const obj = getObj(self);
        defer obj.deinit();
        return runPattern(interpreter, diag, input.getOrigin(), obj, input, &.{}, &offset);
    }
};

pub const Table = Rml.map.TypedMap(Symbol, Rml.ObjData);

pub fn runPattern(
    interpreter: ptr(Rml.Interpreter),
    diag: ?*?Rml.Diagnostic,
    origin: Rml.Origin,
    pattern: Obj(Pattern),
    input: Object,
    objects: []const Object,
    offset: *usize,
) Rml.Result! ?Obj(Table) {
    const env = try Obj(Table).init(getRml(interpreter), pattern.getOrigin());

    switch (pattern.data.*) {
        .wildcard => {},

        .symbol => |symbol| {
            try env.data.set(symbol.clone(), input.clone());
        },

        .block => |block| {
            const patts = block.data.array.items();
            if (Rml.castObj(Rml.Block, input)) |inputBlock| {
                defer inputBlock.deinit();

                const seqOrigin = pattern.getOrigin();
                switch (block.data.kind) {
                    .doc => {
                        var newOffset: usize = 0;
                        const result = try runSequence(interpreter, diag, inputBlock.getOrigin(), patts, inputBlock.data.array.items(), &newOffset) orelse return null;
                        defer result.deinit();

                        try env.data.copyFrom(result);
                    },
                    else =>
                        if (inputBlock.data.kind == block.data.kind) {
                            var newOffset: usize = 0;
                            const result = try runSequence(interpreter, diag, inputBlock.getOrigin(), patts, inputBlock.data.array.items(), &newOffset) orelse return null;
                            defer result.deinit();

                            try env.data.copyFrom(result);
                        } else return patternAbort(
                            diag,
                            seqOrigin,
                            "expected a `{s}{s}` block, found `{s}{s}`",
                            .{
                                block.data.kind.toOpenStr(),
                                block.data.kind.toCloseStr(),
                                inputBlock.data.kind.toOpenStr(),
                                inputBlock.data.kind.toCloseStr(),
                            }
                        ),

                }
            } else return patternAbort(diag, pattern.getOrigin(), "expected a block, found `{}`", .{input});
        },

        .value_literal => |value_literal| {
            if (value_literal.onCompare(input) != .Equal)
                return patternAbort(diag, input.getOrigin(),
                    "expected `{}`, got `{}`", .{value_literal, input});
        },

        .procedure => |procedure| {
            const result = try interpreter.invoke(input.getOrigin(), procedure, &.{input});
            defer result.deinit();

            if (!coerceBool(result))
                return patternAbort(diag, input.getOrigin(),
                    "expected a truthy value, got `{}`", .{input});
        },

        .quote => |quote| switch (quote.data.kind) {
            .basic => {
                const patt = quote.data.body;
                if (patt.onCompare(input) != .Equal) return patternAbort(diag, input.getOrigin(),
                    "expected `{}`, got `{}`", .{patt, input});
            },
            .quasi => {
                const q = try Rml.quote.runQuasi(interpreter, quote.data.body);
                defer q.deinit();

                const patt = try PatternParser.parsePattern(q);
                defer patt.deinit();

                const result = try runPattern(interpreter, diag, origin, patt, input, objects, offset) orelse return null;
                defer result.deinit();

                try env.data.copyFrom(result);
            },
            .to_quote => {
                const v = try interpreter.eval(quote.data.body);
                defer v.deinit();

                const q = try Obj(Rml.Quote).wrap(getRml(interpreter), quote.getOrigin(), .{ .kind = .basic, .body = v});
                defer q.deinit();

                if (q.onCompare(input) != .Equal) return patternAbort(diag, input.getOrigin(),
                    "expected `{}`, got `{}`", .{q, input});
            },
            .to_quasi => {
                const v = try interpreter.eval(quote.data.body);
                defer v.deinit();

                const q = try Obj(Rml.Quote).wrap(getRml(interpreter), quote.getOrigin(), .{ .kind = .quasi, .body = v});
                defer q.deinit();

                if (q.onCompare(input) != .Equal)
                    return patternAbort(diag, input.getOrigin(),
                        "expected `{}`, got `{}`", .{q, input});
            },
            .unquote => {
                const pattObject = try interpreter.eval(quote.data.body);
                defer pattObject.deinit();

                const patt = Rml.castObj(Rml.Pattern, pattObject) orelse {
                    try interpreter.abort(quote.data.body.getOrigin(), error.UnexpectedInput,
                        "unquote syntax expects a pattern in this context, found `{}`", .{quote.data.body});
                };

                const result = try runPattern(interpreter, diag, origin, patt, input, objects, offset) orelse return null;
                defer result.deinit();

                try env.data.copyFrom(result);
            },
            .unquote_splice => {
                const body: Object = try interpreter.eval(quote.data.body);
                defer body.deinit();

                const array = try Rml.object.coerceArray(body) orelse {
                    try interpreter.abort(quote.data.body.getOrigin(), error.TypeError,
                        "unquote-splice syntax expects an array-like value in this context, found `{}`", .{body});
                };

                const result = try runSequence(interpreter, diag, origin, array.data.items(), objects, offset) orelse return null;
                defer result.deinit();

                try env.data.copyFrom(result);
            },
        },

        .alias => |alias| {
            const sub: Obj(Pattern) = Rml.castObj(Pattern, alias.sub) orelse {
                try interpreter.abort(alias.sub.getOrigin(), error.UnexpectedInput,
                    "alias syntax expects a pattern in this context, found `{}`", .{alias.sub});
            };
            defer sub.deinit();

            const result = try runPattern(interpreter, diag, origin, sub, input, objects, offset) orelse return null;
            defer result.deinit();

            if (result.data.length() > 0) {
                try env.data.set(alias.sym.clone(), result.typeErase());
            } else {
                try env.data.set(alias.sym.clone(), input.clone());
            }
        },

        .sequence => |sequence| {
            const subEnv = try runSequence(interpreter, diag, sequence.getOrigin(), sequence.data.items(), objects, offset) orelse return null;
            defer subEnv.deinit();

            try env.data.copyFrom(subEnv);
        },

        .optional => |optional| {
            const patt = Rml.castObj(Rml.Pattern, optional) orelse {
                try interpreter.abort(optional.getOrigin(), error.TypeError,
                    "optional syntax expects a pattern in this context, found `{}`", .{optional});
            };

            var subOffset = offset.*;
            const result = try runPattern(interpreter, null, origin, patt, input, objects, &subOffset);

            if (result) |res| {
                defer res.deinit();

                offset.* = subOffset;

                try env.data.copyFrom(res);
            }
        },

        .zero_or_more => |zero_or_more| {
            const patt = Rml.castObj(Rml.Pattern, zero_or_more) orelse {
                try interpreter.abort(zero_or_more.getOrigin(), error.TypeError,
                    "zero-or-more syntax expects a pattern in this context, found `{}`", .{zero_or_more});
            };

            while (offset.* < objects.len) : (offset.* += 1) {
                var subOffset = offset.*;
                const result = try runPattern(interpreter, null, origin, patt, objects[offset.*], objects, &subOffset);

                if (result) |res| {
                    defer res.deinit();

                    offset.* = subOffset;

                    try env.data.copyFrom(res);
                } else break;
            }
        },

        .one_or_more => |one_or_more| {
            const patt = Rml.castObj(Rml.Pattern, one_or_more) orelse {
                try interpreter.abort(one_or_more.getOrigin(), error.TypeError,
                    "one-or-more syntax expects a pattern in this context, found `{}`", .{one_or_more});
            };

            var i: usize = 0;
            while (offset.* < objects.len) : (offset.* += 1) {
                var subOffset = offset.*;

                const result = try runPattern(interpreter, null, origin, patt, objects[offset.*], objects, &subOffset);
                if (result) |res| {
                    defer res.deinit();

                    i += 1;

                    offset.* = subOffset;

                    try env.data.copyFrom(res);
                } else if (i > 0) {
                    break;
                } else {
                    return patternAbort(diag, input.getOrigin(),
                        "expected one or more of the pattern `{}`, found `{}`", .{patt, input});
                }
            }
        },

        .alternation => |alternation| {
            const pattObjs = alternation.data.items();
            var errs: Rml.string.StringUnmanaged = .{};
            defer errs.deinit(getRml(interpreter));

            const errWriter = errs.writer(getRml(interpreter));

            loop: for (pattObjs) |pattObj| {
                const patt = Rml.castObj(Rml.Pattern, pattObj) orelse {
                    try interpreter.abort(pattObj.getOrigin(), error.UnexpectedInput,
                        "alternation syntax expects a pattern in this context, found `{}`", .{pattObj});
                };
                defer patt.deinit();

                var diagStorage: ?Rml.Diagnostic = null;
                const newDiag = if (diag != null) &diagStorage else null;

                var subOffset = offset.*;
                const result = try runPattern(interpreter, newDiag, origin, patt, input, objects, &subOffset);

                if (result) |res| {
                    defer res.deinit();

                    offset.* = subOffset;

                    try env.data.copyFrom(res);

                    break :loop;
                } else if (newDiag) |dx| {
                    if (dx.*) |d| {
                        const formatter = d.formatter(error.PatternMatch);
                        Rml.log.debug("failed alternative {}", .{formatter});
                        errWriter.print("\t{}\n", .{formatter}) catch |e| @panic(@errorName(e));
                    } else {
                        Rml.log.warn("requested pattern diagnostic is null", .{});
                    }
                }
            }

            return patternAbort(diag, input.getOrigin(),
                "all alternatives failed:\n{s}", .{errs.text()});
        }
    }

    return env;
}

fn runSequence(
    interpreter: ptr(Rml.Interpreter),
    diag: ?*?Rml.Diagnostic,
    origin: Rml.Origin,
    patterns: []const Object,
    objects: []const Object,
    offset: *usize,
) Rml.Result! ?Obj(Table) {
    const env: Obj(Table) = try .init(getRml(interpreter), origin);
    errdefer env.deinit();

    for (patterns, 0..) |patternObj, p| {
        _ = p;

        const input = objects[offset.*];
        offset.* += 1;

        const pattern = Rml.castObj(Rml.Pattern, patternObj) orelse {
            try interpreter.abort(patternObj.getOrigin(), error.UnexpectedInput,
                "sequence syntax expects a pattern in this context, found `{}`", .{patternObj});
        };

        const result = try runPattern(interpreter, diag, input.getOrigin(), pattern, input, objects, offset) orelse return null;
        try env.data.copyFrom(result);
    }

    return env;
}

pub fn patternAbort(diagnostic: ?*?Rml.Diagnostic, origin: Rml.Origin, comptime fmt: []const u8, args: anytype) ?Obj(Table) {
    const diagPtr = diagnostic orelse return null;

    var diag = Rml.Diagnostic {
        .error_origin = origin,
    };

    // the error produced is only NoSpaceLeft, if the buffer is too small, so give the length of the buffer
    diag.message_len = len: {
        break :len (std.fmt.bufPrintZ(&diag.message_mem, fmt, args) catch {
            Rml.log.warn("Pattern Diagnostic message too long, truncating", .{});
            break :len Rml.Diagnostic.MAX_LENGTH;
        }).len;
    };

    diagPtr.* = diag;

    return null;
}


pub const PatternParser = struct {
    pub fn parsePattern(quote: Object) !Obj(Pattern) {
        MiscUtils.todo(noreturn, quote);
    }
};
