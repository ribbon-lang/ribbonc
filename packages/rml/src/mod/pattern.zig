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

    // x                    ;variable
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


    pub fn onFormat (self: ptr(Pattern), writer: Obj(Rml.Writer)) Error! void {
        switch (self.*) {
            .wildcard => try writer.data.writeAll("_"),
            .symbol => try writer.data.print("{}", .{self.symbol}),
            .block => try writer.data.print("{}", .{self.block}),
            .value_literal => try writer.data.print("{}", .{self.value_literal}),
            .procedure => try writer.data.print("{}", .{self.procedure}),
            .quote => try writer.data.print("{}", .{self.quote}),
            .alias => try writer.data.print("{{as {} {}}}", .{self.alias.sym, self.alias.sub}),
            .sequence => try writer.data.print("{s}", .{self.sequence}),
            .optional => try writer.data.print("{{? {s}}}", .{self.optional}),
            .zero_or_more => try writer.data.print("{{* {s}}}", .{self.zero_or_more}),
            .one_or_more => try writer.data.print("{{+ {s}}}", .{self.one_or_more}),
            .alternation => try writer.data.print("{{| {s}}}", .{self.alternation}),
        }
    }

    pub fn onDeinit (self: *Pattern) void {
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

    pub fn parse(input: Object, diag: ?*?Rml.Diagnostic) Rml.Result! Obj(Pattern) {
        var offset: usize = 0;
        return parsePattern(diag, input, &.{}, &offset);
    }
};

pub const Table = Rml.map.TypedMap(Symbol, Rml.ObjData);

pub fn patternBinders(patternObj: Object) (OOM || error{BadDomain})! Rml.env.Domain {
    const rml = patternObj.getRml();

    const pattern = Rml.castObj(Pattern, patternObj) orelse return .{};

    var domain: Rml.env.Domain = .{};
    errdefer domain.deinit(rml);

    switch (pattern.data.*) {
        .wildcard,
        .value_literal,
        .procedure,
        .quote,
            => {},

        .symbol => |symbol| try domain.set(rml, symbol.clone()),

        .block => |block| for (block.data.array.items()) |item| {
            var subDomain = try patternBinders(item);
            defer subDomain.deinit(rml);

            try domain.copyFrom(rml, &subDomain);
        },

        .alias => |alias| try domain.set(rml, alias.sym.clone()),

        .sequence => |sequence| for (sequence.data.items()) |item| {
            var subDomain = try patternBinders(item);
            defer subDomain.deinit(rml);

            try domain.copyFrom(rml, &subDomain);
        },

        .optional => |optional| {
            var subDomain = try patternBinders(optional);
            defer subDomain.deinit(rml);

            try domain.copyFrom(rml, &subDomain);
        },

        .zero_or_more => |zero_or_more| {
            var subDomain = try patternBinders(zero_or_more);
            defer subDomain.deinit(rml);

            try domain.copyFrom(rml, &subDomain);
        },

        .one_or_more => |one_or_more| {
            var subDomain = try patternBinders(one_or_more);
            defer subDomain.deinit(rml);

            try domain.copyFrom(rml, &subDomain);
        },

        .alternation => |alternation| {
            var referenceSubDomain: ?Rml.env.Domain = null;
            defer if (referenceSubDomain) |*d| d.deinit(rml);

            for (alternation.data.items()) |item| {
                var subDomain = try patternBinders(item);

                if (referenceSubDomain) |refDomain| {
                    defer subDomain.deinit(rml);

                    if (Rml.equal(refDomain, subDomain)) {
                        try domain.copyFrom(rml, &subDomain);
                    } else {
                        return error.BadDomain;
                    }
                } else {
                    referenceSubDomain = subDomain;
                }
            }
        },
    }

    return domain;
}

pub fn runPattern(
    interpreter: ptr(Rml.Interpreter),
    diag: ?*?Rml.Diagnostic,
    origin: Rml.Origin,
    pattern: Obj(Pattern),
    input: Object,
    objects: []const Object,
    offset: *usize,
) Rml.Result! ?Obj(Table) {
    const env: Obj(Table) = try .init(getRml(interpreter), pattern.getOrigin());
    defer env.deinit();

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
            const result = try interpreter.invoke(input.getOrigin(), pattern.typeEraseLeak(), procedure, &.{input});
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
                const w = try Rml.quote.runQuasi(interpreter, quote.data.body);
                defer w.deinit();

                if (w.onCompare(input) != .Equal) return patternAbort(diag, input.getOrigin(),
                    "expected `{}`, got `{}`", .{w, input});
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

                const w = try Rml.quote.runQuasi(interpreter, q.typeEraseLeak());
                defer w.deinit();

                if (w.onCompare(input) != .Equal) return patternAbort(diag, input.getOrigin(),
                    "expected `{}`, got `{}`", .{q, input});
            },
            .unquote, .unquote_splice => try interpreter.abort(quote.getOrigin(), error.UnexpectedInput,
                "unquote syntax is not allowed in this context, found `{}`", .{quote}),
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
            defer patt.deinit();

            var subOffset = offset.*;
            const result = try runPattern(interpreter, null, origin, patt, input, objects, &subOffset);

            if (result) |res| {
                defer res.deinit();

                offset.* = subOffset;

                try env.data.copyFrom(res);
            } else {
                var binders = patternBinders(patt.typeEraseLeak()) catch |err| switch (err) {
                    error.BadDomain => try interpreter.abort(patt.getOrigin(), error.PatternError,
                        "bad domain in pattern `{}`", .{patt}),
                    error.OutOfMemory => return error.OutOfMemory,
                };
                defer binders.deinit(getRml(interpreter));

                const nil = (try Obj(Rml.Nil).init(getRml(interpreter), origin)).typeEraseLeak();
                defer nil.deinit();

                for (binders.keys()) |key| {
                    try env.data.set(key.clone(), nil.clone());
                }
            }
        },

        .zero_or_more => |zero_or_more| {
            const patt = Rml.castObj(Rml.Pattern, zero_or_more) orelse {
                try interpreter.abort(zero_or_more.getOrigin(), error.TypeError,
                    "zero-or-more syntax expects a pattern in this context, found `{}`", .{zero_or_more});
            };
            defer patt.deinit();

            while (offset.* < objects.len) {
                var subOffset = offset.* + 1;
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
            defer patt.deinit();

            var i: usize = 0;
            while (offset.* < objects.len) {
                var subOffset = offset.* + 1;

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

    return env.clone();
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
    defer env.deinit();

    for (patterns, 0..) |patternObj, p| {
        _ = p;

        const input =
            if (offset.* < objects.len) adv: {
                const out = objects[offset.*];
                offset.* += 1;
                break :adv out.clone();
            } else (try Obj(Rml.Nil).init(getRml(interpreter), origin)).typeEraseLeak();
        defer input.deinit();

        const pattern = Rml.castObj(Rml.Pattern, patternObj) orelse {
            try interpreter.abort(patternObj.getOrigin(), error.UnexpectedInput,
                "sequence syntax expects a pattern in this context, found `{}`", .{patternObj});
        };
        defer pattern.deinit();

        const result = try runPattern(interpreter, diag, input.getOrigin(), pattern, input, objects, offset) orelse return null;
        defer result.deinit();

        try env.data.copyFrom(result);
    }

    if (offset.* < objects.len) return patternAbort(diag, origin, "unexpected input `{}`", .{objects[offset.*]});

    return env.clone();
}

fn patternAbort(diagnostic: ?*?Rml.Diagnostic, origin: Rml.Origin, comptime fmt: []const u8, args: anytype) ?Obj(Table) {
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


fn abortParse(diagnostic: ?*?Rml.Diagnostic, origin: Rml.Origin, err: (OOM || Rml.SyntaxError), comptime fmt: []const u8, args: anytype) (OOM || Rml.SyntaxError)! noreturn {
    const diagPtr = diagnostic orelse return err;

    var diag = Rml.Diagnostic {
        .error_origin = origin,
    };

    // the error produced is only NoSpaceLeft, if the buffer is too small, so give the length of the buffer
    diag.message_len = len: {
        break :len (std.fmt.bufPrintZ(&diag.message_mem, fmt, args) catch {
            Rml.log.warn("Diagnostic message too long, truncating", .{});
            break :len Rml.Diagnostic.MAX_LENGTH;
        }).len;
    };

    diagPtr.* = diag;

    return err;
}

fn parseSequence(diag: ?*?Rml.Diagnostic, rml: *Rml, objects: []const Object, offset: *usize) (OOM || Rml.SyntaxError)! Rml.array.ArrayUnmanaged {
    var output: Rml.array.ArrayUnmanaged = .{};
    errdefer output.deinit(rml);

    while (offset.* < objects.len) {
        const obj = objects[offset.*];
        offset.* += 1;

        const patt = try parsePattern(diag, obj, objects, offset);
        defer patt.deinit();

        try output.append(rml, patt.typeErase());
    }

    return output;
}

fn parsePattern(diag: ?*?Rml.Diagnostic, input: Object, objects: []const Object, offset: *usize) (OOM || Rml.SyntaxError)! Obj(Pattern) {
    const rml = input.getRml();

    if (Rml.castObj(Pattern, input)) |patt| {
        Rml.parser.parsing.debug("parsePattern got existing pattern `{}`", .{patt});
        return patt;
    } else {
        Rml.parser.parsing.debug("parsePattern `{}` {any} {}", .{input, objects, offset.*});
        var body: Pattern =
            if (Rml.castObj(Rml.Symbol, input)) |sym| (
                if (BUILTIN_SYMBOLS.matchText(sym.data.text())) |fun| {
                    sym.deinit();
                    return fun(diag, input, objects, offset);
                } else .{.symbol = sym}
            ) else if (Rml.isAtom(input)) .{.value_literal = input.clone()}
            else if (Rml.castObj(Rml.Quote, input)) |quote| .{.quote = quote}
            else if (Rml.castObj(Rml.Block, input)) |block| block: {
                defer block.deinit();

                if (block.data.length() > 0) not_a_block: {
                    const items = block.data.array.items();

                    const symbol = Rml.castObj(Rml.Symbol, items[0]) orelse break :not_a_block;
                    defer symbol.deinit();

                    inline for (comptime std.meta.declarations(NOT_A_BLOCK)) |decl| {
                        if (std.mem.eql(u8, decl.name, symbol.data.text())) {
                            Rml.log.debug("using {s} as a not-a-block pattern", .{symbol});
                            var subOffset: usize = 1;
                            return @field(NOT_A_BLOCK, decl.name)(diag, input, block.data.array.items(), &subOffset);
                        }
                    }
                }

                var subOffset: usize = 0;
                var seq: Rml.array.ArrayUnmanaged = try parseSequence(diag, rml, block.data.array.items(), &subOffset);
                errdefer seq.deinit(rml);

                break :block Pattern { .block = try Obj(Rml.Block).wrap(rml, input.getOrigin(), .{ .kind = .doc, .array = seq }) };
            }
            else try abortParse(diag, input.getOrigin(), error.SyntaxError, "`{}` is not a valid pattern", .{input});

        errdefer body.onDeinit();

        return Obj(Pattern).wrap(rml, input.getOrigin(), body);
    }
}

const BUILTIN_SYMBOLS = struct {
    fn matchText(text: []const u8) ?*const fn (?*?Rml.Diagnostic, Object, []const Object, *usize) (OOM || Rml.SyntaxError)! Obj(Pattern) {
        inline for (comptime std.meta.declarations(BUILTIN_SYMBOLS)) |decl| {
            if (std.mem.eql(u8, decl.name, text)) return @field(BUILTIN_SYMBOLS, decl.name);
        }
        return null;
    }

    pub fn @"_"(_: ?*?Rml.Diagnostic, input: Object, _: []const Object, _: *usize) (OOM || Rml.SyntaxError)! Obj(Pattern) {
        return Obj(Pattern).wrap(input.getRml(), input.getOrigin(), .wildcard);
    }

    /// literal block syntax; expect a block, return that exact block kind (do not change to doc like default)
    pub fn @"~"(diag: ?*?Rml.Diagnostic, input: Object, objects: []const Object, offset: *usize) (OOM || Rml.SyntaxError)! Obj(Pattern) {
        const rml = input.getRml();
        const origin = input.getOrigin();

        if (offset.* >= objects.len) return error.UnexpectedEOF;

        const block = Rml.castObj(Rml.Block, objects[offset.*]) orelse {
            return error.SyntaxError;
        };
        defer block.deinit();
        offset.* += 1;

        var subOffset: usize = 0;
        const body = try parseSequence(diag, rml, block.data.array.items(), &subOffset);

        const patternBlock = try Obj(Rml.Block).wrap(rml, origin, .{ .kind = block.data.kind, .array = body });

        return Obj(Pattern).wrap(rml, origin, .{.block = patternBlock});
    }

    pub fn @"$"(diag: ?*?Rml.Diagnostic, input: Object, objects: []const Object, offset: *usize) (OOM || Rml.SyntaxError)! Obj(Pattern) {
        const rml = input.getRml();
        const origin = input.getOrigin();

        if (offset.* >= objects.len) return error.UnexpectedEOF;

        const block = Rml.castObj(Rml.Block, objects[offset.*])
            orelse try abortParse(diag, origin, error.SyntaxError, "expected a block to escape following `$`, got `{}`", .{objects[offset.*]});
        defer block.deinit();
        offset.* += 1;

        const seq = seq: {
            var items = try block.data.array.clone(input.getRml());
            errdefer items.deinit(input.getRml());

            break :seq try Obj(Rml.Array).wrap(rml, origin, .{ .unmanaged = items });
        };
        defer seq.deinit();

        return Obj(Pattern).wrap(rml, origin, .{.sequence = seq});
    }
};

const NOT_A_BLOCK = struct {
    fn recursive(comptime name: []const u8) *const fn (?*?Rml.Diagnostic, Object, []const Object, *usize) (OOM || Rml.SyntaxError)! Obj(Pattern) {
        return &struct {
            pub fn fun(diag: ?*?Rml.Diagnostic, obj: Object, objects: []const Object, offset: *usize) (OOM || Rml.SyntaxError)! Obj(Pattern) {
                Rml.log.debug("recursive-{s} `{}` {any} {}", .{name, obj, objects, offset.*});
                const rml = obj.getRml();

                if (offset.* != 1)
                    try abortParse(diag, obj.getOrigin(), error.SyntaxError, "{s}-syntax should be at start of expression", .{name});

                if (offset.* >= objects.len)
                    try abortParse(diag, obj.getOrigin(), error.SyntaxError, "expected a pattern for {s}-syntax", .{name});

                const array = array: {
                    var seq = try parseSequence(diag, rml, objects, offset);
                    errdefer seq.deinit(rml);

                    break :array try Obj(Rml.Array).wrap(rml, obj.getOrigin(), .{ .unmanaged = seq });
                };
                errdefer array.deinit();

                const sub = switch (array.data.length()) {
                    0 => unreachable,
                    1 => one: {
                        defer array.deinit();
                        const singleObj = array.data.get(0).?;
                        defer singleObj.deinit();
                        break :one Rml.object.castObj(Pattern, singleObj).?;
                    },
                    else => try Obj(Pattern).wrap(obj.getRml(), obj.getOrigin(), .{.sequence = array}),
                };
                defer sub.deinit();

                return Obj(Pattern).wrap(obj.getRml(), obj.getOrigin(), @unionInit(Pattern, name, sub.typeErase()));
            }
        }.fun;
    }

    pub const @"?" = recursive("optional");
    pub const @"*" = recursive("zero_or_more");
    pub const @"+" = recursive("one_or_more");

    pub fn @"|"(diag: ?*?Rml.Diagnostic, input: Object, objects: []const Object, offset: *usize) (OOM || Rml.SyntaxError)! Obj(Pattern) {
        if (offset.* != 1)
            try abortParse(diag, input.getOrigin(), error.SyntaxError, "alternation-syntax should be at start of expression", .{});

        if (offset.* >= objects.len)
            try abortParse(diag, input.getOrigin(), error.SyntaxError, "expected a block to follow `|`", .{});

        const rml = input.getRml();
        const origin = input.getOrigin();

        const array = array: {
            var seq = try parseSequence(diag, rml, objects, offset);
            errdefer seq.deinit(rml);

            break :array try Obj(Rml.Array).wrap(rml, origin, .{ .unmanaged = seq });
        };
        errdefer array.deinit();

        return Obj(Pattern).wrap(rml, origin, .{.alternation = array});
    }
};
