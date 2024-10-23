const std = @import("std");

const Support = @import("ZigUtils").Misc;

const TypeUtils = @import("ZigUtils").Type;
const TextUtils = @import("ZigUtils").Text;
const Char = TextUtils.Char;

const Core = @import("root.zig");
const Source = Core.Source;
const Context = Core.Context;
const SExpr = Core.SExpr;

context: *Context,
sexprBuffer: std.ArrayList(SExpr),
textBuffer: std.ArrayList(u8),
fileName: []const u8,
input: []const u8,
peekCache: ?Char,
pos: Source.Pos,
posOffset: Source.Pos,

const Parser = @This();

pub const SyntaxError = error{ UnexpectedInput, UnexpectedEOF };
pub const ParseError = SyntaxError || TextUtils.Error;
pub const ParserError = ParseError || Context.Error;

pub fn isSyntaxError(err: anyerror) bool {
    return TypeUtils.isInErrorSet(SyntaxError, err);
}

pub fn isParseError(err: anyerror) bool {
    return TypeUtils.isInErrorSet(ParseError, err);
}

pub fn asSyntaxError(err: anyerror) ?SyntaxError {
    return TypeUtils.narrowErrorSet(SyntaxError, err);
}

pub fn asParseError(err: anyerror) ?ParseError {
    return TypeUtils.narrowErrorSet(ParseError, err);
}

pub fn init(context: *Context) Context.Error!*Parser {
    const sexprBuffer = std.ArrayList(SExpr).init(context.allocator);

    const textBuffer = try std.ArrayList(u8).initCapacity(context.allocator, 1024);
    errdefer textBuffer.deinit();

    const ptr: *Parser = try context.allocator.create(Parser);
    errdefer context.allocator.destroy(ptr);

    ptr.* = Parser{
        .context = context,
        .sexprBuffer = sexprBuffer,
        .textBuffer = textBuffer,
        .fileName = "unknown",
        .input = "",
        .peekCache = null,
        .pos = Source.Pos.init(0, 0, 0),
        .posOffset = Source.Pos.init(1, 1, 0),
    };

    return ptr;
}

pub fn compare(self: Parser, other: Parser) Support.Ordering {
    var res = Support.compare(self.fileName, other.fileName);

    if (res == Support.Ordering.Equal) {
        res = Support.compare(self.pos, other.pos);
    }

    return res;
}

pub fn hashWith(self: Parser, hasher: anytype) void {
    Support.hashWith(hasher, self.fileName);
    Support.hashWith(hasher, self.pos);
}

pub fn deinit(self: *Parser) void {
    self.sexprBuffer.deinit();
    self.textBuffer.deinit();
    self.context.allocator.destroy(self);
}

pub fn setFileName(self: *Parser, fileName: []const u8) Context.Error!void {
    self.fileName = try self.context.newBuffer(u8, fileName);
}

pub fn setInput(self: *Parser, input: []const u8, posOffset: ?Source.Pos) void {
    self.input = input;
    self.pos = Source.Pos.init(0, 0, 0);
    self.posOffset = posOffset orelse Source.Pos.init(1, 1, 0);
    self.peekCache = null;
}

pub inline fn reset(self: *Parser, pos: Source.Pos) @TypeOf(null) {
    self.pos = pos;
    self.peekCache = null;
    return null;
}

pub fn isEof(self: *Parser) bool {
    return self.pos.offset >= self.input.len;
}

pub fn notEof(self: *Parser) bool {
    return self.pos.offset < self.input.len;
}

pub fn peekChar(self: *Parser) ParseError!?Char {
    if (self.isEof()) {
        return null;
    }

    if (self.peekCache) |char| {
        return char;
    } else {
        const len = try TextUtils.sequenceLengthByte(self.input[self.pos.offset]);
        const slice = self.input[self.pos.offset .. self.pos.offset + len];

        const char = try TextUtils.decode(slice);
        self.peekCache = char;

        return char;
    }
}

pub fn advance(self: *Parser) ParseError!void {
    if (try self.peekChar()) |char| {
        switch (char) {
            '\n' => {
                self.pos.line += 1;
                self.pos.column = 0;
                self.pos.offset += 1;
            },

            else => {
                self.pos.column += 1;
                self.pos.offset += try TextUtils.sequenceLength(char);
            },
        }

        self.peekCache = null;
    }
}

pub fn nextChar(self: *Parser) ParseError!?Char {
    if (try self.peekChar()) |char| {
        switch (char) {
            '\n' => {
                self.pos.line += 1;
                self.pos.column = 0;
                self.pos.offset += 1;
            },

            else => {
                self.pos.column += 1;
                self.pos.offset += try TextUtils.sequenceLength(char);
            },
        }

        self.peekCache = null;

        return char;
    } else {
        return null;
    }
}

pub fn mkAttr(self: *Parser, start: ?Source.Pos, end: ?Source.Pos) Context.Error!*Source.Attr {
    return self.context.bindAttrExistingFile(self.fileName, Source.Range.init(start, end).addPos(self.posOffset));
}

pub fn spaceP(self: *Parser) ParserError!?[]const u8 {
    const start = self.pos;

    while (try self.peekChar()) |char| {
        if (TextUtils.isSpace(char)) {
            try self.advance();
        } else {
            break;
        }
    }

    if (start.offset == self.pos.offset) {
        return null;
    }

    return self.input[start.offset..self.pos.offset];
}

pub fn digitP(self: *Parser) ParserError!?u8 {
    if (try self.peekChar()) |char| {
        if (TextUtils.decimalValue(char)) |value| {
            try self.advance();
            return value;
        }
    }

    return null;
}

pub fn signP(self: *Parser, comptime T: type) ParserError!?T {
    if (try self.peekChar()) |char| {
        if (char == '-') {
            try self.advance();
            return -1;
        } else if (char == '+') {
            try self.advance();
        }

        return 1;
    }

    return null;
}

pub fn expectCharP(self: *Parser, char: Char) ParserError!bool {
    if (try self.peekChar() == char) {
        try self.advance();
        return true;
    }

    return false;
}

pub fn expectAnyCharP(self: *Parser, chars: []const Char) ParserError!?Char {
    if (try self.peekChar()) |char| {
        for (chars) |c| {
            if (char == c) {
                try self.advance();
                return c;
            }
        }
    }

    return null;
}

pub fn expectStringP(self: *Parser, str: []const u8) ParserError!bool {
    var i: usize = 0;
    const start = self.pos;

    while (i < str.len) {
        const dec = try TextUtils.decode1(str[i..]);

        if (try self.expectCharP(dec.ch)) {
            i += dec.len;
        } else {
            _ = self.reset(start);
            return false;
        }
    }

    return true;
}

pub fn failed(self: *Parser) ParserError {
    if (self.isEof()) {
        return SyntaxError.UnexpectedEOF;
    } else {
        return SyntaxError.UnexpectedInput;
    }
}

pub fn require(self: *Parser, comptime T: type, value: ?T) ParserError!T {
    if (value) |v| {
        return v;
    } else {
        return self.failed();
    }
}

pub fn scanP(self: *Parser, comptime T: type, callback: anytype, args: anytype) ParserError!T {
    _ = try self.spaceP();
    return @call(.auto, callback, .{self} ++ args);
}

pub fn totalP(self: *Parser, comptime T: type, callback: anytype, args: anytype) ParserError!T {
    const result = try self.scanP(?T, callback, args) orelse {
        return SyntaxError.UnexpectedInput;
    };
    try self.scanP(void, Parser.eofP, .{});
    return result;
}

pub fn requireP(self: *Parser, comptime T: type, callback: anytype, args: anytype) ParserError!T {
    return try @call(.auto, callback, .{self} ++ args) orelse {
        return self.failed();
    };
}

pub fn requireCondP(self: *Parser, callback: anytype, args: anytype) ParserError!void {
    if (!try @call(.auto, callback, .{self} ++ args)) {
        return self.failed();
    }
}

pub fn requireCondScanP(self: *Parser, callback: anytype, args: anytype) ParserError!void {
    if (!try self.scanP(bool, callback, args)) {
        return self.failed();
    }
}

pub fn requireScanP(self: *Parser, comptime T: type, callback: anytype, args: anytype) ParserError!T {
    return try self.scanP(?T, callback, args) orelse {
        return self.failed();
    };
}

pub fn requireScanOrEofP(self: *Parser, comptime T: type, callback: anytype, args: anytype) ParserError!?T {
    _ = try self.spaceP();

    if (self.isEof()) {
        return null;
    }

    return try @call(.auto, callback, .{self} ++ args) orelse return SyntaxError.UnexpectedInput;
}

pub fn nextIsSentinel(self: *Parser) ParserError!bool {
    if (try self.peekChar()) |next| {
        switch (next) {
            inline ')', ']', '}' => return true,
            else => return TextUtils.isSpace(next),
        }
    }

    return true;
}

pub fn eofP(self: *Parser) ParserError!void {
    if (self.notEof()) {
        return SyntaxError.UnexpectedInput;
    }
}

pub inline fn scanSExprP(self: *Parser) ParserError!?SExpr {
    return self.requireScanOrEofP(SExpr, sexprP, .{});
}

pub fn sexprP(self: *Parser) ParserError!?SExpr {
    return try self.atomP() orelse try self.listP() orelse try self.quoteP() orelse try self.quasiP() orelse try self.unquoteP();
}

pub fn atomP(self: *Parser) ParserError!?SExpr {
    const start = self.pos;

    const atom = try self.floatP() orelse try self.intP() orelse try self.charP() orelse try self.stringP() orelse try self.symbolP();

    if (try self.nextIsSentinel()) {
        return atom;
    } else {
        return self.reset(start);
    }
}

pub fn listP(self: *Parser) ParserError!?SExpr {
    const start = self.pos;

    if (!try self.expectCharP('(')) {
        return null;
    }

    const sexprBufferBase = self.sexprBuffer.items.len;
    // shrink and free is less efficient, but retaining capacity would cause leaks in GC
    defer self.sexprBuffer.shrinkAndFree(sexprBufferBase);

    while (self.notEof()) {
        _ = try self.spaceP();

        if (self.sexprBuffer.items.len - sexprBufferBase > 0 and try self.expectCharP('.')) {
            const restItem = try self.requireScanP(SExpr, Parser.sexprP, .{});

            try self.requireCondScanP(Parser.expectCharP, .{')'});

            const attr = try self.mkAttr(start, self.pos);

            return try SExpr.ListTail(attr, self.sexprBuffer.items[sexprBufferBase..], restItem);
        }

        if (try self.expectCharP(')')) {
            const attr = try self.mkAttr(start, self.pos);

            return try SExpr.List(attr, self.sexprBuffer.items[sexprBufferBase..]);
        }

        const item = try self.requireScanP(SExpr, Parser.sexprP, .{});

        try self.sexprBuffer.append(item);
    }

    return SyntaxError.UnexpectedEOF;
}

pub fn unquoteP(self: *Parser) ParserError!?SExpr {
    const start = self.pos;

    if (!try self.expectCharP(',')) {
        return null;
    }

    const symText = symText: {
        if (try self.expectCharP('@')) {
            break :symText "unquote-splicing";
        } else {
            break :symText "unquote";
        }
    };

    const item = try self.requireP(SExpr, Parser.sexprP, .{});

    const attr = try self.mkAttr(start, self.pos);

    const quoteSym = try SExpr.Symbol(attr, symText);

    return try SExpr.List(attr, &[_]SExpr{ quoteSym, item });
}

pub fn quasiP(self: *Parser) ParserError!?SExpr {
    const start = self.pos;

    if (!try self.expectCharP('`')) {
        return null;
    }

    const item = try self.requireP(SExpr, Parser.sexprP, .{});

    const attr = try self.mkAttr(start, self.pos);

    const quoteSym = try SExpr.Symbol(attr, "quasiquote");

    return try SExpr.List(attr, &[_]SExpr{ quoteSym, item });
}

pub fn quoteP(self: *Parser) ParserError!?SExpr {
    const start = self.pos;

    if (!try self.expectStringP("'")) {
        return null;
    }

    const item = try self.requireP(SExpr, Parser.sexprP, .{});

    const attr = try self.mkAttr(start, self.pos);

    const quoteSym = try SExpr.Symbol(attr, "quote");

    return try SExpr.List(attr, &[_]SExpr{ quoteSym, item });
}

pub fn intP(self: *Parser) ParserError!?SExpr {
    const start = self.pos;

    var int: i64 = 0;

    const sign = try self.signP(i64) orelse return null;

    var digits: usize = 0;

    while (try self.digitP()) |value| {
        int = int * 10 + value;
        digits += 1;
    }

    if (digits == 0) {
        return self.reset(start);
    }

    const attr = try self.mkAttr(start, self.pos);

    return try SExpr.Int(attr, int * sign);
}

pub fn floatP(self: *Parser) ParserError!?SExpr {
    const start = self.pos;

    var int: f64 = 0;
    var frac: f64 = 0;
    var exp: f64 = 0;

    const sign = try self.signP(f64) orelse return null;

    var digits: usize = 0;

    while (try self.digitP()) |value| {
        int = int * 10 + @as(f64, @floatFromInt(value));
        digits += 1;
    }

    if (try self.expectCharP('.')) {
        var fracDiv: f64 = 1;

        while (try self.digitP()) |value| {
            frac = frac * 10 + @as(f64, @floatFromInt(value));
            fracDiv *= 10;
            digits += 1;
        }

        frac /= fracDiv;

        if (digits > 0) {
            if (try self.expectAnyCharP(&[_]Char{ 'e', 'E' }) != null) {
                const expSign = try self.require(f64, try self.signP(f64));

                while (try self.digitP()) |value| {
                    exp = exp * 10 + @as(f64, @floatFromInt(value));
                    digits += 1;
                }

                exp *= expSign;
            }
        }
    } else {
        return self.reset(start);
    }

    if (digits == 0) {
        return self.reset(start);
    }

    const attr = try self.mkAttr(start, self.pos);

    return try SExpr.Float(attr, (int + frac) * sign * std.math.pow(f64, 10.0, exp));
}

pub fn symbolP(self: *Parser) ParserError!?SExpr {
    const start = self.pos;

    while (try self.peekChar()) |char| {
        switch (char) {
            inline '(',
            ')',
            ';',
            ',',
            '#',
            '\'',
            '`',
            '"',
            '\\',
            => break,
            else => if (TextUtils.isSpace(char) or TextUtils.isControl(char)) {
                break;
            },
        }

        try self.advance();
    }

    if (start.offset == self.pos.offset) {
        return null;
    }

    const sub = self.input[start.offset..self.pos.offset];

    if (std.mem.eql(u8, sub, ".")) {
        self.pos = start;
        self.peekCache = null;
        return null;
    }

    const attr = try self.mkAttr(start, self.pos);

    return try SExpr.Symbol(attr, sub);
}

pub fn escapeP(self: *Parser) ParserError!?Char {
    const start = self.pos;

    if (!try self.expectCharP('\\')) {
        return null;
    }

    if (try self.nextChar()) |char| ch: {
        const x: Char = switch (char) {
            '0' => '\x00',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '"' => '"',
            '\'' => '\'',
            'e' => if (try self.expectStringP("sc")) '\x1b' else break :ch,
            else => break :ch,
        };

        return x;
    }

    return self.reset(start);
}

pub fn charP(self: *Parser) ParserError!?SExpr {
    const start = self.pos;

    if (!try self.expectCharP('\'')) {
        return null;
    }

    const char = char: {
        if (try self.peekChar()) |char| {
            if (char == '\\') {
                break :char try self.requireP(Char, Parser.escapeP, .{});
            } else if (char != '\'' and !TextUtils.isControl(char)) {
                try self.advance();
                break :char char;
            } else {
                return SyntaxError.UnexpectedInput;
            }
        } else {
            return SyntaxError.UnexpectedEOF;
        }
    };

    if (!try self.expectCharP('\'')) {
        return self.reset(start);
    }

    const attr = try self.mkAttr(start, self.pos);

    return try SExpr.Char(attr, char);
}

pub fn stringP(self: *Parser) ParserError!?SExpr {
    const start = self.pos;

    if (!try self.expectCharP('"')) {
        return null;
    }

    const textBufferBase = self.textBuffer.items.len;
    defer self.textBuffer.shrinkRetainingCapacity(textBufferBase);

    var charBuffer = [1]u8{0} ** 4;

    while (try self.peekChar()) |char| {
        if (char == '"') {
            try self.advance();

            const attr = try self.mkAttr(start, self.pos);

            return try SExpr.String(attr, self.textBuffer.items[textBufferBase..]);
        }

        const i = encode: {
            if (char == '\\') {
                const char2 = try self.requireP(Char, Parser.escapeP, .{});
                break :encode try TextUtils.encode(char2, &charBuffer);
            } else if (!TextUtils.isControl(char)) {
                try self.advance();
                break :encode try TextUtils.encode(char, &charBuffer);
            } else {
                return SyntaxError.UnexpectedInput;
            }
        };

        try self.textBuffer.appendSlice(charBuffer[0..i]);
    }

    return SyntaxError.UnexpectedEOF;
}

test {
    const expectEqual = std.testing.expectEqual;
    const expectApproxEq = std.testing.expectApproxEqRel;
    const expectEqualSlices = std.testing.expectEqualSlices;
    const expectFmt = std.testing.expectFmt;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var ctx = try Context.initAllocator(arena.allocator());
    defer ctx.deinit();

    {
        var parser = try Parser.init(ctx);
        defer parser.deinit();

        try parser.setFileName("stdin");
        parser.setInput("  foobar\n 11 \n\t-23 +3       1.0 0.1 1.0e4 -1.0 -100.3e+5 -133.5e-5 .1 2. 0.e2 .4e4 llama \\n \\0 \\esc \"foo\\\"\\nbar\" (foo bar) 'quux '(foo bar) 'x' '\\0' `(foo `(bar ,',baz))", null);

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectEqualSlices(u8, "foobar", x.forceSymbol().toSlice());

            const attr = x.getAttr();
            try expectEqual(1, attr.range.?.start.?.line);
            try expectEqual(3, attr.range.?.start.?.column);
            try expectEqual(1, attr.range.?.end.?.line);
            try expectEqual(9, attr.range.?.end.?.column);
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectEqual(11, x.forceInt());

            const attr = x.getAttr();
            try expectEqual(2, attr.range.?.start.?.line);
            try expectEqual(2, attr.range.?.start.?.column);
            try expectEqual(2, attr.range.?.end.?.line);
            try expectEqual(4, attr.range.?.end.?.column);
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectEqual(-23, x.forceInt());

            const attr = x.getAttr();
            try expectEqual(3, attr.range.?.start.?.line);
            try expectEqual(2, attr.range.?.start.?.column);
            try expectEqual(3, attr.range.?.end.?.line);
            try expectEqual(5, attr.range.?.end.?.column);
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectEqual(3, x.forceInt());
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectApproxEq(1.0, x.forceFloat(), std.math.floatEps(f64));
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectApproxEq(0.1, x.forceFloat(), std.math.floatEps(f64));
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectApproxEq(1.0e4, x.forceFloat(), std.math.floatEps(f64));
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectApproxEq(-1.0, x.forceFloat(), std.math.floatEps(f64));
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectApproxEq(-100.3e+5, x.forceFloat(), std.math.floatEps(f64));
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectApproxEq(-133.5e-5, x.forceFloat(), std.math.floatEps(f64));
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectApproxEq(0.1, x.forceFloat(), std.math.floatEps(f64));
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectApproxEq(2.0, x.forceFloat(), std.math.floatEps(f64));
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectApproxEq(0.0e2, x.forceFloat(), std.math.floatEps(f64));
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectApproxEq(0.4e4, x.forceFloat(), std.math.floatEps(f64));
        }

        {
            const x = (try parser.scanP(bool, Parser.expectStringP, .{"llama"}));
            try expectEqual(true, x);
        }

        {
            const x = (try parser.scanP(?Char, Parser.escapeP, .{})) orelse unreachable;
            try expectEqual('\n', x);
        }

        {
            const x = (try parser.scanP(?Char, Parser.escapeP, .{})) orelse unreachable;
            try expectEqual('\x00', x);
        }

        {
            const x = (try parser.scanP(?Char, Parser.escapeP, .{})) orelse unreachable;
            try expectEqual('\x1b', x);
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectEqualSlices(u8, "foo\"\nbar", x.forceString().toSlice());
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectFmt("(foo . (bar . ()))", "{.}", .{x});
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectFmt("(quote . (quux . ()))", "{.}", .{x});
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectFmt("(quote . ((foo . (bar . ())) . ()))", "{.}", .{x});
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectFmt("'x'", "{}", .{x});
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectFmt("'\\0'", "{}", .{x});
        }

        {
            const x = (try parser.scanP(?SExpr, Parser.sexprP, .{})) orelse unreachable;

            try expectFmt("(quasiquote . ((foo . ((quasiquote . ((bar . ((unquote . ((quote . ((unquote . (baz . ())) . ())) . ())) . ())) . ())) . ())) . ()))", "{.}", .{x});
        }
    }
}
