const std = @import("std");
const MiscUtils = @import("Utils").Misc;
const TextUtils = @import("Utils").Text;

const Rml = @import("root.zig");
const Ordering = Rml.Ordering;
const Error = Rml.Error;
const OOM = Rml.OOM;
const log = Rml.log;
const Writer = Rml.Writer;
const Object = Rml.Object;
const String = Rml.String;
const Symbol = Rml.Symbol;
const Obj = Rml.Obj;
const ptr = Rml.ptr;
const getObj = Rml.getObj;
const getTypeId = Rml.getTypeId;
const getRml = Rml.getRml;
const castObj = Rml.castObj;
const forceObj = Rml.forceObj;
const PropertySet = Rml.object.PropertySet;
const Origin = Rml.source.Origin;
const Range = Rml.source.Range;
const Pos = Rml.source.Pos;
const str = Rml.str;
const char = Rml.char;


pub const parsing = std.log.scoped(.parsing);


pub const Parser = Obj(Memory);

pub const SyntaxError = error{ UnexpectedInput, UnexpectedEOF } || TextUtils.Error;

pub const Memory = struct {
    input: String,
    filename: str,
    pos: Pos,
    posOffset: Pos,
    peekCache: ?char,

    pub fn onInit(self: ptr(Memory), filename: str, input: String) OOM! void {
        parsing.debug("creating Parser{x}", .{@intFromPtr(self)});

        self.input = input;
        self.filename = filename;
        self.pos = Pos { .line = 0, .column = 0, .offset = 0 };
        self.posOffset = Pos { .line = 1, .column = 1, .offset = 0 };
        self.peekCache = null;
    }

    pub fn onCompare(a: ptr(Memory), other: Object) Ordering {
        var ord = Rml.compare(getTypeId(a), other.getHeader().type_id);

        if (ord == .Equal) {
            const b = forceObj(Memory, other);
            defer b.deinit();

            ord = Rml.compare(@intFromPtr(a), @intFromPtr(b.data));
        }

        return ord;
    }

    pub fn onFormat(self: ptr(Memory), writer: Writer) Error! void {
        return writer.data.print("Parser{x}", .{@intFromPtr(self)});
    }

    pub fn onDeinit(self: ptr(Memory)) void {
        self.input.deinit();
    }



    pub fn setOffset(self: ptr(Memory), offset: Pos) void {
        self.posOffset = offset;
    }

    pub fn clearOffset(self: ptr(Memory)) void {
        self.posOffset = Pos { .line = 1, .column = 1, .offset = 0 };
    }

    pub fn getOrigin(self: ptr(Memory), start: ?Pos, end: ?Pos) Origin {
        const a = if (start) |x| Pos{.line = x.line + self.posOffset.line, .column = x.column + self.posOffset.column, .offset = x.offset + self.posOffset.offset}
        else null;

        const b = if (end) |x| Pos{.line = x.line + self.posOffset.line, .column = x.column + self.posOffset.column, .offset = x.offset + self.posOffset.offset}
        else null;

        return Origin {
            .filename = self.filename,
            .range = Range.init(a, b),
        };
    }

    pub fn parseDocument(self: ptr(Memory)) Error! Rml.Block {
        parsing.debug("parseDocument", .{});
        errdefer parsing.debug("parseDocument failed", .{});

        const result = try self.parseBlockTail(self.pos, .doc);

        parsing.debug("result: {}", .{result});

        return result;
    }

    pub fn parseObject(self: ptr(Memory)) Error! ?Object {
        parsing.debug("parseObject {?u}", .{self.peekChar() catch null});
        errdefer parsing.debug("parseObject failed", .{});

        const result = try self.parseAtom()
        orelse if (try self.parseBlock()) |x| erase: { defer x.deinit(); break :erase x.typeErase(); } else null;

        parsing.debug("parseObject result: {?}", .{result});

        return result;
    }

    pub fn parseAtom(self: ptr(Memory)) Error! ?Object {
        parsing.debug("parseAtom", .{});
        errdefer parsing.debug("parseAtom failed", .{});

        const result = (if (try self.parseInt()) |x| erase: { defer x.deinit(); break :erase x.typeErase(); } else null)
        orelse (if (try self.parseFloat()) |x| erase: { defer x.deinit(); break :erase x.typeErase(); } else null)
        orelse (if (try self.parseChar()) |x| erase: { defer x.deinit(); break :erase x.typeErase(); } else null)
        orelse (if (try self.parseString()) |x| erase: { defer x.deinit(); break :erase x.typeErase(); } else null)
        orelse (if (try self.parseSymbol()) |x| erase: { defer x.deinit(); break :erase x.typeErase(); } else null);

        parsing.debug("parseAtom result: {?}", .{result});

        return result;
    }

    pub fn parseBlock(self: ptr(Memory)) Error! ?Rml.Block {
        parsing.debug("parseBlock", .{});
        errdefer parsing.debug("parseBlock failed", .{});

        const start = self.pos;

        const blockKind = try self.parseAnyBlockOpening() orelse return null;

        const result = try self.parseBlockTail(start, blockKind);

        parsing.debug("parseBlock result: {?}", .{result});

        return result;
    }

    fn parseBlockTail(self: ptr(Memory), start: Pos, blockKind: Rml.block.BlockKind) Error! Rml.Block {
        const rml = getRml(self);

        var array: Rml.array.ObjectMemoryUnmanaged = .{};
        errdefer array.deinit(rml);

        var properties = try self.scan() orelse PropertySet{};
        defer properties.deinit(rml);

        var tailDeinit = true;
        var tailProperties: Rml.object.PropertySet = .{};
        errdefer if (tailDeinit) tailProperties.deinit(rml);

        while (true) {
            if (self.isEof() and blockKind != .doc) {
                return SyntaxError.UnexpectedEOF;
            }

            if (try self.parseBlockClosing(blockKind)) {
                tailProperties = try properties.clone(rml);
                break;
            }

            const obj = try self.parseObject() orelse return self.failed();
            try obj.getHeader().properties.copyFrom(rml, &properties);

            try array.append(rml, obj);

            if (try self.scan()) |props| {
                properties.deinit(rml);
                properties = props;
            } else {
                if (try self.parseBlockClosing(blockKind)) {
                    tailProperties = try properties.clone(rml);
                    break;
                } else {
                    return self.failed();
                }
            }
        }

        const origin = self.getOrigin(start, self.pos);

        const block: Rml.Block = try .wrap(rml, origin, .{
            .block_kind = blockKind,
            .array = array
        });

        if (tailProperties.length() > 0) {
            const sym: Symbol = try .init(rml, origin, .{"tail"});
            defer sym.deinit();

            const map: Rml.map.ObjectMap = try .wrap(rml, origin, .{ .unmanaged = tailProperties });
            defer map.deinit();
            tailDeinit = false;

            try block.getHeader().properties.set(rml, sym.typeErase(), map.typeErase());
        }

        return block;
    }

    pub fn parseAnyBlockOpening(self: ptr(Memory)) Error! ?Rml.block.BlockKind {
        const start = self.pos;

        inline for (comptime std.meta.fieldNames(Rml.block.BlockKind)) |blockKindName| {
            const blockKind = @field(Rml.block.BlockKind, blockKindName);
            const openStr = comptime blockKind.toOpenStr();

            if (comptime std.mem.eql(u8, openStr, "")) continue;

            if (try self.expectSlice(openStr)) {
                return blockKind;
            }
        }

        self.reset(start);
        return null;
    }

    pub fn parseBlockClosing(self: ptr(Memory), kind: Rml.block.BlockKind) Error! bool {
        const closeStr = kind.toCloseStr();

        if (std.mem.eql(u8, closeStr, "")) {
            parsing.debug("checking for eof", .{});
            const is = self.isEof();
            parsing.debug("eof: {}", .{is});
            return is;
        } else {
            parsing.debug("checking for {s}", .{closeStr});
            const is = try self.expectSlice(closeStr);
            parsing.debug("{s}: {}", .{closeStr, is});
            return is;
        }
    }

    pub fn parseInt(self: ptr(Memory)) Error! ?Rml.Int {
        parsing.debug("parseInt {?u}", .{self.peekChar() catch null});
        errdefer parsing.debug("parseInt failed", .{});

        const rml = getRml(self);
        const start = self.pos;

        var int: Rml.int = 0;

        const sign = try self.expectOptionalSign(Rml.int) orelse {
            parsing.debug("parseInt stop: no input", .{});
            return null;
        };

        var digits: usize = 0;

        while (try self.expectDecimalDigit()) |value| {
            int = int * 10 + value;
            digits += 1;
        }

        if (digits == 0) {
            parsing.debug("parseInt reset: no digits", .{});
            self.reset(start);
            return null;
        }

        const result: Rml.Int = try .wrap(rml, self.getOrigin(start, self.pos), int * sign);

        parsing.debug("parseInt result: {}", .{result});

        return result;
    }

    pub fn parseFloat(self: ptr(Memory)) Error! ?Rml.Float {
        parsing.debug("parseFloat {?u}", .{self.peekChar() catch null});
        errdefer parsing.debug("parseFloat failed", .{});

        const rml = getRml(self);
        const start = self.pos;

        var int: f64 = 0;
        var frac: f64 = 0;
        var exp: f64 = 0;

        const sign = try self.expectOptionalSign(f64) orelse {
            parsing.debug("parseFloat stop: no input", .{});
            return null;
        };

        var digits: usize = 0;

        while (try self.expectDecimalDigit()) |value| {
            int = int * 10 + @as(f64, @floatFromInt(value));
            digits += 1;
        }

        if (try self.expectChar('.')) {
            var fracDiv: f64 = 1;

            while (try self.expectDecimalDigit()) |value| {
                frac = frac * 10 + @as(f64, @floatFromInt(value));
                fracDiv *= 10;
                digits += 1;
            }

            frac /= fracDiv;

            if (digits > 0) {
                if (try self.expectAnyChar(&.{ 'e', 'E' }) != null) {
                    const expSign = try self.require(f64, expectOptionalSign, .{f64});

                    while (try self.expectDecimalDigit()) |value| {
                        exp = exp * 10 + @as(f64, @floatFromInt(value));
                        digits += 1;
                    }

                    exp *= expSign;
                }
            }
        } else {
            parsing.debug("parseFloat reset: no frac", .{});
            self.reset(start);
            return null;
        }

        if (digits == 0) {
            parsing.debug("parseFloat reset: no digits", .{});
            self.reset(start);
            return null;
        }

        const result = try Rml.Float.wrap(rml, self.getOrigin(start, self.pos), (int + frac) * sign * std.math.pow(f64, 10.0, exp));

        parsing.debug("parseFloat result: {}", .{result});

        return result;
    }

    pub fn parseChar(self: ptr(Memory)) Error! ?Rml.Char {
        parsing.debug("parseChar {?u}", .{self.peekChar() catch null});
        errdefer parsing.debug("parseChar failed", .{});

        const rml = getRml(self);
        const start = self.pos;

        if (!try self.expectChar('\'')) {
            parsing.debug("parseChar stop: expected '\''", .{});
            return null;
        }

        const ch = ch: {
            if (try self.peekChar()) |ch| {
                if (ch == '\\') {
                    break :ch try self.require(char, expectEscape, .{});
                } else if (ch != '\'' and !TextUtils.isControl(ch)) {
                    try self.advChar();
                    break :ch ch;
                } else {
                    return SyntaxError.UnexpectedInput;
                }
            } else {
                return SyntaxError.UnexpectedEOF;
            }
        };

        if (!try self.expectChar('\'')) {
            parsing.debug("parseChar reset: expected '\''", .{});
            self.reset(start);
            return null;
        }

        const result: Rml.Char = try .wrap(rml, self.getOrigin(start, self.pos), ch);

        parsing.debug("parseChar result: {}", .{result});

        return result;
    }

    pub fn parseString(self: ptr(Memory)) Error! ?String {
        parsing.debug("parseString {?u}", .{self.peekChar() catch null});
        errdefer parsing.debug("parseString failed", .{});

        const rml = getRml(self);
        const start = self.pos;

        if (!try self.expectChar('"')) {
            parsing.debug("parseString stop: expected '\"'", .{});
            return null;
        }

        var textBuffer: Rml.string.MemoryUnmanaged = .{};
        errdefer textBuffer.deinit(rml);

        while (try self.peekChar()) |ch| {
            if (ch == '"') {
                try self.advChar();

                parsing.debug("parseString result: {s}", .{textBuffer.text()});

                return try String.wrap(rml, self.getOrigin(start, self.pos), .{ .unmanaged = textBuffer });
            }

            const i =
                if (ch == '\\') try self.require(char, expectEscape, .{})
                else if (!TextUtils.isControl(ch)) try self.nextChar() orelse return SyntaxError.UnexpectedEOF
                else return SyntaxError.UnexpectedInput;

            try textBuffer.append(rml, i);
        }

        return SyntaxError.UnexpectedEOF;
    }


    pub fn parseSymbol(self: ptr(Memory)) Error! ?Symbol {
        const rml = getRml(self);

        const start = self.pos;

        while (try self.peekChar()) |ch| {
            switch (ch) {
                inline '(', ')', ';', ',', '#', '\'', '`', '"', '\\', => break,

                else => if (TextUtils.isSpace(ch) or TextUtils.isControl(ch)) break,
            }

            try self.advChar();
        }

        if (start.offset == self.pos.offset) {
            parsing.debug("parseSymbol reset: nothing recognized", .{});
            return null;
        }

        const result: Symbol = try .init(rml, self.getOrigin(start, self.pos), .{self.input.data.text()[start.offset..self.pos.offset]});

        parsing.debug("parseSymbol result: {s}", .{result});

        return result;
    }

    pub fn expectChar(self: ptr(Memory), ch: char) Error! bool {
        if (try self.peekChar() == ch) {
            try self.advChar();
            return true;
        }

        return false;
    }

    pub fn expectAnyChar(self: ptr(Memory), chars: []const char) Error! ?char {
        if (try self.peekChar()) |ch| {
            for (chars) |c| {
                if (ch == c) {
                    try self.advChar();
                    return c;
                }
            }
        }

        return null;
    }

    pub fn expectSlice(self: ptr(Memory), slice: []const u8) Error! bool {
        for (slice) |ch| {
            if (try self.peekChar() != ch) {
                return false;
            }

            try self.advChar();
        }

        return true;
    }

    pub fn expectEscape(self: ptr(Memory)) Error! ?char {
        const start = self.pos;

        if (!try self.expectChar('\\')) {
            return null;
        }

        if (try self.nextChar()) |ch| ch: {
            const x: char = switch (ch) {
                '0' => '\x00',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '"' => '"',
                '\'' => '\'',
                'e' => if (try self.expectSlice("sc")) '\x1b' else break :ch,
                else => break :ch,
            };

            return x;
        }

        self.reset(start);
        return null;
    }

    pub fn expectDecimalDigit(self: ptr(Memory)) Error! ?u8 {
        if (try self.peekChar()) |ch| {
            if (TextUtils.decimalValue(ch)) |value| {
                try self.advChar();
                return value;
            }
        }

        return null;
    }


    pub fn expectOptionalSign(self: ptr(Memory), comptime T: type) Error! ?T {
        if (try self.peekChar()) |ch| {
            if (ch == '-') {
                try self.advChar();
                return -1;
            } else if (ch == '+') {
                try self.advChar();
            }

            return 1;
        }

        return null;
    }

    pub fn scan(self: ptr(Memory)) Error! ?PropertySet {
        const rml = getRml(self);
        var propertyState: union(enum) { none, start, inside: struct { []const u8, u32 } } = .none;

        var propertySet: PropertySet = .{};

        var start: Pos = undefined;

        while (try self.peekChar()) |ch| {
            switch (propertyState) {
                .none => if (ch == ';') {
                    propertyState = .start;
                    start = self.pos;
                    try self.advChar();
                } else if (TextUtils.isSpace(ch)) {
                    try self.advChar();
                } else {
                    break;
                },
                .start => if (ch == '!') {
                    propertyState = .{ .inside = .{ "documentation", self.pos.offset + 1 } };
                    try self.advChar();
                } else if (ch == '\n') {
                    propertyState = .none;
                    try self.advChar();
                } else {
                    propertyState = .{ .inside = .{ "comment", self.pos.offset } };
                    try self.advChar();
                },
                .inside => |state| {
                    if (ch == '\n') {
                        propertyState = .none;

                        const origin = self.getOrigin(start, self.pos);

                        const sym: Symbol = try .init(rml, origin, .{state[0]});
                        defer sym.deinit();

                        const string: String = try .init(rml, origin, .{self.input.data.text()[state[1]..self.pos.offset]});
                        defer string.deinit();

                        try propertySet.set(rml, sym.typeErase(), string.typeErase());
                    }

                    try self.advChar();
                },
            }
        }

        if (start.offset == self.pos.offset) return null;

        return propertySet;
    }

    pub fn reset(self: ptr(Memory), pos: Pos) void {
        self.pos = pos;
        self.peekCache = null;
    }

    pub fn failed(self: ptr(Memory)) Error {
        if (self.isEof()) {
            return SyntaxError.UnexpectedEOF;
        } else {
            return SyntaxError.UnexpectedInput;
        }
    }

    pub fn require(self: ptr(Memory), comptime T: type, callback: anytype, args: anytype) !T {
        return try @call(.auto, callback, .{self} ++ args) orelse {
            return self.failed();
        };
    }

    pub fn isEof(self: ptr(Memory)) bool {
        return self.pos.offset >= self.input.data.text().len;
    }

    pub fn peekChar(self: ptr(Memory)) Error! ?char {
        if (self.isEof()) {
            return null;
        }

        if (self.peekCache) |ch| {
            return ch;
        } else {
            const len = try TextUtils.sequenceLengthByte(self.input.data.text()[self.pos.offset]);
            const slice = self.input.data.text()[self.pos.offset .. self.pos.offset + len];

            const ch = try TextUtils.decode(slice);
            self.peekCache = ch;

            return ch;
        }
    }

    pub fn nextChar(self: ptr(Memory)) Error! ?char {
        if (try self.peekChar()) |ch| {
            switch (ch) {
                '\n' => {
                    self.pos.line += 1;
                    self.pos.column = 0;
                    self.pos.offset += 1;
                },

                else => {
                    self.pos.column += 1;
                    self.pos.offset += try TextUtils.sequenceLength(ch);
                },
            }

            self.peekCache = null;

            return ch;
        } else {
            return null;
        }
    }

    pub fn advChar(self: ptr(Memory)) Error! void {
        _ = try self.nextChar();
    }
};
