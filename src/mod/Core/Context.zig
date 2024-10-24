const std = @import("std");
const Allocator = std.mem.Allocator;

const TextUtils = @import("ZigUtils").Text;
const MiscUtils = @import("ZigUtils").Misc;

const Core = @import("root.zig");
const Source = Core.Source;
const SExpr = Core.SExpr;

const gc = @import("bdwgc");

id: Id,
allocator: Allocator,
isGc: bool,
symbolInterner: SymbolInterner,
attr: *const Source.Attr,
nil: SExpr,
fresh: u64 = 1,

const Context = @This();

pub const Id = enum(u64) {
    _,

    pub fn compare(a: Id, b: Id) MiscUtils.Ordering {
        return MiscUtils.compare(@intFromEnum(a), @intFromEnum(b));
    }
};

pub const Error = Allocator.Error;

pub const SymbolStorage = [SYMBOL_MAX_LEN]u8;
pub const SymbolInternerContext = struct {
    pub fn hash(_: @This(), key: SymbolStorage) u64 {
        return std.hash.Fnv1a_64.hash(&key);
    }

    pub fn eql(_: @This(), a: SymbolStorage, b: SymbolStorage) bool {
        return MiscUtils.equal(a, b);
    }
};
pub const SymbolInterner = std.HashMap(SymbolStorage, []const u8, SymbolInternerContext, 75);

pub const SYMBOL_MAX_LEN = 256;

var COUNTER = std.atomic.Value(u64).init(1);

pub fn initGc() Error!*Context {
    return init(gc.allocator(), true);
}

pub fn initAllocator(allocator: Allocator) Error!*Context {
    return init(allocator, false);
}

inline fn init(allocator: Allocator, isGc: bool) Error!*Context {
    const index = COUNTER.fetchAdd(1, .monotonic);
    const id: Id = @enumFromInt(index);

    var symbolInterner = SymbolInterner.init(allocator);
    errdefer symbolInterner.deinit();

    const ptr = try allocator.create(Context);
    errdefer allocator.destroy(ptr);

    ptr.* = Context{
        .id = id,
        .allocator = allocator,
        .isGc = isGc,
        .symbolInterner = symbolInterner,
        .attr = undefined,
        .nil = undefined,
    };

    const attr = try ptr.bindAttr("Context xxxxxxxxxxxxxxxxxx"[0 .. 8 + TextUtils.numDigits(index, 10)], null);

    const fp = @constCast(attr.filename);
    _ = std.fmt.bufPrint(fp, "Context {d}", .{index}) catch unreachable;

    ptr.attr = attr;

    ptr.nil = try SExpr.Nil(attr);

    return ptr;
}

pub fn deinit(self: *Context) void {
    self.symbolInterner.deinit();
    self.allocator.destroy(self);
}

pub fn new(self: *Context, value: anytype) Error!*@TypeOf(value) {
    const res = try self.allocator.create(@TypeOf(value));

    res.* = value;

    return res;
}

pub fn newBuffer(self: *Context, comptime T: type, value: []const T) Error![]T {
    const res = try self.allocator.alloc(T, value.len);

    std.mem.copyForwards(T, res, value);

    return res;
}

pub fn setFinalizer(self: *Context, ptr: anytype, comptime finalizer: anytype) void {
    if (!self.isGc) return;

    const Fx = struct {
        fn finalizerWrapper(uptr: ?*anyopaque, _: ?*anyopaque) callconv(.C) void {
            finalizer(@ptrCast(@constCast(@alignCast(uptr))));
        }
    };

    gc.registerFinalizer(@ptrCast(@constCast(@alignCast(ptr))), Fx.finalizerWrapper, null, null, null);
}

pub fn collectGarbage(self: *Context) void {
    if (self.isGc) gc.collect();
}

pub fn genId(self: *Context) u64 {
    const id = self.fresh;
    self.fresh += 1;
    return id;
}

pub fn genSymbol(self: *Context) Error![]const u8 {
    var symbol = std.mem.zeroes([SYMBOL_MAX_LEN]u8);
    const id = self.genId();
    const base = std.fmt.bufPrint(&symbol, "#{}", .{id}) catch unreachable;
    return self.bindSymbol(base);
}

pub fn bindSymbol(self: *Context, value: []const u8) Error![]const u8 {
    std.debug.assert(value.len > 0 and value.len <= SYMBOL_MAX_LEN);

    var symbol = std.mem.zeroes([SYMBOL_MAX_LEN]u8);
    std.mem.copyForwards(u8, symbol[0..value.len], value);

    const interned = self.symbolInterner.get(symbol);

    if (interned) |rp| {
        return rp;
    }

    const res = try self.newBuffer(u8, value);

    try self.symbolInterner.put(symbol, res);

    return res;
}

pub fn resetSymbolInterner(self: *Context) void {
    self.symbolInterner.clearRetainingCapacity();
}

pub fn bindAttr(self: *Context, fileName: []const u8, range: ?Source.Range) Error!*Source.Attr {
    const fileNameAl = try self.newBuffer(u8, fileName);

    return try self.bindAttrExistingFile(fileNameAl, range);
}

pub fn bindAttrExistingFile(self: *Context, filename: []const u8, range: ?Source.Range) Error!*Source.Attr {
    return try self.new(Source.Attr{
        .context = self,
        .filename = filename,
        .range = range,
    });
}

pub fn compare(a: Context, b: Context) MiscUtils.Ordering {
    return MiscUtils.compare(a.id, b.id);
}

test {
    const expect = std.testing.expect;
    const expectFmt = std.testing.expectFmt;

    {
        var ctx = try Context.initGc();
        defer ctx.deinit();

        const stdinAttr = try ctx.bindAttr("stdin", null);

        const S1 = try SExpr.String(ctx.attr, "stdio");

        try expectFmt("\"stdio\" @ [Context 1]", "{attr}", .{S1});

        const B1 = try SExpr.Symbol(stdinAttr, "bingus");

        const B2 = try SExpr.Symbol(stdinAttr, "bingus");

        try expect(MiscUtils.equal(B1.forceSymbol(), B2.forceSymbol()));

        const I1 = try SExpr.Int(stdinAttr, 1);

        const I2 = try SExpr.Int(stdinAttr, 2);

        const N1 = try SExpr.Nil(stdinAttr);

        const C1 = try SExpr.Cons(stdinAttr, I2, N1);

        const C2 = try SExpr.Cons(stdinAttr, I1, C1);

        const expectedConsString = "(1 2) @ [stdin]";
        try expectFmt(expectedConsString, "{attr}", .{C2});

        try expectFmt(expectedConsString, "{attr}", .{C2});
        try expectFmt("bingus @ [stdin]", "{attr}", .{B1});
    }
}

test {
    const ctx = try Context.initGc();
    const at = ctx.attr;

    try std.testing.expectEqual(0, gc.collectLittle());
    try std.testing.expectEqual(0x00_00_FF_FF_FF_FF_FF_FF, gc.getPointerMask());

    const baseSize = gc.getHeapSize();

    const retained = try SExpr.String(at, "I am retained and stuff");

    var sizeIncr: usize = 0;
    var lastSize = baseSize;

    for (0..10_000_000) |i| {
        const spam = try SExpr.String(at, "I am spammy spam spamington");

        _ = spam;

        if (i % 100_000 == 0) {
            const newSize = gc.getHeapSize();
            if (newSize > lastSize) {
                sizeIncr += 1;
                lastSize = newSize;
            }
        }
    }

    try std.testing.expect(sizeIncr == 1);

    try std.testing.expectEqualSlices(u8, "I am retained and stuff", retained.forceString().toSlice());
}
