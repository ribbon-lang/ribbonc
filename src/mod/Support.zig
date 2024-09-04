const std = @import("std");

const TypeUtils = @import("ZigTypeUtils");
const TextUtils = @import("ZigTextUtils");

const Extern = @import("Extern");

pub const Unit = extern struct {};

const Support = @This();

pub const IOError = std.fs.File.WriteError || std.fs.File.ReadError || std.fs.File.OpenError || error{
    StreamTooLong,
};

pub fn isIOError(err: anyerror) bool {
    return TypeUtils.isInErrorSet(IOError, err);
}

pub fn asIOError(err: anyerror) ?IOError {
    return TypeUtils.narrowErrorSet(IOError, err);
}

pub inline fn todo(comptime T: type, _: anytype) T {
    @panic("not yet implemented");
}

pub fn BufferIterator(comptime T: type) type {
    return struct {
        buffer: []const T,
        index: usize,

        pub fn init(buffer: []const T) BufferIterator(T) {
            return BufferIterator(T){
                .buffer = buffer,
                .index = 0,
            };
        }

        pub fn next(self: *BufferIterator(T)) ?T {
            if (self.index >= self.buffer.len) {
                return null;
            }

            const elem = self.buffer[self.index];
            self.index += 1;
            return elem;
        }
    };
}

pub fn captureStackTrace(comptime N: usize, traceAddr: ?usize, allocator: std.mem.Allocator) ![*:0]u8 {
    var buf = std.ArrayList(u8).init(allocator);

    var addresses: [N]usize = [1]usize{0} ** N;
    var trace = std.builtin.StackTrace{
        .index = 0,
        .instruction_addresses = &addresses,
    };

    std.debug.captureStackTrace(traceAddr, &trace);

    try buf.writer().print("{}", .{trace});

    try TextUtils.indentStr(&buf, "    ");

    return try buf.toOwnedSliceSentinel(0);
}

pub fn stackAllocator(buf: []u8) std.mem.Allocator {
    const padding = Support.alignmentDelta(@intFromPtr(buf.ptr), @alignOf(std.heap.FixedBufferAllocator));
    const allocRegion = buf[padding .. padding + @sizeOf(std.heap.FixedBufferAllocator)];
    const ptr: *std.heap.FixedBufferAllocator = @ptrCast(@alignCast(allocRegion.ptr));
    const memRegion = buf[padding + @sizeOf(std.heap.FixedBufferAllocator) ..];
    ptr.* = std.heap.FixedBufferAllocator.init(memRegion);
    return ptr.allocator();
}

pub fn dumpStackTrace(comptime N: usize, traceAddr: ?usize) void {
    var buf = [1]u8{0} ** (1024 * N + @sizeOf(std.heap.FixedBufferAllocator));
    const alloc = stackAllocator(&buf);
    const trace = Support.captureStackTrace(N, traceAddr, alloc) catch unreachable;
    std.debug.print("{s}", .{trace});
}

pub fn default(comptime T: type) T {
    const info = @typeInfo(T);

    switch (info) {
        .Bool => {
            return false;
        },
        .Int, .Float, .ComptimeInt, .ComptimeFloat => {
            return 0;
        },
        .Optional => {
            return null;
        },
        else => {
            if (comptime std.meta.hasFn(T, "default")) {
                return T.default();
            } else {
                @compileError("Cannot get default value for type `" ++ @typeName(T) ++ "`");
            }
        },
    }
}

pub inline fn alignmentDelta(baseAddress: anytype, alignment: @TypeOf(baseAddress)) @TypeOf(baseAddress) {
    return (alignment - (baseAddress % alignment)) % alignment;
}

pub inline fn bitOrBool(a: bool, b: bool) bool {
    return @as(u1, @intFromBool(a)) | @as(u1, @intFromBool(b)) == 1;
}

pub inline fn bitAndBool(a: bool, b: bool) bool {
    return @as(u1, @intFromBool(a)) & @as(u1, @intFromBool(b)) == 1;
}

pub inline fn sliceCast(comptime T: type, comptime U: type, buffer: []U) []T {
    const ucount = buffer.len * @sizeOf(U);
    const tcount = ucount / @sizeOf(T);
    const ptr = @intFromPtr(buffer.ptr);
    return @as([*]T, @ptrFromInt(ptr))[0..tcount];
}

pub inline fn sliceCastConst(comptime T: type, comptime U: type, buffer: []const U) []const T {
    const ucount = buffer.len * @sizeOf(U);
    const tcount = ucount / @sizeOf(T);
    const ptr = @intFromPtr(buffer.ptr);
    return @as([*]const T, @ptrFromInt(ptr))[0..tcount];
}

pub inline fn makeSlice(comptime T: type, ptr: [*]T, len: usize) []T {
    return ptr[0..len];
}

pub inline fn makeSliceConst(comptime T: type, ptr: [*]const T, len: usize) []const T {
    return ptr[0..len];
}

pub inline fn tryCreateObj(al: std.mem.Allocator, comptime T: type) !*T {
    const obj = try al.create(T);
    obj.* = try T.init(al);
    return obj;
}

pub inline fn createObj(al: std.mem.Allocator, comptime T: type) !*T {
    const obj = try al.create(T);
    obj.* = T.init(al);
    return obj;
}

pub inline fn destroyObj(obj: anytype) void {
    const al = obj.allocator;
    obj.deinit();
    al.destroy(obj);
}

pub inline fn hashRawWith(comptime T: type, hasher: anytype, value: *const T) void {
    return hasher.update(rawBytes(T, value));
}

pub inline fn rawBytes(comptime T: type, value: *const T) []const u8 {
    return @as([*]const u8, @ptrCast(value))[0..@sizeOf(T)];
}

pub fn find(comptime T: type, buf: []const T, value: *const T) ?usize {
    const finder = Finder(T);
    finder.findComp = value;
    defer finder.findComp = null;
    return findWith(T, buf, finder.eqlFind);
}

fn Finder(comptime T: type) type {
    return struct {
        threadlocal var findComp: ?*const T = null;
        fn eqlFind(item: *const T) bool {
            return equal(item, findComp.?);
        }
    };
}

pub fn findWith(comptime T: type, buf: []const T, pred: fn (*const T) bool) ?usize {
    for (0..buf.len) |i| {
        if (pred(&buf[i])) {
            return i;
        }
    }

    return null;
}

pub fn externHash(a: anytype) u32 {
    var hasher = Extern.Hasher.initFnv1a32();
    hashWith(&hasher, a);
    return hasher.final();
}

pub fn fnv1a_32(a: anytype) u32 {
    var hasher = std.hash.Fnv1a_32.init();
    hashWith(&hasher, a);
    return hasher.final();
}

pub fn fnv1a_64(a: anytype) u64 {
    var hasher = std.hash.Fnv1a_64.init();
    hashWith(&hasher, a);
    return hasher.final();
}

pub fn fnv1a_128(a: anytype) u128 {
    var hasher = std.hash.Fnv1a_128.init();
    hashWith(&hasher, a);
    return hasher.final();
}

pub fn hashWith(hasher: anytype, a: anytype) void {
    const T = @TypeOf(a);
    const info = @typeInfo(T);

    switch (info) {
        .bool, .int, .float, .comptime_int, .comptime_float => {
            return hashRawWith(T, hasher, &a);
        },
        // .ComptimeInt => {
        //     return hashRawWith(u128, hasher, &a);
        // },
        // .ComptimeFloat => {
        //     return hashRawWith(f64, hasher, &a);
        // },
        .optional => {
            if (a) |ax| {
                return hashWith(hasher, ax);
            } else {
                return hasher.update("\x00N\x00U\x00L\x00L\x00");
            }
        },
        .pointer => |x| {
            switch (x.size) {
                .One, .C => {
                    if (x.child == anyopaque) {
                        @compileError("Cannot hash opaque pointers");
                    } else {
                        return hashWith(hasher, a.*);
                    }
                },
                .Slice => {
                    if (x.child == u8) {
                        return hasher.update(a);
                    }

                    for (a) |xa| {
                        hashWith(hasher, xa);
                    }
                },
                else => {
                    @compileError("Cannot do hash for type `" ++ @typeName(T) ++ "`, no length for pointer");
                },
            }
        },
        .array => |x| {
            if (x.child == u8) {
                return hasher.update(&a);
            }

            for (a) |xa| {
                hashWith(hasher, xa);
            }
        },
        else => {
            if (comptime std.meta.hasFn(T, "hashWith")) {
                return T.hashWith(a, hasher);
            } else {
                @compileError("Cannot do hash for type `" ++ @typeName(T) ++ "`");
            }
        },
    }
}

pub const Ordering = enum(u8) {
    Less,
    Equal,
    Greater,
};

pub fn compare(a: anytype, b: @TypeOf(a)) Ordering {
    const T = @TypeOf(a);
    const info = @typeInfo(T);

    switch (info) {
        .bool => {
            if (a == b) {
                return Ordering.Equal;
            } else if (a) {
                return Ordering.Greater;
            } else {
                return Ordering.Less;
            }
        },
        .int, .float, .comptime_int, .comptime_float => {
            if (a < b) {
                return Ordering.Less;
            } else if (a > b) {
                return Ordering.Greater;
            } else {
                return Ordering.Equal;
            }
        },
        .optional => {
            if (a == null) {
                if (b == null) {
                    return Ordering.Equal;
                } else {
                    return Ordering.Less;
                }
            } else if (b == null) {
                return Ordering.Greater;
            } else {
                return compare(a.?, b.?);
            }
        },
        .pointer => |x| {
            switch (x.size) {
                .One, .C => {
                    if (@intFromPtr(a) == @intFromPtr(b)) {
                        return Ordering.Equal;
                    } else if (x.child == anyopaque) {
                        return compare(@intFromPtr(a), @intFromPtr(b));
                    } else {
                        return compare(a.*, b.*);
                    }
                },
                .Slice => {
                    if (@intFromPtr(a.ptr) == @intFromPtr(b.ptr)) {
                        return Ordering.Equal;
                    } else if (a.len < b.len) {
                        return Ordering.Less;
                    } else if (a.len > b.len) {
                        return Ordering.Greater;
                    } else {
                        for (0..a.len) |i| {
                            const result = compare(a[i], b[i]);
                            if (result != Ordering.Equal) {
                                return result;
                            }
                        }
                        return Ordering.Equal;
                    }
                },
                else => {
                    @compileError("Cannot do compare for type `" ++ @typeName(T) ++ "`, no length for pointer");
                },
            }
        },
        .array => |x| {
            for (0..x.len) |i| {
                const result = compare(a[i], b[i]);
                if (result != Ordering.Equal) {
                    return result;
                }
            }
            return Ordering.Equal;
        },
        else => {
            if (comptime std.meta.hasFn(T, "compare")) {
                return T.compare(a, b);
            } else {
                @compileError("Cannot do compare for type `" ++ @typeName(T) ++ "`");
            }
        },
    }
}

pub fn compareAddress(a: anytype, b: @TypeOf(a)) Ordering {
    const T = @TypeOf(a);
    const info = @typeInfo(T);

    switch (info) {
        .bool => {
            if (a == b) {
                return Ordering.Equal;
            } else if (a) {
                return Ordering.Greater;
            } else {
                return Ordering.Less;
            }
        },
        .int, .float, .comptime_int, .comptime_float => {
            if (a < b) {
                return Ordering.Less;
            } else if (a > b) {
                return Ordering.Greater;
            } else {
                return Ordering.Equal;
            }
        },
        .optional => {
            if (a == null) {
                if (b == null) {
                    return Ordering.Equal;
                } else {
                    return Ordering.Less;
                }
            } else if (b == null) {
                return Ordering.Greater;
            } else {
                return compare(a.?, b.?);
            }
        },
        .pointer => |x| {
            switch (x.size) {
                .One, .C, .Many => return compare(@intFromPtr(a), @intFromPtr(b)),
                .Slice => return compare(@intFromPtr(a.ptr), @intFromPtr(b.ptr)),
            }
        },
        .array => |x| {
            for (0..x.len) |i| {
                const result = compareAddress(a[i], b[i]);
                if (result != Ordering.Equal) {
                    return result;
                }
            }
            return Ordering.Equal;
        },
        else => {
            if (comptime std.meta.hasFn(T, "compareAddress")) {
                return T.compareAddress(a, b);
            } else {
                @compileError("Cannot do compareAddress for type `" ++ @typeName(T) ++ "`");
            }
        },
    }
}

pub inline fn less(a: anytype, b: @TypeOf(a)) bool {
    return compare(a, b) == Ordering.Less;
}

pub inline fn greater(a: anytype, b: @TypeOf(a)) bool {
    return compare(a, b) == Ordering.Greater;
}

pub inline fn greaterOrEqual(a: anytype, b: @TypeOf(a)) bool {
    return compare(a, b) != Ordering.Less;
}

pub inline fn lessOrEqual(a: anytype, b: @TypeOf(a)) bool {
    return compare(a, b) != Ordering.Greater;
}

pub inline fn equal(a: anytype, b: @TypeOf(a)) bool {
    return compare(a, b) == Ordering.Equal;
}

pub inline fn notEqual(a: anytype, b: @TypeOf(a)) bool {
    return compare(a, b) != Ordering.Equal;
}

pub inline fn lessAddress(a: anytype, b: @TypeOf(a)) bool {
    return compareAddress(a, b) == Ordering.Less;
}

pub inline fn greaterAddress(a: anytype, b: @TypeOf(a)) bool {
    return compareAddress(a, b) == Ordering.Greater;
}

pub inline fn greaterOrEqualAddress(a: anytype, b: @TypeOf(a)) bool {
    return compareAddress(a, b) != Ordering.Less;
}

pub inline fn lessOrEqualAddress(a: anytype, b: @TypeOf(a)) bool {
    return compareAddress(a, b) != Ordering.Greater;
}

pub inline fn equalAddress(a: anytype, b: @TypeOf(a)) bool {
    return compareAddress(a, b) == Ordering.Equal;
}

pub inline fn notEqualAddress(a: anytype, b: @TypeOf(a)) bool {
    return compareAddress(a, b) != Ordering.Equal;
}

test {
    const expectEqual = std.testing.expectEqual;

    {
        var mem = [1]u128{0} ** 4;
        const buf: []u128 = &mem;

        const res = sliceCast(u8, u128, buf);

        try expectEqual(@intFromPtr(buf.ptr), @intFromPtr(res.ptr));
        try expectEqual(buf.len * @sizeOf(u128), res.len);
    }

    {
        const mem = [1]u128{0} ** 4;
        const buf: []const u128 = &mem;

        const res = sliceCastConst(u8, u128, buf);

        try expectEqual(@intFromPtr(buf.ptr), @intFromPtr(res.ptr));
        try expectEqual(buf.len * @sizeOf(u128), res.len);
    }
}
