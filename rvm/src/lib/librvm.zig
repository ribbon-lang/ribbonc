const std = @import("std");

const Config = @import("Config");
const MiscUtils = @import("Utils").Misc;

const Extern = @import("Utils").Extern;
const HeaderGenUtils = @import("Utils").Build.HeaderGenUtils;

pub const std_options = std.Options {
    .log_level = Config.LOG_LEVEL,
    .logFn = MiscUtils.FilteredLogger(Config.LOG_SCOPES),
};

const log = std.log.scoped(.librvm);

inline fn tryCall(err_out: ?*BB_Error, func: anytype, args: anytype) ?@typeInfo(@typeInfo(@TypeOf(func)).Fn.return_type.?).ErrorUnion.payload {
    if (@call(.always_inline, func, args)) |res| {
        return res;
    } else |err| {
        if (err_out) |ptr| ptr.* = BB_Error.fromNative(err);
    }

    return null;
}

inline fn sliceFromCStr(s: BB_CStr) []const u8 {
    return std.mem.span(@as([*:0]const u8, @ptrCast(s)));
}

inline fn cStrFromSlice(s: [:0]const u8) BB_CStr {
    return @ptrCast(s.ptr);
}

inline fn calcAlignLog2(alignment: usize) !u8 {
    if (alignment != 0) {
        if (std.math.isPowerOfTwo(alignment)) {
            const x = std.math.log2(alignment);

            if (x <= std.math.maxInt(u8)) {
                return @truncate(x);
            } else {
                return error.InvalidAlignment;
            }
        } else {
            return error.InvalidAlignment;
        }
    } else {
        return 1;
    }
}

inline fn allocatorFromExtern(allocator: BB_Allocator) std.mem.Allocator {
    return .{
        .ptr = allocator.ptr,
        .vtable = @ptrCast(@constCast(@alignCast(allocator.vtable))),
    };
}

inline fn externFromAllocator(allocator: std.mem.Allocator) BB_Allocator {
    return .{
        .ptr = allocator.ptr,
        .vtable = @ptrCast(@constCast(@alignCast(allocator.vtable))),
    };
}

const opaquetype = HeaderGenUtils.opaquetype;
const customtype = HeaderGenUtils.customtype;

pub const @"HEADER-GENERATION-DATA" = HeaderGenUtils.MakeData(struct {
    pub const CustomType = union(enum) {
        generative: void,
        @"enum": struct {
            suffix: ?[]const u8,
            variants: []const []const u8,
        },
        platform: struct {
            linux: []const u8,
            windows: []const u8,
        },
        function: struct {
            params: []const u8,
            retTy: []const u8,
        },

        pub fn render(self: CustomType, name: []const u8, expr: []const u8, generator: anytype, writer: anytype) anyerror!void {
            _ = generator;
            _ = expr;

            switch (self) {
                .generative => return error.InvalidType,
                .@"enum" => |t| {
                    try writer.print("typedef enum {s} {{", .{name});
                    if (t.variants.len > 1) {
                        try writer.writeAll("\n");
                        for (t.variants) |x| {
                            if (t.suffix) |suff| {
                                try writer.print("    {s}{s}_{s},\n", .{ prefix, x, suff });
                            } else {
                                try writer.print("    {s}{s},\n", .{ prefix, x });
                            }
                        }
                    } else {
                        for (t.variants) |x| {
                            if (t.suffix) |suff| {
                                try writer.print(" {s}{s}_{s} ", .{ prefix, x, suff });
                            } else {
                                try writer.print(" {s}{s} ", .{ prefix, x });
                            }
                        }
                    }
                    try writer.print("}} {s};", .{name});
                },
                .platform => |p| {
                    try writer.print(
                        \\#ifdef __linux__
                        \\    typedef {s} {s};
                        \\#elif _WIN32
                        \\    typedef {s} {s};
                        \\#endif
                    , .{ p.linux, name, p.windows, name });
                },
                .function => |f| {
                    try writer.print("typedef {s} (*{s}) ({s});", .{ f.retTy, name, f.params });
                },
            }
        }
    };

    pub const customTypes = .{
        .BB_FileHandle = CustomType{ .platform = .{ .linux = "int", .windows = "void*" } },
    };

    pub const ignoredDecls = .{};

    pub const head =
        \\#include <stddef.h>
        \\#include <stdint.h>
        \\#include <stdbool.h>
    ;

    pub const foot = "";

    pub const prefix = "BB_";

    pub const enumSuffixes = .{
    };

    pub const procArgs = .{
        .BB_HasherProc = .{
            .{ *u32, "state" },
            .{ [*]const u8, "bytes" },
            .{ usize, "bytes_len" },
        },
    };
});

pub const BB_Error: customtype = Extern.Error;
pub const BB_Ordering: type = MiscUtils.Ordering;
pub const BB_Arena: opaquetype = std.heap.ArenaAllocator;

pub const BB_CStr: type = [*:0]const c_char;
pub const BB_UStr: type = Extern.UStr;
pub const BB_Unit: type = MiscUtils.Unit;

pub const BB_Writer: customtype = Extern.Writer;
pub const BB_HasherProc: type = Extern.Hasher.Proc;
pub const BB_Hasher: type = Extern.Hasher;
pub const BB_Allocator: type = extern struct {
    ptr: *anyopaque,
    vtable: *anyopaque,
};
pub const BB_FileHandle: customtype = std.fs.File.Handle;

pub export fn BB_Arena_init(err_out: ?*BB_Error) ?*BB_Arena {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);

    const ptr = if (arena.allocator().create(std.heap.ArenaAllocator)) |ptr| ptr else |err| {
        if (err_out) |ptr| ptr.* = BB_Error.fromNative(err);
        arena.deinit();
        return null;
    };

    ptr.* = arena;

    return ptr;
}

pub export fn BB_Arena_deinit(arena: *BB_Arena) void {
    arena.deinit();
}

pub export fn BB_Arena_allocator(arena: *BB_Arena) BB_Allocator {
    return externFromAllocator(arena.allocator());
}

pub export fn BB_Allocator_alloc(allocator: BB_Allocator, size: usize, alignment: usize, err_out: ?*BB_Error) ?*anyopaque {
    const alloc = allocatorFromExtern(allocator);
    const alignLog2 = calcAlignLog2(alignment) catch |err| {
        if (err_out) |ptr| ptr.* = BB_Error.fromNative(err);
        return null;
    };

    if (alloc.rawAlloc(size, alignLog2, @returnAddress())) |buf| {
        return @ptrCast(buf);
    } else {
        if (err_out) |ptr| ptr.* = BB_Error.fromNative(error.OutOfMemory);
    }
    return null;
}

pub export fn BB_Allocator_free(allocator: BB_Allocator, buf: *anyopaque, size: usize, alignment: usize) void {
    const alloc = allocatorFromExtern(allocator);
    const alignLog2 = calcAlignLog2(alignment) catch unreachable;
    alloc.rawFree(@as([*]u8, @ptrCast(buf))[0..size], alignLog2, @returnAddress());
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
