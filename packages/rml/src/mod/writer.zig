const std = @import("std");

const Rml = @import("root.zig");
const Ordering = Rml.Ordering;
const Error = Rml.Error;
const OOM = Rml.OOM;
const ptr = Rml.ptr;
const Obj = Rml.Obj;
const ref = Rml.ref;
const Object = Rml.Object;
const getHeader = Rml.getHeader;
const getObj = Rml.getObj;
const forceObj = Rml.forceObj;


pub const Native = std.io.AnyWriter;

pub const Writer = Obj(Memory);

pub const Memory = struct {
    unmanaged: MemoryUnmanaged,

    pub fn onInit(native_writer: Native) Memory {
        return Memory { .unmanaged = .{ .native_writer = native_writer } };
    }

    pub fn onCompare(self: ptr(Memory), other: Object) Ordering {
        var ord = Rml.compare(getHeader(self).type_id, other.getHeader().type_id);
        if (ord == .Equal) {
            const b = forceObj(Memory, other);
            defer b.deinit();

            ord = self.unmanaged.compare(b.data.unmanaged);
        }
        return ord;
    }

    pub fn onFormat(self: ptr(Memory), writer: Writer) Error! void {
        try writer.data.print("{}", .{self.unmanaged});
    }

    pub fn print(self: ptr(Memory), comptime fmt: []const u8, args: anytype) Error! void {
        return self.unmanaged.print(fmt, args);
    }

    pub fn write(self: ptr(Memory), val: []const u8) Error! usize {
        return self.unmanaged.write(val);
    }

    pub fn writeAll(self: ptr(Memory), val: []const u8) Error! void {
        return self.unmanaged.writeAll(val);
    }
};

pub const MemoryUnmanaged = struct {
    native_writer: Native,

    pub fn native(self: *MemoryUnmanaged) Native {
        return self.native_writer;
    }

    pub fn compare(self: MemoryUnmanaged, other: MemoryUnmanaged) Ordering {
        return Rml.compare(@intFromPtr(self.native_writer.context), @intFromPtr(other.native_writer.context));
    }

    pub fn format(_: *const MemoryUnmanaged, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) anyerror!void {
        try writer.writeAll("Writer");
    }

    pub fn print(self: *MemoryUnmanaged, comptime fmt: []const u8, args: anytype) Error! void {
        self.native_writer.print(fmt, args) catch |err| return Rml.errorCast(err);
    }

    pub fn write(self: *MemoryUnmanaged, val: []const u8) Error! usize {
        return self.native_writer.write(val) catch |err| Rml.errorCast(err);
    }

    pub fn writeAll(self: *MemoryUnmanaged, val: []const u8) Error! void {
        self.native_writer.writeAll(val) catch |err| return Rml.errorCast(err);
    }
};
