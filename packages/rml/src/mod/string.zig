const std = @import("std");
const TextUtils = @import("Utils").Text;

const Rml = @import("root.zig");
const char = Rml.char;
const OOM = Rml.OOM;
const Error = Rml.Error;
const ptr = Rml.ptr;
const Obj = Rml.Obj;
const Object = Rml.Object;
const getObj = Rml.getObj;
const getRml = Rml.getRml;


pub const String = Obj(Memory);

pub const Memory = struct {
    unmanaged: MemoryUnmanaged = .{},

    pub fn onInit(self: ptr(Memory), str: []const u8) OOM! void {
        return self.unmanaged.appendSlice(getRml(self), str);
    }

    pub fn onFormat(self: ptr(Memory), writer: Rml.Writer) Error! void {
        try writer.data.print("{}", .{self.unmanaged});
    }

    pub fn onDeinit(self: ptr(Memory)) void {
        self.unmanaged.deinit(getRml(self));
    }

    pub fn text(self: ptr(Memory)) []const u8 {
        return self.unmanaged.text();
    }

    pub fn length(self: ptr(Memory)) usize {
        return self.unmanaged.length();
    }

    pub fn append(self: ptr(Memory), ch: char) OOM! void {
        return self.unmanaged.append(getRml(self), ch);
    }

    pub fn appendSlice(self: ptr(Memory), str: []const u8) OOM! void {
        return self.unmanaged.appendSlice(getRml(self), str);
    }

    pub fn makeInternedSlice(self: ptr(Memory)) OOM! []const u8 {
        return self.unmanaged.makeInternedSlice(getRml(self));
    }
};

pub const MemoryUnmanaged = struct {
    native_string: std.ArrayListUnmanaged(u8) = .{},

    pub fn format(self: *const MemoryUnmanaged, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) Error! void {
        // TODO: escape non-ascii & control etc chars
        writer.print("\"{s}\"", .{self.native_string.items}) catch |err| return Rml.errorCast(err);
    }

    pub fn deinit(self: *MemoryUnmanaged, rml: *Rml) void {
        self.native_string.deinit(rml.storage.object);
    }

    pub fn text(self: *MemoryUnmanaged) []const u8 {
        return self.native_string.items;
    }

    pub fn length(self: *MemoryUnmanaged) usize {
        return self.native_string.items.len;
    }

    pub fn append(self: *MemoryUnmanaged, rml: *Rml, ch: char) OOM! void {
        var buf = [1]u8{0} ** 4;
        const len = TextUtils.encode(ch, &buf) catch @panic("invalid ch");
        return self.native_string.appendSlice(rml.storage.object, buf[0..len]);
    }

    pub fn appendSlice(self: *MemoryUnmanaged, rml: *Rml, str: []const u8) OOM! void {
        return self.native_string.appendSlice(rml.storage.object, str);
    }

    pub fn makeInternedSlice(self: *MemoryUnmanaged, rml: *Rml) OOM! []const u8 {
        return try rml.storage.interner.get(self.native_string.items);
    }
};


