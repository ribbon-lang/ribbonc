const std = @import("std");
const MiscUtils = @import("Utils").Misc;

const Rml = @import("root.zig");
const OOM = Rml.OOM;

const Storage = @This();

object: std.mem.Allocator,
interner: StringInterner,
frame: std.mem.Allocator,
_frame_arena: *std.heap.ArenaAllocator,
_fresh: usize = 0,

object_count: isize = 0,

/// callback should use rml.storage.object for return allocator
read_file_callback: ?*const fn (rml: *Rml, []const u8) (Rml.IOError || OOM)![]const u8 = null,
userstate: *anyopaque = undefined,
origin: Rml.Origin = undefined,

pub fn init(object: std.mem.Allocator) OOM! Storage {
    const _frame_arena = try object.create(std.heap.ArenaAllocator);
    _frame_arena.* = std.heap.ArenaAllocator.init(object);
    return .{
        .object = object,
        .interner = StringInterner.init(object),
        .frame = _frame_arena.allocator(),
        ._frame_arena = _frame_arena,
    };
}

pub fn deinit(self: *Storage) void {
    self._frame_arena.deinit();
    self.interner.deinit();
    self.object.destroy(self._frame_arena);
}

pub fn fresh(self: *Storage, comptime T: type) T {
    const i = self._fresh;
    self._fresh += 1;
    return @enumFromInt(i);
}


pub const StringInterner = struct {
    object: std.mem.Allocator,
    arena: Mem,
    map: Map = .{},
    const Mem = std.heap.ArenaAllocator;
    const Map = std.ArrayHashMapUnmanaged([]const u8, void, MiscUtils.SimpleHashContext, true);

    fn init(object: std.mem.Allocator) StringInterner {
        return StringInterner { .object = object, .arena = std.heap.ArenaAllocator.init(object) };
    }

    fn deinit(self: *StringInterner) void {
        self.arena.deinit();
        self.map.deinit(self.object);
    }

    pub fn length(self: *const StringInterner) usize {
        return self.map.count();
    }

    pub fn contents(self: *const StringInterner) []Rml.str {
        return self.map.keys();
    }

    pub fn contains(self: *const StringInterner, key: []const u8) bool {
        return self.map.contains(key);
    }

    pub fn getNoAlloc(self: *const StringInterner, key: []const u8) ?Rml.str {
        if (self.map.getEntry(key)) |existing| {
            return existing.key_ptr.*;
        }
        return null;
    }

    pub fn get(self: *StringInterner, key: []const u8) OOM! Rml.str {
        return self.getNoAlloc(key) orelse {
            const ownedKey = try self.arena.allocator().dupe(u8, key);
            try self.map.put(self.object, ownedKey, {});

            return ownedKey;
        };
    }
};
