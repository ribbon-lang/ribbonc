const std = @import("std");
const MiscUtils = @import("Utils").Misc;
const ISA = @import("ISA");
const RbcCore = @import("Rbc:Core");
const RbcBuilder = @import("Rbc:Builder");

const Core = @import("root.zig");

const Module = @This();


root: *Core.IR,
id: Core.ModuleId,
global_list: std.ArrayListUnmanaged(*Core.Global) = .{},
function_list: std.ArrayListUnmanaged(*Core.Function) = .{},


pub fn init(root: *Core.IR, id: Core.ModuleId) !*Module {
    const ptr = try root.allocator.create(Module);
    errdefer root.allocator.destroy(ptr);

    ptr.* = Module {
        .root = root,
        .id = id,
    };

    return ptr;
}

pub fn deinit(self: *Module) void {
    for (self.global_list.items) |g| {
        g.deinit();
    }

    self.global_list.deinit(self.root.allocator);

    for (self.function_list.items) |f| {
        f.deinit();
    }

    self.function_list.deinit(self.root.allocator);

    self.root.allocator.destroy(self);
}

/// Calls `Type.clone` on the input, if the type is not found in the map
pub inline fn typeId(self: *Module, ty: Core.Type) !Core.TypeId {
    return self.root.typeId(ty);
}

/// Does not call `Type.clone` on the input
pub inline fn typeIdPreallocated(self: *Module, ty: Core.Type) !Core.TypeId {
    return self.root.typeIdPreallocated(ty);
}

pub inline fn typeFromNative(self: *const Core, comptime T: type) !Core.Type {
    return self.root.typeFromNative(T);
}

pub inline fn typeIdFromNative(self: *Module, comptime T: type) !Core.TypeId {
    return self.root.typeIdFromNative(T);
}

pub inline fn getType(self: *Module, id: Core.TypeId) !Core.Type {
    return self.root.getType(id);
}

/// Calls `allocator.dupe` on the input locals
pub inline fn foreign(self: *Module, tyId: Core.TypeId, locals: []Core.TypeId) !Core.ForeignId {
    return self.root.foreign(tyId, locals);
}

/// Does not call `allocator.dupe` on the input locals
pub inline fn foreignPreallocated(self: *Module, tyId: Core.TypeId, locals: []Core.TypeId) !Core.ForeignId {
    return self.root.foreignPreallocated(tyId, locals);
}

pub inline fn getForeign(self: *Module, id: Core.ForeignId) !Core.Foreign {
    return self.root.getForeign(id);
}

/// Calls `allocator.dupe` on the input bytes
pub fn globalFromBytes(self: *Module, tyId: Core.TypeId, bytes: []const u8) !*Core.Global {
    const dupeBytes = try self.root.allocator.dupe(u8, bytes);
    errdefer self.root.allocator.free(dupeBytes);

    return self.globalFromBytesPreallocated(tyId, dupeBytes);
}

/// Does not call `allocator.dupe` on the input bytes
pub fn globalFromBytesPreallocated(self: *Module, tyId: Core.TypeId, bytes: []u8) !*Core.Global {
    const index = self.global_list.items.len;

    if (index >= Core.MAX_GLOBALS) {
        return error.TooManyGlobals;
    }

    const global = try Core.Global.init(self, @truncate(index), tyId, bytes);
    errdefer self.root.allocator.destroy(global);

    try self.global_list.append(self.root.allocator, global);

    return global;
}

pub fn globalFromNative(self: *Module, value: anytype) !*Core.Global {
    const T = @TypeOf(value);
    const tyId = try self.typeIdFromNative(T);

    return self.globalFromBytes(tyId, @as([*]const u8, @ptrCast(&value))[0..@sizeOf(T)]);
}

pub fn getGlobal(self: *Module, id: Core.GlobalId) !*Core.Global {
    if (id >= self.global_list.items.len) {
        return error.InvalidGlobal;
    }

    return self.global_list.items[id];
}

pub fn function(self: *Module, tyId: Core.TypeId) !*Core.Function {
    const index = self.function_list.items.len;

    if (index >= Core.MAX_FUNCTIONS) {
        return error.TooManyFunctions;
    }

    const builder = try Core.Function.init(self, @truncate(index), tyId);

    try self.function_list.append(self.root.allocator, builder);

    return builder;
}

pub fn getFunction(self: *Module, id: Core.FunctionId) !*Core.Function {
    if (id >= self.function_list.items.len) {
        return error.InvalidFunction;
    }

    return self.function_list.items[id];
}


test {
    std.testing.refAllDeclsRecursive(Module);
}
