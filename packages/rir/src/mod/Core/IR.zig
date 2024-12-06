const std = @import("std");

const Core = @import("root.zig");
const IR = @This();


allocator: std.mem.Allocator,
type_map: Core.Type.Map = .{},
foreign_list: std.ArrayListUnmanaged(*Core.Foreign) = .{},
module_list: std.ArrayListUnmanaged(*Core.Module) = .{},


pub fn init(alloc: std.mem.Allocator) !IR {
    return IR {
        .allocator = alloc,
    };
}

pub fn deinit(self: *IR) void {
    self.type_map.deinit(self.allocator);

    for (self.foreign_list.items) |f| {
        f.deinit();
    }

    self.foreign_list.deinit(self.allocator);

    for (self.module_list.items) |mod| {
        mod.deinit();
    }

    self.module_list.deinit(self.allocator);
}


/// If the type is not found in the map,
/// calls `Type.clone` on the input and puts the clone in the map
pub inline fn typeId(self: *IR, ty: Core.Type) !Core.TypeId {
    return self.type_map.typeId(self.allocator, ty);
}

/// Does not call `Type.clone` on the input
pub inline fn typeIdPreallocated(self: *IR, ty: Core.Type) !Core.TypeId {
    return self.type_map.typeIdPreallocated(self.allocator, ty);
}

pub inline fn typeFromNative(self: *const Core, comptime T: type) !Core.Type {
    return self.type_map.typeFromNative(T, self.allocator);
}

pub inline fn typeIdFromNative(self: *IR, comptime T: type) !Core.TypeId {
    return self.type_map.typeIdFromNative(T, self.allocator);
}

pub inline fn getType(self: *IR, id: Core.TypeId) !Core.Type {
    return self.type_map.getType(id);
}




/// Calls `allocator.dupe` on the input locals
pub fn foreign(self: *IR, tyId: Core.TypeId, locals: []Core.TypeId) !*Core.Foreign {
    const dupeLocals = try self.allocator.dupe(Core.TypeId, locals);
    errdefer self.allocator.free(dupeLocals);

    return self.foreignPreallocated(tyId, dupeLocals);
}

/// Does not call `allocator.dupe` on the input locals
pub fn foreignPreallocated(self: *IR, tyId: Core.TypeId, locals: []Core.TypeId) !*Core.Foreign {
    const index = self.foreign_list.items.len;

    if (index >= Core.MAX_FUNCTIONS) {
        return error.TooManyForeignFunctions;
    }

    const f = try Core.Foreign.init(self, @truncate(index), tyId, locals);
    errdefer self.allocator.destroy(f);

    try self.foreign_list.append(self.allocator, f);

    return f;
}

pub fn getForeign(self: *IR, id: Core.ForeignId) !*Core.Foreign {
    if (id >= self.foreign_list.items.len) {
        return error.InvalidForeignFunction;
    }

    return self.foreign_list.items[id];
}


pub fn module(self: *IR) !*Core.Module {
    const id = self.module_list.items.len;

    if (id >= Core.MAX_MODULES) {
        return error.InvalidModule;
    }

    const mod = try Core.Module.init(self, @truncate(id));
    errdefer self.allocator.destroy(mod);

    try self.module_list.append(self.allocator, mod);

    return mod;
}

pub fn getModule(self: *IR, id: Core.ModuleId) !*Core.Module {
    if (id >= self.module_list.items.len) {
        return error.InvalidModule;
    }

    return self.module_list.items[id];
}

pub fn getGlobal(self: *IR, ref: Core.Ref(Core.GlobalId)) !*Core.Global {
    const mod = try self.getModule(ref.module);

    return mod.getGlobal(ref.id);
}

pub fn getFunction(self: *IR, ref: Core.Ref(Core.FunctionId)) !*Core.Function {
    const mod = try self.getModule(ref.module);

    return mod.getFunction(ref.id);
}


test {
    std.testing.refAllDeclsRecursive(Core);
}
