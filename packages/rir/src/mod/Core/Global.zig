const Core = @import("root.zig");

const Global = @This();


module: *Core.Module,
id: Core.GlobalId,
type: Core.TypeId,
value: []u8,


pub fn init(mod: *Core.Module, id: Core.GlobalId, ty: Core.TypeId, value: []u8) !*Global {
    errdefer mod.root.allocator.free(value);

    const ptr = try mod.root.allocator.create(Global);
    errdefer mod.root.allocator.destroy(ptr);

    ptr.* = Global {
        .module = mod,
        .id = id,
        .type = ty,
        .value = value,
    };

    return ptr;
}

pub fn deinit(self: *Global) void {
    self.module.root.allocator.free(self.value);
    self.module.root.allocator.destroy(self);
}
