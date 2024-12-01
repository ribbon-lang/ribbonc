const std = @import("std");

const Core = @import("root.zig");

const Function = @This();


module: *Core.Module,
id: Core.FunctionId,
type: Core.TypeId,
blocks: std.ArrayListUnmanaged(*Core.Block),
local_types: std.ArrayListUnmanaged(Core.TypeId),
parent: ?*Function = null,
upvalue_indices: std.ArrayListUnmanaged(Core.LocalId) = .{},
handler_sets: std.ArrayListUnmanaged(*Core.HandlerSet) = .{},


pub fn init(module: *Core.Module, id: Core.FunctionId, tyId: Core.TypeId) !*Function {
    const ptr = try module.root.allocator.create(Function);
    errdefer module.root.allocator.destroy(ptr);

    const ty = try module.getType(tyId);
    const funcTy = try ty.asFunction();

    var blocks = std.ArrayListUnmanaged(*Core.Block){};
    errdefer blocks.deinit(module.root.allocator);

    var local_types = std.ArrayListUnmanaged(Core.TypeId){};
    errdefer local_types.deinit(module.root.allocator);

    for (funcTy.parameter_types) |param| {
        try local_types.append(module.root.allocator, param);
    }

    ptr.* = Function {
        .module = module,
        .id = id,
        .type = tyId,
        .blocks = blocks,
        .local_types = local_types,
    };

    const entryBlock = try Core.Block.init(ptr, null, 0);
    errdefer entryBlock.deinit();

    try ptr.blocks.append(module.root.allocator, entryBlock);

    return ptr;
}

pub fn deinit(self: *Function) void {
    for (self.blocks.items) |b| {
        b.deinit();
    }

    self.blocks.deinit(self.module.root.allocator);

    self.local_types.deinit(self.module.root.allocator);

    self.upvalue_indices.deinit(self.module.root.allocator);

    for (self.handler_sets.items) |hs| {
        hs.deinit();
    }

    self.handler_sets.deinit(self.module.root.allocator);

    self.module.root.allocator.destroy(self);
}

pub fn parameterTypes(self: *const Function) ![]Core.TypeId {
    const ty = try self.module.getType(self.type);
    const funcTy = try ty.asFunction();

    return funcTy.parameter_types;
}

pub fn local(self: *Function, tyId: Core.TypeId) !Core.LocalId {
    const numParams = (try self.parameterTypes()).len;
    const index = self.local_types.items.len + numParams;

    if (index >= Core.MAX_LOCALS) {
        return error.TooManyLocals;
    }

    try self.local_types.append(self.module.root.allocator, tyId);

    return @truncate(index);
}

pub fn getLocalType(self: *const Function, l: Core.LocalId) !Core.TypeId {
    const params = try self.parameterTypes();

    if (l < params.len) {
        return params[l];
    }

    if (l >= self.local_types.items.len) {
        return error.InvalidRegister;
    }

    return self.local_types.items[l];
}

pub fn upvalue(self: *Function, parentLocal: Core.LocalId) !Core.UpvalueId {
    if (self.parent) |parent| {
        _ = try parent.getLocalType(parentLocal);

        const index = self.upvalue_indices.items.len;

        if (index >= Core.MAX_LOCALS) {
            return error.TooManyUpvalues;
        }

        try self.upvalue_indices.append(self.module.root.allocator, parentLocal);

        return @truncate(index);
    } else {
        return error.InvalidUpvalue;
    }
}

pub fn getUpvalueType(self: *const Function, u: Core.UpvalueId) !Core.TypeId {
    if (self.parent) |parent| {
        if (u >= self.upvalue_indices.items.len) {
            return error.InvalidRegister;
        }

        return parent.getLocalType(self.upvalue_indices.items[u]);
    } else {
        return error.InvalidUpvalue;
    }
}

pub fn entry(self: *Function) *Core.Block {
    return self.blocks.items[0];
}

pub fn block(self: *Function, parent: *Core.Block) !*Core.Block {
    const index = self.blocks.items.len;

    if (index >= Core.MAX_BLOCKS) {
        return error.TooManyBlocks;
    }

    const newBlock = try Core.Block.init(self, parent, @truncate(index));

    try self.blocks.append(self.module.root.allocator, newBlock);

    return newBlock;
}

pub fn getBlock(self: *Function, id: Core.BlockId) !*Core.Block {
    if (id >= self.blocks.items.len) {
        return error.InvalidBlock;
    }

    return self.blocks.items[id];
}

pub fn handlerSet(self: *Function) !*Core.HandlerSet {
    const index = self.handler_sets.items.len;

    if (index >= Core.MAX_HANDLER_SETS) {
        return error.TooManyHandlerSets;
    }

    const builder = try Core.HandlerSet.init(self, @truncate(index));

    try self.handler_sets.append(self.module.root.allocator, builder);

    return builder;
}

pub fn getHandlerSet(self: *Function, id: Core.HandlerSetId) !*Core.HandlerSet {
    if (id >= self.handler_sets.items.len) {
        return error.InvalidHandlerSet;
    }

    return self.handler_sets.items[id];
}
