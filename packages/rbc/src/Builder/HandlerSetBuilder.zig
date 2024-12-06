const std = @import("std");

const Core = @import("Core");

const Builder = @import("root.zig");
const Error = Builder.Error;
const FunctionBuilder = Builder.FunctionBuilder;


const HandlerSetBuilder = @This();


parent: *Builder,
index: Core.HandlerSetIndex,
handler_map: Builder.HandlerMap,

/// for use by the parent Builder only
pub fn init(parent: *Builder, index: Core.HandlerSetIndex) std.mem.Allocator.Error!*HandlerSetBuilder {
    const ptr = try parent.allocator.create(HandlerSetBuilder);

    var handler_map = Builder.HandlerMap {};
    try handler_map.ensureTotalCapacity(parent.allocator, 8);

    ptr.* = HandlerSetBuilder {
        .parent = parent,
        .index = index,
        .handler_map = handler_map,
    };

    return ptr;
}

pub fn assemble(self: *const HandlerSetBuilder, allocator: std.mem.Allocator) Error!Core.Handler.Set {
    const handlerSet = try allocator.alloc(Core.Handler.Binding, self.handler_map.count());
    errdefer allocator.free(handlerSet);

    for (self.handler_map.keys(), 0..) |e, i| {
        const funIndex = try self.getHandler(e);
        handlerSet[i] = Core.Handler.Binding { .id = e, .handler = funIndex };
    }

    return handlerSet;
}

pub fn getEvidence(self: *const HandlerSetBuilder) []const Core.EvidenceIndex {
    return self.handler_map.keys();
}

pub fn containsEvidence(self: *const HandlerSetBuilder, e: Core.EvidenceIndex) bool {
    return self.handler_map.contains(e);
}

pub fn getHandler(self: *const HandlerSetBuilder, e: Core.EvidenceIndex) Error!Core.FunctionIndex {
    return self.handler_map.get(e) orelse Error.MissingHandler;
}

pub fn handler(self: *HandlerSetBuilder, e: Core.EvidenceIndex) Error!*FunctionBuilder {
    if (self.handler_map.contains(e)) {
        return Error.EvidenceOverlap;
    }

    const fun = try self.parent.function();

    fun.evidence = e;

    try self.handler_map.put(self.parent.allocator, e, fun.index);

    return fun;
}

pub fn foreignHandler(self: *HandlerSetBuilder, e: Core.EvidenceIndex, num_arguments: Core.RegisterIndex, num_registers: Core.RegisterIndex) Error!*Builder.Function.Foreign {
    if (self.handler_map.contains(e)) {
        return Error.EvidenceOverlap;
    }

    const fun = try self.parent.foreign(num_arguments, num_registers);

    fun.evidence = e;

    try self.handler_map.put(self.parent.allocator, e, fun.index);

    return fun;
}

pub fn foreignNativeHandler(self: *HandlerSetBuilder, e: Core.EvidenceIndex, comptime T: type) Error!*Builder.Function.Foreign {
    if (self.handler_map.contains(e)) {
        return Error.EvidenceOverlap;
    }

    const ev = try self.parent.getEvidence(e);

    const fun = try self.parent.foreignNative(ev.type, T);

    fun.evidence = e;

    try self.handler_map.put(self.parent.allocator, e, fun.index);

    return fun;
}


test {
    std.testing.refAllDeclsRecursive(@This());
}
