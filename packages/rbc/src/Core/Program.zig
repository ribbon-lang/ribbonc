const std = @import("std");

const Core = @import("root.zig");

const Program = @This();


globals: []const [*]u8,
global_memory: []u8,
functions: []const Core.Function,
foreign_functions: []const Core.Function.Foreign,
handler_sets: []const Core.Handler.Set,
main: Core.FunctionIndex,


pub fn deinit(self: Program, allocator: std.mem.Allocator) void {
    allocator.free(self.globals);

    allocator.free(self.global_memory);

    for (self.functions) |fun| {
        fun.deinit(allocator);
    }

    allocator.free(self.functions);

    allocator.free(self.foreign_functions);

    for (self.handler_sets) |handlerSet| {
        allocator.free(handlerSet);
    }

    allocator.free(self.handler_sets);
}
