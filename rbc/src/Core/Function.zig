const std = @import("std");

const Core = @import("root.zig");

const Function = @This();

num_arguments: Core.RegisterIndex,
num_registers: Core.RegisterIndex,
bytecode: Core.Bytecode,

pub fn deinit(self: Function, allocator: std.mem.Allocator) void {
    self.bytecode.deinit(allocator);
}


pub const Foreign = struct {
    num_arguments: Core.RegisterIndex,
    num_registers: Core.RegisterIndex,
};
