const std = @import("std");

const MiscUtils = @import("Utils").Misc;
const TextUtils = @import("Utils").Text;
const TypeUtils = @import("Utils").Type;

const IO = @import("IO");
const ISA = @import("ISA");

const Core = @import("root.zig");


const Bytecode = @This();


blocks: []const [*]const Core.Instruction,
instructions: []const Core.Instruction,

pub fn deinit(self: Bytecode, allocator: std.mem.Allocator) void {
    allocator.free(self.blocks);
    allocator.free(self.instructions);
}


test {
    std.testing.refAllDeclsRecursive(@This());
}
