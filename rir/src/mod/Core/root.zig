const std = @import("std");
const RbcCore = @import("Rbc:Core");

const Core = @This();

pub const Block = @import("Block.zig");
pub const Foreign = @import("Foreign.zig");
pub const Function = @import("Function.zig");
pub const Global = @import("Global.zig");
pub const HandlerSet = @import("HandlerSet.zig");
pub const IR = @import("IR.zig");
pub const Module = @import("Module.zig");
pub const Op = @import("Op.zig");
pub const Operand = @import("Operand.zig").Operand;
pub const Type = @import("Type.zig").Type;

pub const ModuleId = u16;
pub const RegisterId = RbcCore.RegisterIndex;
pub const RegisterOffset = RbcCore.RegisterLocalOffset;
pub const HandlerSetId = RbcCore.HandlerSetIndex;
pub const EvidenceId = RbcCore.EvidenceIndex;
pub const TypeId = RbcCore.Info.TypeIndex;
pub const BlockId = RbcCore.BlockIndex;
pub const FunctionId = RbcCore.FunctionIndex;
pub const ForeignId = RbcCore.ForeignId;
pub const GlobalId = RbcCore.GlobalIndex;
pub const UpvalueId = RbcCore.UpvalueIndex;
pub const LocalId = u16;

pub const Instruction = packed struct {
    code: Op.Code,
    data: Op.Data,

    pub fn format(self: Instruction, comptime _: []const u8, _: anytype, writer: anytype) !void {
        try writer.print("{} ", .{self.code});
        try self.data.dump(self.code, writer);
    }

    comptime {
        if (@sizeOf(Instruction) != 8) {
            @compileError(std.fmt.comptimePrint("Instruction size changed: {}", .{@sizeOf(Instruction)}));
        }
    }
};

pub fn Ref (comptime T: type) type {
    return packed struct {
        module: Core.ModuleId,
        id: T,

        const Self = @This();

        pub fn format(self: Self, comptime _: []const u8, _: anytype, writer: anytype) !void {
            try writer.print("{}:{}", .{self.module, self.id});
        }
    };
}

pub const MAX_MODULES = std.math.maxInt(Core.ModuleId);
pub const MAX_TYPES = std.math.maxInt(Core.TypeId);
pub const MAX_GLOBALS = std.math.maxInt(Core.GlobalId);
pub const MAX_FUNCTIONS = std.math.maxInt(Core.FunctionId);
pub const MAX_HANDLER_SETS = std.math.maxInt(Core.HandlerSetId);
pub const MAX_EVIDENCE = RbcCore.MAX_EVIDENCE;
pub const MAX_BLOCKS = RbcCore.MAX_BLOCKS;
pub const MAX_REGISTERS = RbcCore.MAX_REGISTERS;
pub const MAX_LOCALS = std.math.maxInt(Core.LocalId);

