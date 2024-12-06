const std = @import("std");

const ISA = @import("ISA");

const Core = @import("root.zig");




pub const Data = op_data: {
    var fields: []const std.builtin.Type.UnionField = &[0]std.builtin.Type.UnionField{};

    var i = 0;

    for (ISA.Instructions) |category| {
        for (category.kinds) |kind| {
            for (kind.instructions) |instr| {
                const name = ISA.computeInstructionName(kind, instr);

                var operands: []const std.builtin.Type.StructField = &[0]std.builtin.Type.StructField{};

                if (instr.operands.len > 0) {
                    var size = 0;
                    var operandCounts = [1]u8 {0} ** std.meta.fieldNames(ISA.OperandDescriptor).len;
                    for (instr.operands) |operand| {
                        const opType = switch (operand) {
                            .register => Core.RegisterIndex,
                            .byte => u8,
                            .short => u16,
                            .immediate => u32,
                            .handler_set_index => Core.HandlerSetIndex,
                            .evidence_index => Core.EvidenceIndex,
                            .global_index => Core.GlobalIndex,
                            .upvalue_index => Core.UpvalueIndex,
                            .function_index => Core.FunctionIndex,
                            .block_index => Core.BlockIndex,
                        };

                        size += @bitSizeOf(opType);

                        operands = operands ++ [1]std.builtin.Type.StructField { .{
                            .name = std.fmt.comptimePrint("{u}{}", .{switch (operand) {
                                .register => 'R',
                                .byte => 'b',
                                .short => 's',
                                .immediate => 'i',
                                .handler_set_index => 'H',
                                .evidence_index => 'E',
                                .global_index => 'G',
                                .upvalue_index => 'U',
                                .function_index => 'F',
                                .block_index => 'B',
                            }, operandCounts[@intFromEnum(operand)]}),
                            .type = opType,
                            .is_comptime = false,
                            .default_value = null,
                            .alignment = 0,
                        } };

                        operandCounts[@intFromEnum(operand)] += 1;
                    }

                    if (size > 48) {
                        @compileError("Operand set size too large in instruction `"
                            ++ name ++ "`");
                    }

                    const backingType = std.meta.Int(.unsigned, size);
                    const ty = @Type(.{ .@"struct" = .{
                        .layout = .@"packed",
                        .backing_integer = backingType,
                        .fields = operands,
                        .decls = &[0]std.builtin.Type.Declaration {},
                        .is_tuple = false,
                    } });

                    // @compileLog(std.fmt.comptimePrint("{s} {s}", .{name, std.meta.fieldNames(ty)}));

                    fields = fields ++ [1]std.builtin.Type.UnionField { .{
                        .name = name,
                        .type = ty,
                        .alignment = @alignOf(backingType),
                    } };
                } else {
                    fields = fields ++ [1]std.builtin.Type.UnionField { .{
                        .name = name,
                        .type = void,
                        .alignment = 0,
                    } };
                }

                i += 1;
            }
        }
    }

    break :op_data @Type(.{ .@"union" = .{
        .layout = .@"packed",
        .tag_type = null,
        .fields = fields,
        .decls = &[0]std.builtin.Type.Declaration {},
    } });
};



pub const Code = op_code: {
    var fields: []const std.builtin.Type.EnumField = &[0]std.builtin.Type.EnumField{};

    var i: u16 = 0;
    for (ISA.Instructions) |category| {
        for (category.kinds) |kind| {
            for (kind.instructions) |instr| {
                const name = ISA.computeInstructionName(kind, instr);
                fields = fields ++ [1]std.builtin.Type.EnumField { .{
                    .name = name,
                    .value = i,
                } };

                i += 1;
            }
        }
    }

    break :op_code @Type(.{ .@"enum" = .{
        .tag_type = u16,
        .fields = fields,
        .decls = &[0]std.builtin.Type.Declaration {},
        .is_exhaustive = true,
    } });
};

pub fn TypeOf(comptime code: Code) type  {
    @setEvalBranchQuota(2000);
    inline for (std.meta.fieldNames(Code)) |name| {
        if (@field(Code, name) == code) {
            for (std.meta.fields(Data)) |field| {
                if (std.mem.eql(u8, field.name, name)) {
                    return field.type;
                }
            }
            unreachable;
        }
    }
    unreachable;
}
