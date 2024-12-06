const std = @import("std");
const ISA = @import("ISA");
const RbcCore = @import("Rbc:Core");

const Core = @import("root.zig");


pub const ZeroCheck = enum(u1) { zero, non_zero };
pub const OptZeroCheck = enum(u2) { none, zero, non_zero };
pub const BitSize = enum(u2) { b8, b16, b32, b64 };

pub const Code = enum(u8) {
    // ISA instructions:
    nop,
    halt, trap, block, with, @"if", when, re, br, call, prompt, ret, term,
    alloca, addr, read, write, load, store, clear, swap, copy,
    add, sub, mul, div, rem, neg,
    band, bor, bxor, bnot, bshiftl, bshiftr,
    eq, ne, lt, gt, le, ge,
    ext, trunc, cast,

    // IR-specific instructions:
    ref_local,
    ref_block,
    ref_function,
    ref_foreign,
    ref_global,
    ref_upvalue,

    discard,

    im_b, im_s, im_i, im_w,

    pub fn format(self: Code, comptime _: []const u8, _: anytype, writer: anytype) !void {
        try writer.writeAll(@tagName(self));
    }

    comptime {
        for (std.meta.fieldNames(Data)) |opName| {
            if (!@hasField(Code, opName)) {
                @compileError("missing OpCode: `" ++ opName ++ "`");
            }
        }
    }
};

comptime {
    for (ISA.Instructions) |category| {
        for (category.kinds) |kind| {
            const name = kind.humanFriendlyName();

            if (!@hasField(Code, name)) {
                @compileError("missing OpCode: `" ++ name ++ "`");
            }
        }
    }
}

pub const Data = packed union {
    nop: void,
    halt: void, trap: void, block: void, with: void, @"if": ZeroCheck, when: ZeroCheck, re: OptZeroCheck, br: OptZeroCheck, call: void, prompt: void, ret: void, term: void,
    alloca: RbcCore.RegisterLocalOffset, addr: void, read: void, write: void, load: void, store: void, clear: void, swap: void, copy: void,
    add: void, sub: void, mul: void, div: void, rem: void, neg: void,
    band: void, bor: void, bxor: void, bnot: void, bshiftl: void, bshiftr: void,
    eq: void, ne: void, lt: void, gt: void, le: void, ge: void,
    ext: BitSize, trunc: BitSize, cast: Core.TypeId,

    ref_local: Core.LocalId,
    ref_block: Core.BlockId,
    ref_function: Core.Ref(Core.FunctionId),
    ref_foreign: Core.ForeignId,
    ref_global: Core.Ref(Core.GlobalId),
    ref_upvalue: Core.UpvalueId,

    discard: void,

    im_b: Immediate(u8),
    im_s: Immediate(u16),
    im_i: Immediate(u32),
    im_w: Core.TypeId,

    pub fn dump(self: Data, code: Code, writer: anytype) !void {
        switch (code) {
            .nop,
            .halt, .trap, .block, .with,
            .call, .prompt, .ret, .term,
            .addr, .read, .write, .load, .store, .clear, .swap, .copy,
            .add, .sub, .mul, .div, .rem, .neg,
            .band, .bor, .bxor, .bnot, .bshiftl, .bshiftr,
            .eq, .ne, .lt, .gt, .le, .ge,
            .discard,
            => return,

            .@"if", .when => try writer.writeAll(@tagName(self.@"if")),
            .re, .br => try writer.writeAll(@tagName(self.re)),
            .alloca => try writer.print("{}", .{self.alloca}),

            .ext, .trunc => try writer.writeAll(@tagName(self.ext)),
            .cast => try writer.print("{}", .{self.cast}),

            .ref_local => try writer.print("{}", .{self.ref_local}),
            .ref_block => try writer.print("{}", .{self.ref_block}),
            .ref_function => try writer.print("{}", .{self.ref_function}),
            .ref_foreign => try writer.print("{}", .{self.ref_foreign}),
            .ref_global => try writer.print("{}", .{self.ref_global}),
            .ref_upvalue => try writer.print("{}", .{self.ref_upvalue}),

            .im_b => try writer.print("{}", .{self.im_b.data}),
            .im_s => try writer.print("{}", .{self.im_s.data}),
            .im_i => try writer.print("{}", .{self.im_i.data}),
            .im_w => try writer.print("{}", .{self.im_w}),
        }
    }

    pub fn Immediate (comptime T: type) type {
        return packed struct {
            type: Core.TypeId,
            data: T,
        };
    }

    comptime {
        for (std.meta.fieldNames(Code)) |opName| {
            if (!@hasField(Data, opName)) {
                @compileError("missing OpData: `" ++ opName ++ "`");
            }
        }
    }
};


pub fn TypeOf(comptime code: Code) type {
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
