const std = @import("std");

const Rml = @import("root.zig");
const Error = Rml.Error;
const Ordering = Rml.Ordering;
const OOM = Rml.OOM;
const const_ptr = Rml.const_ptr;
const ptr = Rml.ptr;
const Obj = Rml.Obj;
const Object = Rml.Object;
const Block = Rml.Block;
const Pattern = Rml.Pattern;
const getHeader = Rml.getHeader;
const getObj = Rml.getObj;
const getRml = Rml.getRml;
const forceObj = Rml.forceObj;

pub const ProcedureKind = enum {
    macro,
    function,
    native,
};

pub const Procedure = Obj(Memory);

pub const ProcedureBody = struct {
    argument_pattern: Pattern,
    body: Block,

    pub fn deinit(self: *ProcedureBody) void {
        self.argument_pattern.deinit();
        self.body.deinit();
    }
};

pub const Memory = union(ProcedureKind) {
    macro: ProcedureBody,
    function: ProcedureBody,
    native: Rml.bindgen.NativeFunction,

    pub fn onInit(_: ptr(Memory)) OOM! void {
        return;
    }

    pub fn onCompare(self: ptr(Memory), other: Object) Ordering {
        return Rml.compare(getHeader(self).type_id, other.getHeader().type_id);
    }

    pub fn onFormat(self: ptr(Memory), writer: Rml.Writer) Error! void {
        return writer.data.print("[{s}-{x}]", .{@tagName(self.*), @intFromPtr(self)});
    }

    pub fn onDeinit(self: ptr(Memory)) void {
        switch (self.*) {
            .macro => |*data| data.deinit(),
            .function => |*data| data.deinit(),
            .native => {},
        }
    }
};
