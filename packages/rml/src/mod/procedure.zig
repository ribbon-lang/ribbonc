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
const Writer = Rml.Writer;
const getHeader = Rml.getHeader;
const getObj = Rml.getObj;
const getRml = Rml.getRml;
const forceObj = Rml.forceObj;

pub const ProcedureKind = enum {
    macro,
    function,
    native_macro,
    native_function,
};

pub const ProcedureBody = struct {
    argument_pattern: Obj(Pattern),
    body: Obj(Block),

    pub fn deinit(self: *ProcedureBody) void {
        self.argument_pattern.deinit();
        self.body.deinit();
    }
};

pub const Procedure = union(ProcedureKind) {
    macro: ProcedureBody,
    function: ProcedureBody,
    native_macro: Rml.bindgen.NativeFunction,
    native_function: Rml.bindgen.NativeFunction,

    pub fn onInit(_: ptr(Procedure)) OOM! void {
        return;
    }

    pub fn onCompare(self: ptr(Procedure), other: Object) Ordering {
        return Rml.compare(getHeader(self).type_id, other.getTypeId());
    }

    pub fn onFormat(self: ptr(Procedure), writer: Rml.Obj(Writer)) Error! void {
        return writer.data.print("[{s}-{x}]", .{@tagName(self.*), @intFromPtr(self)});
    }

    pub fn onDeinit(self: ptr(Procedure)) void {
        switch (self.*) {
            .macro => |*data| data.deinit(),
            .function => |*data| data.deinit(),
            .native_macro => {},
            .native_function => {},
        }
    }
};
