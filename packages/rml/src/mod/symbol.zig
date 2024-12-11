const std = @import("std");

const Rml = @import("root.zig");
const Ordering = Rml.Ordering;
const OOM = Rml.OOM;
const Error = Rml.Error;
const ptr = Rml.ptr;
const Obj = Rml.Obj;
const Object = Rml.Object;
const getHeader = Rml.getHeader;
const forceObj = Rml.forceObj;
const getObj = Rml.getObj;
const getRml = Rml.getRml;

pub const Symbol = Obj(Memory);

pub const Memory = struct {
    str: Rml.str,

    pub fn onInit(self: ptr(Memory), str: []const u8) OOM! void {
        self.str = try getRml(self).storage.interner.get(str);
    }

    pub fn onCompare(self: ptr(Memory), other: Object) Ordering {
        var ord = Rml.compare(getHeader(self).type_id, other.getHeader().type_id);

        if (ord == .Equal) {
            const b = forceObj(Memory, other);
            defer b.deinit();

            ord = Rml.compare(@intFromPtr(self.str.ptr), @intFromPtr(b.data.str.ptr));
            if (ord == .Equal) {
                ord = Rml.compare(self.str.len, b.data.str.len);
            }
        }

        return ord;
    }

    pub fn onFormat(self: ptr(Memory), writer: Rml.Writer) Error! void {
        // TODO: escape non-ascii & control etc chars
        try writer.data.print("{s}", .{self.str});
    }

    pub fn text(self: ptr(Memory)) Rml.str {
        return self.str;
    }
};

