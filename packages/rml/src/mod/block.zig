const std = @import("std");

const Rml = @import("root.zig");
const Ordering = Rml.Ordering;
const Error = Rml.Error;
const OOM = Rml.OOM;
const ptr = Rml.ptr;
const Obj = Rml.Obj;
const ObjData = Rml.ObjData;
const Object = Rml.Object;
const Writer = Rml.Writer;
const getObj = Rml.getObj;
const getTypeId = Rml.getTypeId;
const forceObj = Rml.forceObj;
const getRml = Rml.getRml;


pub const Block = Obj(Memory);

pub const BlockKind = enum {
    doc,
    curly,
    square,
    paren,

    pub fn toOpenStr(self: BlockKind) []const u8 {
        return switch (self) {
            .doc => "",
            .curly => "{",
            .square => "[",
            .paren => "(",
        };
    }

    pub fn toCloseStr(self: BlockKind) []const u8 {
        return switch (self) {
            .doc => "",
            .curly => "}",
            .square => "]",
            .paren => ")",
        };
    }
};

pub const Memory = struct {
    block_kind: BlockKind = .doc,
    array: Rml.array.MemoryUnmanaged(ObjData) = .{},

    pub fn onInit(self: ptr(Memory), block_kind: BlockKind, data: []const Object) OOM! void {
        self.block_kind = block_kind;
        try self.appendSlice(data);
    }

    pub fn onDeinit(self: ptr(Memory)) void {
        self.array.deinit(getRml(self));
    }

    pub fn onCompare(a: ptr(Memory), other: Object) Ordering {
        var ord = Rml.compare(getTypeId(a), other.getHeader().type_id);

        if (ord == .Equal) {
            const b = forceObj(Memory, other);
            defer b.deinit();

            ord = Rml.compare(a.block_kind, b.data.block_kind);

            if (ord == .Equal) {
                ord = Rml.compare(a.array, b.data.array);
            }
        }

        return ord;
    }

    pub fn onFormat(self: ptr(Memory), writer: Writer) Error! void {
        try writer.data.writeAll(self.block_kind.toOpenStr());
        try writer.data.print("{}", .{self.array});
        try writer.data.writeAll(self.block_kind.toCloseStr());
    }

    /// Length of the block.
    pub fn length(self: ptr(Memory)) usize {
        return self.array.length();
    }

    /// Contents of the block.
    /// Pointers to elements in this slice are invalidated by various functions of this ArrayList in accordance with the respective documentation.
    /// In all cases, "invalidated" means that the memory has been passed to an allocator's resize or free function.
    pub fn items(self: ptr(Memory)) []Object {
        return self.array.items();
    }

    /// Extend the block by 1 element.
    /// Allocates more memory as necessary.
    /// Invalidates element pointers if additional memory is needed.
    pub fn append(self: ptr(Memory), obj: Object) OOM! void {
        const rml = getRml(self);
        return self.array.append(rml, obj);
    }

    /// Append the slice of items to the block. Allocates more memory as necessary.
    /// Invalidates element pointers if additional memory is needed.
    pub fn appendSlice(self: ptr(Memory), slice: []const Object) OOM! void {
        const rml = getRml(self);
        return self.array.appendSlice(rml, slice);
    }
};
