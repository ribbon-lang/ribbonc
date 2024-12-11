const std = @import("std");

const Rml = @import("root.zig");
const Error = Rml.Error;
const Ordering = Rml.Ordering;
const OOM = Rml.OOM;
const const_ptr = Rml.const_ptr;
const ptr = Rml.ptr;
const Obj = Rml.Obj;
const Object = Rml.Object;
const String = Rml.String;
const Symbol = Rml.Symbol;
const Procedure = Rml.Procedure;
const getHeader = Rml.getHeader;
const getObj = Rml.getObj;
const getRml = Rml.getRml;
const forceObj = Rml.forceObj;

pub const Pattern = Obj(Memory);

pub const Alias = struct {
    sym: Symbol,
    sub: Object,

    pub fn deinit(self: *Alias) void {
        self.sub.deinit();
    }
};

pub const Optional = struct {
    sym: ?Symbol,
    sub: Object,

    pub fn deinit(self: *Alias) void {
        self.sub.deinit();
    }
};

pub const Sequence = Rml.array.MemoryUnmanaged(Object);

pub const Memory = union(enum) {
    wildcard: void,

    symbol: Symbol,

    bool_literal: Rml.Bool,
    int_literal: Rml.Int,
    float_literal: Rml.Float,
    char_literal: Rml.Char,
    string_literal: Rml.String,

    escape: Object,
    predicate: Object,
    transformer: Object,

    alias: Alias,
    sequence: Sequence,
    optional: Optional,
};
