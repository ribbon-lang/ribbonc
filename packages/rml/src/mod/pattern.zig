const std = @import("std");

const Rml = @import("root.zig");
const Error = Rml.Error;
const Ordering = Rml.Ordering;
const OOM = Rml.OOM;
const const_ptr = Rml.const_ptr;
const ptr = Rml.ptr;
const Obj = Rml.Obj;
const Object = Rml.Object;
const Procedure = Rml.Procedure;
const Char = Rml.Char;
const String = Rml.String;
const Array = Rml.Array;
const Symbol = Rml.Symbol;
const getHeader = Rml.getHeader;
const getObj = Rml.getObj;
const getRml = Rml.getRml;
const forceObj = Rml.forceObj;

pub const Alias = struct {
    sym: Obj(Symbol),
    sub: Object,

    pub fn deinit(self: Alias) void {
        self.sub.deinit();
    }
};

pub const Optional = struct {
    sym: ?Obj(Symbol),
    sub: Object,

    pub fn deinit(self: Optional) void {
        self.sub.deinit();
    }
};

pub const Sequence = Obj(Array);

pub const Pattern = union(enum) {
    wildcard: void,

    symbol: Obj(Symbol),

    bool_literal: Obj(Rml.Bool),
    int_literal: Obj(Rml.Int),
    float_literal: Obj(Rml.Float),
    char_literal: Obj(Char),
    string_literal: Obj(String),

    escape: Object,
    predicate: Object,
    transformer: Object,

    alias: Alias,
    sequence: Sequence,
    optional: Optional,

    pub fn onDeinit (self: ptr(Pattern)) void {
        switch (self.*) {
            .wildcard => {},
            .symbol => |x| x.deinit(),
            .bool_literal => |x| x.deinit(),
            .int_literal => |x| x.deinit(),
            .float_literal => |x| x.deinit(),
            .char_literal => |x| x.deinit(),
            .string_literal => |x| x.deinit(),
            .escape => |x| x.deinit(),
            .predicate => |x| x.deinit(),
            .transformer => |x| x.deinit(),
            .alias => |x| x.deinit(),
            .sequence => |x| x.deinit(),
            .optional => |x| x.deinit(),
        }
    }
};
