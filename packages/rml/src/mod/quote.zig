const std = @import("std");
const MiscUtils = @import("Utils").Misc;


const Rml = @import("root.zig");
const Ordering = Rml.Ordering;
const Error = Rml.Error;
const OOM = Rml.OOM;
const ptr = Rml.ptr;
const Obj = Rml.Obj;
const ObjData = Rml.ObjData;
const Object = Rml.Object;
const Writer = Rml.Writer;
const Interpreter = Rml.Interpreter;
const getObj = Rml.getObj;
const getTypeId = Rml.getTypeId;
const forceObj = Rml.forceObj;
const getRml = Rml.getRml;
const isAtom = Rml.isAtom;


pub const QuoteKind = enum {
    basic,
    quasi,
    to_quote,
    to_quasi,
    unquote,
    unquote_splice,

    pub fn toStr(self: QuoteKind) []const u8 {
        return switch (self) {
            .basic => "'",
            .quasi => "`",
            .to_quote => "~'",
            .to_quasi => "~`",
            .unquote => ",",
            .unquote_splice => ",@",
        };
    }

    pub fn fromStr(str: []const u8) ?QuoteKind {
        return if (std.mem.eql(u8, str, "'")) .basic
          else if (std.mem.eql(u8, str, "`")) .quasi
          else if (std.mem.eql(u8, str, "~'")) .to_quote
          else if (std.mem.eql(u8, str, "~`")) .to_quasi
          else if (std.mem.eql(u8, str, ",")) .unquote
          else if (std.mem.eql(u8, str, ",@")) .unquote_splice
          else null;
    }
};

pub const Quote = struct {
    kind: QuoteKind,
    body: Object,

    pub fn onInit(self: ptr(Quote), kind: QuoteKind, body: Object) void {
        self.kind = kind;
        self.body = body;
    }

    pub fn onCompare(self: ptr(Quote), other: Object) Ordering {
        var ord = Rml.compare(getTypeId(self), other.getTypeId());

        if (ord == .Equal) {
            const other_quote = forceObj(Quote, other);
            defer other_quote.deinit();

            ord = Rml.compare(self.kind, other_quote.data.kind);

            if (ord == .Equal) {
                ord = self.body.compare(other_quote.data.body);
            }
        }

        return ord;
    }

    pub fn onFormat(self: ptr(Quote), writer: Obj(Writer)) Error! void {
        try writer.data.writeAll(self.kind.toStr());
        try self.body.onFormat(writer);
    }

    pub fn onDeinit(self: ptr(Quote)) void {
        self.body.deinit();
    }
};



pub fn runQuasi(interpreter: ptr(Interpreter), body: Object) !Object {
    if (isAtom(body)) return body;

    MiscUtils.todo(noreturn, interpreter);
}
