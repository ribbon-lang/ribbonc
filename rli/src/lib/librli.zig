const std = @import("std");

const MiscUtils = @import("Utils").Misc;

const Config = @import("Config");

const Core = @import("Core");
const Builtin = @import("Builtin");
const Extern = @import("Utils").Extern;
const HeaderGenUtils = @import("Utils").Build.HeaderGenUtils;

const gc = @import("bdwgc");

const log = std.log.scoped(.librli);

pub const std_options = std.Options {
    .log_level = .warn,
    .logFn = MiscUtils.FilteredLogger(Config.LOG_SCOPES),
};

inline fn tryCall(err_out: ?*BB_Error, func: anytype, args: anytype) ?@typeInfo(@typeInfo(@TypeOf(func)).@"fn".return_type.?).error_union.payload {
    if (@call(.always_inline, func, args)) |res| {
        return res;
    } else |err| {
        if (err_out) |ptr| ptr.* = BB_Error.fromNative(err);
    }

    return null;
}

inline fn sliceFromCStr(s: BB_CStr) []const u8 {
    return std.mem.span(@as([*:0]const u8, @ptrCast(s)));
}

inline fn cStrFromSlice(s: [:0]const u8) BB_CStr {
    return @ptrCast(s.ptr);
}

inline fn calcAlignLog2(alignment: usize) !u8 {
    if (alignment != 0) {
        if (std.math.isPowerOfTwo(alignment)) {
            const x = std.math.log2(alignment);

            if (x <= std.math.maxInt(u8)) {
                return @truncate(x);
            } else {
                return error.InvalidAlignment;
            }
        } else {
            return error.InvalidAlignment;
        }
    } else {
        return 1;
    }
}

inline fn allocatorFromExtern(allocator: BB_Allocator) std.mem.Allocator {
    return .{
        .ptr = allocator.ptr,
        .vtable = @ptrCast(@constCast(@alignCast(allocator.vtable))),
    };
}

inline fn externFromAllocator(allocator: std.mem.Allocator) BB_Allocator {
    return .{
        .ptr = allocator.ptr,
        .vtable = @ptrCast(@constCast(@alignCast(allocator.vtable))),
    };
}

const opaquetype = HeaderGenUtils.opaquetype;
const customtype = HeaderGenUtils.customtype;

pub const @"HEADER-GENERATION-DATA" = HeaderGenUtils.MakeData(struct {
    pub const CustomType = union(enum) {
        generative: void,
        @"enum": struct {
            suffix: ?[]const u8,
            variants: []const []const u8,
        },
        platform: struct {
            linux: []const u8,
            windows: []const u8,
        },
        function: struct {
            params: []const u8,
            retTy: []const u8,
        },

        pub fn render(self: CustomType, name: []const u8, expr: []const u8, generator: anytype, writer: anytype) anyerror!void {
            _ = generator;
            _ = expr;

            switch (self) {
                .generative => return error.InvalidType,
                .@"enum" => |t| {
                    try writer.print("typedef enum {s} {{", .{name});
                    if (t.variants.len > 1) {
                        try writer.writeAll("\n");
                        for (t.variants) |x| {
                            if (t.suffix) |sfx| {
                                try writer.print("    {s}{s}_{s},\n", .{ prefix, x, sfx });
                            } else {
                                try writer.print("    {s}{s},\n", .{ prefix, x });
                            }
                        }
                    } else {
                        for (t.variants) |x| {
                            if (t.suffix) |sfx| {
                                try writer.print(" {s}{s}_{s} ", .{ prefix, x, sfx });
                            } else {
                                try writer.print(" {s}{s} ", .{ prefix, x });
                            }
                        }
                    }
                    try writer.print("}} {s};", .{name});
                },
                .platform => |p| {
                    try writer.print(
                        \\#ifdef __linux__
                        \\    typedef {s} {s};
                        \\#elif _WIN32
                        \\    typedef {s} {s};
                        \\#endif
                    , .{ p.linux, name, p.windows, name });
                },
                .function => |f| {
                    try writer.print("typedef {s} (*{s}) ({s});", .{ f.retTy, name, f.params });
                },
            }
        }
    };

    pub const customTypes = .{
        .BB_FileHandle = CustomType{ .platform = .{ .linux = "int", .windows = "void*" } },
        .BB_Finalizer = CustomType{ .function = .{ .retTy = "void", .params = "void* object, void* userdata" } },
    };

    pub const ignoredDecls = .{};

    pub const head =
        \\#include <stddef.h>
        \\#include <stdint.h>
        \\#include <stdbool.h>
    ;

    pub const foot = "";

    pub const prefix = "BB_";

    pub const enumSuffixes = .{
        .BB_EnvName = "ENV",
    };

    pub const procArgs = .{
        .BB_Proc = .{
            .{ *Core.Interpreter, "interpreter" },
            .{ *const Core.Source.Attr, "attr" },
            .{ *Core.Interpreter.ExternMessage, "msg" },
            .{ *Core.SExpr, "out_result" },
            .{ Core.SExpr, "args" },
        },
        .BB_HasherProc = .{
            .{ *u32, "state" },
            .{ [*]const u8, "bytes" },
            .{ usize, "bytes_len" },
        },
        .BB_VCompareProc = .ignore,
        .BB_VFormatProc = .ignore,
        .BB_VHasherProc = .ignore,
        .BB_VFinalizerProc = .ignore,
    };
});

pub const BB_Error: customtype = Extern.Error;
pub const BB_EnvName: type = Builtin.EnvName;
pub const BB_FunctionKind: type = Core.SExpr.Types.Function.Kind;
pub const BB_Message: type = Core.Interpreter.ExternMessage;
pub const BB_Ordering: type = MiscUtils.Ordering;
pub const BB_Tag: type = Core.SExpr.Tag;
pub const BB_Attr: opaquetype = Core.Source.Attr;
pub const BB_Arena: opaquetype = std.heap.ArenaAllocator;
pub const BB_Context: opaquetype = Core.Context;
pub const BB_Interpreter: opaquetype = Core.Interpreter;
pub const BB_Parser: opaquetype = Core.Parser;

pub const BB_CStr: type = [*:0]const c_char;
pub const BB_UStr: type = Extern.UStr;
pub const BB_Unit: type = MiscUtils.Unit;
pub const BB_SExprData: type = Core.SExpr.Data;
pub const BB_SExpr: type = Core.SExpr;
pub const BB_Obj_Symbol: type = Core.SExpr.Types.Symbol;
pub const BB_Obj_String: type = Core.SExpr.Types.String;
pub const BB_Obj_Cons: type = Core.SExpr.Types.Cons;
pub const BB_Obj_Function: type = Core.SExpr.Types.Function;
pub const BB_Obj_Builtin: opaquetype = Core.SExpr.Types.Builtin;

pub const BB_Writer: customtype = Extern.Writer;
pub const BB_HasherProc: type = Extern.Hasher.Proc;
pub const BB_Hasher: type = Extern.Hasher;
pub const BB_VCompareProc: type = BB_VTable.CompareProc;
pub const BB_VFormatProc: type = BB_VTable.FormatProc;
pub const BB_VHasherProc: type = BB_VTable.HasherProc;
pub const BB_VFinalizerProc: type = BB_VTable.FinalizerProc;
pub const BB_VTable: type = Core.SExpr.Types.ExternData.VTable(anyopaque);
pub const BB_Obj_ExternData: type = Core.SExpr.Types.ExternData;
pub const BB_Proc: type = Core.SExpr.Types.ExternFunction.Proc;
pub const BB_Obj_ExternFunction: type = Core.SExpr.Types.ExternFunction;
pub const BB_Allocator: type = extern struct {
    ptr: *anyopaque,
    vtable: *anyopaque,
};
pub const BB_FileHandle: customtype = std.fs.File.Handle;
pub const BB_Finalizer: customtype = gc.Finalizer;

pub const BB_Pos: type = Core.Source.Pos;
pub const BB_Opt_Pos: customtype = Extern.Option(BB_Pos);

pub const BB_Range: type = extern struct {
    start: BB_Opt_Pos,
    end: BB_Opt_Pos,

    pub inline fn fromNative(range: Core.Source.Range) BB_Range {
        return .{
            .start = BB_Opt_Pos.fromNative(range.start),
            .end = BB_Opt_Pos.fromNative(range.end),
        };
    }

    pub inline fn toNative(self: BB_Range) Core.Source.Range {
        return Core.Source.Range{
            .start = self.start.toNative(),
            .end = self.end.toNative(),
        };
    }
};
pub const BB_Opt_Range: customtype = Extern.Option(BB_Range);

pub const BB_CAttr: type = extern struct {
    context: *BB_Context,
    filename: BB_UStr,
    range: BB_Opt_Range,

    fn fromNative(attr: Core.Source.Attr) BB_CAttr {
        return .{
            .context = attr.context,
            .filename = BB_UStr.fromNative(attr.filename),
            .range = if (attr.range) |x| BB_Opt_Range.Some(BB_Range.fromNative(x)) else BB_Opt_Range.None,
        };
    }

    fn toNative(self: BB_CAttr) Core.Source.Attr {
        return Core.Source.Attr{
            .context = self.context,
            .filename = self.filename.toNative(),
            .range = if (self.range.toNative()) |x| x.toNative() else null,
        };
    }
};

pub export fn BB_Attr_read(attr: *BB_Attr) BB_CAttr {
    return BB_CAttr.fromNative(attr.*);
}

pub export fn BB_Attr_write(attr: *BB_Attr, cAttr: BB_CAttr) void {
    attr.* = cAttr.toNative();
}

pub export fn BB_Arena_init(err_out: ?*BB_Error) ?*BB_Arena {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);

    const ptr = if (arena.allocator().create(std.heap.ArenaAllocator)) |ptr| ptr else |err| {
        if (err_out) |ptr| ptr.* = BB_Error.fromNative(err);
        arena.deinit();
        return null;
    };

    ptr.* = arena;

    return ptr;
}

pub export fn BB_Arena_deinit(arena: *BB_Arena) void {
    arena.deinit();
}

pub export fn BB_Arena_allocator(arena: *BB_Arena) BB_Allocator {
    return externFromAllocator(arena.allocator());
}

pub export fn BB_GC_allocator() BB_Allocator {
    return externFromAllocator(gc.allocator());
}

pub export fn BB_Allocator_alloc(allocator: BB_Allocator, size: usize, alignment: usize, err_out: ?*BB_Error) ?*anyopaque {
    const alloc = allocatorFromExtern(allocator);
    const alignLog2 = calcAlignLog2(alignment) catch |err| {
        if (err_out) |ptr| ptr.* = BB_Error.fromNative(err);
        return null;
    };

    if (alloc.rawAlloc(size, alignLog2, @returnAddress())) |buf| {
        return @ptrCast(buf);
    } else {
        if (err_out) |ptr| ptr.* = BB_Error.fromNative(error.OutOfMemory);
    }
    return null;
}

pub export fn BB_Allocator_free(allocator: BB_Allocator, buf: *anyopaque, size: usize, alignment: usize) void {
    const alloc = allocatorFromExtern(allocator);
    const alignLog2 = calcAlignLog2(alignment) catch unreachable;
    alloc.rawFree(@as([*]u8, @ptrCast(buf))[0..size], alignLog2, @returnAddress());
}

pub export fn BB_Context_initAllocator(allocator: BB_Allocator, err_out: ?*BB_Error) ?*BB_Context {
    return tryCall(err_out, BB_Context.initAllocator, .{allocatorFromExtern(allocator)});
}

pub export fn BB_Context_initGc(err_out: ?*BB_Error) ?*BB_Context {
    return tryCall(err_out, BB_Context.initGc, .{});
}

pub export fn BB_Context_deinit(ctx: *BB_Context) void {
    ctx.deinit();
}

pub export fn BB_Context_new(ctx: *BB_Context, value: *anyopaque, size: usize, alignment: usize, err_out: ?*BB_Error) ?*anyopaque {
    const ptr = BB_Allocator_alloc(externFromAllocator(ctx.allocator), size, alignment, err_out) orelse return null;

    @memcpy(@as([*]u8, @ptrCast(ptr))[0..size], @as([*]u8, @ptrCast(value))[0..size]);

    return ptr;
}

pub export fn BB_Context_newBuffer(ctx: *BB_Context, values: *anyopaque, count: usize, size: usize, alignment: usize, err_out: ?*BB_Error) ?*anyopaque {
    const len = size * count;
    const ptr = BB_Allocator_alloc(externFromAllocator(ctx.allocator), len, alignment, err_out) orelse return null;

    @memcpy(@as([*]u8, @ptrCast(ptr))[0..len], @as([*]u8, @ptrCast(values))[0..len]);

    return ptr;
}

pub export fn BB_Context_setFinalizer(ctx: *BB_Context, ptr: *anyopaque, finalizer: BB_Finalizer, userdata: *anyopaque) void {
    if (!ctx.isGc) return;

    gc.registerFinalizer(ptr, finalizer, userdata, null, null);
}

pub export fn BB_Context_collectGarbage(ctx: *BB_Context) void {
    ctx.collectGarbage();
}

pub export fn BB_Context_genId(ctx: *BB_Context) u64 {
    return ctx.genId();
}

pub export fn BB_Context_genSymbol(ctx: *BB_Context, symbol_out: *BB_UStr, err_out: ?*BB_Error) void {
    if (tryCall(err_out, BB_Context.genSymbol, .{ctx})) |buf| {
        symbol_out.* = BB_UStr.fromNative(buf);
    }
}

pub export fn BB_Context_bindSymbolU(ctx: *BB_Context, value: BB_UStr, symbol_out: *BB_UStr, err_out: ?*BB_Error) void {
    if (tryCall(err_out, BB_Context.bindSymbol, .{ ctx, value.toNative() })) |buf| {
        symbol_out.* = BB_UStr.fromNative(buf);
    }
}

pub export fn BB_Context_bindSymbolC(ctx: *BB_Context, value: BB_CStr, symbol_out: *BB_UStr, err_out: ?*BB_Error) void {
    if (tryCall(err_out, BB_Context.bindSymbol, .{ ctx, sliceFromCStr(value) })) |buf| {
        symbol_out.* = BB_UStr.fromNative(buf);
    }
}

pub export fn BB_Context_resetSymbolInterner(ctx: *BB_Context) void {
    ctx.resetSymbolInterner();
}

pub export fn BB_Context_bindAttrU(ctx: *BB_Context, fileName: BB_UStr, range: ?*BB_Range, err_out: ?*BB_Error) ?*BB_Attr {
    return tryCall(err_out, BB_Context.bindAttr, .{ ctx, fileName.toNative(), if (range) |r| r.toNative() else null });
}

pub export fn BB_Context_bindAttrC(ctx: *BB_Context, fileName: BB_CStr, range: ?*BB_Range, err_out: ?*BB_Error) ?*BB_Attr {
    return tryCall(err_out, BB_Context.bindAttr, .{ ctx, sliceFromCStr(fileName), if (range) |r| r.toNative() else null });
}

pub export fn BB_Context_bindAttrExistingFileU(ctx: *BB_Context, fileName: BB_UStr, range: ?*BB_Range, err_out: ?*BB_Error) ?*BB_Attr {
    return tryCall(err_out, BB_Context.bindAttrExistingFile, .{ ctx, fileName.toNative(), if (range) |r| r.toNative() else null });
}

pub export fn BB_Context_bindAttrExistingFileC(ctx: *BB_Context, fileName: BB_CStr, range: ?*BB_Range, err_out: ?*BB_Error) ?*BB_Attr {
    return tryCall(err_out, BB_Context.bindAttrExistingFile, .{ ctx, sliceFromCStr(fileName), if (range) |r| r.toNative() else null });
}

pub export fn BB_Interpreter_init(ctx: *BB_Context, err_out: ?*BB_Error) ?*BB_Interpreter {
    return tryCall(err_out, BB_Interpreter.init, .{ctx});
}

pub export fn BB_Interpreter_deinit(interpreter: *BB_Interpreter) void {
    interpreter.deinit();
}

pub export fn BB_Interpreter_eval(interpreter: *BB_Interpreter, sexpr: BB_SExpr, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_Interpreter.eval, .{ interpreter, sexpr })) |sexprR| {
        return sexprR;
    }

    return interpreter.context.nil;
}

pub export fn BB_Interpreter_getEnv(interpreter: *BB_Interpreter) BB_SExpr {
    return interpreter.env;
}

pub export fn BB_Interpreter_setEnv(interpreter: *BB_Interpreter, env: BB_SExpr, err_out: ?*BB_Error) void {
    if (tryCall(err_out, BB_Interpreter.validateEnv, .{env})) |_| {
        interpreter.env = env;
    }
}

pub export fn BB_Interpreter_bindBuiltinEnvs(interpreter: *BB_Interpreter, output_env: BB_SExpr, builtin_env: BB_EnvName, err_out: ?*BB_Error) void {
    _ = tryCall(err_out, BB_Interpreter.bindBuiltinEnv, .{ interpreter, output_env, builtin_env });
}

pub export fn BB_Parser_init(ctx: *BB_Context, err_out: ?*BB_Error) ?*BB_Parser {
    if (tryCall(err_out, BB_Parser.init, .{ctx})) |parser| {
        return parser;
    }

    return null;
}

pub export fn BB_Parser_deinit(parser: *BB_Parser) void {
    parser.deinit();
}

pub export fn BB_Parser_setFileNameU(parser: *BB_Parser, filename: BB_UStr, err_out: ?*BB_Error) void {
    _ = tryCall(err_out, BB_Parser.setFileName, .{ parser, filename.toNative() });
}

pub export fn BB_Parser_setFileNameC(parser: *BB_Parser, filename: BB_CStr, err_out: ?*BB_Error) void {
    _ = tryCall(err_out, BB_Parser.setFileName, .{ parser, sliceFromCStr(filename) });
}

pub export fn BB_Parser_setInputU(parser: *BB_Parser, src: BB_UStr, offset: ?*BB_Pos) void {
    parser.setInput(src.toNative(), if (offset) |o| o.* else null);
}

pub export fn BB_Parser_setInputC(parser: *BB_Parser, src: BB_CStr, offset: ?*BB_Pos) void {
    parser.setInput(sliceFromCStr(src), if (offset) |o| o.* else null);
}

pub export fn BB_Parser_isEof(parser: *BB_Parser) bool {
    return parser.isEof();
}

pub export fn BB_Parser_sexprP(parser: *BB_Parser, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_Parser.scanSExprP, .{parser})) |sexprM| {
        if (sexprM) |sexpr| {
            return sexpr;
        }
    }

    return parser.context.nil;
}

pub export fn BB_SExpr_Nil(attr: *const BB_Attr, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.Nil, .{attr})) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_Bool(attr: *const BB_Attr, value: bool, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.Bool, .{ attr, value })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_Int(attr: *const BB_Attr, value: i64, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.Int, .{ attr, value })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_Float(attr: *const BB_Attr, value: f64, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.Float, .{ attr, value })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_Char(attr: *const BB_Attr, value: u32, err_out: ?*BB_Error) BB_SExpr {
    if (value > 0x10FFFF) {
        if (err_out) |ptr| ptr.* = @enumFromInt(@intFromError(error.InvalidChar));
        return attr.context.nil;
    }

    if (tryCall(err_out, BB_SExpr.Char, .{ attr, @as(u21, @truncate(value)) })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_SymbolU(attr: *const BB_Attr, value: BB_UStr, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.Symbol, .{ attr, value.toNative() })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_SymbolC(attr: *const BB_Attr, value: BB_CStr, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.Symbol, .{ attr, sliceFromCStr(value) })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_GenSymbol(attr: *const BB_Attr, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.GenSymbol, .{attr})) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_StringU(attr: *const BB_Attr, value: BB_UStr, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.String, .{ attr, value.toNative() })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_StringC(attr: *const BB_Attr, value: BB_CStr, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.String, .{ attr, sliceFromCStr(value) })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_StringPreallocatedU(attr: *const BB_Attr, buf: BB_UStr, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.StringPreallocated, .{ attr, buf.toNative() })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_StringPreallocatedC(attr: *const BB_Attr, buf: BB_CStr, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.StringPreallocated, .{ attr, sliceFromCStr(buf) })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_ListTail(attr: *const BB_Attr, values: [*]const BB_SExpr, values_len: usize, tail: BB_SExpr, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.ListTail, .{ attr, values[0..values_len], tail })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_List(attr: *const BB_Attr, values: [*]const BB_SExpr, values_len: usize, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.List, .{ attr, values[0..values_len] })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_Cons(attr: *const BB_Attr, car: BB_SExpr, cdr: BB_SExpr, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.Cons, .{ attr, car, cdr })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_Function(attr: *const BB_Attr, kind: BB_FunctionKind, args: BB_SExpr, env: BB_SExpr, body: BB_SExpr, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.Function, .{ attr, kind, args, env, body })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_ExternDataU(attr: *const BB_Attr, ptr: *anyopaque, vtable: *const BB_VTable, typeName: BB_UStr, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.ExternDataErased, .{ attr, ptr, vtable, typeName.toNative() })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_ExternDataC(attr: *const BB_Attr, ptr: *anyopaque, vtable: *const BB_VTable, typeName: BB_CStr, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.ExternDataErased, .{ attr, ptr, vtable, sliceFromCStr(typeName) })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_ExternFunctionU(attr: *const BB_Attr, name: BB_UStr, proc: BB_Proc, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.ExternFunction, .{ attr, name.toNative(), proc })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_ExternFunctionC(attr: *const BB_Attr, name: BB_CStr, proc: BB_Proc, err_out: ?*BB_Error) BB_SExpr {
    if (tryCall(err_out, BB_SExpr.ExternFunction, .{ attr, sliceFromCStr(name), proc })) |sexpr| {
        return sexpr;
    }

    return attr.context.nil;
}

pub export fn BB_SExpr_compare(a: BB_SExpr, b: BB_SExpr) BB_Ordering {
    return a.compare(b);
}

pub export fn BB_SExpr_getTag(sexpr: BB_SExpr) BB_Tag {
    return sexpr.getTag();
}

pub export fn BB_SExpr_forcePtr(sexpr: BB_SExpr) *anyopaque {
    return sexpr.forcePtr(anyopaque);
}

pub export fn BB_SExpr_isNil(sexpr: BB_SExpr) bool {
    return sexpr.isNil();
}

pub export fn BB_SExpr_isBool(sexpr: BB_SExpr) bool {
    return sexpr.isBool();
}

pub export fn BB_SExpr_isInt(sexpr: BB_SExpr) bool {
    return sexpr.isInt();
}

pub export fn BB_SExpr_isFloat(sexpr: BB_SExpr) bool {
    return sexpr.isFloat();
}

pub export fn BB_SExpr_isChar(sexpr: BB_SExpr) bool {
    return sexpr.isChar();
}

pub export fn BB_SExpr_isSymbol(sexpr: BB_SExpr) bool {
    return sexpr.isSymbol();
}

pub export fn BB_SExpr_isExactSymbolU(sexpr: BB_SExpr, value: BB_UStr) bool {
    return sexpr.isExactSymbol(value.toNative());
}

pub export fn BB_SExpr_isExactSymbolC(sexpr: BB_SExpr, value: BB_CStr) bool {
    return sexpr.isExactSymbol(sliceFromCStr(value));
}

pub export fn BB_SExpr_isString(sexpr: BB_SExpr) bool {
    return sexpr.isString();
}

pub export fn BB_SExpr_isExactStringU(sexpr: BB_SExpr, value: BB_UStr) bool {
    return sexpr.isExactString(value.toNative());
}

pub export fn BB_SExpr_isExactStringC(sexpr: BB_SExpr, value: BB_CStr) bool {
    return sexpr.isExactString(sliceFromCStr(value));
}

pub export fn BB_SExpr_isCons(sexpr: BB_SExpr) bool {
    return sexpr.isCons();
}

pub export fn BB_SExpr_isList(sexpr: BB_SExpr) bool {
    return sexpr.isList();
}

pub export fn BB_SExpr_isFunction(sexpr: BB_SExpr) bool {
    return sexpr.isFunction();
}

pub export fn BB_SExpr_isLambda(sexpr: BB_SExpr) bool {
    return sexpr.isLambda();
}

pub export fn BB_SExpr_isMacro(sexpr: BB_SExpr) bool {
    return sexpr.isMacro();
}

pub export fn BB_SExpr_isBuiltin(sexpr: BB_SExpr) bool {
    return sexpr.isBuiltin();
}

pub export fn BB_SExpr_isExternData(sexpr: BB_SExpr) bool {
    return sexpr.isExternData();
}

pub export fn BB_SExpr_isNamedExternDataU(sexpr: BB_SExpr, typeName: BB_UStr) bool {
    return sexpr.isNamedExternData(typeName.toNative());
}

pub export fn BB_SExpr_isNamedExternDataC(sexpr: BB_SExpr, typeName: BB_CStr) bool {
    return sexpr.isNamedExternData(sliceFromCStr(typeName));
}

pub export fn BB_SExpr_isExternFunction(sexpr: BB_SExpr) bool {
    return sexpr.isExternFunction();
}

pub export fn BB_SExpr_isInContext(sexpr: BB_SExpr, context: *const BB_Context) bool {
    return sexpr.isInContext(context);
}

pub export fn BB_SExpr_getContext(sexpr: BB_SExpr) *BB_Context {
    return sexpr.getContext();
}

pub export fn BB_SExpr_getAttr(sexpr: BB_SExpr) *const BB_Attr {
    return sexpr.getAttr();
}

pub export fn BB_SExpr_forceNil(sexpr: BB_SExpr) BB_Unit {
    return sexpr.forceNil();
}

pub export fn BB_SExpr_forceBool(sexpr: BB_SExpr) bool {
    return sexpr.forceBool();
}

pub export fn BB_SExpr_forceInt(sexpr: BB_SExpr) i64 {
    return sexpr.forceInt();
}

pub export fn BB_SExpr_forceFloat(sexpr: BB_SExpr) f64 {
    return sexpr.forceFloat();
}

pub export fn BB_SExpr_forceChar(sexpr: BB_SExpr) u32 {
    return sexpr.forceChar();
}

pub export fn BB_SExpr_forceSymbol(sexpr: BB_SExpr) *BB_Obj_Symbol {
    return sexpr.forceSymbol();
}

pub export fn BB_SExpr_forceSymbolSlice(sexpr: BB_SExpr) BB_UStr {
    return BB_UStr.fromNative(sexpr.forceSymbolSlice());
}

pub export fn BB_SExpr_forceString(sexpr: BB_SExpr) *BB_Obj_String {
    return sexpr.forceString();
}

pub export fn BB_SExpr_forceStringSlice(sexpr: BB_SExpr) BB_UStr {
    return BB_UStr.fromNative(sexpr.forceStringSlice());
}

pub export fn BB_SExpr_forceCons(sexpr: BB_SExpr) *BB_Obj_Cons {
    return sexpr.forceCons();
}

pub export fn BB_SExpr_forceFunction(sexpr: BB_SExpr) *BB_Obj_Function {
    return sexpr.forceFunction();
}

pub export fn BB_SExpr_forceBuiltin(sexpr: BB_SExpr) *BB_Obj_Builtin {
    return sexpr.forceBuiltin();
}

pub export fn BB_SExpr_forceExternData(sexpr: BB_SExpr) *BB_Obj_ExternData {
    return sexpr.forceExternData();
}

pub export fn BB_SExpr_forceExternDataPtr(sexpr: BB_SExpr) *anyopaque {
    return sexpr.forceExternDataPtr();
}

pub export fn BB_SExpr_forceExternDataNamedPtrU(sexpr: BB_SExpr, typeName: BB_UStr) *anyopaque {
    return sexpr.forceExternDataNamedPtr(typeName.toNative());
}

pub export fn BB_SExpr_forceExternDataNamedPtrC(sexpr: BB_SExpr, typeName: BB_CStr) *anyopaque {
    return sexpr.forceExternDataNamedPtr(sliceFromCStr(typeName));
}

pub export fn BB_SExpr_forceExternFunction(sexpr: BB_SExpr) *BB_Obj_ExternFunction {
    return sexpr.forceExternFunction();
}

pub export fn BB_SExpr_forceNamedExternFunctionU(sexpr: BB_SExpr, name: BB_UStr) *BB_Obj_ExternFunction {
    return sexpr.forceNamedExternFunction(name.toNative());
}

pub export fn BB_SExpr_forceNamedExternFunctionC(sexpr: BB_SExpr, name: BB_CStr) *BB_Obj_ExternFunction {
    return sexpr.forceNamedExternFunction(sliceFromCStr(name));
}

pub export fn BB_SExpr_forceExternFunctionProc(sexpr: BB_SExpr) BB_Proc {
    return sexpr.forceExternFunctionProc();
}

pub export fn BB_SExpr_forceExternFunctionNamedProcU(sexpr: BB_SExpr, name: BB_UStr) BB_Proc {
    return sexpr.forceExternFunctionNamedProc(name.toNative());
}

pub export fn BB_SExpr_forceExternFunctionNamedProcC(sexpr: BB_SExpr, name: BB_CStr) BB_Proc {
    return sexpr.forceExternFunctionNamedProc(sliceFromCStr(name));
}

pub export fn BB_SExpr_castNil(sexpr: BB_SExpr, out_unit: *BB_Unit) bool {
    if (sexpr.castNil()) |unit| {
        out_unit.* = unit;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castBool(sexpr: BB_SExpr, out_bool: *bool) bool {
    if (sexpr.castBool()) |boolean| {
        out_bool.* = boolean;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castInt(sexpr: BB_SExpr, out_int: *i64) bool {
    if (sexpr.castInt()) |int| {
        out_int.* = int;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castFloat(sexpr: BB_SExpr, out_float: *f64) bool {
    if (sexpr.castFloat()) |float| {
        out_float.* = float;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castChar(sexpr: BB_SExpr, out_char: *u32) bool {
    if (sexpr.castChar()) |char| {
        out_char.* = char;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castSymbol(sexpr: BB_SExpr, out_symbol: **BB_Obj_Symbol) bool {
    if (sexpr.castSymbol()) |symbol| {
        out_symbol.* = symbol;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castSymbolSlice(sexpr: BB_SExpr, out_symbol: *BB_UStr) bool {
    if (sexpr.castSymbolSlice()) |symbol| {
        out_symbol.* = BB_UStr.fromNative(symbol);
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castString(sexpr: BB_SExpr, out_string: **BB_Obj_String) bool {
    if (sexpr.castString()) |string| {
        out_string.* = string;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castStringSlice(sexpr: BB_SExpr, out_string: *BB_UStr) bool {
    if (sexpr.castStringSlice()) |string| {
        out_string.* = BB_UStr.fromNative(string);
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castCons(sexpr: BB_SExpr, out_cons: **BB_Obj_Cons) bool {
    if (sexpr.castCons()) |cons| {
        out_cons.* = cons;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castFunction(sexpr: BB_SExpr, out_function: **BB_Obj_Function) bool {
    if (sexpr.castFunction()) |function| {
        out_function.* = function;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castBuiltin(sexpr: BB_SExpr, out_builtin: **BB_Obj_Builtin) bool {
    if (sexpr.castBuiltin()) |builtin| {
        out_builtin.* = builtin;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castExternData(sexpr: BB_SExpr, out_extern_data: **BB_Obj_ExternData) bool {
    if (sexpr.castExternData()) |extern_data| {
        out_extern_data.* = extern_data;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castNamedExternDataU(sexpr: BB_SExpr, typeName: BB_UStr, out_extern_data: **BB_Obj_ExternData) bool {
    if (sexpr.castNamedExternData(typeName.toNative())) |extern_data| {
        out_extern_data.* = extern_data;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castNamedExternDataC(sexpr: BB_SExpr, typeName: BB_CStr, out_extern_data: **BB_Obj_ExternData) bool {
    if (sexpr.castNamedExternData(sliceFromCStr(typeName))) |extern_data| {
        out_extern_data.* = extern_data;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castExternDataPtr(sexpr: BB_SExpr, out_ptr: **anyopaque) bool {
    if (sexpr.castExternDataPtr()) |ptr| {
        out_ptr.* = ptr;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castExternDataNamedPtrU(sexpr: BB_SExpr, typeName: BB_UStr, out_ptr: **anyopaque) bool {
    if (sexpr.castExternDataNamedPtr(typeName.toNative())) |ptr| {
        out_ptr.* = ptr;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castExternDataNamedPtrC(sexpr: BB_SExpr, typeName: BB_CStr, out_ptr: **anyopaque) bool {
    if (sexpr.castExternDataNamedPtr(sliceFromCStr(typeName))) |ptr| {
        out_ptr.* = ptr;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castExternFunction(sexpr: BB_SExpr, out_extern_function: **BB_Obj_ExternFunction) bool {
    if (sexpr.castExternFunction()) |extern_function| {
        out_extern_function.* = extern_function;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castNamedExternFunctionU(sexpr: BB_SExpr, name: BB_UStr, out_extern_function: **BB_Obj_ExternFunction) bool {
    if (sexpr.castNamedExternFunction(name.toNative())) |extern_function| {
        out_extern_function.* = extern_function;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castNamedExternFunctionC(sexpr: BB_SExpr, name: BB_CStr, out_extern_function: **BB_Obj_ExternFunction) bool {
    if (sexpr.castNamedExternFunction(sliceFromCStr(name))) |extern_function| {
        out_extern_function.* = extern_function;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castExternFunctionProc(sexpr: BB_SExpr, out_proc: *BB_Proc) bool {
    if (sexpr.castExternFunctionProc()) |proc| {
        out_proc.* = proc;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castExternFunctionNamedProcU(sexpr: BB_SExpr, name: BB_UStr, out_proc: *BB_Proc) bool {
    if (sexpr.castExternFunctionNamedProc(name.toNative())) |proc| {
        out_proc.* = proc;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_castExternFunctionNamedProcC(sexpr: BB_SExpr, name: BB_CStr, out_proc: *BB_Proc) bool {
    if (sexpr.castExternFunctionNamedProc(sliceFromCStr(name))) |proc| {
        out_proc.* = proc;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_coerceNativeBool(sexpr: BB_SExpr) bool {
    return sexpr.coerceNativeBool();
}

pub export fn BB_SExpr_coerceNativeInt(sexpr: BB_SExpr, out_int: *i64) bool {
    if (sexpr.coerceNativeInt()) |int| {
        out_int.* = int;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_coerceNativeFloat(sexpr: BB_SExpr, out_float: *f64) bool {
    if (sexpr.coerceNativeFloat()) |float| {
        out_float.* = float;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_coerceNativeChar(sexpr: BB_SExpr, out_char: *u32) bool {
    if (sexpr.coerceNativeChar()) |char| {
        out_char.* = char;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_listLen(sexpr: BB_SExpr, out_len: *usize) bool {
    if (sexpr.listLen()) |len| {
        out_len.* = len;
        return true;
    }
    return false;
}

pub export fn BB_SExpr_hashWith(sexpr: BB_SExpr, hasher: *BB_Hasher) void {
    sexpr.hashWith(hasher);
}

pub export fn BB_SExpr_print(sexpr: BB_SExpr, out: BB_FileHandle, err_out: ?*BB_Error) void {
    const f = std.fs.File{ .handle = out };
    f.writer().print("{}", .{sexpr}) catch |err| {
        if (err_out) |ptr| ptr.* = BB_Error.fromNative(err);
    };
}

pub export fn BB_Error_name(err: BB_Error) BB_CStr {
    if (err == BB_Error.Okay) return cStrFromSlice("Okay");

    return cStrFromSlice(@errorName(err.toNative()));
}
