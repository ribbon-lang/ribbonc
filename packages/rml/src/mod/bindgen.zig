const std = @import("std");
const TextUtils = @import("Utils").Text;
const TypeUtils = @import("Utils").Type;

const Rml = @import("root.zig");
const Symbol = Rml.Symbol;
const Env = Rml.Env;
const Result = Rml.interpreter.Result;
const Error = Rml.Error;
const Ordering = Rml.Ordering;
const OOM = Rml.OOM;
const SymbolAlreadyBound = Rml.SymbolAlreadyBound;
const const_ptr = Rml.const_ptr;
const ptr = Rml.ptr;
const Origin = Rml.Origin;
const Writer = Rml.Writer;
const Obj = Rml.Obj;
const Object = Rml.Object;
const getObj = Rml.getObj;
const getHeader = Rml.getHeader;
const getRml = Rml.getRml;
const forceObj = Rml.forceObj;
const downgradeCast = Rml.downgradeCast;


pub fn bindGlobals(rml: *Rml, env: Env, globals: anytype) (OOM || SymbolAlreadyBound)! void {
    inline for (comptime std.meta.fields(@TypeOf(globals))) |field| {
        const symbol: Symbol = try .init(rml, rml.storage.origin, .{field.name});
        errdefer symbol.deinit();

        const object = try toObject(rml, rml.storage.origin, @field(globals, field.name));
        defer object.deinit();

        try env.data.bind(symbol, object.typeErase());
    }
}

pub fn bindGlobalsConst(rml: *Rml, env: Env, comptime globals: anytype) (OOM || SymbolAlreadyBound)! void {
    inline for (comptime std.meta.fields(@TypeOf(globals))) |field| {
        const symbol: Symbol = try .init(rml, rml.storage.origin, .{field.name});
        errdefer symbol.deinit();

        const object = try toObjectConst(rml, rml.storage.origin, @field(globals, field.name));
        defer object.deinit();

        try env.data.bind(symbol, object.typeErase());
    }
}


pub fn bindNamespaces(rml: *Rml, env: Env, comptime namespaces: anytype) (OOM || SymbolAlreadyBound)! void {
    inline for (comptime std.meta.fields(@TypeOf(namespaces))) |field| {
        const builtinEnv: Env = try .init(rml, rml.storage.origin);
        builtinEnv.data.parent = downgradeCast(env);
        defer std.debug.assert(builtinEnv.getHeader().ref_count == 1);
        defer builtinEnv.deinit();

        const Ns = Namespace(@field(namespaces, field.name));

        const methods = try Ns.methods(rml, rml.storage.origin);
        defer Ns.deinitMethods(methods);

        try builtinEnv.data.bindNamespace(methods);

        try env.data.bind(try Symbol.init(rml, rml.storage.origin, .{field.name}), builtinEnv.typeErase());
    }
}


pub fn Support(comptime T: type) type {
    return struct {
        pub const onCompare = switch (@typeInfo(T)) {
            else => struct {
                pub fn onCompare(a: ptr(T), obj: Object) Ordering {
                    var ord = Rml.compare(getHeader(a).type_id, obj.getHeader().type_id);

                    if (ord == .Equal) {
                        const b = forceObj(T, obj);
                        defer b.deinit();

                        ord = Rml.compare(a.*, b.data.*);
                    }

                    return ord;
                }
            }
        }.onCompare;

        pub const onFormat = switch (T) {
            Rml.char => struct {
                pub fn onFormat(self: ptr(T), writer: Writer) Error! void {
                    var buf = [1]u8{0} ** 4;
                    const len = TextUtils.encode(self.*, buf[0..]) catch 0;
                    try writer.data.print("'{s}'", .{buf[0..len]});
                }
            },
            else => switch(@typeInfo(T)) {
                .pointer => |info| if (@typeInfo(info.child) == .@"fn") struct {
                    pub fn onFormat(self: ptr(T), writer: Writer) Error! void {
                        try writer.data.print("[native-function {s} {x}]", .{fmtNativeType(T), @intFromPtr(self)});
                    }
                } else struct {
                    pub fn onFormat(self: ptr(T), writer: Writer) Error! void {
                        try writer.data.print("[native-{s} {x}]", .{@typeName(T), @intFromPtr(self)});
                    }
                },
                .array => struct {
                    pub fn onFormat(self: ptr(T), writer: Writer) Error! void {
                        try writer.data.print("{any}", .{self.*});
                    }
                },
                else => struct {
                    pub fn onFormat(self: ptr(T), writer: Writer) Error! void {
                        try writer.data.print("{}", .{self.*});
                    }
                },
            },
        }.onFormat;

        pub const onDeinit = switch(@typeInfo(T)) {
            .array => struct {
                pub fn onDeinit(self: ptr(T)) void {
                    Rml.object.refcount.debug("{s}/onDeinit ", .{@typeName(T)});
                    for (self.*) |item| item.deinit();
                }
            },
            else => struct {
                pub fn onDeinit(_: ptr(T)) void {
                    Rml.object.refcount.debug("{s}/onDeinit-auto", .{@typeName(T)});
                }
            }
        }.onDeinit;
    };
}


pub fn fmtNativeType(comptime T: type) []const u8 {
    return comptime switch(T) {
        else => switch (@typeInfo(T)) {
            .void, .null, .undefined, .noreturn => "Nil",
            .@"opaque" => "Opaque",
            .bool => "Bool",
            .int => |info| std.fmt.comptimePrint("{u}{}", .{switch (info.signedness) { .signed => 'S', .unsigned => 'U' }, info.bits}),
            .float => |info| std.fmt.comptimePrint("F{}", .{info.bits}),
            .error_set => "Error",
            .error_union => |info| fmtNativeType(info.error_set) ++ "! " ++ fmtNativeType(info.payload),
            .pointer => |info|
                if (@typeInfo(info.child) == .@"fn") fmtNativeType(info.child)
                else switch (info.size) {
                    .C, .One, .Many =>
                        if (info.alignment == Rml.object.OBJ_ALIGN) fmtNativeType(info.child)
                        else "*" ++ fmtNativeType(info.child),
                    .Slice => "[]" ++ fmtNativeType(info.child),
                },
            .array => |info| std.fmt.comptimePrint("[{}]", .{info.len} ++ fmtNativeType(info.child)),
            .vector => |info| std.fmt.comptimePrint("<{}>", .{info.len} ++ fmtNativeType(info.child)),
            .optional => |info| "?" ++ fmtNativeType(info.child),
            .@"struct" => fmtTypeName(T),
            .@"enum" => fmtTypeName(T),
            .@"union" => fmtTypeName(T),
            .@"fn" => |info| fun: {
                var x: []const u8 = "(";

                for (info.params) |param| {
                    x = x ++ fmtNativeType(param.type.?) ++ " ";
                }

                x = x ++ "-> " ++ fmtNativeType(info.return_type.?);

                break :fun x ++ ")";
            },
            .enum_literal => "Symbol",
            else => fmtTypeName(T),
        }
    };
}

pub fn fmtTypeName(comptime T: type) []const u8 {
    const OBJECT = "object/Obj_object.ObjData_";
    const OBJ_GENERIC = "/Memory_object.ObjData_";
    const OBJ_MEMORY = "/Memory_object/ObjData_object.ObjData_";
    const OBJ_START = "object/Obj_";
    const END_MEMORY = ".Memory";
    const OBJ_END_MEMORY = END_MEMORY ++ "_";
    return comptime fmt: {
        var out = zigNameToPath(@typeName(T));
        if (std.mem.eql(u8, out, OBJECT)) out = "Object";
        if (std.mem.endsWith(u8, out, OBJ_MEMORY)) out = toCamel(out[0..out.len - OBJ_MEMORY.len]);
        if (std.mem.startsWith(u8, out, OBJ_START)) {
            out =
                if (std.mem.endsWith(u8, out, OBJ_END_MEMORY)) toCamel(out[OBJ_START.len..out.len - OBJ_END_MEMORY.len])
                else toCamel(out[OBJ_START.len..]);
        }
        if (std.mem.endsWith(u8, out, END_MEMORY)) out = toCamel(out[0..out.len - END_MEMORY.len]);
        if (std.mem.endsWith(u8, out, OBJ_GENERIC)) out = toCamel(out[0..out.len - OBJ_GENERIC.len]);
        break :fmt out;
    };
}

pub fn toCamel(comptime name: []const u8) []const u8 {
    const uppercaseEdgeChars = " _-";
    comptime var edges = [1]usize {0} ** (name.len + 1);
    comptime var edgesLen = 1;
    inline for (name, 1..) |b, i| {
        if (std.mem.indexOfScalar(u8, uppercaseEdgeChars, b) != null) {
            edges[edgesLen] = i;
            edgesLen += 1;
        }
    }
    comptime var out = [1]u8 {0} ** name.len;
    comptime var outLen = 0;
    comptime var i = 0;
    comptime var edgeIndex = 0;
    inline while (i < name.len) : (i += 1) {
        if (i == edges[edgeIndex]) {
            edgeIndex += 1;

            const offset = if (i == 0) 0 else 1;
            out[outLen] = std.ascii.toUpper(name[i + offset]);
            i += offset;
        } else {
            out[outLen] = name[i];
        }
        outLen += 1;
    }
    return out[0..outLen];
}

pub fn zigNameToPath(comptime name: []const u8) []const u8 {
    @setEvalBranchQuota(10000);
    const ESCAPE_CHARS = " ,;:[]{}()<>+-*/%&|^!~@#$?=";
    comptime var dotIndices = [1]usize {0} ** name.len;
    comptime var dotIndicesLen = 0;
    comptime var escapeIndices = [1]usize {0} ** name.len;
    comptime var escapeIndicesLen = 0;
    inline for (name, 0..) |b, i| {
        if (b == '.') {
            dotIndices[dotIndicesLen] = i;
            dotIndicesLen += 1;
        }
        if (std.mem.indexOfScalar(u8, ESCAPE_CHARS, b) != null) {
            escapeIndices[escapeIndicesLen] = i;
            escapeIndicesLen += 1;
        }
    }
    comptime var path = @as(*const [name.len]u8, @ptrCast(name.ptr)).*;
    for (dotIndices[0..dotIndicesLen], 0..) |dotIndex, i| {
        if (i < dotIndicesLen - 1) {
            path[dotIndex] = '/';
        }
    }
    for (escapeIndices[0..escapeIndicesLen]) |escapeIndex| path[escapeIndex] = '_';
    return &path;
}

pub const VTABLE_METHOD_NAMES
     = std.meta.fieldNames(Rml.object.VTable.ObjDataFunctions)
    ++ std.meta.fieldNames(Rml.object.VTable.ObjMemoryFunctions)
    ++ .{"onInit"};

pub fn isVTableMethodName(name: []const u8) bool {
    for (VTABLE_METHOD_NAMES) |vtableName| {
        if (std.mem.eql(u8, name, vtableName)) return true;
    }
    return false;
}


pub const NativeFunction = *const fn (Rml.Interpreter, Origin, []Object) Result! Object;

pub fn Namespace(comptime T: type) type {
    @setEvalBranchQuota(10_000);
    const BaseMethods = BaseMethods: {
        const Api = Api: {
            const ApiEntry = struct { type: type, name: [:0]const u8 };
            const decls = std.meta.declarations(T);

            var entries = [1]ApiEntry {undefined} ** decls.len;
            var i = 0;

            for (decls) |decl| {
                if (std.meta.hasMethod(T, decl.name)
                and !isVTableMethodName(decl.name)) {
                    const F = @TypeOf(@field(T, decl.name));
                    if (@typeInfo(F) != .@"fn") continue;
                    const fInfo = @typeInfo(F).@"fn";
                    if (fInfo.is_generic or fInfo.is_var_args) continue;
                    entries[i] = ApiEntry {
                        .type = F,
                        .name = decl.name,
                    };
                    i += 1;
                }
            }

            break :Api entries[0..i];
        };

        var fields = [1] std.builtin.Type.StructField {undefined} ** Api.len;
        for (Api, 0..) |apiEntry, i| {
            const fInfo = @typeInfo(apiEntry.type).@"fn";

            const GenericType = generic: {
                break :generic @Type(.{.@"fn" = std.builtin.Type.Fn {
                    .calling_convention = .auto,
                    .is_generic = false,
                    .is_var_args = false,
                    .return_type = ret_type: {
                        const R = fInfo.return_type.?;
                        const rInfo = @typeInfo(R);
                        if (rInfo != .error_union) break :ret_type R;
                        break :ret_type @Type(.{.error_union = std.builtin.Type.ErrorUnion {
                            .error_set = Result,
                            .payload = rInfo.error_union.payload,
                        }});
                    },
                    .params = fInfo.params,
                }});
            };

            fields[i] = std.builtin.Type.StructField {
                .name = apiEntry.name,
                .type = *const GenericType,
                .default_value = null,
                .is_comptime = false,
                .alignment = @alignOf(*const fn () void),
            };
        }

        break :BaseMethods @Type(.{.@"struct" = std.builtin.Type.Struct {
            .layout = .auto,
            .fields = &fields,
            .decls = &.{},
            .is_tuple = false,
        }});
    };

    const baseMethods = baseMethods: {
        var ms: BaseMethods = undefined;
        for (std.meta.fields(BaseMethods)) |field| {
            @field(ms, field.name) = @as(field.type, @ptrCast(&@field(T, field.name)));
        }
        break :baseMethods ms;
    };

    const Methods = Methods: {
        var fields = [1] std.builtin.Type.StructField {undefined} ** std.meta.fields(BaseMethods).len;

        for (std.meta.fields(BaseMethods), 0..) |field, fieldIndex| {
            fields[fieldIndex] = std.builtin.Type.StructField {
                .name = field.name,
                .type = Rml.Procedure,
                .default_value = null,
                .is_comptime = false,
                .alignment = @alignOf(*const fn () void),
            };
        }

        break :Methods @Type(.{.@"struct" = std.builtin.Type.Struct {
            .layout = .auto,
            .fields = &fields,
            .decls = &.{},
            .is_tuple = false,
        }});
    };

    return struct {
        pub fn deinitMethods(ms: Methods) void {
            inline for (comptime std.meta.fieldNames(Methods)) |fieldName| {
                @field(ms, fieldName).deinit();
            }
        }

        pub fn methods(rml: *Rml, origin: Origin) OOM! Methods {
            var ms: Methods = undefined;

            inline for (comptime std.meta.fieldNames(Methods)) |fieldName| {
                @field(ms, fieldName) = try wrapNativeFunction(rml, origin, @field(baseMethods, fieldName));
            }

            return ms;
        }
    };
}

pub fn fromObject(comptime T: type, _: *Rml, value: Object) Error! T {
    const tInfo = @typeInfo(T);

    switch (tInfo) {
        .pointer => |info| {
            if (info.alignment == Rml.object.OBJ_ALIGN) {
                if (!Rml.equal(Rml.TypeId.of(T), value.getHeader().type_id)) {
                    return error.TypeError;
                }

                const obj = forceObj(info.child, value);
                defer obj.deinit();

                return obj.data;
            }
        },
        else => {
            if (comptime std.mem.startsWith(u8, @typeName(T), "object.Obj")) {
                const O = @typeInfo(tInfo.@"struct".fields[0].type).pointer.child;

                if (!Rml.equal(Rml.TypeId.of(O), value.getHeader().type_id)) {
                    return error.TypeError;
                }

                return forceObj(O, value);
            }
        },
    }

    unreachable;
}

pub fn ObjectRepr(comptime T: type) type {
    const tInfo = @typeInfo(T);
    return switch (T) {
        Rml.int => Rml.Int,
        Rml.float => Rml.Float,
        Rml.char => Rml.Char,
        else => switch (tInfo) {
            .bool => Rml.Bool,

            .void, .null, .undefined, .noreturn
                => Rml.Nil,

            .int, .float, .error_set, .error_union, .@"enum", .@"opaque", .enum_literal, .array, .vector,
                => Rml.Obj(T),

            .pointer => |info|
                if (@typeInfo(info.child) == .@"fn") Rml.Procedure
                else if (info.alignment == Rml.object.OBJ_ALIGN) ObjectRepr(info.child)
                     else Rml.Obj(T),

            .@"struct" =>
                if (std.mem.startsWith(u8, @typeName(T), "object.Obj")) T
                else Rml.Obj(T),

            .@"union" => Rml.Obj(T),

            .@"fn" => Rml.Procedure,

            .optional => Rml.Object,

            else => @compileError("unsupported return type: " ++ @typeName(T)),
        }
    };
}

pub fn toObject(rml: *Rml, origin: Origin, value: anytype) OOM! ObjectRepr(@TypeOf(value)) {
    const T = @TypeOf(value);
    const tInfo = @typeInfo(T);
    return switch (T) {
        Rml.int => Rml.Int.wrap(rml, origin, value),
        Rml.float => Rml.Float.wrap(rml, origin, value),
        Rml.char => Rml.Char.wrap(rml, origin, value),
        Rml.str => Rml.Obj([]const u8).wrap(rml, origin, value),
        Rml.Object => return value.clone(),
        else => switch (tInfo) {
            .bool =>
                Rml.Bool.wrap(rml, origin, value),

            .void, .null, .undefined, .noreturn, =>
                Rml.Nil.wrap(rml, origin, Rml.nil{}),

            .int, .float, .error_set, .error_union, .@"enum",
            .@"opaque", .enum_literal, .array, .vector, =>
                Rml.Obj(T).wrap(rml, origin, value),

            .pointer => |info|
                if (@typeInfo(info.child) == .@"fn") @compileError("wrap functions with wrapNativeFunction")
                else if (info.alignment == Rml.object.OBJ_ALIGN) getObj(value)
                     else Rml.Obj(T).wrap(rml, origin, value),

            .@"struct" =>
                if (comptime std.mem.startsWith(u8, @typeName(T), "object.Obj")) value
                else Rml.Obj(T).wrap(rml, origin, value),

            .@"union" =>
                Rml.Obj(T).wrap(rml, origin, value),

            .optional =>
                if (value) |v| v: {
                    const x = try toObject(rml, origin, v);
                    defer x.deinit();
                    break :v x.typeErase();
                } else nil: {
                    const x = try Rml.Nil.wrap(rml, origin, Rml.nil{});
                    defer x.deinit();
                    break :nil x.typeErase();
                },

            else => @compileError("unsupported type: " ++ @typeName(T)),
        }
    };
}

pub fn toObjectConst(rml: *Rml, origin: Origin, comptime value: anytype) OOM! ObjectRepr(@TypeOf(value)) {
    const T = @TypeOf(value);
    const tInfo = @typeInfo(T);
    return switch (T) {
        Rml.int => Rml.Int.wrap(rml, origin, value),
        Rml.float => Rml.Float.wrap(rml, origin, value),
        Rml.char => Rml.Char.wrap(rml, origin, value),
        Rml.str => Rml.Obj([]const u8).wrap(rml, origin, value),
        Rml.Object => value.clone(),
        else => switch (tInfo) {
            .bool =>
                Rml.Bool.wrap(rml, origin, value),

            .void, .null, .undefined, .noreturn, =>
                Rml.Nil.wrap(rml, origin, Rml.nil{}),

            .int, .float, .error_set, .error_union, .@"enum",
            .@"opaque", .enum_literal, .array, .vector, =>
                Rml.Obj(T).wrap(rml, origin, value),

            .pointer => |info|
                if (@typeInfo(info.child) == .@"fn") wrapNativeFunction(rml, origin, value)
                else if (info.alignment == Rml.object.OBJ_ALIGN) getObj(value)
                     else Rml.Obj(T).wrap(rml, origin, value),

            .@"struct", .@"union", =>
                Rml.Obj(T).wrap(rml, origin, value),

            .optional =>
                if (value) |v| v: {
                    const x = try toObject(rml, origin, v);
                    defer x.deinit();
                    break :v x.typeErase();
                } else nil: {
                    const x = try Rml.Nil.wrap(rml, origin, Rml.nil{});
                    defer x.deinit();
                    break :nil x.typeErase();
                },

            else => @compileError("unsupported type: " ++ @typeName(T)),
        }
    };
}


pub fn wrapNativeFunction(rml: *Rml, origin: Origin, comptime value: anytype) OOM! Rml.Procedure {
    const T = @typeInfo(@TypeOf(value)).pointer.child;
    const info = @typeInfo(T).@"fn";

    return Rml.Procedure.wrap(rml, origin, .{ .native = struct {
        pub fn method (interpreter: Rml.Interpreter, callOrigin: Origin, args: []Object) Result! Object {
            if (args.len != info.params.len) {
                return error.InvalidArgumentCount;
            }

            var nativeArgs: std.meta.ArgsTuple(T) = undefined;

            inline for (info.params, 0..) |param, i| {
                nativeArgs[i] = try fromObject(param.type.?, interpreter.getRml(), args[i]);
            }

            const nativeResult = nativeResult: {
                const r = @call(.auto, value, nativeArgs);
                break :nativeResult if (comptime TypeUtils.causesErrors(T)) try r else r;
            };

            const objWrapper = try toObject(interpreter.getRml(), callOrigin, nativeResult);
            defer objWrapper.deinit();

            return objWrapper.typeErase();
        }
    }.method });
}
