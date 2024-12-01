const std = @import("std");
const builtin = std.builtin;
const ZigType = builtin.Type;
const zig = std.zig;

pub const std_options = std.Options {
    .log_level = .warn,
};

const log = std.log.scoped(.headergen);

const Generator = HeaderGenerator(@import("#HEADER_GENERATION_SOURCE_MODULE#"));

const DATA_SOURCE_NAME = "HEADER-GENERATION-DATA";

var RENDER_LINE_COMMENT = false;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    if (args.len < 3) {
        return error.NotEnoughArguments;
    } else if (args.len > 3) {
        return error.TooManyArguments;
    }



    const cwd = std.fs.cwd();


    var generator = try Generator.init(allocator, args[1]);

    var source = cwd.openFile(generator.path, .{ .mode = .read_only }) catch |err| {
        log.err("failed to open source file [{s}]: {s}", .{ generator.path, @errorName(err) });
        return err;
    };
    defer source.close();

    const sourceText = try source.readToEndAllocOptions(allocator, std.math.maxInt(usize), null, 128, 0);

    const ast = try zig.Ast.parse(allocator, sourceText, .zig);

    const members = try parseMembers(&generator, ast, ast.rootDecls());

    try generator.gatherDecls();

    const headerText = try render(&generator, members);


    if (!std.mem.eql(u8, args[2], "-no-static")) {
        const outputFileName = args[2];
        log.debug("output file: {s}", .{outputFileName});

        if (std.fs.path.dirname(outputFileName)) |dirname| {
            cwd.makePath(dirname) catch |err| {
                log.err("cannot make output path: {}", .{err});
                return error.InvalidOutputPath;
            };
        }
        const tempOutputFileName = try std.fmt.allocPrint(allocator, "{s}.tmp", .{outputFileName});
        const outputFile = cwd.createFile(tempOutputFileName, .{ .exclusive = true }) catch |err| {
            log.err("Unable to create output file `{s}`: {}", .{ tempOutputFileName, err });
            return error.CannotCreateFile;
        };
        errdefer cwd.deleteFile(tempOutputFileName) catch {};
        defer outputFile.close();

        const output = outputFile.writer();
        try output.writeAll(headerText);

        cwd.deleteFile(outputFileName) catch |err| {
            if (err != error.FileNotFound) {
                log.err("Unable to delete `{s}`: {}", .{ outputFileName, err });
                return err;
            }
        };

        cwd.rename(tempOutputFileName, outputFileName) catch |err| {
            log.err("Unable to rename `{s}` to `{s}`: {}", .{ tempOutputFileName, outputFileName, err });
            return error.CannotRenameFile;
        };

        const meta = outputFile.metadata() catch |err| {
            log.err("Unable to stat `{s}`: {}", .{ outputFileName, err });
            return error.CannotStatFile;
        };

        var perms = meta.permissions();

        perms.setReadOnly(true);

        outputFile.setPermissions(perms) catch |err| {
            log.err("Unable to set permissions on `{s}`: {}", .{ outputFileName, err });
            return error.CannotSetPermissions;
        };
    } else {
        log.debug("output stdout", .{});
        try std.io.getStdOut().writer().writeAll(headerText);
    }
}

fn render(generator: *const Generator, members: []const Member) ![]const u8 {
    var output = std.ArrayList(u8).init(generator.allocator);
    const writer = output.writer().any();

    try writer.print("{s}", .{generator.head});

    for (members) |member| {
        try member.render(generator, writer);
        try writer.writeAll("\n\n");
    }

    try writer.print("{s}", .{generator.foot});

    return output.items;
}

fn parseMembers(gen: *Generator, ast: zig.Ast, members: []const zig.Ast.Node.Index) ![]const Member {
    var memberBuf = std.ArrayList(Member).init(gen.allocator);
    for (members) |member| {
        if (try parseMember(gen, ast, member)) |memb| {
            try memberBuf.append(memb);
        }
    }
    return memberBuf.items;
}

const Member = union(enum) {
    type_def: TypeDef,
    constant: Const,
    variable: Var,
    function: Function,

    const TypeDef = struct {
        name: []const u8,
        location: LocationFmt,
        kind: Kind,
        expr: []const u8,

        const Kind = enum {
            custom,
            @"opaque",
            @"extern",
        };
    };

    const Const = struct {
        name: []const u8,
        location: LocationFmt,
        typeExpr: []const u8,
        valueExpr: []const u8,
    };

    const Var = struct {
        name: []const u8,
        location: LocationFmt,
        typeExpr: []const u8,
    };

    const Function = struct {
        name: []const u8,
        location: LocationFmt,
        returnType: []const u8,
        params: []const Param,

        const Param = struct {
            name: []const u8,
            location: LocationFmt,
            typeExpr: []const u8,
        };
    };

    pub fn render(self: Member, generator: *const Generator, writer: std.io.AnyWriter) !void {
        switch (self) {
            .type_def => |x| {
                switch (x.kind) {
                    .custom => {
                        try writer.print("{comment}", .{x.location});
                        if (generator.lookupType(x.name)) |t| {
                            if (t.info == .generative) {
                                return try t.info.generative(x.name, x.expr, generator, writer);
                            }
                        }
                        const t = generator.customTypes.get(x.name) orelse {
                            log.err("{}: type {s} not found in custom type table", .{ x.location, x.name });
                            return error.CustomTypeNotFound;
                        };
                        try t.render(x.name, x.expr, generator, writer);
                    },
                    .@"opaque" => try writer.print("{comment}typedef {s}OPAQUE {s};", .{ x.location, generator.prefix, x.name }),
                    .@"extern" => {
                        try writer.print("{comment}", .{x.location});
                        const t = generator.lookupType(x.name) orelse {
                            log.err("{}: type {s} not found in generator table", .{ x.location, x.name });
                            var iter = generator.nameToId.keyIterator();
                            log.err("available names:", .{});
                            while (iter.next()) |key| {
                                log.err("{s}", .{key.*});
                            }
                            return error.ExternTypeNotFound;
                        };
                        t.renderDecl(x.name, t.declName, generator, writer) catch |err| {
                            log.err("{}: failed to render type {s} aka {s}, {s}", .{ x.location, x.name, t.declName orelse "{unknown}", t.zigName });
                            return err;
                        };
                        try writer.writeAll(";");
                    },
                }
            },
            .constant => |x| {
                try writer.print("{comment}static const {s} {s} = {s};", .{ x.location, x.typeExpr, x.name, x.valueExpr });
            },
            .variable => |x| {
                try writer.print("{comment}extern {s}{s};", .{ x.location, x.typeExpr, x.name });
            },
            .function => |x| {
                try writer.print("{comment}{s}{s} (", .{ x.location, x.returnType, x.name });
                for (x.params, 0..) |param, i| {
                    try writer.print("{s}{s}", .{ param.typeExpr, param.name });
                    if (i < x.params.len - 1) {
                        try writer.writeAll(", ");
                    }
                }
                try writer.writeAll(");");
            },
        }
    }
};

fn parseMember(
    gen: *Generator,
    ast: zig.Ast,
    decl: zig.Ast.Node.Index,
) !?Member {
    const token_tags = ast.tokens.items(.tag);
    switch (ast.nodes.items(.tag)[decl]) {
        .fn_decl,
        .fn_proto_simple,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_proto,
        => {
            var buf = std.mem.zeroes([1]zig.Ast.Node.Index);
            const fun_proto = zig.Ast.fullFnProto(ast, &buf, decl).?;

            const name_tok = fun_proto.name_token orelse 0;
            var name = ast.tokenSlice(name_tok);

            log.debug("fn_decl: {s}", .{name});

            if(std.mem.startsWith(u8, name, "@\"") and std.mem.endsWith(u8, name, "\"")) {
                name = name[2..name.len - 1];
                log.debug("stripped name: {s}", .{name});
            }

            if (gen.ignoredDecls.contains(name)) return null;

            const location = Location(gen.path, ast.tokenLocation(0, fun_proto.firstToken()));

            const vis_tok = fun_proto.visib_token orelse 0;
            const vis = token_tags[vis_tok];
            if (vis != .keyword_pub) return null;

            const extern_export_inline_tok = fun_proto.extern_export_inline_token orelse 0;
            const extern_export_inline = token_tags[extern_export_inline_tok];

            if (extern_export_inline != .keyword_export) {
                log.warn("{}: public function {s} is not exported", .{ location, name });
                return null;
            }

            const ret_type_node = fun_proto.ast.return_type;
            const ret_type_start = ast.firstToken(ret_type_node);
            const ret_type_end = ast.lastToken(ret_type_node);
            const ret_type_src = tokensSlice(ast, ret_type_start, ret_type_end);
            const ret_type = try convertTypeExpr(gen.allocator, ret_type_src);

            var paramBuf = std.ArrayList(Member.Function.Param).init(gen.allocator);

            var param_iter = fun_proto.iterate(&ast);
            while (param_iter.next()) |param| {
                const param_name_tok = param.name_token orelse 0;
                const param_name = if (param_name_tok != 0) ast.tokenSlice(param_name_tok) else continue;

                const param_type_node = param.type_expr;
                const param_type_start = ast.firstToken(param_type_node);
                const param_type_end = ast.lastToken(param_type_node);
                const param_type_src = tokensSlice(ast, param_type_start, param_type_end);
                const param_type = try convertTypeExpr(gen.allocator, param_type_src);

                const param_location = Location(gen.path, ast.tokenLocation(0, param_name_tok));

                try paramBuf.append(Member.Function.Param{
                    .name = param_name,
                    .location = param_location,
                    .typeExpr = param_type,
                });
            }

            if (!std.mem.startsWith(u8, name, gen.prefix)) {
                log.warn("{}: exported function {s} does not start with {s}", .{ location, name, gen.prefix });
            }

            return Member{
                .function = .{
                    .name = name,
                    .location = location,
                    .returnType = ret_type,
                    .params = paramBuf.items,
                },
            };
        },

        .global_var_decl,
        .local_var_decl,
        .simple_var_decl,
        .aligned_var_decl,
        => {
            const var_decl = zig.Ast.fullVarDecl(ast, decl).?;

            const vis = token_tags[var_decl.visib_token orelse 0];
            if (vis != .keyword_pub) return null;

            const exp = token_tags[var_decl.extern_export_token orelse 0];

            const mut_tok = var_decl.ast.mut_token;
            std.debug.assert(mut_tok != 0);

            const mut = token_tags[mut_tok];

            const name_tok = mut_tok + 1;
            var name = ast.tokenSlice(name_tok);

            log.debug("var_decl: {s}", .{name});

            if(std.mem.startsWith(u8, name, "@\"") and std.mem.endsWith(u8, name, "\"")) {
                name = name[2..name.len - 1];
                log.debug("stripped name: {s}", .{name});
            }

            if (gen.ignoredDecls.contains(name)) return null;

            const location = Location(gen.path, ast.tokenLocation(0, var_decl.firstToken()));

            const var_type_node = var_decl.ast.type_node;
            const var_type_start = ast.firstToken(var_type_node);
            const var_type_end = ast.lastToken(var_type_node);
            const var_type_src = tokensSlice(ast, var_type_start, var_type_end);

            const var_init_node = var_decl.ast.init_node;
            const var_init_start = ast.firstToken(var_init_node);
            const var_init_end = ast.lastToken(var_init_node);
            const var_init = tokensSlice(ast, var_init_start, var_init_end);

            if (!std.mem.startsWith(u8, name, gen.prefix)) {
                log.warn("{}: public variable {s} does not start with {s}", .{ location, name, gen.prefix });
            }

            const typeKind: ?Member.TypeDef.Kind =
                if (std.mem.eql(u8, var_type_src, "customtype")) .custom
                else if (std.mem.eql(u8, var_type_src, "opaquetype")) .@"opaque"
                else if (std.mem.eql(u8, var_type_src, "type")) .@"extern"
                else null;

            if (typeKind) |kind| {
                std.debug.assert(mut == .keyword_const);

                if (kind == .custom) {
                    try gen.registerCustomType(name);
                }

                return Member{
                    .type_def = .{
                        .name = name,
                        .location = location,
                        .kind = kind,
                        .expr = var_init,
                    },
                };
            } else if (mut == .keyword_const) {
                if (exp == .keyword_export) {
                    log.warn("{}: public constant {s} does not need to be exported", .{ location, name });
                }

                return Member{
                    .constant = .{
                        .name = name,
                        .location = location,
                        .typeExpr = if (var_type_src.len > 0) try convertTypeExpr(gen.allocator, var_type_src) else {
                            log.err("{}: missing type expr for {s}", .{location, name});
                            return error.MissingTypeExpr;
                        },
                        .valueExpr = var_init,
                    },
                };
            } else if (mut == .keyword_var) {
                if (exp != .keyword_export) {
                    log.warn("{}: public variable {s} is not exported", .{ location, name });
                    return null;
                }

                return Member{
                    .variable = .{
                        .name = name,
                        .location = location,
                        .typeExpr = if (var_type_src.len > 0) try convertTypeExpr(gen.allocator, var_type_src) else {
                            log.err("{}: miissing type expr for {s}", .{location, name});
                            return error.MissingTypeExpr;
                        },
                    },
                };
            } else unreachable;
        },

        else => return null,
    }
}

fn convertTypeExpr(allocator: std.mem.Allocator, src: []const u8) ![]const u8 {
    var slice = src;

    var isOpt = false;

    if (slice[0] == '?') {
        isOpt = true;
        slice = slice[1..];
    }

    const MAX_PTRS: usize = 16;
    var isPtr: usize = 0;
    var isConst = std.mem.zeroes([MAX_PTRS]bool);

    while (strStartCase(slice, .{"*", "[*]", "[*:0]"})) |ptrStart| {
        slice = slice[ptrStart..];
        if (isPtr >= MAX_PTRS) {
            log.err("Too many pointers in type expr `{s}`", .{src});
            return error.TooManyPointers;
        }
        if (std.mem.startsWith(u8, slice, "const ")) {
            slice = slice[6..];
            isConst[isPtr] = true;
        }
        isPtr += 1;
    }

    if (isOpt and isPtr == 0) {
        log.err("Cannot convert non-pointer optional type `{s}`", .{src});
        return error.InvalidType;
    }

    const inner = translatePrimitiveType(allocator, slice) catch |err| {
        log.err("Failed to convert type expr `{s}`", .{src});
        return err;
    };

    if (isPtr == 0) return try std.fmt.allocPrint(allocator, "{s} ", .{inner});

    var buf = std.ArrayList(u8).init(allocator);

    try buf.appendSlice(inner);
    try buf.append(' ');

    while (isPtr > 0) {
        if (isConst[isPtr - 1]) {
            if (buf.items.len > inner.len + 1) try buf.append(' ');
            try buf.appendSlice("const");
        }
        try buf.append('*');
        isPtr -= 1;
    }

    return buf.items;
}

fn translatePrimitiveType(allocator: std.mem.Allocator, slice: []const u8) ![]const u8 {
    if (std.mem.startsWith(u8, slice, "BB_")) {
        return slice;
    } else if (strCase(slice, .{ "bool", "void" })) {
        return slice;
    } else if (strCase(slice, .{ "usize", "isize" })) {
        if (slice[0] == 'u') {
            return "size_t";
        } else {
            return "ssize_t";
        }
    } else if (strCase(slice, .{ "i8", "i16", "i32", "i64" })) {
        const size = slice[1..];
        return try std.fmt.allocPrint(allocator, "int{s}_t", .{size});
    } else if (strCase(slice, .{ "u8", "u16", "u32", "u64" })) {
        const size = slice[1..];
        return try std.fmt.allocPrint(allocator, "uint{s}_t", .{size});
    } else if (strCase(slice, .{ "f32", "f64" })) {
        const size = slice[1..];
        if (std.mem.eql(u8, size, "32")) {
            return "float";
        } else if (std.mem.eql(u8, size, "64")) {
            return "double";
        } else unreachable;
    } else if (strCase(slice, .{"anyopaque"})) {
        return "void";
    } else {
        return error.UnrecognizedType;
    }
}

inline fn strCase(slice: []const u8, comptime options: anytype) bool {
    inline for (0..options.len) |i| {
        if (std.mem.eql(u8, slice, options[i])) {
            return true;
        }
    }

    return false;
}

inline fn strStartCase(slice: []const u8, comptime options: anytype) ?usize {
    inline for (0..options.len) |i| {
        if (std.mem.startsWith(u8, slice, options[i])) {
            return options[i].len;
        }
    }

    return null;
}

fn tagStr(kw: zig.Token.Tag) []const u8 {
    switch (kw) {
        .keyword_pub => return "pub",
        .keyword_const => return "const",
        .keyword_var => return "var",
        .keyword_fn => return "fn",
        .keyword_export => return "export",
        else => return "unknown",
    }
}

fn tokensSlice(ast: zig.Ast, start: zig.Ast.TokenIndex, end: zig.Ast.TokenIndex) []const u8 {
    const span = ast.tokensToSpan(start, end, end);

    return ast.source[span.start..span.end];
}

const LocationFmt = struct {
    path: []const u8,
    line: usize,

    pub fn format(self: LocationFmt, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (std.mem.eql(u8, "comment", fmt)) {
            if (RENDER_LINE_COMMENT) {
                try writer.print("// [{s}:{d}]\n", .{ self.path, self.line + 1 });
            }
        } else {
            try writer.print("[{s}:{d}]", .{ self.path, self.line + 1 });
        }
    }
};

fn Location(path: []const u8, loc: zig.Ast.Location) LocationFmt {
    return .{ .path = path, .line = loc.line };
}

const TypeId = enum(u48) {
    _,

    fn of(comptime T: type) TypeId {
        const H = struct {
            var byte: u8 = 0;
            var _ = T;
        };

        return @enumFromInt(@as(u48, @truncate(@intFromPtr(&H.byte))));
    }

    fn render(self: TypeId, gen: *const Generator, writer: anytype) !void {
        const ty: Type = gen.idToType.get(self).?;
        try ty.render(gen, writer);
    }

    fn renderDecl(self: TypeId, name: ?[]const u8, declName: ?[]const u8, gen: *const Generator, writer: anytype) !void {
        const ty: Type = gen.idToType.get(self).?;
        try ty.renderDecl(name, declName, gen, writer);
    }

    fn renderField(self: TypeId, name: []const u8, isGeneratedParam: bool, gen: *const Generator, writer: anytype) !void {
        const ty: Type = gen.idToType.get(self).?;
        try ty.renderField(name, isGeneratedParam, gen, writer);
    }

    const Map = std.HashMap(TypeId, Type, TypeId.MapContext, 80);

    const MapContext = struct {
        pub fn hash(_: MapContext, id: TypeId) u64 {
            const bytes: u64 = @intFromEnum(id);
            return std.hash.Fnv1a_64.hash(@as([*]const u8, @ptrCast(&bytes))[0..8]);
        }

        pub fn eql(_: MapContext, a: TypeId, b: TypeId) bool {
            return a == b;
        }
    };
};

const Type = struct {
    declName: ?[]const u8,
    zigName: []const u8,
    info: TypeInfo,

    fn render(self: Type, gen: *const Generator, writer: anytype) !void {
        if (self.declName) |dn| {
            try writer.writeAll(dn);
        } else {
            try self.info.render(self.declName, self.zigName, gen, writer);
        }
    }

    fn renderDecl(self: Type, name: ?[]const u8, declName: ?[]const u8, gen: *const Generator, writer: anytype) !void {
        if (name orelse self.declName) |n| {
            try self.info.renderDecl(n, declName orelse self.declName, self.zigName, gen, writer);
        } else {
            log.err("no decl name for type {s}", .{self.zigName});
            return error.InvalidDecl;
        }
    }

    fn renderField(self: Type, name: []const u8, isGeneratedParam: bool, gen: *const Generator, writer: anytype) !void {
        try self.info.renderField(name, self.declName, self.zigName, isGeneratedParam, gen, writer);
    }
};

fn allStars(str: []const u8) bool {
    for (str) |c| {
        if (c != '*') {
            return false;
        }
    }
    return true;
}

fn toUpperStr(allocator: std.mem.Allocator, str: []const u8) anyerror![]const u8 {
    var buf = std.ArrayList(u8).init(allocator);
    for (str, 0..) |c, i| {
        if (!std.ascii.isASCII(c)) {
            return error.InvalidAscii;
        }

        if (std.ascii.isUpper(c) and i > 0) {
            const lastC = str[i - 1];
            if (lastC != '_' and !std.ascii.isUpper(lastC)) {
                try buf.appendSlice(&[_]u8{'_', c});
                continue;
            }
        }

        try buf.append(std.ascii.toUpper(c));
    }
    return buf.items;
}

const TypeInfo = union(enum) {
    @"enum": []const []const u8,
    @"union": []const Field,
    @"struct": Struct,
    @"opaque": void,
    pointer: TypeId,
    function: Function,
    primitive: []const u8,
    unusable: void,
    generative: *const fn (name: []const u8, expr: []const u8, *const Generator, writer: std.io.AnyWriter) anyerror!void,

    const Struct = struct {
        packType: ?TypeId,
        fields: []const Field,
    };

    const Field = struct {
        name: []const u8,
        type: TypeId,
    };

    const Function = struct {
        returnType: TypeId,
        params: []const TypeId,
    };

    fn render(self: TypeInfo, declName: ?[]const u8, zigName: []const u8, gen: *const Generator, writer: anytype) anyerror!void {
        return self.renderDecl(null, declName, zigName, gen, writer);
    }

    fn renderDecl(self: TypeInfo, name: ?[]const u8, declName: ?[]const u8, zigName: []const u8, gen: *const Generator, writer: anytype) anyerror!void {
        log.info("renderDecl \"{s}\" {s} :: {s}", .{name orelse "", zigName, @tagName(self)});

        switch (self) {
            .@"enum" => |x| {
                try if (name) |n| writer.print("typedef enum {s} {{", .{n}) else writer.writeAll("enum {");
                if (x.len > 0) try writer.writeAll("\n");
                if (if (name) |n| gen.enumSuffixes.get(n) else null) |suffix| {
                    for (x) |variantName| {
                        const upper = toUpperStr(gen.allocator, variantName) catch |err| {
                            log.err("cannot convert variant name {s} to upper case, {}", .{variantName, err});
                            return err;
                        };
                        try writer.print("    {s}{s}_{s},\n", .{ gen.prefix, upper, suffix });
                    }
                } else {
                    for (x) |variantName| {
                        const upper = toUpperStr(gen.allocator, variantName) catch |err| {
                            log.err("cannot convert variant name {s} to upper case, {}", .{variantName, err});
                            return err;
                        };
                        try writer.print("    {s}{s},\n", .{ gen.prefix, upper });
                    }
                }
                try writer.writeAll("}");
                if (name) |n| try writer.print(" {s}", .{n});
            },
            .@"union" => |x| {
                try if (name) |n| writer.print("typedef union {s} {{", .{n}) else writer.writeAll("union {");
                if (x.len > 0) try writer.writeAll("\n");
                for (x) |field| {
                    try writer.writeAll("    ");
                    try field.type.renderField(field.name, false, gen, writer);
                    try writer.writeAll(";\n");
                }
                try writer.writeAll("}");
                if (name) |n| try writer.print(" {s}", .{n});
            },
            .@"struct" => |x| {
                try if (name) |n| writer.print("typedef struct {s} {{", .{n}) else writer.writeAll("struct {");
                if (x.fields.len > 0) try writer.writeAll("\n");
                for (x.fields) |field| {
                    try writer.writeAll("    ");
                    try field.type.renderField(field.name, false, gen, writer);
                    try writer.writeAll(";\n");
                }
                try writer.writeAll("}");
                if (name) |n| try writer.print(" {s}", .{n});
            },
            .@"opaque" => if (name) |n| {
                try writer.print("typedef void {s}", .{n});
            } else {
                try writer.writeAll("void");
            },
            .pointer => |x| if (name) |n| {
                const ptrName = try std.fmt.allocPrint(gen.allocator, "*{s}", .{n});
                try x.renderDecl(ptrName, declName, gen, writer);
            } else {
                try x.render(gen, writer);
                try writer.writeAll("*");
            },
            .function => |x| {
                if (name != null) try writer.writeAll("typedef ");
                try x.returnType.render(gen, writer);
                if (name) |n| {
                    if (std.mem.startsWith(u8, n, "*")) {
                        try writer.print(" ({s}) (", .{n});
                    } else {
                        try writer.print(" (*{s}) (", .{n});
                    }
                } else {
                    log.err("function type without name not inside a structure, {s}", .{zigName});
                    return error.InvalidFunctionPrint;
                }
                for (x.params, 0..) |param, i| {
                    var generatedParamName = false;
                    const paramName = if (if (declName) |n| gen.procArgs.get(n) else null) |procEntry| procArgs: {
                        switch (procEntry) {
                            .ignore => {
                                generatedParamName = true;
                                break :procArgs try std.fmt.allocPrint(gen.allocator, "v{}", .{i});
                            },
                            .pairs => |procArgs| {
                                if (procArgs.len <= i) {
                                    log.err("procArgs entry for procedure type {s} is too short", .{declName orelse zigName});
                                    return error.InvalidProcArgs;
                                }

                                if (procArgs[i].type != param) {
                                    log.err("procArgs entry for procedure type {s} does not match at parameter type {}", .{declName orelse zigName, i});
                                    return error.InvalidProcArgs;
                                }

                                break :procArgs procArgs[i].name;
                            }
                        }
                    } else noProcArgs: {
                        if (declName) |dn| {
                            log.warn("no procArgs entry for procedure type {s}", .{dn});
                        }
                        generatedParamName = true;
                        break :noProcArgs try std.fmt.allocPrint(gen.allocator, "v{}", .{i});
                    };
                    try param.renderField(paramName, generatedParamName, gen, writer);
                    if (i < x.params.len - 1) {
                        try writer.writeAll(", ");
                    }
                }
                try writer.writeAll(")");
            },
            .primitive => |x| if (name) |n| {
                try writer.print("typedef {s} {s}", .{x, n});
            } else {
                try writer.print("{s}", .{x});
            },
            .unusable => {
                log.err("unusable type info for {s}", .{declName orelse zigName});
                return error.InvalidType;
            },
            .generative => {
                log.err("incorrect use of generative type {s}", .{declName orelse zigName});
                return error.InvalidType;
            },
        }
    }

    fn renderField(self: TypeInfo, name: []const u8, declName: ?[]const u8, zigName: []const u8, isGeneratedParam: bool, gen: *const Generator, writer: anytype) anyerror!void {
        log.info("renderField {s} {s} :: {s}", .{name, zigName, @tagName(self)});

        if (declName) |dn| {
            if (isGeneratedParam) {
                try writer.writeAll(dn);
            } else {
                try writer.writeAll(dn);
                if (!allStars(name)) try writer.writeAll(" ");
                try writer.writeAll(name);
            }
            return;
        }

        switch (self) {
            .@"enum" => |x| {
                try writer.writeAll("enum {");
                if (x.len > 0) try writer.writeAll("\n");
                if (gen.enumSuffixes.get(name)) |suffix| {
                    for (x) |variantName| {
                        const upper = toUpperStr(gen.allocator, variantName) catch |err| {
                            log.err("cannot convert variant name {s} to upper case, {}", .{variantName, err});
                            return err;
                        };
                        try writer.print("    {s}{s}_{s},\n", .{ gen.prefix, upper, suffix });
                    }
                } else {
                    for (x) |variantName| {
                        const upper = toUpperStr(gen.allocator, variantName) catch |err| {
                            log.err("cannot convert variant name {s} to upper case, {}", .{variantName, err});
                            return err;
                        };
                        try writer.print("    {s}{s},\n", .{ gen.prefix, upper });
                    }
                }
                try writer.writeAll("}");
                if (!isGeneratedParam) {
                    if (!allStars(name)) try writer.writeAll(" ");
                    try writer.writeAll(name);
                }
            },
            .@"union" => |x| {
                try writer.writeAll("union {");
                if (x.len > 0) try writer.writeAll("\n");
                for (x) |field| {
                    try writer.writeAll("    ");
                    try field.type.renderField(field.name, false, gen, writer);
                    try writer.writeAll(";\n");
                }
                try writer.writeAll("}");
                if (!isGeneratedParam) {
                    if (!allStars(name)) try writer.writeAll(" ");
                    try writer.writeAll(name);
                }
            },
            .@"struct" => |x| {
                try writer.writeAll("struct {");
                if (x.fields.len > 0) try writer.writeAll("\n");
                for (x.fields) |field| {
                    try writer.writeAll("    ");
                    try field.type.renderField(field.name, false, gen, writer);
                    try writer.writeAll(";\n");
                }
                try writer.writeAll("}");
                if (!isGeneratedParam) {
                    if (!allStars(name)) try writer.writeAll(" ");
                    try writer.writeAll(name);
                }
            },
            .@"opaque" => {
                try writer.writeAll("void");
                if (!allStars(name)) try writer.writeAll(" ");
                try writer.writeAll(name);
            },
            .pointer => |x| {
                const ptrName =
                    if (isGeneratedParam) "*"
                    else try std.fmt.allocPrint(gen.allocator, "*{s}", .{name});

                try x.renderField(ptrName, false, gen, writer);
            },
            .function => |x| {
                try x.returnType.render(gen, writer);
                if (std.mem.startsWith(u8, name, "*")) {
                    try writer.print(" ({s}) (", .{name});
                } else {
                    try writer.print(" (*{s}) (", .{name});
                }
                for (x.params, 0..) |param, i| {
                    var generatedParamName = false;
                    const paramName = if (if (declName) |n| gen.procArgs.get(n) else null) |procEntry| procArgs: {
                        switch (procEntry) {
                            .ignore => {
                                generatedParamName = true;
                                break :procArgs try std.fmt.allocPrint(gen.allocator, "v{}", .{i});
                            },
                            .pairs => |procArgs| {
                                if (procArgs.len <= i) {
                                    log.err("procArgs entry for procedure type {s} is too short", .{declName orelse zigName});
                                    return error.InvalidProcArgs;
                                }

                                if (procArgs[i].type != param) {
                                    log.err("procArgs entry for procedure type {s} does not match at parameter type {}", .{declName orelse zigName, i});
                                    return error.InvalidProcArgs;
                                }

                                break :procArgs procArgs[i].name;
                            }
                        }
                    } else noProcArgs: {
                        if (declName) |dn| {
                            log.warn("no procArgs entry for procedure type {s}", .{dn});
                        }
                        generatedParamName = true;
                        break :noProcArgs try std.fmt.allocPrint(gen.allocator, "v{}", .{i});
                    };
                    try param.renderField(paramName, generatedParamName, gen, writer);
                    if (i < x.params.len - 1) {
                        try writer.writeAll(", ");
                    }
                }
                try writer.writeAll(")");
            },
            .primitive => |x| if (isGeneratedParam) {
                try writer.writeAll(x);
            } else if (!allStars(name)) {
                try writer.print("{s} {s}", .{x, name});
            } else {
                try writer.print("{s}{s}", .{x, name});
            },
            .unusable => {
                log.err("unusable type info for {s} aka {s} / {s}", .{name, declName orelse "\"\"", zigName});
                return error.InvalidType;
            },
            .generative => {
                log.err("incorrect use of generative type {s} aka {s} / {s}", .{name, declName orelse "\"\"", zigName});
                return error.InvalidType;
            },
        }
    }
};

const ProcPair = struct {
    type: TypeId,
    name: []const u8,
};

const ProcEntry = union(enum) {
    pairs: []const ProcPair,
    ignore: void,
};

fn HeaderGenerator(comptime Module: type) type {
    const S: ZigType.Struct = switch (@typeInfo(Module)) {
        .@"struct" => |s| s,
        else => @compileError("Expected a struct for c type info generation"),
    };

    const GENERATION_DATA = @field(Module, DATA_SOURCE_NAME);

    return struct {
        allocator: std.mem.Allocator,
        ignoredDecls: std.StringHashMap(void),
        path: []const u8,
        nameToId: std.StringHashMap(TypeId),
        idToType: TypeId.Map,
        head: []const u8,
        foot: []const u8,
        prefix: []const u8,
        enumSuffixes: std.StringHashMap([]const u8),
        customTypes: std.StringHashMap(CustomType),
        procArgs: std.StringHashMap(ProcEntry),

        const Self = @This();

        const CustomType = GENERATION_DATA.CustomType;

        fn isValidTag(comptime T: type) bool {
            return switch (@typeInfo(T)) {
                .int => |t| t.bits == 8 or t.bits == 16 or t.bits == 32,
                else => false,
            };
        }

        fn init(allocator: std.mem.Allocator, sourceInput: []const u8) !Self {
            const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
            var source = sourceInput;
            if (std.mem.startsWith(u8, sourceInput, cwd)) {
                source = sourceInput[cwd.len..];
                if (source[0] == '/') source = source[1..];
            }

            const HEADER_NAME = try toUpperStr(allocator, std.fs.path.stem(source));

            const STATIC_HEAD =
                \\/* File generated from {s} */
                \\
                \\#pragma once
                \\
                \\#ifndef {s}_H
                \\#define {s}_H
                \\
                \\#define {s}OPAQUE struct{{}}
                \\
                \\
                ;

            const head =
                if (GENERATION_DATA.head.len > 0)
                    try std.fmt.allocPrint(
                        allocator,
                        STATIC_HEAD ++
                        \\{s}
                        \\
                        \\
                        , .{
                            source,
                            HEADER_NAME,
                            HEADER_NAME,
                            GENERATION_DATA.prefix,
                            GENERATION_DATA.head,
                        }
                    )
                else try std.fmt.allocPrint(allocator, STATIC_HEAD, .{HEADER_NAME, HEADER_NAME, GENERATION_DATA.prefix});

            const STATIC_FOOT =
                \\#endif // {s}_H
                \\
                ;

            const foot =
                if (GENERATION_DATA.foot.len > 0)
                    try std.fmt.allocPrint(
                        allocator,
                        \\{s}
                        \\
                        ++ STATIC_FOOT
                        , .{
                            GENERATION_DATA.foot,
                            HEADER_NAME,
                        }
                    )
                else try std.fmt.allocPrint(allocator, STATIC_FOOT, .{HEADER_NAME});

            var self = Self{
                .allocator = allocator,
                .ignoredDecls = std.StringHashMap(void).init(allocator),
                .path = source,
                .nameToId = std.StringHashMap(TypeId).init(allocator),
                .idToType = TypeId.Map.init(allocator),
                .head = head,
                .foot = foot,
                .prefix = GENERATION_DATA.prefix,
                .enumSuffixes = std.StringHashMap([]const u8).init(allocator),
                .customTypes = std.StringHashMap(CustomType).init(allocator),
                .procArgs = std.StringHashMap(ProcEntry).init(allocator),
            };

            try self.ignoredDecls.put("std_options", {});
            try self.ignoredDecls.put(DATA_SOURCE_NAME, {});

            inline for (0..GENERATION_DATA.ignoredDecls.len) |i| {
                try self.ignoredDecls.put(GENERATION_DATA.ignoredDecls[i], {});
            }

            inline for (comptime std.meta.fieldNames(@TypeOf(GENERATION_DATA.customTypes))) |declName| {
                try self.customTypes.put(declName, @field(GENERATION_DATA.customTypes, declName));
            }

            inline for (comptime std.meta.fieldNames(@TypeOf(GENERATION_DATA.enumSuffixes))) |declName| {
                try self.enumSuffixes.put(declName, @field(GENERATION_DATA.enumSuffixes, declName));
            }

            inline for (comptime std.meta.fieldNames(@TypeOf(GENERATION_DATA.procArgs))) |declName| {
                const field = @field(GENERATION_DATA.procArgs, declName);
                const fieldT = @TypeOf(field);
                const fieldInfo = @typeInfo(fieldT);

                if (fieldInfo == .@"struct") {
                    if (!fieldInfo.@"struct".is_tuple) {
                        log.err("procArgs entry for {s} is not a string with value `ignore` or a tuple struct", .{declName});
                        return error.InvalidProcArgs;
                    }

                    var buf = std.ArrayList(ProcPair).init(allocator);

                    inline for (field) |rawPair| {
                        const typeId = TypeId.of(rawPair[0]);
                        const name = rawPair[1];
                        try buf.append(ProcPair{ .type = typeId, .name = name });
                    }

                    try self.procArgs.put(declName, .{.pairs = buf.items});
                } else if (field == .ignore) {
                    try self.procArgs.put(declName, .ignore);
                } else {
                    log.err("procArgs entry for {s} is not an enum literal with value `ignore` or a tuple struct", .{declName});
                    return error.InvalidProcArgs;
                }
            }

            return self;
        }

        fn registerCustomType(self: *Self, name: []const u8) !void {
            if (self.customTypes.contains(name)) {
                return;
            }

            try self.customTypes.put(name, .generative);
        }

        fn gatherDecls(self: *Self) !void {
            inline for (S.decls) |decl| {
                const T = @field(Module, decl.name);

                if (@TypeOf(T) != type) {
                    continue;
                }

                const id = TypeId.of(T);
                if (self.ignoredDecls.contains(decl.name)) {
                    try self.nameToId.put(decl.name, id);
                    try self.idToType.put(id, Type {.declName = decl.name, .zigName = @typeName(T), .info = .unusable});
                } else if (self.customTypes.contains(decl.name)) {
                    try self.nameToId.put(decl.name, id);

                    if (std.meta.hasFn(T, "generate_c_repr")) {
                        try self.idToType.put(id, Type {.declName = decl.name, .zigName = @typeName(T), .info = .{.generative = &struct {
                            fn fun (name: []const u8, expr: []const u8, generator: *const Self, writer: std.io.AnyWriter) anyerror!void {
                                try T.generate_c_repr(name, expr, generator, writer);
                            }
                        }.fun}});
                    } else {
                        try self.idToType.put(id, Type {.declName = decl.name, .zigName = @typeName(T), .info = .unusable});
                    }
                } else {
                    _ = try self.genTypeDecl(decl.name, T);
                }
            }
        }

        pub fn findTypeName(self: *const Self, comptime T: type) ![]const u8 {
            const id = TypeId.of(T);

            const ty = self.idToType.get(id) orelse {
                log.err("type not found: {s}", .{@typeName(T)});
                return error.TypeNotFound;
            };

            return ty.declName orelse ty.zigName;
        }

        pub fn lookupType(self: *const Self, name: []const u8) ?*Type {
            const id = self.nameToId.get(name) orelse return null;
            return self.idToType.getPtr(id) orelse unreachable;
        }

        pub fn genType(self: *Self, comptime T: type) !TypeId {
            return try self.genTypeDecl(null, T);
        }

        pub fn genTypeDecl(self: *Self, declName: ?[]const u8, comptime T: type) !TypeId {
            const zigName = @typeName(T);

            const id = TypeId.of(T);

            if (declName) |dn| {
                if (self.nameToId.get(dn)) |eid| {
                    std.debug.assert(eid == id);
                    return id;
                }
                try self.nameToId.put(dn, id);
            }

            if (self.idToType.getPtr(id)) |t| {
                if (declName) |dn| {
                    if (t.declName) |edn| {
                        std.debug.assert(std.mem.eql(u8, edn, dn));
                    } else {
                        t.declName = dn;
                    }
                }

                return id;
            }

            try self.idToType.put(id, Type {.declName = declName, .zigName = zigName, .info = .unusable});

            const info = try self.genTypeInfo(T);
            const ty = Type{
                .declName = declName,
                .zigName = zigName,
                .info = info,
            };

            try self.idToType.put(id, ty);

            return id;
        }

        pub fn genTypeInfo(self: *Self, comptime T: type) !TypeInfo {
            const info = @typeInfo(T);

            return switch (T) {
                c_char => TypeInfo{ .primitive = "char" },
                c_short => TypeInfo{ .primitive = "short" },
                c_int => TypeInfo{ .primitive = "int" },
                c_long => TypeInfo{ .primitive = "long" },
                c_longlong => TypeInfo{ .primitive = "long long" },
                c_longdouble => TypeInfo{ .primitive = "long double" },
                c_uint => TypeInfo{ .primitive = "unsigned int" },
                c_ushort => TypeInfo{ .primitive = "unsigned short" },
                c_ulong => TypeInfo{ .primitive = "unsigned long" },
                c_ulonglong => TypeInfo{ .primitive = "unsigned long long" },

                else => switch (info) {
                    .void => TypeInfo{ .primitive = "void" },
                    .bool => TypeInfo{ .primitive = "bool" },
                    .int => |x| switch (x.signedness) {
                        .signed => switch (x.bits) {
                            8 => TypeInfo{ .primitive = "int8_t" },
                            16 => TypeInfo{ .primitive = "int16_t" },
                            32 => TypeInfo{ .primitive = "int32_t" },
                            64 => TypeInfo{ .primitive = "int64_t" },
                            else => return .unusable,
                        },
                        .unsigned => switch (x.bits) {
                            8 => TypeInfo{ .primitive = "uint8_t" },
                            16 => TypeInfo{ .primitive = "uint16_t" },
                            32 => TypeInfo{ .primitive = "uint32_t" },
                            64 => TypeInfo{ .primitive = "uint64_t" },
                            else => return .unusable,
                        },
                    },
                    .float => |x| switch (x.bits) {
                        32 => TypeInfo{ .primitive = "float" },
                        64 => TypeInfo{ .primitive = "double" },
                        else => return .unusable,
                    },
                    .@"opaque" => TypeInfo{ .@"opaque" = {} },
                    .@"fn" => |x| fun: {
                        if (!x.calling_convention.eql(std.builtin.CallingConvention.c)) return .unusable;

                        const returnType = try self.genType(x.return_type orelse return .unusable);

                        var paramTypes = std.ArrayList(TypeId).init(self.allocator);
                        inline for (x.params) |param| {
                            try paramTypes.append(try self.genType(param.type orelse return .unusable));
                        }

                        break :fun TypeInfo{ .function = .{ .returnType = returnType, .params = paramTypes.items } };
                    },
                    .@"struct" => |x| str: {
                        if (x.layout != .@"extern" and x.layout != .@"packed") return .unusable;

                        var fields = std.ArrayList(TypeInfo.Field).init(self.allocator);
                        inline for (x.fields) |field| {
                            try fields.append(TypeInfo.Field{ .name = field.name, .type = try self.genType(field.type) });
                        }

                        const packType = if (x.layout != .@"packed") null else if (x.backing_integer) |bi| pack: {
                            break :pack try self.genType(bi);
                        } else null;

                        break :str TypeInfo{ .@"struct" = .{
                            .fields = fields.items,
                            .packType = packType,
                        } };
                    },
                    .@"union" => |x| un: {
                        if (x.layout != .@"extern") return .unusable;
                        if (x.tag_type != null) return .unusable;

                        var fields = std.ArrayList(TypeInfo.Field).init(self.allocator);
                        inline for (x.fields) |field| {
                            try fields.append(TypeInfo.Field{ .name = field.name, .type = try self.genType(field.type) });
                        }

                        break :un TypeInfo{
                            .@"union" = fields.items,
                        };
                    },
                    .@"enum" => |x| en: {
                        if (!isValidTag(x.tag_type)) return .unusable;

                        var fields = std.ArrayList([]const u8).init(self.allocator);
                        inline for (x.fields) |field| {
                            try fields.append(field.name);
                        }

                        break :en TypeInfo{ .@"enum" = fields.items };
                    },
                    .pointer => |x| ptr: {
                        if (x.size == .Slice) return .unusable;
                        const child = try self.genType(x.child);
                        break :ptr TypeInfo{ .pointer = child };
                    },
                    .optional => |x| opt: {
                        if (@typeInfo(x.child) != .pointer) return .unusable;
                        break :opt self.genTypeInfo(x.child);
                    },
                    else => return .unusable,
                },
            };
        }
    };
}
