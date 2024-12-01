const std = @import("std");

pub const SOURCE_MODULE_NAME = "#HEADER_GENERATION_SOURCE_MODULE#";
pub const DATA_SOURCE_NAME = "HEADER-GENERATION-DATA";

/// A type either described in the HEADER-GENERATION-DATA source or implementing a function with the following signature:
/// - `fn generate_c_repr(name: []const u8, expr: []const u8, generator: anytype, writer: anytype) anyerror!void`
///
/// the provided `generator` will contain at least an `allocator` field, as well as the following api:
/// - `fn findTypeName(self: *const Self, comptime T: type) ![]const u8`
/// - `fn lookupType(self: *const Self, name: []const u8) ?*Type`
/// - `fn genType(self: *Self, comptime T: type) !TypeId`
/// - `fn genTypeDecl(self: *Self, declName: ?[]const u8, comptime T: type) !TypeId`
/// - `fn genTypeInfo(self: *Self, comptime T: type) !TypeInfo`
pub const customtype = type;
pub const opaquetype = type;

pub inline fn MakeData(
    comptime hData: type,
) type {
    validateHData(hData);
    validateCustomType(hData.CustomType, hData.customTypes);

    return struct {
        pub const CustomType = hData.CustomType;
        pub const ignoredDecls = hData.ignoredDecls;
        pub const head = hData.head;
        pub const foot = hData.foot;
        pub const prefix = hData.prefix;
        pub const customTypes = hData.customTypes;
        pub const enumSuffixes = hData.enumSuffixes;
        pub const procArgs = hData.procArgs;
    };
}


fn validateHData(comptime hData: type) void {
    const required = comptime &[_]struct {[]const u8, []const u8} {
        .{ "CustomType",
        \\    A tagged union describing the kinds of custom types that will be used in the header;
        \\    it should contain at least the field `Generative` of type `void`;
        \\    it should contain a member function with the following signature:
        \\        `pub fn render(self: CustomType, name: []const u8, generator: anytype, writer: anytype) anyerror!void`
        \\    Information about `generator` can be found in the doc for `customtype`
        },
        .{ "customTypes",
        \\    A struct mapping custom type names to their values, which should all be of the CustomType provided
        },
        .{ "ignoredDecls",
        \\    A tuple struct listing names of pub declarations that should be ignored by the header generator.
        },
        .{ "head",
        \\    A string containing C source code to include at the start of the generated header
        },
        .{ "foot",
        \\    A string containing C source code to include at the end of the generated header
        },
        .{ "prefix",
        \\    A string to use for namespacing all identifiers in the generated header
        },
        .{ "enumSuffixes",
        \\    A struct mapping enum names to suffixes to append to their variants
        },
        .{ "procArgs",
        \\    A struct mapping procedure type names to their argument types and names.
        \\    The types can be found through reflection, but they are required here to ensure that the names don't become desynced.
        },
    };

    inline for (required) |req| {
        if (!@hasDecl(hData, req[0])) {
            @compileError("`" ++ DATA_SOURCE_NAME ++ "` must have a `" ++ req[0] ++ "` decl, which should be:\n" ++ req[1]);
        }
    }

    inline for (comptime std.meta.fieldNames(hData)) |name| {
        @compileLog("`" ++ DATA_SOURCE_NAME ++ "` has unexpected field `" ++ name ++ "`");
    }

    inline for (comptime std.meta.declarations(hData)) |decl| {
        comptime var okay = false;

        inline for (required) |req| {
            if (comptime std.mem.eql(u8, decl.name, req[0])) {
                okay = true;
                break;
            }
        }

        if (comptime !okay) {
            @compileError("`" ++ DATA_SOURCE_NAME ++ "` has unexpected decl `" ++ decl.name ++ "`");
        }
    }
}

fn validateCustomType(comptime hCustomType: type, comptime hCustomTypes: anytype) void {
    if (comptime !@hasField(hCustomType, "generative")) {
        @compileError("CustomType must have a generative field");
    }

    if (comptime @typeInfo(hCustomType) != .@"union") {
        @compileError("CustomType must be a tagged union");
    }

    if (comptime @typeInfo(hCustomType).@"union".tag_type == null) {
        @compileError("CustomType must be a tagged union");
    }

    inline for (@typeInfo(hCustomType).@"union".fields) |field| {
        if (comptime !std.mem.eql(u8, field.name, "generative")) {
            continue;
        }

        if (comptime field.type != void) {
            @compileError("CustomType.generative must be of type void");
        }
    }

    if (comptime !@hasDecl(hCustomType, "render")) {
        @compileError("CustomType must have a render method");
    }

    inline for (comptime std.meta.fieldNames(@TypeOf(hCustomTypes))) |name| {
        if (comptime !(@TypeOf(@field(hCustomTypes, name)) == hCustomType)) {
            @compileError("CustomTypes values must be of the provided CustomType");
        }
    }
}
