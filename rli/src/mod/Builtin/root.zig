////%src/mod/Builtin/private////
////%src/mod/Builtin/scripts////

const std = @import("std");

const MetaModule = @This();

pub const Envs = GenEnvs;

pub const AllEnvs: EnvSet = allEnvs: {
    var set: EnvSet = undefined;
    for (EnvNames) |envName| {
        @field(set, envName) = true;
    }
    break :allEnvs set;
};

const GenEnvs = struct {
    pub const alist = @import("alist.zig");
    pub const arithmetic = @import("arithmetic.zig");
    pub const attr = @import("attr.zig");
    pub const binding = @import("binding.zig");
    pub const control = @import("control.zig");
    pub const conversion = @import("conversion.zig");
    pub const effect = @import("effect.zig");
    pub const env = @import("env.zig");
    pub const io = @import("io.zig");
    pub const list = @import("list.zig");
    pub const logical = @import("logical.zig");
    pub const meta = @import("meta.zig");
    pub const pair = @import("pair.zig");
    pub const parser = @import("parser.zig");
    pub const pattern = @import("pattern.zig");
    pub const procedure = @import("procedure.zig");
    pub const string = @import("string.zig");
    pub const symbol = @import("symbol.zig");
    pub const text = @import("text.zig");
    pub const @"type" = @import("type.zig");
};

pub const Scripts = struct {
    pub const module = @embedFile("module.bb");
};

pub const EnvSet = envSet: {
    var set = [1]std.builtin.Type.StructField{undefined} ** EnvNames.len;
    for (EnvNames, 0..) |envName, i| {
        set[i] = std.builtin.Type.StructField{ .name = envName, .type = bool, .alignment = 0, .is_comptime = false, .default_value = @ptrCast(&false) };
    }
    break :envSet @Type(.{ .@"struct" = std.builtin.Type.Struct{
        .fields = &set,
        .layout = .@"packed",
        .backing_integer = std.meta.Int(.unsigned, EnvNames.len),
        .decls = &.{},
        .is_tuple = false,
    } });
};

pub const EnvIterator = struct {
    set: EnvSet,
    index: usize = 0,

    pub fn from(envSet: EnvSet) EnvIterator {
        return EnvIterator{ .set = envSet };
    }

    pub fn reset(self: *EnvIterator) void {
        self.index = 0;
    }

    pub fn next(self: *EnvIterator) ?EnvName {
        if (self.index >= EnvNames.len) {
            return null;
        }

        inline for (EnvNames, 0..) |name, i| {
            if (i >= self.index) {
                if (@field(self.set, EnvNames[i])) {
                    self.index = i + 1;
                    return @field(EnvName, name);
                }
            }
        } else {
            self.index = std.math.maxInt(usize);
            return null;
        }
    }
};

pub const EnvNameTagType = u8;
pub const MAX_ENVS = std.math.maxInt(EnvNameTagType);
pub const MAX_DECLS = 10_000;

pub const EnvNames = std.meta.fieldNames(EnvName);

pub const EnvName = envName: {
    var envIndex: comptime_int = 0;
    var envFields = [1]std.builtin.Type.EnumField{undefined} ** MAX_ENVS;

    for (@typeInfo(GenEnvs).@"struct".decls) |decl| {
        const envName = decl.name;
        envFields[envIndex] = std.builtin.Type.EnumField{ .name = envName, .value = envIndex };
        envIndex += 1;
    }

    break :envName @Type(std.builtin.Type{ .@"enum" = .{
        .tag_type = EnvNameTagType,
        .fields = envFields[0..envIndex],
        .decls = &[0]std.builtin.Type.Declaration{},
        .is_exhaustive = true,
    } });
};
