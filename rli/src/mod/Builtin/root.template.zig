////%src/mod/Builtin/private////
////%src/mod/Builtin/scripts////

const std = @import("std");

const MetaModule = @This();

pub const Envs = struct {
    pub usingnamespace GenEnvs;
    pub const Full = MetaModule.Full;
};

const GenEnvs = struct {
    ////#Source:Builtin envs////
};

pub const Docs = struct {
    ////#Source:Builtin docs////
};

pub const Scripts = .{
    ////#Source:Builtin scripts////
};

pub const EnvNameTagType = u8;
pub const MAX_ENVS = std.math.maxInt(EnvNameTagType);
pub const MAX_DECLS = 10_000;

pub const EnvNames = envNames: {
    const base = std.meta.fieldNames(EnvName);

    break :envNames base[0 .. base.len - 1];
};

pub const EnvName = envName: {
    var envIndex: comptime_int = 0;
    var envFields = ([1]std.builtin.Type.EnumField{undefined}) ** (MAX_ENVS + 1);

    for (@typeInfo(GenEnvs).@"struct".decls) |decl| {
        const envName = decl.name;
        envFields[envIndex] = std.builtin.Type.EnumField{ .name = envName, .value = envIndex };
        envIndex += 1;
    }

    envFields[envIndex] = std.builtin.Type.EnumField{ .name = "Full", .value = envIndex };
    envIndex += 1;

    break :envName @Type(std.builtin.Type{ .@"enum" = .{
        .tag_type = EnvNameTagType,
        .fields = envFields[0..envIndex],
        .decls = &[0]std.builtin.Type.Declaration{},
        .is_exhaustive = true,
    } });
};

const Full = full: {
    var fullIndex: comptime_int = 0;
    var full: FullType = undefined;

    for (@typeInfo(GenEnvs).@"struct".decls) |decl| {
        const envName = decl.name;
        const env = @field(GenEnvs, envName);

        for (0..env.len) |envIdx| {
            full[fullIndex] = env[envIdx];
            fullIndex += 1;
        }
    }

    break :full full;
};

const FullType = fullType: {
    var fullIndex: comptime_int = 0;
    var fullFields = ([1]std.builtin.Type.StructField{undefined}) ** MAX_DECLS;

    for (@typeInfo(GenEnvs).@"struct".decls) |decl| {
        const envName = decl.name;
        const env = @field(GenEnvs, envName);

        for (0..env.len) |envIdx| {
            const envItem = env[envIdx];
            const envT = @TypeOf(envItem);

            fullFields[fullIndex] = std.builtin.Type.StructField{
                .name = std.fmt.comptimePrint("{}", .{fullIndex}),
                .type = envT,
                .default_value = null,
                .is_comptime = false,
                .alignment = @alignOf(envT),
            };

            fullIndex += 1;
        }
    }

    break :fullType @Type(std.builtin.Type{ .@"struct" = .{
        .layout = .auto,
        .backing_integer = null,
        .fields = fullFields[0..fullIndex],
        .decls = &[0]std.builtin.Type.Declaration{},
        .is_tuple = true,
    } });
};
