const std = @import("std");

const Manifest = @import("Utils").Build.Manifest;

const BuildMetaData = @import("BuildMetaData").make(@import("Utils").Type);
const commands = BuildMetaData.commands;
const options = BuildMetaData.options;
const buildOptions = BuildMetaData.buildOptions;

pub const std_options = std.Options{
    .log_level = .warn,
};

const log = std.log.scoped(.@"templater:readme:build");

const Mode = enum {
    ZIG_VERSION,
    VERSION,
    COMMANDS,
    OPTIONS,
};

const ZIG_VERSION = "zig-version";
const VERSION = "version";
const COMMANDS = "commands";
const OPTIONS = "options";

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const manifest = try Manifest.readFile(allocator, "build.zig.zon");

    const args = try std.process.argsAlloc(allocator);
    if (args.len < 2) {
        log.err("expected `{s}`, `{s}`, `{s}` or `{s}`, got nothing", .{ ZIG_VERSION, VERSION, COMMANDS, OPTIONS });
        return error.NotEnoughArguments;
    } else if (args.len > 2) {
        log.err("expected 1 argument, got {}:", .{args.len - 1});
        for (args[1..]) |arg| {
            log.err("`{s}`", .{arg});
        }
        return error.TooManyArguments;
    }

    const mode: Mode = mode: {
        if (std.mem.eql(u8, args[1], ZIG_VERSION)) {
            break :mode .ZIG_VERSION;
        } else if (std.mem.eql(u8, args[1], VERSION)) {
            break :mode .VERSION;
        } else if (std.mem.eql(u8, args[1], COMMANDS)) {
            break :mode .COMMANDS;
        } else if (std.mem.eql(u8, args[1], OPTIONS)) {
            break :mode .OPTIONS;
        } else {
            log.err("expected `{s}`, `{s}`, `{s}` or `{s}`, got `{s}`", .{ ZIG_VERSION, VERSION, COMMANDS, OPTIONS, args[1] });
            return error.InvalidModeArgument;
        }
    };

    const out = std.io.getStdOut().writer();

    switch (mode) {
        .ZIG_VERSION => {
            try out.print("{s}", .{@import("builtin").zig_version_string});
        },
        .VERSION => {
            try out.print("v{}", .{manifest.version});
        },
        .COMMANDS => {
            try out.writeAll("| Command | Description |\n|-|-|\n");

            const commandNames = comptime std.meta.fieldNames(@TypeOf(commands));

            inline for (commandNames) |commandName| {
                const commandDesc = @field(commands, commandName);
                try out.print("|`{s}`| {s} |\n", .{ commandName, commandDesc });
            }
        },
        .OPTIONS => {
            try out.writeAll("| Option | Description | Default |\n|-|-|-|\n");
            try printOptions(options, out);
            try printOptions(buildOptions, out);
        },
    }
}

fn printOptions(opts: anytype, out: anytype) !void {
    const optionNames = comptime std.meta.fieldNames(@TypeOf(opts));

    inline for (optionNames) |optionName| {
        const option = @field(opts, optionName);

        const optionType = option[0];
        const optionDesc = option[1];
        const optionDefault = if (comptime option.len == 3) option[2] else "null";

        if (comptime optionType == []const u8) {
            try out.print("|`-D{s}=<string>`| {s} |`{s}`|\n", .{ optionName, optionDesc, optionDefault });
        } else {
            try out.print("|`-D{s}=<{s}>`| {s} |`{any}`|\n", .{ optionName, @typeName(optionType), optionDesc, optionDefault });
        }
    }
}
