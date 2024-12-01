const std = @import("std");

const MiscUtils = @import("Utils").Misc;
const TypeUtils = @import("Utils").Type;

const clap = @import("clap");

const Config = @import("Config");

const ANSI = @import("Utils").ANSI;


pub const options =
    clap.parseParamsComptime(std.fmt.comptimePrint(
    \\--help                       Display options help message, and exit
    \\--version                    Display SemVer2 version number for Rvm, and exit
    \\--use-emoji <bool>           Use emoji in the output [Default: {}]
    \\--use-ansi-styles <bool>     Use ANSI styles in the output [Default: {}]
    \\<path>...                    Files to execute
, .{
    Config.USE_EMOJI_DEFAULT,
    Config.USE_ANSI_STYLES_DEFAULT,
}));

pub const ArgsResult = union(enum) {
    exit: void,
    execute: Execute,

    pub const Execute = struct {
        allocator: std.mem.Allocator,
        rootFiles: []const []const u8,
    };

    pub fn deinit(self: ArgsResult) void {
        switch (self) {
            .exit => {},
            .execute => |x| {
                for (x.rootFiles) |path| {
                    x.allocator.free(path);
                }

                x.allocator.free(x.rootFiles);
            },
        }
    }
};

pub const CLIError = error{
    InvalidArgument,
    ClapError,
};

pub fn isCLIError(err: anyerror) bool {
    return TypeUtils.isInErrorSet(CLIError, err);
}

pub fn asCLIError(err: anyerror) ?CLIError {
    return TypeUtils.narrowErrorSet(CLIError, err);
}

pub fn processArgs(allocator: std.mem.Allocator, args: []const []const u8) (MiscUtils.IOError || std.mem.Allocator.Error || CLIError)!ArgsResult {
    if (args.len == 0) {
        return ArgsResult{
            .execute = ArgsResult.Execute{
                .allocator = allocator,
                .rootFiles = &[0][]const u8{},
            },
        };
    }

    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    const parsers = .{
        .bool = struct {
            fn parse(in: []const u8) !bool {
                if (std.mem.eql(u8, in, "true")) {
                    return true;
                } else if (std.mem.eql(u8, in, "false")) {
                    return false;
                } else {
                    return error.InvalidArgument;
                }
            }
        }.parse,
        .path = clap.parsers.default.string,
        .uint = clap.parsers.default.usize,
    };

    var iterator = MiscUtils.BufferIterator([]const u8).init(args);

    var diag = clap.Diagnostic{};
    var res = clap.parseEx(clap.Help, &options, parsers, &iterator, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        var longest = diag.name.longest();
        if (longest.kind == .positional) {
            longest.name = diag.arg;
        }

        try switch (err) {
            error.DoesntTakeValue => stderr.print(
                "The argument '{s}{s}' does not take a value\n\n",
                .{ longest.kind.prefix(), longest.name },
            ),
            error.MissingValue => stderr.print(
                "The argument '{s}{s}' requires a value but none was supplied\n\n",
                .{ longest.kind.prefix(), longest.name },
            ),
            error.InvalidArgument => stderr.print(
                "Invalid argument '{s}'\n\n",
                .{longest.name},
            ),
            else => stderr.print("Error while parsing arguments: {s}\n\n", .{@errorName(err)}),
        };

        try printUsage(stderr);

        return error.InvalidArgument;
    };
    defer res.deinit();

    if (res.args.version != 0) {
        try stdout.print("Rvm v{}\n", .{Config.VERSION});
        if (res.args.help != 0) {
            try stdout.print("\n", .{});
        }
    }

    if (res.args.help != 0) {
        try printOptions(stderr);
    }

    if (res.args.help != 0 or res.args.version != 0) {
        return ArgsResult.exit;
    }

    if (res.args.@"use-emoji") |styles| {
        if (styles == Config.USE_EMOJI_DEFAULT) {
            try stderr.print("Note: --use-emoji flag is unnecessary, config is already set to {} by default\n", .{styles});
        }
        Config.USE_EMOJI = styles;
    }

    if (res.args.@"use-ansi-styles") |styles| {
        if (styles == Config.USE_ANSI_STYLES_DEFAULT) {
            try stderr.print("Note: --use-ansi-styles flag is unnecessary, config is already set to {} by default\n", .{styles});
        }
        Config.USE_ANSI_STYLES = styles;
    }

    var rootFiles = std.ArrayList([]const u8).init(allocator);
    defer rootFiles.deinit();

    for (res.positionals) |path| {
        try rootFiles.append(try allocator.dupe(u8, path));
    }

    return ArgsResult{
        .execute = ArgsResult.Execute{
            .allocator = allocator,
            .rootFiles = try rootFiles.toOwnedSlice(),
        },
    };
}

pub fn printUsage(out: anytype) !void {
    try out.print("Usage: rvm ", .{});
    clap.usage(out, clap.Help, options[2..]) catch |err| {
        try out.print("Error while printing usage: {s}\n", .{@errorName(err)});
        return error.ClapError;
    };
    try out.print("\n     | rvm --help", .{});
    try out.print("\n     | rvm --version", .{});
    try out.print("\n", .{});
}

pub fn printOptions(out: anytype) !void {
    try printUsage(out);
    try out.print("\nDetails:\n", .{});
    clap.help(out, clap.Help, &options, .{
        .description_on_new_line = false,
    }) catch |err| {
        try out.print("Error while printing options: {s}\n", .{@errorName(err)});
        return error.ClapError;
    };
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
