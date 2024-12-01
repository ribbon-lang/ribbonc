const std = @import("std");

const MiscUtils = @import("Utils").Misc;
const TypeUtils = @import("Utils").Type;

const clap = @import("clap");

const Config = @import("Config");

const ANSI = @import("Utils").Ansi;

pub const commands = .{
    .help = .{"Display commands help message"},
    .quit = .{ "q", "Exit the REPL" },
    .@"clear-screen" = .{ "clear", "Clear the screen" },
    .@"clear-history" = .{"Clear the history"},
};

pub const commandNames = gatherCommandNames(std.meta.fieldNames(@TypeOf(commands)));

fn gatherCommandNames(comptime names: []const []const u8) []const []const u8 {
    comptime {
        var outNames: []const []const u8 = &[0][]const u8{};

        for (names) |name| {
            outNames = outNames ++ &[1][]const u8{name};

            const field = @field(commands, name);

            if (field.len == 2) {
                outNames = outNames ++ &[1][]const u8{field[0]};
            }
        }

        return outNames;
    }
}

pub fn printCommands(out: anytype) !void {
    try out.print("Commands:\n", .{});

    const longNames = comptime std.meta.fieldNames(@TypeOf(commands));

    comptime var longestShort = 0;
    comptime var longestLong = 0;

    comptime for (longNames) |commandName| {
        const command = @field(commands, commandName);
        const lenShort = switch (command.len) {
            1 => 0,
            2 => command[0].len + 2,
            else => unreachable,
        };
        const lenLong = commandName.len;
        if (longestShort < lenShort) longestShort = lenShort;
        if (longestLong < lenLong) longestLong = lenLong;
    };

    inline for (longNames) |commandName| {
        const command = @field(commands, commandName);

        const short = switch (command.len) {
            1 => "",
            2 => command[0],
            else => unreachable,
        };

        const shortFill = [1]u8{' '} ** (longestShort + 2 - short.len);

        const longFill = [1]u8{' '} ** (longestLong + 12 - commandName.len);

        const desc = switch (command.len) {
            1 => command[0],
            2 => command[1],
            else => unreachable,
        };

        const Style = if (Config.USE_ANSI_STYLES) ANSI.Style else ANSI.NoStyle;

        try if (short.len > 0) out.print("{s}{s}:{s}{s}, {s}:{s}{s}{s}{s}\n", .{
            shortFill,
            Style.Color.Foreground.Yellow,
            short,
            Style.Color.Foreground.Default,
            Style.Color.Foreground.Yellow,
            commandName,
            Style.Color.Foreground.Default,
            longFill,
            desc,
        }) else out.print("   {s}{s}:{s}{s}{s}{s}\n", .{
            shortFill,
            Style.Color.Foreground.Yellow,
            commandName,
            Style.Color.Foreground.Default,
            longFill,
            desc,
        });
    }
}

pub const options =
    clap.parseParamsComptime(std.fmt.comptimePrint(
    \\--help                       Display options help message, and exit
    \\--version                    Display SemVer2 version number for rli, and exit
    \\-i, --interactive            Run the compiler in REPL mode
    \\--dump-stdin <bool>          (REPL) Dump stdin to a file [Default: {}]
    \\--history <path>             (REPL) Path to the REPL history file [Default: {s}]
    \\--disable-raw-mode           (REPL) Disable raw line editing mode
    \\--use-emoji <bool>           Use emoji in the output [Default: {}]
    \\--use-ansi-styles <bool>     Use ANSI styles in the output [Default: {}]
    \\--max-comptime-depth <uint>  Maximum call stack depth for the compile time evaluator [Default: {}; Note: going higher may cause segfaults due to native stack overflow; Minimum: {}]
    \\<path>...                    Root files to include in the compilation
, .{
    Config.REPL_DUMP_STDIN_DEFAULT,
    Config.REPL_HISTORY_PATH_DEFAULT,
    Config.USE_EMOJI_DEFAULT,
    Config.USE_ANSI_STYLES_DEFAULT,
    Config.MAX_COMPTIME_DEPTH_DEFAULT,
    Config.MAX_COMPTIME_DEPTH_MIN,
}));

pub const ArgsResult = union(enum) {
    exit: void,
    execute: Execute,

    pub const Execute = struct {
        interactive: bool,
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
                .interactive = true,
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
        try stdout.print("rli v{}\n", .{Config.VERSION});
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

    if (res.args.@"max-comptime-depth") |depth| {
        if (depth < Config.MAX_COMPTIME_DEPTH_MIN) {
            try stderr.print("Error: --max-comptime-depth ({} provided) must be >= {}\n", .{ depth, Config.MAX_COMPTIME_DEPTH_MIN });
            try printUsage(stderr);
            return error.InvalidArgument;
        }
        if (Config.MAX_COMPTIME_DEPTH == depth) {
            try stderr.print("Note: --max-comptime-depth flag is unnecessary, config is already set to {} by default\n", .{depth});
        }
        Config.MAX_COMPTIME_DEPTH = depth;
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

    const interactive = interactive: {
        if (res.args.interactive != 0) {
            if (res.args.@"disable-raw-mode" != 0) {
                Config.REPL_DISABLE_RAW_MODE = true;
            }

            if (res.args.@"dump-stdin") |dump| {
                if (Config.REPL_DUMP_STDIN == dump) {
                    try stderr.print("Note: --dump-stdin flag is unnecessary, config is already set to {} by default\n", .{dump});
                }
                Config.REPL_DUMP_STDIN = dump;
            }

            if (res.args.history) |history| {
                if (std.mem.eql(u8, Config.REPL_HISTORY_PATH, history)) {
                    try stderr.print("Note: --history flag is unnecessary, config is already set to {s} by default\n", .{history});
                }
                Config.REPL_HISTORY_PATH = history;
            }

            break :interactive true;
        } else {
            if (res.args.@"disable-raw-mode" != 0) {
                try stderr.print("Warning: --disable-raw-mode requires --interactive, it will be ignored for this compilation\n", .{});
            }

            if (res.args.@"dump-stdin") |_| {
                try stderr.print("Warning: --dump-stdin requires --interactive, it will be ignored for this compilation\n", .{});
            }

            if (res.args.history) |_| {
                try stderr.print("Warning: --history requires --interactive, it will be ignored for this compilation\n", .{});
            }

            break :interactive false;
        }
    };

    var rootFiles = std.ArrayList([]const u8).init(allocator);
    defer rootFiles.deinit();

    for (res.positionals) |path| {
        try rootFiles.append(try allocator.dupe(u8, path));
    }

    return ArgsResult{
        .execute = ArgsResult.Execute{
            .allocator = allocator,
            .interactive = interactive,
            .rootFiles = try rootFiles.toOwnedSlice(),
        },
    };
}

pub fn printUsage(out: anytype) !void {
    try out.print("Usage: rli ", .{});
    clap.usage(out, clap.Help, options[2..]) catch |err| {
        try out.print("Error while printing usage: {s}\n", .{@errorName(err)});
        return error.ClapError;
    };
    try out.print("\n     | rli --help", .{});
    try out.print("\n     | rli --version", .{});
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
