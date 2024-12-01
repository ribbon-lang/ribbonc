const std = @import("std");

const clap = @import("clap");
const CLIMetaData = @import("CLIMetaData");
const commands = CLIMetaData.commands;
const options = CLIMetaData.options;

pub const std_options = std.Options{
    .log_level = .warn,
};

const log = std.log.scoped(.@"templater:readme:cli");

const Mode = enum {
    COMMANDS,
    USAGE,
    OPTIONS,
};

const COMMANDS = "commands";
const USAGE = "usage";
const OPTIONS = "options";

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    if (args.len < 2) {
        log.err("expected either `{s}`, `{s}` or `{s}`, got nothing", .{ COMMANDS, USAGE, OPTIONS });
        return error.NotEnoughArguments;
    } else if (args.len > 2) {
        log.err("expected 1 argument, got {}:", .{args.len - 1});
        for (args[1..]) |arg| {
            log.err("`{s}`", .{arg});
        }
        return error.TooManyArguments;
    }

    const mode: Mode = mode: {
        if (std.mem.eql(u8, args[1], COMMANDS)) {
            break :mode .COMMANDS;
        } else if (std.mem.eql(u8, args[1], USAGE)) {
            break :mode .USAGE;
        } else if (std.mem.eql(u8, args[1], OPTIONS)) {
            break :mode .OPTIONS;
        } else {
            log.err("expected either `{s}`, `{s}` or `{s}`, got `{s}`", .{ COMMANDS, USAGE, OPTIONS, args[1] });
            return error.InvalidModeArgument;
        }
    };

    const out = std.io.getStdOut().writer();

    switch (mode) {
        .COMMANDS => {
            try out.writeAll(
                \\| Command | Description |
                \\|-|-|
                \\|`:help`| Display a help message |
                \\|`:quit`| Exit the REPL |
                \\|`:clear-screen`| Clear the screen |
                \\|`:clear-history`| Clear the history |
                \\|`Ctrl`+`D`| Exit the REPL |
                \\|`Ctrl`+`C`| Cancel current line input |
            );
        },
        .USAGE => {
            try out.writeAll("```\nrli ");
            try clap.usage(out, clap.Help, options[2..]);
            try out.writeAll("\n```\n");
            for (0..2) |i| {
                try out.print("```\nrli --{s}\n```", .{options[i].names.long.?});
                if (i < 1) {
                    try out.writeAll("\n");
                }
            }
        },
        .OPTIONS => {
            try out.writeAll("| Option | Description |\n|-|-|\n");

            for (options, 0..) |option, i| {
                try out.writeAll("|");
                if (option.names.short != null or option.names.long != null) {
                    if (option.names.short) |s| short: {
                        try out.print("`-{u}", .{s});

                        if (option.takes_value == .none) {
                            try out.writeAll("`");
                            break :short;
                        } else {
                            try out.writeAll(" ");
                        }

                        try out.writeAll("<");
                        try out.writeAll(option.id.value());
                        try out.writeAll(">");

                        if (option.takes_value == .many) {
                            try out.writeAll("...");
                        }

                        try out.writeAll("`");
                    }

                    if (option.names.long) |l| long: {
                        if (option.names.short != null) {
                            try out.writeAll(", ");
                        }

                        try out.print("`--{s}", .{l});
                        if (option.takes_value == .none) {
                            try out.writeAll("`");
                            break :long;
                        } else {
                            try out.writeAll(" ");
                        }

                        try out.writeAll("<");
                        try out.writeAll(option.id.value());
                        try out.writeAll(">");

                        if (option.takes_value == .many) {
                            try out.writeAll("...");
                        }

                        try out.writeAll("`");
                    }
                } else if (option.takes_value != .none) {
                    try out.writeAll("`");
                    try out.writeAll("<");
                    try out.writeAll(option.id.value());
                    try out.writeAll(">");
                    if (option.takes_value == .many) {
                        try out.writeAll("...");
                    }
                    try out.writeAll("`");
                }
                try out.print("| {s} |", .{option.id.description()});

                if (i < options.len - 1) {
                    try out.writeAll("\n");
                }
            }
        },
    }
}
