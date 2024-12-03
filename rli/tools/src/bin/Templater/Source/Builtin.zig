const std = @import("std");

const envPath = "src/mod/Builtin/private";
const scriptPath = "src/mod/Builtin/scripts";

pub const std_options = std.Options{
    .log_level = .warn,
};

const log = std.log.scoped(.@"templater:source:builtin");

const Mode = enum {
    ENVS,
    SCRIPTS,
    DOCS,
};

const ENVS = "envs";
const SCRIPTS = "scripts";
const DOCS = "docs";

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    if (args.len < 2) {
        log.err("expected `{s}`, `{s}` or `{s}`, got nothing", .{ ENVS, SCRIPTS, DOCS });
        return error.NotEnoughArguments;
    } else if (args.len > 2) {
        log.err("expected 1 argument, got {}:", .{args.len - 1});
        for (args[1..]) |arg| {
            log.err("`{s}`", .{arg});
        }
        return error.TooManyArguments;
    }

    const mode: Mode = mode: {
        if (std.mem.eql(u8, args[1], ENVS)) {
            break :mode .ENVS;
        } else if (std.mem.eql(u8, args[1], SCRIPTS)) {
            break :mode .SCRIPTS;
        } else if (std.mem.eql(u8, args[1], DOCS)) {
            break :mode .DOCS;
        } else {
            log.err("expected `{s}`, `{s}` or `{s}`, got `{s}`", .{ ENVS, SCRIPTS, DOCS, args[1] });
            return error.InvalidModeArgument;
        }
    };

    const output = std.io.getStdOut().writer();

    switch (mode) {
        .ENVS => {
            const envFiles = try readDir(allocator, envPath);
            for (envFiles) |fileName| {
                const envName = std.fs.path.stem(fileName);
                const filteredName = try filterName(allocator, envName);

                try output.print(
                \\pub const {s} = @import("Builtin:{s}").Env;
                \\
                , .{ filteredName, envName });
            }
        },
        .SCRIPTS => {
            var dir = try std.fs.cwd().openDir(scriptPath, .{.iterate = true});
            defer dir.close();

            var iter = dir.iterate();

            while (try iter.next()) |entry| {
                if (entry.kind != .file) {
                    log.warn("skipping `{s}`: not a file", .{entry.name});
                    continue;
                }

                const fileName = entry.name;
                const scriptName = std.fs.path.stem(fileName);
                const filteredName = try filterName(allocator, scriptName);

                const file = try dir.openFile(fileName, .{ .mode = .read_only });
                defer file.close();

                const reader = file.reader();

                try output.print("    .{s} =\n", .{ filteredName });

                while (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', std.math.maxInt(usize))) |line| {
                    try output.print("        \\\\{s}\n", .{ line });
                }

                try output.writeAll("        \\\\\n    ,\n");
            }
        },
        .DOCS => {
            const envFiles = try readDir(allocator, envPath);
            for (envFiles, 0..) |fileName, i| {
                const envName = std.fs.path.stem(fileName);
                const filteredName = try filterName(allocator, envName);

                try output.print(
                \\pub const {s}: ?[]const u8 = {s}: {{
                \\    const m = @import("Builtin:{s}");
                \\
                \\    if (@hasDecl(m, "Doc")) {{
                \\        break :{s} m.Doc;
                \\    }} else {{
                \\        break :{s} null;
                \\    }}
                \\}};
                \\
                , .{ filteredName, filteredName, envName, filteredName, filteredName });

                if (i < envFiles.len - 1) {
                    try output.writeAll("\n");
                }
            }
        },
    }
}

const ZigBuiltinNames = [_][]const u8 {"type", "struct", "union", "fn", "const", "var", "enum", "pub", "comptime", "extern", "export", "inline"};
const ZigAllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";

fn nameNeedsFilter(name: []const u8) bool {
    validate: for (name) |c| {
        inline for (ZigAllowedChars) |allowedChar| {
            if (c == allowedChar) {
                continue :validate;
            }
        }
        return true;
    }

    inline for (ZigBuiltinNames) |zigName| {
        if (std.mem.eql(u8, zigName, name)) return true;
    }

    return false;
}

fn filterName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
    if (nameNeedsFilter(name)) {
        const filtered = try allocator.alloc(u8, name.len + 3);
        filtered[0] = '@';
        filtered[1] = '"';
        std.mem.copyForwards(u8, filtered[2..], name);
        filtered[name.len + 2] = '"';
        return filtered;
    }

    return name;
}

fn readDir(allocator: std.mem.Allocator, path: []const u8) ![]const []const u8 {
    const cwd = std.fs.cwd();

    var dir = cwd.openDir(path, .{ .iterate = true }) catch |err| {
        log.err("cannot open directory `{s}`: {s}", .{ path, @errorName(err) });
        return err;
    };
    defer dir.close();

    var iter = dir.iterate();

    var files = std.ArrayList([]const u8).init(allocator);

    while (try iter.next()) |entry| {
        if (entry.kind != .file) {
            log.warn("skipping `{s}`: not a file", .{entry.name});
            continue;
        }

        try files.append(try allocator.dupe(u8, entry.name));
    }

    std.mem.sort([]const u8, files.items, @as(void, {}), struct {
        fn fun(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.lessThan(u8, a, b);
        }
    }.fun);

    return files.items;
}
