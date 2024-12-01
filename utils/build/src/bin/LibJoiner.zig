const std = @import("std");

pub const std_options = std.Options {
    .log_level = .warn,
};

const log = std.log.scoped(.libjoiner);

pub fn main () !void {
    log.info("libjoiner ", .{});

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);

    for (args) |arg| {
        log.info("{s} ", .{arg});
    }

    const realpath = try std.fs.cwd().realpathAlloc(allocator, ".");

    log.info("\ncwd: {s}\n", .{realpath});

    if (args.len < 4) {
        return error.NotEnoughArguments;
    }

    const target = args[1];
    const out = args[2];
    const libs = args[3..];

    try joiner(allocator, target, out, libs);
}


pub fn joiner(allocator: std.mem.Allocator, target: []const u8, out: []const u8, libs: [][]const u8) !void {
    const outTempBase = try std.fmt.allocPrint(allocator, "{s}-{s}", .{target, out});

    var cmd: []const u8 = "";

    var outTempName = outTempBase;

    var outName = out;

    var libNames = std.ArrayList([]const u8).init(allocator);

    var child: std.process.Child = child: {
        if (std.mem.startsWith(u8, target, "x86_64-windows") or std.mem.startsWith(u8, target, "aarch64-windows")) {
            log.debug("using zig lib\n", .{});

            cmd = "lib";

            var args = std.ArrayList([]const u8).init(allocator);

            try args.append("zig");
            try args.append("lib");

            outTempName = try std.fmt.allocPrint(allocator, "{s}.lib", .{outTempBase});

            const outArg = try std.fmt.allocPrint(allocator, "/out:{s}", .{outTempName});

            try args.append(outArg);

            outName = try std.fmt.allocPrint(allocator, "{s}.lib", .{out});
            try args.append(outName);

            for (libs) |lib| {
                const libName = try std.fmt.allocPrint(allocator, "{s}.lib", .{lib});
                try libNames.append(libName);
                try args.append(libName);
            }

            log.debug("generated command:\n", .{});
            for (args.items) |arg| {
                log.debug("{s} ", .{arg});
            }
            log.debug("\n", .{});

            var ch = std.process.Child.init(args.items, allocator);
            ch.stdin_behavior = .Ignore;
            ch.stdout_behavior = .Pipe;
            ch.stderr_behavior = .Pipe;
            try ch.spawn();

            log.debug("spawned zig lib\n", .{});

            break :child ch;
        } else {
            log.debug("using zig ar\n", .{});

            cmd = "ar";

            var script = std.ArrayList(u8).init(allocator);

            const writer = script.writer();

            outTempName = try std.fmt.allocPrint(allocator, "lib{s}.a", .{outTempBase});

            try writer.print("create {s}\n", .{outTempName});

            outName = try std.fmt.allocPrint(allocator, "lib{s}.a", .{out});

            try writer.print("addlib {s}\n", .{outName});

            for (libs) |lib| {
                const libName = try std.fmt.allocPrint(allocator, "lib{s}.a", .{lib});
                try libNames.append(libName);
                try writer.print("addlib {s}\n", .{libName});
            }

            try writer.print("save\nend\n", .{});

            log.debug("generated command:\necho \"{s}\" | zig ar -M\n", .{script.items});

            var ch = std.process.Child.init(&[_][]const u8 {"zig", "ar", "-M"}, allocator);
            ch.stdin_behavior = .Pipe;
            ch.stdout_behavior = .Pipe;
            ch.stderr_behavior = .Pipe;
            try ch.spawn();

            log.debug("spawned zig ar\n", .{});

            try ch.stdin.?.writer().writeAll(script.items);

            log.debug("wrote mri script to zig ar\n", .{});

            ch.stdin.?.close();
            ch.stdin = null;

            break :child ch;
        }
    };

    var stdout = std.ArrayList(u8).init(allocator);

    var stderr = std.ArrayList(u8).init(allocator);

    log.debug("collecting output\n", .{});

    try child.collectOutput(&stdout, &stderr, std.math.maxInt(usize));

    log.debug("collected output, waiting ...\n", .{});

    const term = try child.wait();

    log.debug("wait complete\n", .{});

    if (stdout.items.len > 0) {
        log.info("zig {s} stdout:\n{s}\n", .{cmd, stdout.items});
    }

    if (stderr.items.len > 0) {
        log.err("zig {s} stderr:\n{s}\n", .{cmd, stderr.items});
    }

    switch (term) {
        .Exited => |code| {
            if (code != 0) {
                log.err("zig {s} exited with code {}\n", .{cmd, code});
                return error.JoinFailed;
            }
        },
        else => {
            log.err("zig {s} failed, {}\n", .{cmd, term});
            return error.JoinFailed;
        }
    }

    log.debug("cleanup\n", .{});

    const cwd = std.fs.cwd();

    log.debug("copy {s} to {s}\n", .{outTempName, outName});
    try cwd.copyFile(outTempName, cwd, outName, .{});

    log.debug("delete {s}\n", .{outTempName});
    try cwd.deleteFile(outTempName);

    for (libNames.items) |lib| {
        log.debug("delete {s}\n", .{lib});
        try cwd.deleteFile(lib);
    }

    log.info("done\n", .{});
}
