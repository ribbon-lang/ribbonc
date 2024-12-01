const std = @import("std");

const Manifest = @import("ZigBuildUtils").Manifest;

const ISA = @import("ISA");

pub const std_options = std.Options{
    .log_level = .warn,
};

const log = std.log.scoped(.@"templater");

const Mode = enum {
    VERSION,
    TOC,
    BODY,
};

const VERSION = "version";
const TOC = "toc";
const BODY = "body";

pub fn main() !void {
    @setEvalBranchQuota(10_000);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const manifest = try Manifest.readFile(allocator, "build.zig.zon");

    const args = try std.process.argsAlloc(allocator);
    if (args.len < 2) {
        log.err("expected either `{s}`, `{s}` or `{s}`, got nothing", .{ VERSION, TOC, BODY });
        return error.NotEnoughArguments;
    } else if (args.len > 2) {
        log.err("expected 1 argument, got {}:", .{args.len - 1});
        for (args[1..]) |arg| {
            log.err("`{s}`", .{arg});
        }
        return error.TooManyArguments;
    }

    const mode: Mode = mode: {
        if (std.mem.eql(u8, args[1], VERSION)) {
            break :mode .VERSION;
        } else if (std.mem.eql(u8, args[1], TOC)) {
            break :mode .TOC;
        } else if (std.mem.eql(u8, args[1], BODY)) {
            break :mode .BODY;
        } else {
            log.err("expected either `{s}`, `{s}` or `{s}`, got `{s}`", .{ VERSION, TOC, BODY, args[1] });
            return error.InvalidModeArgument;
        }
    };

    const out = std.io.getStdOut().writer();

    switch (mode) {
        .VERSION => {
            try out.print("v{}", .{ manifest.version });
        },
        .TOC => {
            inline for (ISA.Instructions) |category| {
                try out.print("* [{s}](#{s})\n", .{ category.name, comptime kebabCase(category.name) });
            }
        },
        .BODY => try body(out),
    }
}


fn body(out: anytype) !void {
    @setEvalBranchQuota(25_000);

    comptime var i: u16 = 0;

    inline for (ISA.Instructions) |category| {
        try out.print("#### {s}\n", .{ category.name });

        try out.print("{s}\n", .{ category.description });

        inline for (category.kinds) |kind| {
            try out.print("+ [{s}](#{s})\n", .{ kind.base_name, comptime kebabCase(kind.base_name) });
        }

        inline for (category.kinds) |kind| {
            try out.print("##### {s}\n", .{ comptime kind.humanFriendlyName() });

            try out.print("{s}\n", .{ kind.description });

            try out.writeAll("| Op code | Name | Description | Operands |\n");
            try out.writeAll("| ------- | ---- | ----------- | -------- |\n");

            inline for (kind.instructions) |instr| {
                try out.print("| `{x:0>4}` | **{s}** | {s} | ", .{ i, comptime ISA.computeInstructionName(kind, instr), instr.description });

                inline for (instr.operands, 0..) |operand, n| {
                    switch (operand) {
                        .register => try out.writeAll("`R`"),
                        .byte => try out.writeAll("`b`"),
                        .short => try out.writeAll("`s`"),
                        .immediate => try out.writeAll("`i`"),
                        .handler_set_index => try out.writeAll("`H`"),
                        .evidence_index => try out.writeAll("`E`"),
                        .global_index => try out.writeAll("`G`"),
                        .upvalue_index => try out.writeAll("`U`"),
                        .function_index => try out.writeAll("`F`"),
                        .block_index => try out.writeAll("`B`"),
                    }
                    if (n < instr.operands.len - 1) {
                        try out.writeAll(",&nbsp;");
                    }
                }

                if (instr.wide_immediate) {
                    if (instr.operands.len > 0) {
                        try out.writeAll("&nbsp;+&nbsp;");
                    }

                    try out.writeAll("`w`");
                }

                comptime i += 1;

                try out.writeAll(" |\n");
            }

            try out.writeAll("\n");
        }

        try out.writeAll("\n");
    }
}

fn kebabCase(comptime str: []const u8) []const u8 {
    comptime var out: []const u8 = "";

    comptime var haveDash = true;

    inline for (str) |c| {
        out = out ++ dash: {
            if ((std.ascii.isUpper(c) or std.ascii.isWhitespace(c)) and !haveDash) {
                haveDash = true;
                break :dash "-";
            } else {
                haveDash = false;
                break :dash "";
            }
        } ++ if (!std.ascii.isWhitespace(c) and std.mem.indexOfScalar(u8, "<>", c) == null) &[1]u8{ std.ascii.toLower(c) } else &[0]u8 {};
    }

    return out;
}
