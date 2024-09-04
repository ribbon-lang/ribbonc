const std = @import("std");

const Builtin = @import("Builtin");
const TextUtils = @import("ZigTextUtils");
const TypeUtils = @import("ZigTypeUtils");

pub const std_options = std.Options{
    .log_level = .warn,
};

const log = std.log.scoped(.@"templater:readme:builtin");

const Mode = enum {
    TOC,
    ENV,
};

const TOC = "toc";
const ENV = "env";

const LIST_SYM = "*";

pub fn main() !void {
    @setEvalBranchQuota(Builtin.MAX_DECLS);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    if (args.len < 2) {
        log.err("expected either `{s}` or `{s}`, got nothing", .{ TOC, ENV });
        return error.NotEnoughArguments;
    } else if (args.len > 2) {
        log.err("expected 1 argument, got {}:", .{args.len - 1});
        for (args[1..]) |arg| {
            log.err("`{s}`", .{arg});
        }
        return error.TooManyArguments;
    }

    const mode: Mode = mode: {
        if (std.mem.eql(u8, args[1], TOC)) {
            break :mode .TOC;
        } else if (std.mem.eql(u8, args[1], ENV)) {
            break :mode .ENV;
        } else {
            log.err("expected either `{s}` or `{s}`, got `{s}`", .{ TOC, ENV, args[1] });
            return error.InvalidModeArgument;
        }
    };

    const out = std.io.getStdOut().writer();
    const envNames = Builtin.EnvNames;

    inline for (envNames, 0..) |envName, i| {
        const doc = @field(Builtin.Docs, envName);
        const env = @field(Builtin.Envs, envName);
        const EnvT = @TypeOf(env);

        switch (mode) {
            .TOC => {
                const lowerEnvName = try TextUtils.toLowerStr(allocator, envName);
                try out.print("{s} [{s}](#{s})", .{ LIST_SYM, envName, lowerEnvName });
            },
            .ENV => {
                try out.print("#### {s}\n", .{envName});
                if (doc) |docText| {
                    try out.print("\n{s}\n", .{docText});
                }
                try out.writeAll("| Symbol | Description |\n|-|-|\n");

                const fieldNames = comptime std.meta.fieldNames(EnvT);
                inline for (fieldNames, 0..) |fieldName, j| {
                    const item = @field(env, fieldName);

                    const itemName = item[0];
                    const itemDesc: []const u8 = if (item.len == 3) item[1] else " ";

                    if (comptime TypeUtils.isString(@TypeOf(itemName))) {
                        try out.print("|`{s}`| {s} |", .{ try escapeName(allocator, itemName), itemDesc });
                    } else if (comptime TypeUtils.isTuple(@TypeOf(itemName))) {
                        try out.print("|", .{});
                        const tupleFields = comptime std.meta.fieldNames(@TypeOf(itemName));
                        inline for (tupleFields, 0..) |tupleFieldName, k| {
                            const alias = @field(itemName, tupleFieldName);
                            try out.print("`{s}`", .{try escapeName(allocator, alias)});
                            if (k < tupleFields.len - 1) {
                                try out.print(", ", .{});
                            }
                        }
                        try out.print("| {s} |", .{itemDesc});
                    } else {
                        @compileError("Expected string for symbol name");
                    }

                    if (j < fieldNames.len - 1) {
                        try out.writeAll("\n");
                    }
                }

                if (i < envNames.len - 1) {
                    try out.writeAll("\n");
                }
            },
        }

        if (i < envNames.len - 1) {
            try out.writeAll("\n");
        }
    }
}

fn escapeName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
    var escapedName = std.ArrayList(u8).init(allocator);

    for (name) |c| {
        if (c == '|') {
            try escapedName.appendSlice("\\|");
        } else {
            try escapedName.append(c);
        }
    }

    return escapedName.items;
}
