const std = @import("std");

pub const std_options = std.Options {
    .log_level = .warn,
};

const log = std.log.scoped(.templater);

pub var DEP_MARKER: u8 = '%';
pub var PARAM_MARKER: u8 = '#';

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);

    log.debug("ARGS:", .{});
    for (args) |arg| {
        log.debug("`{s}`", .{arg});
    }

    if (args.len < 3) {
        return error.NotEnoughArguments;
    }

    const templateFileName = args[1];

    const binaries = try acquireBinaries(allocator, args[3..]);

    const instantiation = try instantiateWithEnv(allocator, templateFileName, binaries);

    if (!std.mem.eql(u8, args[2], "-no-static")) {
        const outputFileName = args[2];
        log.debug("output file is `{s}`", .{outputFileName});
        const cwd = std.fs.cwd();

        const tempOutputFileName = try std.fmt.allocPrint(allocator, "{s}.tmp", .{outputFileName});
        errdefer cwd.deleteFile(tempOutputFileName) catch {};


        if (std.fs.path.dirname(outputFileName)) |dirname| {
            cwd.makePath(dirname) catch |err| {
                log.err("cannot make output path: {}\n", .{err});
                return error.InvalidOutputPath;
            };
        }
        const outputFile = cwd.createFile(tempOutputFileName, .{.exclusive = true}) catch |err| {
            log.err("Unable to create output file `{s}`: {}\n", .{tempOutputFileName, err});
            return error.CannotCreateFile;
        };
        defer outputFile.close();

        const output = outputFile.writer();

        try output.writeAll(instantiation);

        cwd.deleteFile(outputFileName) catch |err| {
            if (err != error.FileNotFound) {
                log.err("Unable to delete `{s}`: {}\n", .{outputFileName, err});
                return err;
            }
        };

        cwd.rename(tempOutputFileName, outputFileName) catch |err| {
            log.err("Unable to rename `{s}` to `{s}`: {}\n", .{tempOutputFileName, outputFileName, err});
            return error.CannotRenameFile;
        };

        const meta = outputFile.metadata() catch |err| {
            log.err("Unable to stat `{s}`: {}\n", .{outputFileName, err});
            return error.CannotStatFile;
        };

        var perms = meta.permissions();

        perms.setReadOnly(true);

        outputFile.setPermissions(perms) catch |err| {
            log.err("Unable to set permissions on `{s}`: {}\n", .{outputFileName, err});
            return error.CannotSetPermissions;
        };
    } else {
        try std.io.getStdOut().writer().writeAll(instantiation);
    }
}

fn acquireBinaries(allocator: std.mem.Allocator, args: []const []const u8) !std.StringHashMap([]const u8) {
    var bindings = std.StringHashMap([]const u8).init(allocator);

    var i: usize = 0;
    while (i < args.len - 1) {
        const name = args[i];
        const bin = args[i + 1];

        try bindings.put(name, bin);

        i += 2;
    }

    return bindings;
}

pub const Markers = struct {
    start: []const u8,
    end: []const u8,
    pub const ByExt = ByExt: {
        const lineComment = Markers {.start = "////", .end = "////"};
        const htmlComment = Markers {.start = "<!--", .end = "-->"};

        break :ByExt .{
            .c = lineComment,
            .h = lineComment,
            .cpp = lineComment,
            .hpp = lineComment,
            .zig = lineComment,
            .html = htmlComment,
            .md = htmlComment,
        };
    };
};

pub fn getMarkers(templateFileName: []const u8) !Markers {
    if (std.mem.lastIndexOfScalar(u8, templateFileName, '.')) |extStart| {
        if (extStart < templateFileName.len) {
            const ext = templateFileName[extStart + 1..];

            inline for (comptime std.meta.fieldNames(@TypeOf(Markers.ByExt))) |fieldName| {
                if (std.mem.eql(u8, ext, fieldName)) return @field(Markers.ByExt, fieldName);
            }

            log.err("unknown extension in template file name: {s}\n", .{ext});
            return error.UnknownTemplateExtension;
        }
    }

    log.err("no extension found in template file name: {s}\n", .{templateFileName});
    return error.NoTemplateExtension;
}


pub const Data = struct {
    deps: []const []const u8,
    params: []const []const u8,
};

pub fn queryData(allocator: std.mem.Allocator, templateFileName: []const u8) anyerror!Data {
    const QueryCtx = struct {
        deps: std.ArrayList([]const u8),
        params: std.StringArrayHashMap(void),

        const Self = @This();

        fn onText(_: *Self, _: []const u8) anyerror!void {}
        fn onDep(ctx: *Self, arguments: []const []const u8) anyerror!void {
            for (arguments) |dep| {
                try ctx.deps.append(dep);
            }
        }
        fn onParam(ctx: *Self, arguments: []const []const u8) anyerror!void {
            try ctx.params.put(arguments[0], {});
        }
    };

    var ctx = QueryCtx {
        .deps = std.ArrayList([]const u8).init(allocator),
        .params = std.StringArrayHashMap(void).init(allocator),
    };

    try iterate(allocator, templateFileName, &ctx);

    return .{
        .deps = ctx.deps.items,
        .params = ctx.params.keys(),
    };
}


pub fn instantiateWithStaticCallTrie(allocator: std.mem.Allocator, templateFileName: []const u8, static: anytype) anyerror![]const u8 {
    const StaticT = @TypeOf(static);
    const InstantiationCtx = struct {
        allocator: std.mem.Allocator,
        static: StaticT,

        const Self = @This();

        fn retrieveParameter(x: anytype, names: []const []const u8) anyerror![]const u8 {
            const name = names[0];

            inline for (comptime std.meta.fieldNames(@TypeOf(x))) |fieldName| {
                if (std.mem.eql(u8, name, fieldName)) {
                    const param = @field(x, fieldName);

                    if (comptime @TypeOf(param) == []const u8) {
                        return param;
                    } else {
                        return retrieveParameter(param, names[1..]);
                    }
                }
            }

            log.err("Unknown parameter `{s}`\n", .{name});
            return error.UnknownParameter;
        }

        fn onParam(ctx: *Self, arguments: []const []const u8) anyerror![]const u8 {
            return retrieveParameter(ctx.static, arguments);
        }
    };

    var ctx = InstantiationCtx {
        .allocator = allocator,
        .static = static,
    };

    return instantiateWith(allocator, templateFileName, &ctx);
}


pub fn instantiateWithStaticFunctions(allocator: std.mem.Allocator, templateFileName: []const u8, static: anytype) anyerror![]const u8 {
    const StaticT = @TypeOf(static);
    const InstantiationCtx = struct {
        allocator: std.mem.Allocator,
        intermediateBuffer: std.ArrayList(u8),
        static: StaticT,

        const Self = @This();

        fn onParam(ctx: *Self, arguments: []const []const u8) anyerror![]const u8 {
            ctx.intermediateBuffer.clearRetainingCapacity();

            const paramFn = paramFn: {
                inline for (comptime std.meta.fieldNames(StaticT)) |fieldName| {
                    if (std.mem.eql(u8, arguments[0], fieldName)) {
                        break :paramFn @field(ctx.static, fieldName);
                    }
                }

                log.err("Unknown parameter `{s}`\n", .{arguments[0]});
                return error.UnknownParameter;
            };

            try paramFn(ctx.allocator, &ctx.intermediateBuffer, arguments[1..]);

            return ctx.intermediateBuffer.items;
        }
    };

    var ctx = InstantiationCtx {
        .allocator = allocator,
        .intermediateBuffer = std.ArrayList(u8).init(allocator),
        .static = static,
    };

    return instantiateWith(allocator, templateFileName, &ctx);
}


pub const InstantiationParam = *const fn (allocator: std.mem.Allocator, outputBuffer: *std.ArrayList(u8), arguments: []const []const u8) anyerror!void;

pub fn instantiateWithFunctionTable(allocator: std.mem.Allocator, templateFileName: []const u8, table: *const std.StringHashMap(InstantiationParam)) anyerror![]const u8 {
    const InstantiationCtx = struct {
        allocator: std.mem.Allocator,
        intermediateBuffer: std.ArrayList(u8),

        const Self = @This();

        fn onParam(ctx: *Self, arguments: []const []const u8) anyerror![]const u8 {
            ctx.intermediateBuffer.clearRetainingCapacity();

            const paramFn = table.get(arguments[0]) orelse {
                log.err("Unknown parameter `{s}`\n", .{arguments[0]});
                return error.UnknownParameter;
            };

            try paramFn(ctx.allocator, &ctx.intermediateBuffer, arguments[1..]);

            return ctx.intermediateBuffer.items;
        }
    };

    var ctx = InstantiationCtx {
        .allocator = allocator,
        .intermediateBuffer = std.ArrayList(u8).init(allocator),
    };

    return instantiateWith(allocator, templateFileName, &ctx);
}


pub fn instantiateWithEnv(allocator: std.mem.Allocator, templateFileName: []const u8, userBinaries: std.StringHashMap([]const u8)) anyerror![]const u8 {
    const InstantiationCtx = struct {
        allocator: std.mem.Allocator,
        binaries: std.StringHashMap([]const u8),
        arguments: std.ArrayList([]const u8),
        instantiation: std.ArrayList(u8),
        stdout: std.ArrayList(u8),
        stderr: std.ArrayList(u8),

        const Self = @This();

        fn onParam(ctx: *Self, arguments: []const []const u8) anyerror![]const u8 {
            ctx.arguments.clearRetainingCapacity();
            ctx.instantiation.clearRetainingCapacity();
            ctx.stdout.clearRetainingCapacity();
            ctx.stderr.clearRetainingCapacity();

            const binary = ctx.binaries.get(arguments[0]) orelse {
                log.err("Unknown binary alias `{s}`\n", .{arguments[0]});
                return error.UnknownBinary;
            };
            try ctx.arguments.append(binary);

            try ctx.instantiation.appendSlice(binary);

            for (arguments[1..]) |arg| {
                try ctx.arguments.append(arg);
                try ctx.instantiation.append(' ');
                try ctx.instantiation.appendSlice(arg);
            }

            var ch = std.process.Child.init(ctx.arguments.items, ctx.allocator);
            ch.stdin_behavior = .Ignore;
            ch.stdout_behavior = .Pipe;
            ch.stderr_behavior = .Pipe;
            ch.spawn() catch |err| {
                log.err("Unable to spawn `{s}`: {}\n", .{ctx.instantiation.items, err});
                return err;
            };

            ch.collectOutput(&ctx.stdout, &ctx.stderr, std.math.maxInt(usize)) catch |err| {
                log.err("Unable to collect output from `{s}`: {}\n", .{ctx.instantiation.items, err});
                return err;
            };
            const term = ch.wait() catch |err| {
                log.err("Unable to wait for `{s}`: {}\n", .{ctx.instantiation.items, err});
                return err;
            };

            if (ctx.stderr.items.len > 0) {
                log.err("{s} stderr:\n{s}\n", .{ctx.instantiation.items, ctx.stderr.items});
            }

            switch (term) {
                .Exited => |code| {
                    if (code != 0) {
                        log.err("{s} exited with code {}\n", .{ctx.instantiation.items, code});
                        return error.TemplateFailed;
                    }
                },
                else => {
                    log.err("{s} failed, {}\n", .{ctx.instantiation.items, term});
                    return error.TemplateFailed;
                }
            }

            return ctx.stdout.items;
        }
    };

    var ctx = InstantiationCtx {
        .allocator = allocator,
        .binaries = userBinaries,
        .arguments = std.ArrayList([]const u8).init(allocator),
        .instantiation = std.ArrayList(u8).init(allocator),
        .stdout = std.ArrayList(u8).init(allocator),
        .stderr = std.ArrayList(u8).init(allocator),
    };

    return instantiateWith(allocator, templateFileName, &ctx);
}


pub fn instantiateWith(allocator: std.mem.Allocator, templateFileName: []const u8, userCtx: anytype) anyerror![]const u8 {
    const UserCtx = @TypeOf(userCtx);

    const InstantiationCtx = struct {
        lastIndent: []const u8,
        instantiation: std.ArrayList(u8),
        intermediateBuffer: std.ArrayList(u8),
        user: UserCtx,

        const Self = @This();

        fn onText(ctx: *Self, text: []const u8) anyerror!void {
            if (text.len == 0) return;

            if (comptime @hasDecl(@typeInfo(UserCtx).pointer.child, "onText")) {
                try ctx.user.onText(text);
            }

            try ctx.instantiation.appendSlice(text);

            var lineStart = text.len - 1;
            while (lineStart > 0) {
                const byte = text[lineStart];
                if (byte == '\n') {
                    lineStart += 1;
                    break;
                } else {
                    lineStart -= 1;
                }
            }

            var indent = text[lineStart..text.len];
            for (indent) |ch| {
                if (!std.ascii.isWhitespace(ch)) {
                    indent = indent[0..0];
                    break;
                }
            }

            ctx.lastIndent = indent;
        }

        fn onDep(_: *Self, _: []const []const u8) anyerror!void { }

        fn onParam(ctx: *Self, arguments: []const []const u8) anyerror!void {
            const prelimInstantiation: []const u8 = try ctx.user.onParam(arguments);

            var lineIter = std.mem.splitScalar(u8, prelimInstantiation, '\n');

            if (lineIter.next()) |line| {
                try ctx.intermediateBuffer.appendSlice(line);

                if (lineIter.peek() != null) {
                    try ctx.intermediateBuffer.append('\n');
                }
            }

            while (lineIter.next()) |line| {
                if (line.len > 0) {
                    try ctx.intermediateBuffer.appendSlice(ctx.lastIndent);
                }

                try ctx.intermediateBuffer.appendSlice(line);

                if (lineIter.peek() != null) {
                    try ctx.intermediateBuffer.append('\n');
                }
            }

            try ctx.instantiation.appendSlice(ctx.intermediateBuffer.items);

            ctx.intermediateBuffer.clearRetainingCapacity();
        }
    };

    var ctx = InstantiationCtx {
        .lastIndent = "",
        .instantiation = std.ArrayList(u8).init(allocator),
        .intermediateBuffer = std.ArrayList(u8).init(allocator),
        .user = userCtx,
    };

    const markers = try getMarkers(templateFileName);

    try ctx.instantiation.writer().print("{s} File generated from {s} {s}\n\n", .{markers.start, templateFileName, markers.end});

    try iterate(allocator, templateFileName, &ctx);

    return ctx.instantiation.items;
}


pub fn iterate(
    allocator: std.mem.Allocator,
    templateFileName: []const u8,
    ctx: anytype,
) anyerror!void {
    const markers = try getMarkers(templateFileName);
    const cwd = std.fs.cwd();

    const templateFile = cwd.openFile(templateFileName, .{.mode = .read_only}) catch |err| {
        log.err("Unable to open template file: {}\n", .{err});
        return error.CannotOpenFile;
    };
    defer templateFile.close();

    const template = try templateFile.reader().readAllAlloc(allocator, std.math.maxInt(usize));

    const depMarker = try std.fmt.allocPrint(allocator, "{s}{u}", .{markers.start, DEP_MARKER});
    const paramMarker = try std.fmt.allocPrint(allocator, "{s}{u}", .{markers.start, PARAM_MARKER});

    var argumentBuffer = std.ArrayList([]const u8).init(allocator);

    var i: usize = 0;
    templateLoop: while (i < template.len) {
        const iStart = i;

        const depOffset = std.mem.indexOf(u8, template[i..], depMarker);
        const paramOffset = std.mem.indexOf(u8, template[i..], paramMarker);

        const Kind = enum { dep, param };
        const start: struct {Kind, usize} = start: {
            if (depOffset != null and paramOffset != null) {
                const dep = depOffset.?;
                const param = paramOffset.?;
                if (dep < param) {
                    break :start .{.dep, dep};
                } else if (param < dep) {
                    break :start .{.param, param};
                } else unreachable;
            } else if (depOffset orelse paramOffset) |o| {
                break :start .{if (depOffset == null) .param else .dep, o};
            } else {
                try ctx.onText(template[i..]);
                break :templateLoop;
            }
        };

        i += start[1];

        try ctx.onText(template[iStart..i]);

        i += markers.start.len + 1;

        const endOffset = std.mem.indexOf(u8, template[i..], markers.end) orelse {
            log.err("No end marker found after offset {}\n", .{i});
            return error.NoEndMarker;
        };

        const iParam = i;

        i += endOffset;

        const param = template[iParam..i];

        i += markers.end.len;

        var tokenIter = std.mem.tokenizeAny(u8, param, " \t\r\n");

        while (tokenIter.next()) |arg| {
            try argumentBuffer.append(arg);
        }

        if (argumentBuffer.items.len == 0) {
            log.err("Empty template {s} at offset {}\n", .{@tagName(start[0]), i});
            return error.EmptyTemplateParameter;
        }

        switch (start[0]) {
            .dep => try ctx.onDep(argumentBuffer.items),
            .param => try ctx.onParam(argumentBuffer.items),
        }

        argumentBuffer.clearRetainingCapacity();
    }
}
