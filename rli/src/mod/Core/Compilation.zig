const std = @import("std");
const ANSI = @import("Utils").Ansi;
const Config = @import("Config");
const MiscUtils = @import("Utils").Misc;
const TypeUtils = @import("Utils").Type;
const Core = @import("root.zig");
const SExpr = Core.SExpr;
const Context = Core.Context;
const Parser = Core.Parser;
const Eval = Core.Eval;

const Builtin = @import("Builtin");

const log = Core.log;

const Compilation = @This();

const EmojiTableT = struct {
    Ribbon: []const u8,
    Heart: []const u8,
};

const Emoji = EmojiTableT{
    .Ribbon = "🎀",
    .Heart = "💝",
};

const NoEmoji = EmojiTableT{
    .Ribbon = "",
    .Heart = "",
};

gpa: std.mem.Allocator,
out: std.io.AnyWriter,
style: ANSI.StyleT,
emoji: EmojiTableT,
ctx: *Context,
parser: *Parser,
eval: *Eval,

pub const Error = Parser.SyntaxError || Eval.Result || MiscUtils.IOError;

pub fn init(gpa: std.mem.Allocator, out: std.io.AnyWriter, args: []const []const u8) Error!*Compilation {
    log.info("initializing context ...", .{});
    var ctx = ctx: {
        if (Context.initGc()) |ptr| {
            log.info("... context ready", .{});
            break :ctx ptr;
        } else |err| {
            log.err("... failed to initialize context", .{});
            return err;
        }
    };
    errdefer ctx.deinit();

    log.info("initializing parser ...", .{});
    var parser = parser: {
        if (Parser.init(ctx)) |ptr| {
            log.info("... parser ready", .{});
            break :parser ptr;
        } else |err| {
            log.err("... failed to initialize parser", .{});
            return err;
        }
    };
    errdefer parser.deinit();

    log.info("intializing evaluator ...", .{});
    var eval = eval: {
        if (Eval.init(ctx)) |ptr| {
            log.info("... evaluator ready", .{});
            break :eval ptr;
        } else |err| {
            log.err("... failed to initialize evaluator", .{});
            return err;
        }
    };
    errdefer eval.deinit();

    const self = try gpa.create(Compilation);

    self.* = .{ .gpa = gpa, .out = out, .emoji = if (Config.USE_EMOJI) Emoji else NoEmoji, .style = if (Config.USE_ANSI_STYLES) ANSI.Style else ANSI.NoStyle, .ctx = ctx, .parser = parser, .eval = eval };

    log.info("loading boot code ...", .{});
    {
        log.info("initializing primary process environment ...", .{});
        const sargs = makeArgs(self.ctx, args) catch |err| {
            log.err("... failed to load command line arguments", .{});
            return err;
        };
        log.info("... command line arguments loaded", .{});

        self.eval.bindCustomEnv(self.eval.env, .{
            .{ "process-args", "a list of command line arguments to the compiler", sargs },
        }) catch |err| {
            log.err("... failed to initialize primary process environment", .{});
            return err;
        };
        log.info("... primary process environment loaded", .{});

        log.info("initializing builtin environment ...", .{});
        self.eval.bindBuiltinEnv(self.eval.env, .Full) catch |err| {
            log.err("... failed to initialize builtin environment", .{});
            return err;
        };
        log.info("... builtin environment loaded", .{});

        inline for (comptime std.meta.fieldNames(@TypeOf(Builtin.Scripts))) |scriptName| {
            const script = @field(Builtin.Scripts, scriptName);
            log.info("running builtin script [{s}] ...", .{scriptName});
            const result = self.runFile(scriptName, script) catch |err| {
                log.err("... failed to bind built-in script [{s}]", .{scriptName});
                return err;
            };
            log.info("... finished builtin script [{s}], result:\n{}", .{ scriptName, result });
        }

        log.info("... boot code loaded", .{});
    }


    return self;
}

pub fn deinit(self: *Compilation) void {
    self.eval.deinit();
    self.parser.deinit();
    self.ctx.deinit();
    self.gpa.destroy(self);
}

pub fn expectedOutput(self: *Compilation, comptime fmt: []const u8, args: anytype) Error!void {
    log.info(fmt, args);
    self.out.print(fmt ++ "\n", args) catch |err| {
        if (TypeUtils.narrowErrorSet(Error, err)) |e| {
            return e;
        } else {
            log.err("failed to write to output: {}", .{err});
            return error.Unexpected;
        }
    };
}

fn makeArgs(ctx: *Context, args: []const []const u8) Error!SExpr {
    var sargs = std.ArrayList(SExpr).init(ctx.allocator);
    defer sargs.deinit();

    for (args) |arg| {
        const sarg = try SExpr.String(ctx.attr, arg);

        try sargs.append(sarg);
    }

    return try SExpr.List(ctx.attr, sargs.items);
}

pub fn readFiles(self: *Compilation, files: []const []const u8) Error!void {
    if (files.len > 0) {
        log.info("running root files ...", .{});
        for (files) |file| {
            log.info("running [{s}] ...", .{file});
            const result = try self.readFile(file);
            log.info("... finished [{s}], result: {}", .{ file, result });
        }
        log.info("... finished running root files", .{});
    } else {
        log.info("... no root files provided", .{});
    }
}

pub fn readFile(self: *Compilation, fileName: []const u8) Error!SExpr {
    const text = text: {
        const file = std.fs.cwd().openFile(fileName, .{ .mode = .read_only }) catch |err| {
            log.err("could not open file [{s}]: {}", .{ fileName, err });
            return err;
        };
        defer file.close();

        const reader = file.reader();

        break :text try reader.readAllAlloc(self.gpa, std.math.maxInt(usize));
    };
    defer self.gpa.free(text);

    return self.runFile(fileName, text);
}

pub fn runFile(self: *Compilation, fileName: []const u8, text: []const u8) Error!SExpr {
    try self.parser.setFileName(fileName);
    self.parser.setInput(text, null);

    var result = try SExpr.Nil(try self.parser.mkAttr(null, null));

    while (self.parser.notEof()) {
        if (self.parser.scanSExprP()) |msexpr| {
            if (msexpr) |sexpr| {
                if (self.eval.resolve(sexpr)) |esexpr| {
                    result = esexpr;
                } else |res| {
                    if (Eval.asEvaluationError(res)) |err| {
                        try self.expectedOutput("! {}", .{self.eval.errFmt(err)});
                        return err;
                    } else {
                        log.err("unexpected result in file [{s}]: {}", .{ fileName, res });
                        return res;
                    }
                }
            } else {
                break;
            }
        } else |err| {
            if (Parser.isParseError(err)) {
                try self.expectedOutput("! Syntax error at [{s}:{}]", .{ fileName, self.parser.pos });
                return err;
            } else {
                log.err("unexpected error in file [{s}]: {}", .{ fileName, err });
                return err;
            }
        }
    }

    return result;
}
