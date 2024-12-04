const std = @import("std");
const zig_builtin = @import("builtin");

const ANSI = @import("Utils").Ansi;
const MiscUtils = @import("Utils").Misc;
const TypeUtils = @import("Utils").Type;

pub const Builtin = @import("Builtin/root.zig");
pub const Context = @import("Context.zig");
pub const Interpreter = @import("Interpreter.zig");
pub const Parser = @import("Parser.zig");
pub const SExpr = @import("SExpr.zig").SExpr;
pub const Source = @import("Source.zig");

pub const log = std.log.scoped(.rli);

const Rli = @This();

test {
    std.testing.refAllDecls(@This());
}


allocator: std.mem.Allocator,
out: std.io.AnyWriter,
context: *Context,
parser: *Parser,
interpreter: *Interpreter,
readFileCallback: ?*const fn (rli: *Rli, []const u8) Error![]const u8 = null,
userdata: *anyopaque = undefined,

pub const Error = Parser.SyntaxError || Interpreter.Result || MiscUtils.IOError;

pub fn init(allocator: std.mem.Allocator, out: std.io.AnyWriter, builtinEnvs: Builtin.EnvSet, args: []const []const u8) Error!*Rli {
    log.info("initializing context ...", .{});
    var context = context: {
        if (Context.initGc()) |ptr| {
            log.info("... context ready", .{});
            break :context ptr;
        } else |err| {
            log.err("... failed to initialize context", .{});
            return err;
        }
    };
    errdefer context.deinit();

    log.info("initializing parser ...", .{});
    var parser = parser: {
        if (Parser.init(context)) |ptr| {
            log.info("... parser ready", .{});
            break :parser ptr;
        } else |err| {
            log.err("... failed to initialize parser", .{});
            return err;
        }
    };
    errdefer parser.deinit();

    log.info("initializing interpreter ...", .{});
    var interpreter = interpreter: {
        if (Interpreter.init(context)) |ptr| {
            log.info("... interpreter ready", .{});
            break :interpreter ptr;
        } else |err| {
            log.err("... failed to initialize interpreter", .{});
            return err;
        }
    };
    errdefer interpreter.deinit();

    const self = try allocator.create(Rli);

    self.* = Rli{
        .allocator = allocator,
        .out = out,
        .context = context,
        .parser = parser,
        .interpreter = interpreter
    };

    log.info("loading boot code ...", .{});
    {
        log.info("initializing primary process environment ...", .{});
        const sArgs = makeArgs(self.context, args) catch |err| {
            log.err("... failed to load command line arguments", .{});
            return err;
        };
        log.info("... command line arguments loaded", .{});

        self.interpreter.bindCustomEnv(self.interpreter.env, .{
            .{ "process-args", "a list of command line arguments to the compiler", sArgs },
        }) catch |err| {
            log.err("... failed to initialize primary process environment", .{});
            return err;
        };
        log.info("... primary process environment loaded", .{});

        log.info("initializing builtin environments ...", .{});
        var envIterator = Builtin.EnvIterator.from(builtinEnvs);
        while (envIterator.next()) |env| {
            self.interpreter.bindBuiltinEnv(self.interpreter.env, env) catch |err| {
                log.err("... failed to initialize builtin environment {s}", .{@tagName(env)});
                return err;
            };
            log.debug("... builtin environment {s} loaded", .{@tagName(env)});
        }
        log.info("... builtin environments loaded", .{});

        inline for (comptime std.meta.declarations(Builtin.Scripts)) |scriptDecl| {
            const scriptName = scriptDecl.name;
            log.info("running builtin script [{s}] ...", .{scriptName});
            const script = @field(Builtin.Scripts, scriptName);
            const result = self.runFile(scriptName, script) catch |err| {
                log.err("... failed to bind built-in script [{s}]", .{scriptName});
                return err;
            };
            log.info("... finished builtin script [{s}], result:\n{}", .{ scriptName, result });
        }

        log.info("modularizing environments ...", .{});
        self.interpreter.env = Interpreter.modularizeEnv(self.context.attr, self.interpreter.env) catch |err| {
            log.err("... failed to modularize environment", .{});
            return err;
        };
        log.debug("... environment modularized", .{});
    }


    return self;
}

pub fn deinit(self: *Rli) void {
    self.interpreter.deinit();
    self.parser.deinit();
    self.context.deinit();
    self.allocator.destroy(self);
}

pub fn expectedOutput(self: *Rli, comptime fmt: []const u8, args: anytype) Error!void {
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

fn makeArgs(context: *Context, args: []const []const u8) Error!SExpr {
    var sArgs = std.ArrayList(SExpr).init(context.allocator);
    defer sArgs.deinit();

    for (args) |arg| {
        const sArg = try SExpr.String(context.attr, arg);

        try sArgs.append(sArg);
    }

    return try SExpr.List(context.attr, sArgs.items);
}

pub fn readFile(self: *Rli, fileName: []const u8) Error!SExpr {
    log.info("reading [{s}] ...", .{fileName});
    const src =
        if (self.readFileCallback) |cb| try cb(self, fileName)
        else return error.AccessDenied;
    defer self.allocator.free(src);

    log.info("running [{s}] ...", .{fileName});

    const result = try self.runFile(fileName, src);
    log.info("... finished [{s}], result: {}", .{ fileName, result });
    return result;
}

pub fn readFiles(self: *Rli, fileNames: []const []const u8) Error!void {
    if (fileNames.len > 0) {
        log.info("running root files ...", .{});
        for (fileNames) |fileName| _ = try self.readFile(fileName);
        log.info("... finished running root files", .{});
    } else {
        log.info("... no root files provided", .{});
    }
}

pub fn runFile(self: *Rli, fileName: []const u8, text: []const u8) Error!SExpr {
    try self.parser.setFileName(fileName);
    self.parser.setInput(text, null);

    var result = try SExpr.Nil(try self.parser.mkAttr(null, null));

    while (self.parser.notEof()) {
        if (self.parser.scanSExprP()) |sexprM| {
            if (sexprM) |sexpr| {
                if (self.interpreter.eval(sexpr)) |sexprE| {
                    result = sexprE;
                } else |res| {
                    if (Interpreter.asEvaluationError(res)) |err| {
                        try self.expectedOutput("! {}", .{self.interpreter.errFmt(err)});
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
