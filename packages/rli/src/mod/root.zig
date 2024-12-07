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

pub fn init(allocator: std.mem.Allocator, cwd: std.fs.Dir, out: std.io.AnyWriter, builtinEnvs: Builtin.EnvSet, args: []const []const u8) Error!*Rli {
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

    log.info("initializing interpreter ...", .{});
    var interpreter = interpreter: {
        if (Interpreter.init(context, cwd)) |ptr| {
            log.info("... interpreter ready", .{});
            break :interpreter ptr;
        } else |err| {
            log.err("... failed to initialize interpreter", .{});
            return err;
        }
    };
    errdefer interpreter.deinit();

    log.info("initializing parser ...", .{});
    var parser = parser: {
        if (Parser.init(interpreter)) |ptr| {
            log.info("... parser ready", .{});
            break :parser ptr;
        } else |err| {
            log.err("... failed to initialize parser", .{});
            return err;
        }
    };
    errdefer parser.deinit();

    const self = try allocator.create(Rli);

    self.* = Rli{
        .allocator = allocator,
        .out = out,
        .context = context,
        .parser = parser,
        .interpreter = interpreter
    };

    log.info("initializing primary process environment ...", .{});
    const sArgs = makeArgs(self.context, args) catch |err| {
        log.err("... failed to load command line arguments", .{});
        return err;
    };
    log.info("... command line arguments loaded", .{});

    log.info("loading built-ins ...", .{});
    Builtin.bind(Error, self.interpreter, .{self, false}, runFile, builtinEnvs, .{
        .{ "process-args", "a list of command line arguments to the compiler", sArgs }
    }) catch |err| {
        log.err("... failed to load built-ins", .{});
        return err;
    };
    log.info("... built-ins loaded", .{});

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

    const result = try self.runFile(true, fileName, src);
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

pub fn runFile(self: *Rli, cleanEnv: bool, fileName: []const u8, text: []const u8) Error!SExpr {
    const termData = self.interpreter.terminationData;
    defer self.interpreter.terminationData = termData;
    self.interpreter.terminationData = null;
    try self.interpreter.callStack.append(try self.context.bindAttr(fileName, null, &.{}));
    defer _ = self.interpreter.callStack.pop();

    const old_env = self.interpreter.env;
    defer {
        if (cleanEnv) self.interpreter.env = old_env;
    }

    if (cleanEnv) {
        self.interpreter.env = try Interpreter.envBase(self.interpreter.env);
        try Interpreter.pushNewFrame(self.interpreter.context.attr, &self.interpreter.env);
    }

    const old_evidence = self.interpreter.evidence;
    defer self.interpreter.evidence = old_evidence;
    self.interpreter.evidence = try SExpr.Nil(self.interpreter.context.attr);

    const old_fileName = self.parser.fileName;
    try self.parser.setFileName(fileName);
    defer self.parser.fileName = old_fileName;
    const old_input = self.parser.input;
    const old_pos = self.parser.pos;
    self.parser.setInput(text, null);
    defer self.parser.setInput(old_input, old_pos);


    var result = try SExpr.Nil(try self.parser.mkAttr(null, null, &.{}));

    while (self.parser.notEof()) {
        if (self.parser.scanSExprP()) |sexprM| {
            if (sexprM) |sexpr| {
                if (self.interpreter.eval(sexpr)) |sexprE| {
                    result = sexprE;
                } else |res| {
                    if (Interpreter.asEvaluationError(res)) |err| {
                        try self.expectedOutput("! {}", .{self.interpreter.errFmt(err)});
                        return err;
                    } else if (Parser.asSyntaxError(res)) |err| {
                        try self.expectedOutput("! Syntax error at [{s}:{}]: {s} (last input: {u})", .{ fileName, self.parser.pos, @errorName(err), self.parser.input[self.parser.pos.offset] });
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
                log.err("[{s}]: {}", .{ fileName, err });
                return err;
            }
        }
    }

    return result;
}
