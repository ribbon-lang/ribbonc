const std = @import("std");

const zig_builtin = @import("builtin");

const Config = @import("Config");
const MiscUtils = @import("Utils").Misc;
const REPL = @import("REPL").Builder(Compilation);
const CLIMetaData = @import("CLIMetaData");
const TextUtils = @import("Utils").Text;
const ANSI = @import("Utils").Ansi;
const Builtin = @import("Builtin");

const Core = @import("Core");
const Compilation = Core.Compilation;
const SExpr = Core.SExpr;
const Context = Core.Context;
const Parser = Core.Parser;
const Eval = Core.Eval;

const log = std.log.scoped(.rli);

pub const std_options = std.Options {
    .log_level = .warn,
    .logFn = MiscUtils.FilteredLogger(Config.LOG_SCOPES),
};

const Error = REPL.Error || Compilation.Error || CLIMetaData.CLIError;

pub const main = main: {
    if (zig_builtin.mode == .Debug) {
        break :main entry;
    } else {
        break :main struct {
            fn fun() u8 {
                entry() catch {
                    return 1;
                };

                return 0;
            }
        }.fun;
    }
};

fn entry() Error!void {
    if (zig_builtin.os.tag == .windows) {
        const succ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
        if (succ == 0) {
            const lastErr = std.os.windows.kernel32.GetLastError();
            const safeToPrint = @intFromEnum(lastErr) >= @intFromEnum(std.os.windows.Win32Error.SUCCESS) and @intFromEnum(lastErr) <= @intFromEnum(std.os.windows.Win32Error.IO_REISSUE_AS_CACHED);

            if (safeToPrint) {
                log.err("Warning: failed to set console output code page to UTF-8, error was {s}", .{@tagName(lastErr)});
            } else {
                log.err("Warning: failed to set console output code page to UTF-8, error was {}", .{@intFromEnum(lastErr)});
            }
        }
    }

    const stderr = std.io.getStdErr().writer();

    var GPA = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = GPA.deinit();

    const gpa = GPA.allocator();

    const args = std.process.argsAlloc(gpa) catch |err| {
        log.err("failed to get command line arguments: {}", .{err});
        return error.Unexpected;
    };
    defer gpa.free(args);

    const endOfOwnArgs =
        for (args, 0..) |arg, i| {
            if (std.mem.eql(u8, arg, "--")) {
                break i;
            }
        } else args.len;

    const scriptArgs = args[@min(endOfOwnArgs + 1, args.len)..];

    const argsResult = try CLIMetaData.processArgs(gpa, args[1..endOfOwnArgs]);
    defer argsResult.deinit();

    switch (argsResult) {
        .exit => return,
        .execute => |x| {
            var comp = try Compilation.init(gpa, stderr.any(), scriptArgs);
            defer comp.deinit();

            if (x.interactive) {
                try runRepl(comp, x.rootFiles);
            } else {
                try comp.readFiles(x.rootFiles);
            }
        },
    }
}

const CommandTable = std.StringHashMap(CommandFn);

const CommandFn = *const fn (comp: *Compilation, repl: *REPL) Compilation.Error!CommandDirective;

const CommandControl = enum {
    Break,
    Continue,
};

const CommandDirective = struct {
    saveHistory: bool = false,
    control: CommandControl = .Continue,
};

const COMMANDS = struct {
    fn quit(_: *Compilation, _: *REPL) !CommandDirective {
        return .{ .control = .Break };
    }

    fn @"clear-screen"(_: *Compilation, repl: *REPL) !CommandDirective {
        try repl.clearScreen();
        return .{};
    }

    fn @"clear-history"(_: *Compilation, repl: *REPL) !CommandDirective {
        repl.history.clear();
        repl.history.save(Config.REPL_HISTORY_PATH) catch |err| {
            log.err("failed to erase history on disk, {}", .{err});
        };
        return .{};
    }
};

fn buildCommandTable(allocator: std.mem.Allocator) !CommandTable {
    var commandTable = CommandTable.init(allocator);

    commandTable.put("help", struct {
        fn fun(_: *Compilation, repl: *REPL) !CommandDirective {
            try CLIMetaData.printCommands(repl.output.writer());
            return .{};
        }
    }.fun) catch |err| {
        log.err("... failed to add command 'help'", .{});
        return err;
    };

    inline for (comptime std.meta.fieldNames(@TypeOf(CLIMetaData.commands))) |commandName| {
        if (comptime std.mem.eql(u8, commandName, "help")) continue;

        const commandDesc = @field(CLIMetaData.commands, commandName);
        const commandFn = @field(COMMANDS, commandName);

        commandTable.put(commandName, commandFn) catch |err| {
            log.err("... failed to add command '{s}'", .{commandName});
            return err;
        };

        if (commandDesc.len == 2) {
            const shortName = commandDesc[0];
            commandTable.put(shortName, commandFn) catch |err| {
                log.err("... failed to add command '{s}' (shortcut for `{s}`)", .{ shortName, commandName });
                return err;
            };
        }
    }

    return commandTable;
}

fn getToken(pos: usize, buf: []const u8) TextUtils.Error!?[]const u8 {
    if (pos != buf.len) {
        const ch = (try TextUtils.decode1(buf[pos..])).ch;

        if (!(TextUtils.isSpace(ch) or std.mem.indexOfScalar(TextUtils.Char, &[_]TextUtils.Char{ '(', ')', '[', ']', '{', '}', ',', ';', '`', '\'', '\"', '\\', '#' }, ch) != null)) {
            return null;
        }
    }

    const availBuf = buf[0..pos];

    const lastChar = try TextUtils.lastChar(availBuf) orelse return null;

    if (TextUtils.isSpace(lastChar) or TextUtils.isControl(lastChar)) return null;

    var lastToken = try TextUtils.lastToken(false, TextUtils.isSpace, availBuf) orelse return null;

    while (lastToken.len > 0 and std.mem.indexOfAny(u8, lastToken, "()[]{},;`\'\"\\#") != null) {
        lastToken = lastToken[1..];
    }

    return lastToken;
}

fn completion(comp: *Compilation, allocator: std.mem.Allocator, pos: usize, buf: []const u8) Compilation.Error![]const []const u8 {
    if (buf.len > 0 and buf[0] == ':') {
        const token = buf[1..];

        return findCompletions(allocator, CLIMetaData.commandNames, token);
    }

    const lastToken = try getToken(pos, buf) orelse return &[0][]const u8{};

    const envKeys = try Eval.envKeyStrs(comp.eval.env, allocator);
    defer allocator.free(envKeys);

    return findCompletions(allocator, envKeys, lastToken);
}

fn findCompletions(allocator: std.mem.Allocator, envKeys: []const []const u8, token: []const u8) Compilation.Error![]const []const u8 {
    var out = std.ArrayList([]const u8).init(allocator);
    defer out.deinit();

    for (envKeys) |keyStr| {
        if (std.mem.startsWith(u8, keyStr, token)) {
            try out.append(try allocator.dupe(u8, keyStr[token.len..]));
        }
    }

    return out.toOwnedSlice();
}

fn hints(comp: *Compilation, allocator: std.mem.Allocator, pos: usize, buf: []const u8) Compilation.Error!?[]const u8 {
    if (buf.len > 0 and buf[0] == ':') {
        const token = buf[1..];

        return findHint(allocator, CLIMetaData.commandNames, token);
    }

    const lastToken = try getToken(pos, buf) orelse return null;

    const envKeys = try Eval.envKeyStrs(comp.eval.env, allocator);
    defer allocator.free(envKeys);

    return findHint(allocator, envKeys, lastToken);
}

fn findHint(allocator: std.mem.Allocator, envKeys: []const []const u8, token: []const u8) Compilation.Error!?[]const u8 {
    var out: ?[]const u8 = null;

    for (envKeys) |keyStr| {
        if (std.mem.startsWith(u8, keyStr, token)) {
            const sub = keyStr[token.len..];

            if (out) |dup| {
                allocator.free(dup);
                return null;
            } else {
                out = try allocator.dupe(u8, sub);
            }
        }
    }

    if (out) |x| {
        if (x.len == 0) {
            allocator.free(x);
            return null;
        }
    }

    return out;
}

fn runRepl(comp: *Compilation, files: []const []const u8) Error!void {
    const stdout = std.io.getStdOut().writer();

    try stdout.print("\n{s} {s}rli{s} {s}v{}{s} {s} {s}(REPL mode){s}\n", .{
        comp.emoji.Ribbon,
        comp.style.Color.Foreground.Magenta,
        comp.style.Color.Foreground.Default,
        comp.style.Color.Foreground.Green,
        Config.VERSION,
        comp.style.Color.Foreground.Default,
        comp.emoji.Ribbon,
        comp.style.Decoration.StartDim,
        comp.style.Decoration.EndBoldDim,
    });

    log.info("initializing line accumulator ...", .{});

    var lineAccumulator = acc: {
        if (std.ArrayList(u8).initCapacity(comp.gpa, 1024)) |acc| {
            log.info("... line accumulator ready", .{});
            break :acc acc;
        } else |err| {
            log.err("... failed to initialize line accumulator", .{});
            return err;
        }
    };
    defer lineAccumulator.deinit();

    log.info("initializing REPL ...", .{});
    var repl = REPL.init(comp, comp.gpa);
    defer repl.deinit();

    repl.hints_callback = hints;
    repl.completions_callback = completion;

    log.info("... REPL ready", .{});

    log.info("initializing command table ...", .{});
    var commandTable = try buildCommandTable(comp.gpa);
    defer commandTable.deinit();

    log.info("... command table ready", .{});

    log.info("loading history ...", .{});
    if (repl.history.load(Config.REPL_HISTORY_PATH)) |loaded| {
        if (loaded) {
            log.info("... history loaded", .{});
        } else {
            log.info("... no history found", .{});
        }
    } else |err| {
        log.err("... failed to load history", .{});
        return err;
    }

    defer {
        log.info("saving history ...", .{});

        if (repl.history.save(Config.REPL_HISTORY_PATH)) {
            log.info("... history saved", .{});
        } else |err| {
            log.err("... failed to save history, {}; dumping to stdout", .{err});
            repl.history.write(stdout) catch unreachable;
        }

        stdout.print("\n{s} goodbye!", .{comp.emoji.Heart}) catch {};
    }

    try comp.readFiles(files);

    log.info("beginning repl loop ...", .{});

    try comp.parser.setFileName("stdin");

    try stdout.print("\r\n", .{});

    const startPrompt = try std.fmt.allocPrint(comp.gpa, "{s}{s}>{s}{s} ", .{ comp.style.Decoration.StartBold, comp.style.Color.Foreground.Magenta, comp.style.Color.Foreground.Default, comp.style.Decoration.EndBoldDim });
    defer comp.gpa.free(startPrompt);

    const subsqPrompt = try std.fmt.allocPrint(comp.gpa, "{s}{s}|{s}{s} ", .{ comp.style.Decoration.StartBold, comp.style.Color.Foreground.Cyan, comp.style.Color.Foreground.Default, comp.style.Decoration.EndBoldDim });
    defer comp.gpa.free(subsqPrompt);

    while (true) {
        const prompt = prompt: {
            if (lineAccumulator.items.len == 0) {
                break :prompt startPrompt;
            } else {
                break :prompt subsqPrompt;
            }
        };

        if (repl.getInput(prompt)) |maybeInput| {
            if (maybeInput) |input| {
                if (input[0] == ':') {
                    const commandName = input[1..];

                    if (commandTable.get(commandName)) |command| {
                        const directive = try command(comp, &repl);

                        if (directive.saveHistory) {
                            try repl.history.add(input);
                        }

                        switch (directive.control) {
                            .Break => break,
                            .Continue => continue,
                        }
                    }
                }

                try repl.history.add(input);

                try lineAccumulator.appendSlice(input);
                comp.gpa.free(input);

                if (Config.REPL_DUMP_STDIN) {
                    const file = try std.fs.cwd().createFile("stdin", .{});
                    defer file.close();

                    _ = try file.write(lineAccumulator.items);
                }

                const sexpr = sexpr: {
                    if (lineAccumulator.items.len == 0) {
                        continue;
                    }

                    comp.parser.setInput(lineAccumulator.items, null);

                    if (comp.parser.totalP(SExpr, Parser.sexprP, .{})) |x| {
                        break :sexpr x;
                    } else |err| {
                        switch (err) {
                            error.UnexpectedEOF => {
                                try lineAccumulator.append('\n');
                                continue;
                            },
                            error.UnexpectedInput => {
                                try comp.expectedOutput("{s}!{s} Syntax error at [stdin:{}]", .{ comp.style.Color.Foreground.Red, comp.style.Color.Foreground.Default, comp.parser.pos });
                                lineAccumulator.clearRetainingCapacity();
                                continue;
                            },
                            else => return err,
                        }
                    }
                };

                defer lineAccumulator.clearRetainingCapacity();

                log.debug("# {attr}", .{sexpr});
                log.debug(". {.}", .{sexpr});

                const envs = try comp.eval.save();
                if (comp.eval.resolve(sexpr)) |result| {
                    try comp.expectedOutput("{s}${s} {}", .{ comp.style.Color.Foreground.Green, comp.style.Color.Foreground.Default, result });
                    log.debug(". {.}", .{result});
                } else |res| {
                    if (Eval.asEvaluationError(res)) |err| {
                        try comp.expectedOutput("{s}!{s} {}", .{ comp.style.Color.Foreground.Red, comp.style.Color.Foreground.Default, comp.eval.errFmt(err) });
                        comp.eval.restore(envs);
                    } else {
                        log.err("unexpected result: {}", .{res});
                        return res;
                    }
                }
            }
        } else |err| {
            switch (err) {
                error.CtrlC => lineAccumulator.clearRetainingCapacity(),
                error.EndOfStream => break,
                else => {
                    log.err("! Unexpected REPL error: {}", .{err});
                    return err;
                },
            }
        }
    }
}
