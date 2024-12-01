const std = @import("std");

const zig_builtin = @import("builtin");

const Config = @import("Config");
const MiscUtils = @import("Utils").Misc;
const REPL = @import("REPL").Builder(Driver);
const CLIMetaData = @import("CLIMetaData");
const TextUtils = @import("Utils").Text;
const ANSI = @import("Utils").Ansi;
const Builtin = @import("Builtin");

const Core = @import("Core");
const SExpr = Core.SExpr;
const Context = Core.Context;
const Parser = Core.Parser;
const Interpreter = Core.Interpreter;
const Rli = @import("Rli");

const log = Core.log;

const Driver = @This();


const EmojiTable = struct {
    Ribbon: []const u8,
    Heart: []const u8,
};

const Emoji = EmojiTable{
    .Ribbon = "🎀",
    .Heart = "💝",
};

const NoEmoji = EmojiTable{
    .Ribbon = "",
    .Heart = "",
};

style: ANSI.StyleT,
emoji: EmojiTable,
rli: *Rli,
read_stdin: bool,

fn init(gpa: std.mem.Allocator, out: std.io.AnyWriter, args: []const []const u8, readStdin: bool) !*Driver {
    const driver = try gpa.create(Driver);
    errdefer gpa.destroy(driver);

    driver.emoji = if (Config.USE_EMOJI) Emoji else NoEmoji;
    driver.style = if (Config.USE_ANSI_STYLES) ANSI.Style else ANSI.NoStyle;
    driver.rli = try Rli.init(gpa, out, &.{.Full}, args);
    driver.rli.readFileCallback = readFile;
    driver.read_stdin = readStdin;

    return driver;
}

fn deinit(driver: *Driver) void {
    const gpa = driver.rli.gpa;
    driver.rli.deinit();
    gpa.destroy(driver);
}

pub const std_options = std.Options {
    .log_level = .warn,
    .logFn = MiscUtils.FilteredLogger(Config.LOG_SCOPES),
};

pub const Error = REPL.Error || Rli.Error || CLIMetaData.CLIError;

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

    var endStyle: enum { none, scriptArgs, read_stdin } = .none;
    var endOfOwnArgs = args.len;
    var endOfScriptArgs = args.len;

    for (args, 0..) |arg, i| {
        switch (endStyle) {
            .none => {
                if (std.mem.eql(u8, arg, "-")) {
                    endStyle = .read_stdin;
                } else if (std.mem.eql(u8, arg, "--")) {
                    endStyle = .scriptArgs;
                } else continue;
                endOfOwnArgs = i;
            },
            .read_stdin => {
                log.err("unexpected argument '{s}' following '-' in command line arguments", .{arg});
                return error.Unexpected;
            },
            .scriptArgs => {
                if (std.mem.eql(u8, arg, "-")) {
                    endStyle = .read_stdin;
                } else if (std.mem.eql(u8, arg, "--")) {
                    log.err("unexpected secondary -- in command line arguments", .{});
                } else continue;
                endOfScriptArgs = i;
            }
        }
    }

    const scriptArgs = args[@min(endOfOwnArgs + 1, args.len)..endOfScriptArgs];

    const argsResult = try CLIMetaData.processArgs(gpa, args[1..endOfOwnArgs]);
    defer argsResult.deinit();

    switch (argsResult) {
        .exit => return,
        .execute => |x| {
            var driver = try Driver.init(gpa, stderr.any(), scriptArgs, endStyle == .read_stdin);
            defer driver.deinit();

            if (x.interactive and endStyle != .read_stdin) {
                try driver.runRepl(x.rootFiles);
            } else {
                try driver.readFiles(x.rootFiles);
            }
        },
    }
}

const CommandTable = std.StringHashMap(CommandFn);

const CommandFn = *const fn (driver: *Driver, repl: *REPL) Rli.Error!CommandDirective;

const CommandControl = enum {
    Break,
    Continue,
};

const CommandDirective = struct {
    saveHistory: bool = false,
    control: CommandControl = .Continue,
};

const COMMANDS = struct {
    fn quit(_: *Driver, _: *REPL) !CommandDirective {
        return .{ .control = .Break };
    }

    fn @"clear-screen"(_: *Driver, repl: *REPL) !CommandDirective {
        try repl.clearScreen();
        return .{};
    }

    fn @"clear-history"(_: *Driver, repl: *REPL) !CommandDirective {
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
        fn fun(_: *Driver, repl: *REPL) !CommandDirective {
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

fn completion(driver: *Driver, allocator: std.mem.Allocator, pos: usize, buf: []const u8) Driver.Error![]const []const u8 {
    if (buf.len > 0 and buf[0] == ':') {
        const token = buf[1..];

        return findCompletions(allocator, CLIMetaData.commandNames, token);
    }

    const lastToken = try getToken(pos, buf) orelse return &[0][]const u8{};

    const envKeys = try Interpreter.envKeyStrs(driver.rli.interpreter.env, allocator);
    defer allocator.free(envKeys);

    return findCompletions(allocator, envKeys, lastToken);
}

fn findCompletions(allocator: std.mem.Allocator, envKeys: []const []const u8, token: []const u8) Driver.Error![]const []const u8 {
    var out = std.ArrayList([]const u8).init(allocator);
    defer out.deinit();

    for (envKeys) |keyStr| {
        if (std.mem.startsWith(u8, keyStr, token)) {
            try out.append(try allocator.dupe(u8, keyStr[token.len..]));
        }
    }

    return out.toOwnedSlice();
}

fn hints(driver: *Driver, allocator: std.mem.Allocator, pos: usize, buf: []const u8) Driver.Error!?[]const u8 {
    if (buf.len > 0 and buf[0] == ':') {
        const token = buf[1..];

        return findHint(allocator, CLIMetaData.commandNames, token);
    }

    const lastToken = try getToken(pos, buf) orelse return null;

    const envKeys = try Interpreter.envKeyStrs(driver.rli.interpreter.env, allocator);
    defer allocator.free(envKeys);

    return findHint(allocator, envKeys, lastToken);
}

fn findHint(allocator: std.mem.Allocator, envKeys: []const []const u8, token: []const u8) Driver.Error!?[]const u8 {
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


fn readFile(rli: *Rli, fileName: []const u8) Rli.Error![]const u8 {
    const file = std.fs.cwd().openFile(fileName, .{ .mode = .read_only }) catch |err| {
        log.err("could not open file [{s}]: {s}", .{ fileName, @errorName(err) });
        return err;
    };
    defer file.close();

    const reader = file.reader();

    return reader.readAllAlloc(rli.gpa, std.math.maxInt(usize));
}

pub fn readFiles(driver: *Driver, files: []const []const u8) Error!void {
    if (files.len > 0) {
        log.info("running root files ...", .{});

        try driver.rli.readFiles(files);

        log.info("... finished running root files", .{});
    } else {
        log.info("... no root files provided", .{});
    }

    if (driver.read_stdin) {
        const stdin = std.io.getStdIn().reader();

        const src = stdin.readAllAlloc(driver.rli.gpa, std.math.maxInt(usize)) catch |err| {
            log.err("failed to read from stdin: {s}", .{@errorName(err)});
            return error.NothingRead;
        };
        defer driver.rli.gpa.free(src);

        _ = try driver.rli.runFile("stdin", src);
    }
}

fn runRepl(driver: *Driver, files: []const []const u8) Error!void {
    if (driver.read_stdin) {
        log.warn("cannot read file from stdin in REPL mode", .{});
        driver.read_stdin = false;
    }

    const stdout = std.io.getStdOut().writer();

    try stdout.print("\n{s} {s}Rli{s} {s}v{}{s} {s} {s}(REPL mode){s}\n", .{
        driver.emoji.Ribbon,
        driver.style.Color.Foreground.Magenta,
        driver.style.Color.Foreground.Default,
        driver.style.Color.Foreground.Green,
        Config.VERSION,
        driver.style.Color.Foreground.Default,
        driver.emoji.Ribbon,
        driver.style.Decoration.StartDim,
        driver.style.Decoration.EndBoldDim,
    });

    log.info("initializing line accumulator ...", .{});

    var lineAccumulator = acc: {
        if (std.ArrayList(u8).initCapacity(driver.rli.gpa, 1024)) |acc| {
            log.info("... line accumulator ready", .{});
            break :acc acc;
        } else |err| {
            log.err("... failed to initialize line accumulator", .{});
            return err;
        }
    };
    defer lineAccumulator.deinit();

    log.info("initializing REPL ...", .{});
    var repl = REPL.init(driver, driver.rli.gpa);
    defer repl.deinit();

    repl.hints_callback = hints;
    repl.completions_callback = completion;

    log.info("... REPL ready", .{});

    log.info("initializing command table ...", .{});
    var commandTable = try buildCommandTable(driver.rli.gpa);
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

        stdout.print("\n{s} goodbye!", .{driver.emoji.Heart}) catch {};
    }

    try driver.readFiles(files);

    log.info("beginning repl loop ...", .{});

    try driver.rli.parser.setFileName("stdin");

    try stdout.print("\r\n", .{});

    const startPrompt = try std.fmt.allocPrint(driver.rli.gpa, "{s}{s}>{s}{s} ", .{ driver.style.Decoration.StartBold, driver.style.Color.Foreground.Magenta, driver.style.Color.Foreground.Default, driver.style.Decoration.EndBoldDim });
    defer driver.rli.gpa.free(startPrompt);

    const subsqPrompt = try std.fmt.allocPrint(driver.rli.gpa, "{s}{s}|{s}{s} ", .{ driver.style.Decoration.StartBold, driver.style.Color.Foreground.Cyan, driver.style.Color.Foreground.Default, driver.style.Decoration.EndBoldDim });
    defer driver.rli.gpa.free(subsqPrompt);

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
                        const directive = try command(driver, &repl);

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
                driver.rli.gpa.free(input);

                if (Config.REPL_DUMP_STDIN) {
                    const file = try std.fs.cwd().createFile("stdin", .{});
                    defer file.close();

                    _ = try file.write(lineAccumulator.items);
                }

                const sexpr = sexpr: {
                    if (lineAccumulator.items.len == 0) {
                        continue;
                    }

                    driver.rli.parser.setInput(lineAccumulator.items, null);

                    if (driver.rli.parser.totalP(SExpr, Parser.sexprP, .{})) |x| {
                        break :sexpr x;
                    } else |err| {
                        switch (err) {
                            error.UnexpectedEOF => {
                                try lineAccumulator.append('\n');
                                continue;
                            },
                            error.UnexpectedInput => {
                                try driver.rli.expectedOutput("{s}!{s} Syntax error at [stdin:{}]", .{ driver.style.Color.Foreground.Red, driver.style.Color.Foreground.Default, driver.rli.parser.pos });
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

                const envs = try driver.rli.interpreter.save();
                if (driver.rli.interpreter.eval(sexpr)) |result| {
                    try driver.rli.expectedOutput("{s}${s} {}", .{ driver.style.Color.Foreground.Green, driver.style.Color.Foreground.Default, result });
                    log.debug(". {.}", .{result});
                } else |res| {
                    if (Interpreter.asEvaluationError(res)) |err| {
                        try driver.rli.expectedOutput("{s}!{s} {}", .{ driver.style.Color.Foreground.Red, driver.style.Color.Foreground.Default, driver.rli.interpreter.errFmt(err) });
                        driver.rli.interpreter.restore(envs);
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
