const zig_builtin = @import("builtin");


const std = @import("std");

const MiscUtils = @import("Utils").Misc;
const TypeUtils = @import("Utils").Type;

const clap = @import("clap");

const Config = @import("Config");

const ANSI = @import("Utils").Ansi;

const REPL = @import("REPL.zig").Builder(Driver);
const TextUtils = @import("Utils").Text;

const Rli = @import("Rli");
const Builtin = Rli.Builtin;
const SExpr = Rli.SExpr;
const Context = Rli.Context;
const Parser = Rli.Parser;
const Interpreter = Rli.Interpreter;

const log = Rli.log;

const Driver = @This();


pub const std_options = std.Options {
    .log_level = .warn,
};

pub const Error = REPL.Error || Rli.Error || CLIError;

pub const main = main: {
    if (@intFromEnum(std_options.log_level) > @intFromEnum(std.log.Level.warn)) {
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

const commands = .{
    .help = .{"Display commands help message"},
    .quit = .{ "q", "Exit the REPL" },
    .@"clear-screen" = .{ "clear", "Clear the screen" },
    .@"clear-history" = .{"Clear the history"},
};

const commandNames = gatherCommandNames(std.meta.fieldNames(@TypeOf(commands)));

fn gatherCommandNames(comptime names: []const []const u8) []const []const u8 {
    comptime {
        var outNames: []const []const u8 = &[0][]const u8{};

        for (names) |name| {
            outNames = outNames ++ &[1][]const u8{name};

            const field = @field(commands, name);

            if (field.len == 2) {
                outNames = outNames ++ &[1][]const u8{field[0]};
            }
        }

        return outNames;
    }
}

fn printCommands(out: anytype) !void {
    try out.print("Commands:\n", .{});

    const longNames = comptime std.meta.fieldNames(@TypeOf(commands));

    comptime var longestShort = 0;
    comptime var longestLong = 0;

    comptime for (longNames) |commandName| {
        const command = @field(commands, commandName);
        const lenShort = switch (command.len) {
            1 => 0,
            2 => command[0].len + 2,
            else => unreachable,
        };
        const lenLong = commandName.len;
        if (longestShort < lenShort) longestShort = lenShort;
        if (longestLong < lenLong) longestLong = lenLong;
    };

    inline for (longNames) |commandName| {
        const command = @field(commands, commandName);

        const short = switch (command.len) {
            1 => "",
            2 => command[0],
            else => unreachable,
        };

        const shortFill = [1]u8{' '} ** (longestShort + 2 - short.len);

        const longFill = [1]u8{' '} ** (longestLong + 12 - commandName.len);

        const desc = switch (command.len) {
            1 => command[0],
            2 => command[1],
            else => unreachable,
        };

        const Style = if (Config.USE_ANSI_STYLES) ANSI.Style else ANSI.NoStyle;

        try if (short.len > 0) out.print("{s}{s}:{s}{s}, {s}:{s}{s}{s}{s}\n", .{
            shortFill,
            Style.Color.Foreground.Yellow,
            short,
            Style.Color.Foreground.Default,
            Style.Color.Foreground.Yellow,
            commandName,
            Style.Color.Foreground.Default,
            longFill,
            desc,
        }) else out.print("   {s}{s}:{s}{s}{s}{s}\n", .{
            shortFill,
            Style.Color.Foreground.Yellow,
            commandName,
            Style.Color.Foreground.Default,
            longFill,
            desc,
        });
    }
}

const options =
    clap.parseParamsComptime(
        \\--help                       Display options help message, and exit
        \\--version                    Display SemVer2 version number for rli, and exit
        \\--interactive                Run the interpreter in REPL mode
        \\--dump-stdin <bool>          (REPL) Dump stdin to a file [Default: false]
        \\--history <path>             (REPL) Path to the REPL history file [Default: ".rli-repl-history"]
        \\--disable-raw-mode           (REPL) Disable raw line editing mode
        \\--use-emoji <bool>           Use emoji in the output [Default: true]
        \\--use-ansi-styles <bool>     Use ANSI styles in the output [Default: true]
        \\--max-comptime-depth <uint>  Maximum call stack depth for the compile time interpreter [Default: 512; Note: going higher may cause segfaults due to native stack overflow]
        \\<path>...                    Root files to include in the compilation
    );

const ArgsResult = union(enum) {
    exit: void,
    execute: Execute,

    const Execute = struct {
        interactive: bool,
        allocator: std.mem.Allocator,
        rootFiles: []const []const u8,
    };

    fn deinit(self: ArgsResult) void {
        switch (self) {
            .exit => {},
            .execute => |x| {
                for (x.rootFiles) |path| {
                    x.allocator.free(path);
                }

                x.allocator.free(x.rootFiles);
            },
        }
    }
};

const CLIError = error{
    InvalidArgument,
    ClapError,
};

fn isCLIError(err: anyerror) bool {
    return TypeUtils.isInErrorSet(CLIError, err);
}

fn asCLIError(err: anyerror) ?CLIError {
    return TypeUtils.narrowErrorSet(CLIError, err);
}

fn processArgs(allocator: std.mem.Allocator, args: []const []const u8) (MiscUtils.IOError || std.mem.Allocator.Error || CLIError)!ArgsResult {
    if (args.len == 0) {
        return ArgsResult{
            .execute = ArgsResult.Execute{
                .allocator = allocator,
                .interactive = true,
                .rootFiles = &[0][]const u8{},
            },
        };
    }

    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    const parsers = .{
        .bool = struct {
            fn parse(in: []const u8) !bool {
                if (std.mem.eql(u8, in, "true")) {
                    return true;
                } else if (std.mem.eql(u8, in, "false")) {
                    return false;
                } else {
                    return error.InvalidArgument;
                }
            }
        }.parse,
        .path = clap.parsers.default.string,
        .uint = clap.parsers.default.usize,
    };

    var iterator = MiscUtils.BufferIterator([]const u8).init(args);

    var diag = clap.Diagnostic{};
    var res = clap.parseEx(clap.Help, &options, parsers, &iterator, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        var longest = diag.name.longest();
        if (longest.kind == .positional) {
            longest.name = diag.arg;
        }

        try switch (err) {
            error.DoesntTakeValue => stderr.print(
                "The argument '{s}{s}' does not take a value\n\n",
                .{ longest.kind.prefix(), longest.name },
            ),
            error.MissingValue => stderr.print(
                "The argument '{s}{s}' requires a value but none was supplied\n\n",
                .{ longest.kind.prefix(), longest.name },
            ),
            error.InvalidArgument => stderr.print(
                "Invalid argument '{s}'\n\n",
                .{longest.name},
            ),
            else => stderr.print("Error while parsing arguments: {s}\n\n", .{@errorName(err)}),
        };

        try printUsage(stderr);

        return error.InvalidArgument;
    };
    defer res.deinit();

    if (res.args.version != 0) {
        try stdout.print("rli v{}\n", .{Config.VERSION});
        if (res.args.help != 0) {
            try stdout.print("\n", .{});
        }
    }

    if (res.args.help != 0) {
        try printOptions(stderr);
    }

    if (res.args.help != 0 or res.args.version != 0) {
        return ArgsResult.exit;
    }

    if (res.args.@"max-comptime-depth") |depth| {
        if (Config.MAX_DEPTH == depth) {
            try stderr.print("Note: --max-comptime-depth flag is unnecessary, config is already set to {} by default\n", .{depth});
        }
        Config.MAX_DEPTH = depth;
    }

    if (res.args.@"use-emoji") |styles| {
        if (styles == Config.USE_EMOJI) {
            try stderr.print("Note: --use-emoji flag is unnecessary, config is already set to {} by default\n", .{styles});
        }
        Config.USE_EMOJI = styles;
    }

    if (res.args.@"use-ansi-styles") |styles| {
        if (styles == Config.USE_ANSI_STYLES) {
            try stderr.print("Note: --use-ansi-styles flag is unnecessary, config is already set to {} by default\n", .{styles});
        }
        Config.USE_ANSI_STYLES = styles;
    }

    const interactive = interactive: {
        if (res.args.interactive != 0) {
            if (res.args.@"disable-raw-mode" != 0) {
                Config.REPL_DISABLE_RAW_MODE = true;
            }

            if (res.args.@"dump-stdin") |dump| {
                if (Config.REPL_DUMP_STDIN == dump) {
                    try stderr.print("Note: --dump-stdin flag is unnecessary, config is already set to {} by default\n", .{dump});
                }
                Config.REPL_DUMP_STDIN = dump;
            }

            if (res.args.history) |history| {
                if (std.mem.eql(u8, Config.REPL_HISTORY_PATH, history)) {
                    try stderr.print("Note: --history flag is unnecessary, config is already set to {s} by default\n", .{history});
                }
                Config.REPL_HISTORY_PATH = history;
            }

            break :interactive true;
        } else {
            if (res.args.@"disable-raw-mode" != 0) {
                try stderr.print("Warning: --disable-raw-mode requires --interactive, it will be ignored for this compilation\n", .{});
            }

            if (res.args.@"dump-stdin") |_| {
                try stderr.print("Warning: --dump-stdin requires --interactive, it will be ignored for this compilation\n", .{});
            }

            if (res.args.history) |_| {
                try stderr.print("Warning: --history requires --interactive, it will be ignored for this compilation\n", .{});
            }

            break :interactive false;
        }
    };

    var rootFiles = std.ArrayList([]const u8).init(allocator);
    defer rootFiles.deinit();

    for (res.positionals) |path| {
        try rootFiles.append(try allocator.dupe(u8, path));
    }

    return ArgsResult{
        .execute = ArgsResult.Execute{
            .allocator = allocator,
            .interactive = interactive,
            .rootFiles = try rootFiles.toOwnedSlice(),
        },
    };
}

pub fn printUsage(out: anytype) !void {
    try out.print("Usage: rli ", .{});
    clap.usage(out, clap.Help, options[2..]) catch |err| {
        try out.print("Error while printing usage: {s}\n", .{@errorName(err)});
        return error.ClapError;
    };
    try out.print("\n     | rli --help", .{});
    try out.print("\n     | rli --version", .{});
    try out.print("\n", .{});
}

pub fn printOptions(out: anytype) !void {
    try printUsage(out);
    try out.print("\nDetails:\n", .{});
    clap.help(out, clap.Help, &options, .{
        .description_on_new_line = false,
    }) catch |err| {
        try out.print("Error while printing options: {s}\n", .{@errorName(err)});
        return error.ClapError;
    };
}


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

fn init(allocator: std.mem.Allocator, out: std.io.AnyWriter, args: []const []const u8, readStdin: bool) !*Driver {
    const driver = try allocator.create(Driver);
    errdefer allocator.destroy(driver);

    driver.emoji = if (Config.USE_EMOJI) Emoji else NoEmoji;
    driver.style = if (Config.USE_ANSI_STYLES) ANSI.Style else ANSI.NoStyle;
    driver.rli = try Rli.init(allocator, out, Builtin.AllEnvs, args);
    driver.rli.readFileCallback = readFile;
    driver.read_stdin = readStdin;

    return driver;
}

fn deinit(driver: *Driver) void {
    const allocator = driver.rli.allocator;
    driver.rli.deinit();
    allocator.destroy(driver);
}


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

    const allocator = GPA.allocator();

    const args = std.process.argsAlloc(allocator) catch |err| {
        log.err("failed to get command line arguments: {}", .{err});
        return error.Unexpected;
    };
    defer std.process.argsFree(allocator, args);

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

    const argsResult = try processArgs(allocator, args[1..endOfOwnArgs]);
    defer argsResult.deinit();

    switch (argsResult) {
        .exit => return,
        .execute => |x| {
            var driver = try Driver.init(allocator, stderr.any(), scriptArgs, endStyle == .read_stdin);
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
            try printCommands(repl.output.writer());
            return .{};
        }
    }.fun) catch |err| {
        log.err("... failed to add command 'help'", .{});
        return err;
    };

    inline for (comptime std.meta.fieldNames(@TypeOf(commands))) |commandName| {
        if (comptime std.mem.eql(u8, commandName, "help")) continue;

        const commandDesc = @field(commands, commandName);
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

        return findCompletions(allocator, commandNames, token);
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

        return findHint(allocator, commandNames, token);
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

    return reader.readAllAlloc(rli.allocator, std.math.maxInt(usize));
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

        const src = stdin.readAllAlloc(driver.rli.allocator, std.math.maxInt(usize)) catch |err| {
            log.err("failed to read from stdin: {s}", .{@errorName(err)});
            return error.NothingRead;
        };
        defer driver.rli.allocator.free(src);

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
        if (std.ArrayList(u8).initCapacity(driver.rli.allocator, 1024)) |acc| {
            log.info("... line accumulator ready", .{});
            break :acc acc;
        } else |err| {
            log.err("... failed to initialize line accumulator", .{});
            return err;
        }
    };
    defer lineAccumulator.deinit();

    log.info("initializing REPL ...", .{});
    var repl = REPL.init(driver, driver.rli.allocator);
    defer repl.deinit();

    repl.hints_callback = hints;
    repl.completions_callback = completion;

    log.info("... REPL ready", .{});

    log.info("initializing command table ...", .{});
    var commandTable = try buildCommandTable(driver.rli.allocator);
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

    const startPrompt = try std.fmt.allocPrint(driver.rli.allocator, "{s}{s}>{s}{s} ", .{ driver.style.Decoration.StartBold, driver.style.Color.Foreground.Magenta, driver.style.Color.Foreground.Default, driver.style.Decoration.EndBoldDim });
    defer driver.rli.allocator.free(startPrompt);

    const subsqPrompt = try std.fmt.allocPrint(driver.rli.allocator, "{s}{s}|{s}{s} ", .{ driver.style.Decoration.StartBold, driver.style.Color.Foreground.Cyan, driver.style.Color.Foreground.Default, driver.style.Decoration.EndBoldDim });
    defer driver.rli.allocator.free(subsqPrompt);

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
                driver.rli.allocator.free(input);

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
