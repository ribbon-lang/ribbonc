// adapted from https://github.com/joachimschmidt557/linenoize (MIT License)

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const File = std.fs.File;

const TypeUtils = @import("ZigUtils").Type;
const TextUtils = @import("ZigUtils").Text;

const ANSI = @import("ZigUtils").Ansi;

const Config = @import("Config");

const MiscUtils = @import("ZigUtils").Misc;

const log = std.log.scoped(.repl);

pub fn Builder(comptime Ctx: type) type {
    return struct {
        ctx: *Ctx,
        allocator: Allocator,
        history: History,
        mask_mode: bool = false,
        is_tty: bool = false,
        term_supported: bool = false,
        completions_callback: ?CompletionsCallback = null,
        hints_callback: ?HintsCallback = null,
        input: std.fs.File,
        output: std.fs.File,
        inputOverflow: std.ArrayList(u8),

        const REPL = @This();

        pub const Signal = error{
            CtrlC,
            EndOfStream,
        };

        pub const TerminalError = error{
            InitFailed,
            NotATerminal,
            ProcessOrphaned,
            NothingRead,
        };

        pub fn isTerminalError(err: anyerror) bool {
            return TypeUtils.isInErrorSet(TerminalError, err);
        }

        pub fn asTerminalError(err: anyerror) ?TerminalError {
            return TypeUtils.narrowErrorSet(TerminalError, err);
        }

        pub const OperationalError = std.mem.Allocator.Error || TextUtils.Error;

        pub const Error = OperationalError || TerminalError || MiscUtils.IOError || Signal;

        pub const CompletionsCallback = *const fn (*Ctx, Allocator, usize, []const u8) Ctx.Error![]const []const u8;
        pub const HintsCallback = *const fn (*Ctx, Allocator, usize, []const u8) Ctx.Error!?[]const u8;

        const key_null = 0;
        const key_ctrl_a = 1;
        const key_ctrl_b = 2;
        const key_ctrl_c = 3;
        const key_ctrl_d = 4;
        const key_ctrl_e = 5;
        const key_ctrl_f = 6;
        const key_ctrl_h = 8;
        const key_tab = 9;
        const key_ctrl_k = 11;
        const key_ctrl_l = 12;
        const key_enter = 13;
        const key_ctrl_n = 14;
        const key_ctrl_p = 16;
        const key_ctrl_t = 20;
        const key_ctrl_u = 21;
        const key_ctrl_w = 23;
        const key_esc = 27;
        const key_backspace = 127;

        /// Initialize a REPL struct
        pub fn init(ctx: *Ctx, allocator: Allocator) REPL {
            var self = REPL{
                .ctx = ctx,
                .allocator = allocator,
                .history = History.empty(allocator),
                .input = std.io.getStdIn(),
                .output = std.io.getStdOut(),
                .inputOverflow = std.ArrayList(u8).init(allocator),
            };
            self.examineIo();
            return self;
        }

        /// Free all resources occupied by this struct
        pub fn deinit(self: *REPL) void {
            self.history.deinit();
            self.inputOverflow.deinit();
        }

        /// Re-examine (currently) stdin and environment variables to
        /// check if line editing and prompt printing should be
        /// enabled or not.
        pub fn examineIo(self: *REPL) void {
            self.is_tty = self.input.isTty();
            self.term_supported = if (Config.REPL_DISABLE_RAW_MODE) false else !self.isUnsupportedTerm();
        }

        /// Reads a line from the terminal. Caller owns returned memory
        pub fn getInput(self: *REPL, prompt: []const u8) (Ctx.Error || Error)!?[]const u8 {
            if (self.is_tty and !self.term_supported) {
                try self.output.writeAll(prompt);
            }

            return if (self.is_tty and self.term_supported)
                try self.rawTTY(prompt)
            else
                try self.noTTY();
        }

        const Reader = struct {
            state: *State,
            cursor: ?usize = null,
            inputBuf: [4]u8 = [1]u8{0} ** 4,
            inputLen: usize = 0,

            const Self = @This();

            fn readAllIntoOverflow(self: *Self, wait: bool) !void {
                const readSize: usize = 1024;
                const inputOverflow = &self.state.repl.inputOverflow;

                while (true) {
                    const base = inputOverflow.items.len;
                    try inputOverflow.resize(base + readSize);

                    const buf = inputOverflow.items[base..];
                    const bytesRead = try self.state.repl.input.read(buf);

                    if (bytesRead < readSize) {
                        inputOverflow.items.len = base + bytesRead;
                    }

                    if (bytesRead == readSize) continue;
                    if (!wait or bytesRead != 0) break;
                }
            }

            fn readByte(self: *Self) !?u8 {
                if (self.cursor) |c| {
                    if (c < self.inputLen) {
                        self.cursor = c + 1;
                        return self.inputBuf[c];
                    } else {
                        self.cursor = null;
                    }
                }

                try self.readNextSegment(true);

                if (self.inputLen == 0) return null;

                self.cursor = 1;
                return self.inputBuf[0];
            }

            fn readNextSegment(self: *Self, wait: bool) !void {
                const inputOverflow = &self.state.repl.inputOverflow;

                if (inputOverflow.items.len == 0) {
                    try self.readAllIntoOverflow(wait);
                }

                if (inputOverflow.items.len == 0) {
                    return;
                }

                self.inputLen = @min(inputOverflow.items.len, self.inputBuf.len);
                @memcpy(self.inputBuf[0..self.inputLen], inputOverflow.items[0..self.inputLen]);
                std.mem.copyForwards(u8, inputOverflow.items[0 .. inputOverflow.items.len - self.inputLen], inputOverflow.items[self.inputLen..]);
                inputOverflow.items.len -= self.inputLen;

                self.cursor = 0;
            }

            fn emitError(self: *const Self) !void {
                log.debug("\nUnrecognized input: ", .{});

                var i: usize = 0;
                while (i < self.inputLen) {
                    const dec = try TextUtils.decode1(self.inputBuf[i..]);
                    var escBuf = [1]u8{0} ** 4;
                    const esc = try TextUtils.escape(dec.ch, .None, &escBuf);
                    log.debug("{s}", .{esc});
                    i += dec.len;
                }

                log.debug("\n", .{});

                try self.state.refresh(.active);
            }

            fn hasInputRemaining(self: *const Self) bool {
                return if (self.cursor) |c| c < self.inputLen else false;
            }

            fn remainingInput(self: *const Self) []const u8 {
                return if (self.cursor) |c| self.inputBuf[c..self.inputLen] else self.inputBuf[0..0];
            }

            fn forceLast(self: *const Self) u8 {
                return self.inputBuf[self.inputLen - 1];
            }

            fn peekBufferedByte(self: *Self) ?u8 {
                if (self.cursor) |c| {
                    if (c < self.inputLen) {
                        return self.inputBuf[c];
                    }
                }

                return null;
            }

            fn consumeBufferedSlice(self: *Self, len: usize) ![]const u8 {
                if (self.cursor) |c| c: {
                    const c2 = c + len;

                    switch (MiscUtils.compare(c2, self.inputLen)) {
                        .Less => {
                            self.cursor = c2;
                        },
                        .Equal => {
                            self.cursor = null;
                        },
                        .Greater => break :c,
                    }

                    return self.inputBuf[c..c2];
                }

                return TextUtils.Error.BadEncoding;
            }

            fn remainingLen(self: *Self) usize {
                return if (self.cursor) |c| self.inputLen - c else 0;
            }

            fn advanceCursor(self: *Self, n: usize) !void {
                if (self.cursor) |c| {
                    const c2 = c + n;
                    switch (MiscUtils.compare(c2, self.inputLen)) {
                        .Less => {
                            self.cursor = c2;
                            return;
                        },
                        .Equal => {
                            self.cursor = null;
                            return;
                        },
                        .Greater => {},
                    }
                }

                return TextUtils.Error.BadEncoding;
            }

            fn overflow(self: *Self) !void {
                const rem = self.remainingInput();

                try self.state.repl.inputOverflow.insertSlice(0, rem);
                self.inputLen = 0;
                self.cursor = null;

                try self.readAllIntoOverflow(false);
            }

            fn resetCursor(self: *Self) void {
                self.cursor = 0;
            }
        };

        fn editLine(self: *REPL, prompt: []const u8) !?[]const u8 {
            var state = State.init(self, prompt);
            defer state.buf.deinit(state.allocator);

            try self.history.add("");
            self.history.current = self.history.hist.items.len - 1;
            try state.refresh(.active);

            while (true) {
                var rd = Reader{ .state = &state };

                const c = getC: {
                    const input = try rd.readByte() orelse return null;
                    switch (input) {
                        // Browse completions before editing
                        key_tab => if (try state.browseCompletions()) |new_rd| {
                            rd = new_rd;
                            break :getC rd.forceLast();
                        } else {
                            break :getC key_tab;
                        },
                        else => break :getC input,
                    }
                };

                switch (c) {
                    key_null, key_tab => {},
                    key_ctrl_a => try state.moveHome(),
                    key_ctrl_b => try state.moveLeft(),
                    key_ctrl_c => return error.CtrlC,
                    key_ctrl_d => return error.EndOfStream,
                    key_ctrl_e => try state.moveEnd(),
                    key_ctrl_f => try state.moveRight(),
                    key_ctrl_k => try state.killLineForward(),
                    key_ctrl_l => {
                        try self.clearScreen();
                        try state.refresh(.active);
                    },
                    key_enter => {
                        try rd.overflow();
                        self.history.pop();
                        try state.refresh(.exit);
                        if (state.buf.items.len > 0) {
                            return try self.allocator.dupe(u8, state.buf.items);
                        } else {
                            return null;
                        }
                    },
                    key_ctrl_n => try state.historyNext(.next),
                    key_ctrl_p => try state.historyNext(.prev),
                    key_ctrl_t => try state.swapPrev(),
                    key_ctrl_u => try state.killLineBackward(),
                    key_ctrl_w => try state.deletePrevWord(),
                    key_esc => if (rd.hasInputRemaining()) {
                        switch (try rd.readByte() orelse return null) {
                            'b' => try state.moveWordStart(),
                            'f' => try state.moveWordEnd(),
                            'd' => try state.deleteNextWord(),
                            '[' => switch (try rd.readByte() orelse return null) {
                                '0'...'9' => |num| {
                                    switch (try rd.readByte() orelse return null) {
                                        '~' => switch (num) {
                                            '1', '7' => try state.moveHome(),
                                            '3' => try state.delete(),
                                            '4', '8' => try state.moveEnd(),
                                            else => try rd.emitError(),
                                        },
                                        '0'...'9' => {},
                                        ';' => switch (try rd.readByte() orelse return null) {
                                            '5' => switch (try rd.readByte() orelse return null) {
                                                'C' => try state.moveWordEnd(),
                                                'D' => try state.moveWordStart(),
                                                else => try rd.emitError(),
                                            },
                                            else => {
                                                _ = try rd.readByte();
                                                try rd.emitError();
                                            },
                                        },
                                        else => try rd.emitError(),
                                    }
                                },
                                'A' => try state.historyNext(.prev),
                                'B' => try state.historyNext(.next),
                                'C' => try state.moveRight(),
                                'D' => try state.moveLeft(),
                                'H' => try state.moveHome(),
                                'F' => try state.moveEnd(),
                                else => try rd.emitError(),
                            },
                            '0' => switch (try rd.readByte() orelse return null) {
                                'H' => try state.moveHome(),
                                'F' => try state.moveEnd(),
                                else => {
                                    _ = try rd.readByte();
                                    try rd.emitError();
                                },
                            },
                            else => {
                                try rd.emitError();
                            },
                        }
                    } else {
                        return null;
                    },
                    key_backspace, key_ctrl_h => try state.backspace(),
                    else => {
                        const utf8Len = try TextUtils.sequenceLengthByte(c);

                        var utf8 = [1]u8{c} ++ ([1]u8{0} ** 3);

                        var offset: usize = 1;
                        const usableLen = @min(rd.remainingLen(), utf8Len - offset);

                        if (usableLen > 0) {
                            @memcpy(utf8[offset .. offset + usableLen], rd.consumeBufferedSlice(usableLen) catch unreachable);

                            offset += usableLen;
                        }

                        if (offset < utf8Len) {
                            const rem = utf8Len - offset;
                            try rd.readNextSegment(false);

                            if (rd.inputLen < rem) {
                                return TextUtils.Error.BadEncoding;
                            }

                            @memcpy(utf8[offset .. offset + rem], rd.inputBuf[0..rem]);
                        }

                        try rd.overflow();

                        const slice = utf8[0..utf8Len];

                        if (!TextUtils.isValidStr(slice)) return TextUtils.Error.BadEncoding;

                        try state.insert(slice);
                    },
                }
            }
        }

        /// Read a line with custom line editing mechanics. This includes hints,
        /// completions and history
        fn rawTTY(self: *REPL, prompt: []const u8) !?[]const u8 {
            defer self.output.writeAll("\n") catch {};

            const orig = try self.enableRawMode();
            defer self.disableRawMode(orig);

            return try editLine(self, prompt);
        }

        /// Read a line with no special features (no hints, no completions, no history)
        fn noTTY(self: *REPL) !?[]const u8 {
            var reader = self.input.reader();
            const max_read = std.math.maxInt(usize);
            return reader.readUntilDelimiterAlloc(self.allocator, '\n', max_read) catch |e| switch (e) {
                error.EndOfStream => return null,
                else => return e,
            };
        }

        const builtin = @import("builtin");

        const unsupported_term = [_][]const u8{ "dumb", "cons25", "emacs" };

        const is_windows = builtin.os.tag == .windows;
        const termios = if (!is_windows) std.posix.termios else struct { inMode: w.DWORD, outMode: w.DWORD };

        pub fn isUnsupportedTerm(self: *REPL) bool {
            const env_var = std.process.getEnvVarOwned(self.allocator, "TERM") catch "unknown";
            const writer = self.output.writer();
            if (std.mem.eql(u8, env_var, "unknown")) {
                writer.print("Detected terminal kind: `unknown`\n", .{}) catch unreachable;
                if (is_windows) {
                    writer.print("Utf8 input is not supported on this terminal; if you experience issues, try a different terminal emulator (e.g. Cygwin) or consult the CLI options to disable raw mode\n", .{}) catch unreachable;
                } else {
                    writer.print("You may experience issues with line editing; if so, try a different terminal emulator (e.g. xterm) or consult the CLI options to disable raw mode\n", .{}) catch unreachable;
                }
                return false;
            }
            defer self.allocator.free(env_var);
            return for (unsupported_term) |t| {
                if (std.ascii.eqlIgnoreCase(env_var, t)) {
                    writer.print("Detected terminal kind: `{s}`\n", .{env_var}) catch unreachable;
                    writer.print("This terminal is not supported for raw mode; consider using a different terminal emulator", .{}) catch unreachable;
                    if (is_windows) {
                        writer.print(" (e.g. Cygwin)\n", .{}) catch unreachable;
                    } else {
                        writer.print(" (e.g. xterm)\n", .{}) catch unreachable;
                    }
                    break true;
                }
            } else false;
        }

        const w = struct {
            pub usingnamespace std.os.windows;
            pub const ENABLE_VIRTUAL_TERMINAL_INPUT = @as(c_int, 0x200);
            pub const CP_UTF8 = @as(c_int, 65001);
            pub const INPUT_RECORD = extern struct {
                EventType: w.WORD,
                _ignored: [16]u8,
            };
        };

        const k32 = struct {
            pub usingnamespace std.os.windows.kernel32;
            pub extern "kernel32" fn SetConsoleCP(wCodePageID: w.UINT) callconv(w.WINAPI) w.BOOL;
            pub extern "kernel32" fn PeekConsoleInputW(hConsoleInput: w.HANDLE, lpBuffer: [*]w.INPUT_RECORD, nLength: w.DWORD, lpNumberOfEventsRead: ?*w.DWORD) callconv(w.WINAPI) w.BOOL;
        };

        fn enableRawMode(self: *REPL) !termios {
            if (is_windows) {
                var result: termios = .{
                    .inMode = 0,
                    .outMode = 0,
                };
                var irec: [1]w.INPUT_RECORD = undefined;
                var n: w.DWORD = 0;
                if (k32.PeekConsoleInputW(self.input.handle, &irec, 1, &n) == 0 or
                    k32.GetConsoleMode(self.input.handle, &result.inMode) == 0 or
                    k32.GetConsoleMode(self.output.handle, &result.outMode) == 0)
                    return error.InitFailed;
                _ = k32.SetConsoleMode(self.input.handle, w.ENABLE_VIRTUAL_TERMINAL_INPUT);
                _ = k32.SetConsoleMode(self.output.handle, result.outMode | w.ENABLE_VIRTUAL_TERMINAL_PROCESSING);
                _ = k32.SetConsoleCP(w.CP_UTF8);
                _ = k32.SetConsoleOutputCP(w.CP_UTF8);
                return result;
            } else {
                const orig = try std.posix.tcgetattr(self.input.handle);
                var raw = orig;

                raw.iflag.IGNBRK = false;
                raw.iflag.BRKINT = false;
                raw.iflag.ICRNL = false;
                raw.iflag.IGNCR = false;
                raw.iflag.INLCR = false;
                raw.iflag.INPCK = false;
                raw.iflag.ISTRIP = false;
                raw.iflag.IXON = false;

                raw.oflag.OPOST = false;

                raw.cflag.CSIZE = .CS8;

                raw.lflag.ECHO = false;
                raw.lflag.ICANON = false;
                raw.lflag.IEXTEN = false;
                raw.lflag.ISIG = false;

                // these should be replaced when std.posix has them
                const VTIME = 5;
                const VMIN = 6;

                raw.cc[VTIME] = 0;
                raw.cc[VMIN] = 0;

                try std.posix.tcsetattr(self.input.handle, std.posix.TCSA.FLUSH, raw);

                return orig;
            }
        }

        fn disableRawMode(self: *REPL, orig: termios) void {
            if (is_windows) {
                _ = k32.SetConsoleMode(self.input.handle, orig.inMode);
                _ = k32.SetConsoleMode(self.output.handle, orig.outMode);
            } else {
                std.posix.tcsetattr(self.input.handle, std.posix.TCSA.FLUSH, orig) catch {};
            }
        }

        fn getCursorPosition(self: *REPL) !usize {
            const answer = try ANSI.Cursor.IO.RequestPosition(self.output.writer(), self.input.reader());

            return answer.x;
        }

        fn getColumnsFallback(self: *REPL) !usize {
            return try ANSI.Cursor.IO.GetColumns(self.output.writer(), self.input.reader());
        }

        fn getColumns(self: *REPL) !usize {
            switch (builtin.os.tag) {
                .linux => {
                    var wsz: std.posix.winsize = undefined;
                    if (std.os.linux.ioctl(self.input.handle, std.os.linux.T.IOCGWINSZ, @intFromPtr(&wsz)) == 0) {
                        return wsz.col;
                    } else {
                        return try self.getColumnsFallback();
                    }
                },
                .windows => {
                    var csbi: w.CONSOLE_SCREEN_BUFFER_INFO = undefined;
                    _ = k32.GetConsoleScreenBufferInfo(self.output.handle, &csbi);
                    return @intCast(csbi.dwSize.X);
                },
                else => return try self.getColumnsFallback(),
            }
        }

        pub fn clearScreen(self: *REPL) !void {
            try self.output.writeAll(ANSI.Cursor.MoveHome ++ ANSI.Erase.EntireScreen);
        }

        pub fn beep(_: *REPL) !void {
            const stderr = std.io.getStdErr().writer();
            try stderr.writeByte(ANSI.Scl.Bell);
        }

        fn width(s: []const u8) usize {
            var result: usize = 0;

            var escape_seq = false;
            const view = std.unicode.Utf8View.init(s) catch return 0;
            var iter = view.iterator();
            while (iter.nextCodepoint()) |codepoint| {
                if (escape_seq) {
                    if (codepoint == ANSI.Scl.EscEnd) {
                        escape_seq = false;
                    }
                } else {
                    if (codepoint == ANSI.Scl.Esc) {
                        escape_seq = true;
                    } else {
                        const wcw = TextUtils.wcwidth(codepoint);
                        if (wcw < 0) return 0;
                        result += @intCast(wcw);
                    }
                }
            }

            return result;
        }

        const ArrayListUnmanaged = std.ArrayListUnmanaged;
        const bufferedWriter = std.io.bufferedWriter;
        const math = std.math;

        const Bias = enum {
            left,
            right,
        };

        fn binarySearchBestEffort(
            comptime T: type,
            key: T,
            items: []const T,
            context: anytype,
            comptime compareFn: fn (context: @TypeOf(context), lhs: T, rhs: T) math.Order,
            bias: Bias,
        ) usize {
            var left: usize = 0;
            var right: usize = items.len;

            while (left < right) {
                // Avoid overflowing in the midpoint calculation
                const mid = left + (right - left) / 2;
                // Compare the key with the midpoint element
                switch (compareFn(context, key, items[mid])) {
                    .eq => return mid,
                    .gt => left = mid + 1,
                    .lt => right = mid,
                }
            }

            // At this point, it is guaranteed that left >= right. In order
            // for the bias to work, we need to return the exact opposite
            return switch (bias) {
                .left => right,
                .right => left,
            };
        }

        fn compareLeft(context: []const u8, avail_space: usize, index: usize) math.Order {
            const width_slice = width(context[0..index]);
            return math.order(avail_space, width_slice);
        }

        fn compareRight(context: []const u8, avail_space: usize, index: usize) math.Order {
            const width_slice = width(context[index..]);
            return math.order(width_slice, avail_space);
        }

        const StartOrEnd = enum {
            start,
            end,
        };

        /// Start mode: Calculates the optimal start position such that
        /// buf[start..] fits in the available space taking into account
        /// unicode codepoint widths
        ///
        /// xxxxxxxxxxxxxxxxxxxxxxx buf
        ///    [------------------] available_space
        ///    ^                    start
        ///
        /// End mode: Calculates the optimal end position so that buf[0..end]
        /// fits
        ///
        /// xxxxxxxxxxxxxxxxxxxxxxx buf
        /// [----------]            available_space
        ///            ^            end
        fn calculateStartOrEnd(
            allocator: Allocator,
            comptime mode: StartOrEnd,
            buf: []const u8,
            avail_space: usize,
        ) !usize {
            // Create a mapping from unicode codepoint indices to buf
            // indices
            var map = try std.ArrayListUnmanaged(usize).initCapacity(allocator, buf.len);
            defer map.deinit(allocator);

            var utf8 = (try std.unicode.Utf8View.init(buf)).iterator();
            while (utf8.nextCodepointSlice()) |codepoint| {
                map.appendAssumeCapacity(@intFromPtr(codepoint.ptr) - @intFromPtr(buf.ptr));
            }

            const codepoint_start = binarySearchBestEffort(
                usize,
                avail_space,
                map.items,
                buf,
                switch (mode) {
                    .start => compareRight,
                    .end => compareLeft,
                },
                // When calculating start or end, if in doubt, choose a
                // smaller buffer as we don't want to overflow the line. For
                // calculating start, this means that we would rather have an
                // index more to the right.
                switch (mode) {
                    .start => .right,
                    .end => .left,
                },
            );
            return map.items[codepoint_start];
        }

        const State = struct {
            allocator: Allocator,
            repl: *REPL,

            buf: ArrayListUnmanaged(u8) = .{},
            prompt: []const u8,
            pos: usize = 0,
            old_pos: usize = 0,
            cols: usize,
            max_rows: usize = 0,
            activeCompletion: ?[]const u8 = null,

            const Self = @This();

            fn init(repl: *REPL, prompt: []const u8) Self {
                return Self{
                    .allocator = repl.allocator,
                    .repl = repl,
                    .prompt = prompt,
                    .cols = repl.getColumns() catch 80,
                };
            }

            fn browseCompletions(self: *Self) !?Reader {
                const fun = self.repl.completions_callback orelse return null;
                const completions = try fun(self.repl.ctx, self.repl.allocator, self.pos, self.buf.items);
                defer {
                    for (completions) |x| self.repl.allocator.free(x);
                    self.repl.allocator.free(completions);
                }

                if (completions.len == 1) {
                    try self.buf.insertSlice(self.allocator, self.pos, completions[0]);
                    self.pos += completions[0].len;
                    try self.refresh(.active);
                    return null;
                } else if (completions.len == 0) {
                    try self.repl.beep();
                    return null;
                } else {
                    var i: usize = 0;

                    while (true) {
                        self.activeCompletion = completions[i];
                        try self.refresh(.active);

                        // Read next key
                        var rd = Reader{ .state = self };
                        const c = try rd.readByte() orelse return error.NothingRead;

                        switch (c) {
                            key_esc => if (rd.hasInputRemaining()) {
                                const cursor = rd.cursor;

                                esc_rest: {
                                    switch (try rd.readByte() orelse break :esc_rest) {
                                        '[' => switch (try rd.readByte() orelse break :esc_rest) {
                                            'A' => if (i > 0) {
                                                i -= 1;
                                                continue;
                                            } else {
                                                i = completions.len - 1;
                                                continue;
                                            },
                                            'B' => if (i < completions.len - 1) {
                                                i += 1;
                                                continue;
                                            } else {
                                                i = 0;
                                                continue;
                                            },
                                            else => break :esc_rest,
                                        },
                                        else => break :esc_rest,
                                    }
                                }

                                rd.cursor = cursor;

                                self.activeCompletion = null;
                                try self.refresh(.active);
                                return null;
                            } else {
                                self.activeCompletion = null;
                                try self.refresh(.active);
                                return null;
                            },
                            key_backspace => {
                                self.activeCompletion = null;
                                try self.refresh(.active);
                                return null;
                            },
                            key_tab => {
                                self.activeCompletion = null;
                                try self.buf.insertSlice(self.allocator, self.pos, completions[i]);
                                self.pos += completions[i].len;
                                try self.refresh(.active);
                                return null;
                            },
                            else => {
                                // Stop browsing completions, return to buffer displayed
                                // prior to browsing completions
                                self.activeCompletion = null;
                                try self.refresh(.active);

                                return rd;
                            },
                        }
                    }
                }

                unreachable;
            }

            fn getHint(self: *Self) !?[]const u8 {
                if (self.repl.hints_callback) |fun| {
                    return try fun(self.repl.ctx, self.repl.allocator, self.pos, self.buf.items);
                }

                return null;
            }

            const DisplayMode = enum {
                active,
                exit,
            };

            fn refresh(self: *Self, mode: DisplayMode) !void {
                const Style = if (Config.USE_ANSI_STYLES) ANSI.Style else ANSI.NoStyle;
                var buf = bufferedWriter(self.repl.output.writer());
                var writer = buf.writer();

                const completion = self.activeCompletion;
                const hint = if (completion == null) try self.getHint() else null;
                defer if (hint) |str| self.repl.allocator.free(str);

                // Calculate widths
                const pos = width(self.buf.items[0..self.pos]);
                const prompt_width = width(self.prompt);
                const hint_width = if (completion orelse hint) |str| width(str) else 0;
                const buf_width = width(self.buf.items);
                const total_width = prompt_width + buf_width + hint_width;

                var rows = (total_width + self.cols - 1) / self.cols;
                const old_rpos = (prompt_width + self.old_pos + self.cols) / self.cols;
                const old_max_rows = self.max_rows;

                if (rows > self.max_rows) {
                    self.max_rows = rows;
                }

                // Go to the last row
                if (old_max_rows > old_rpos) {
                    try ANSI.Cursor.W.MoveDown(writer, old_max_rows - old_rpos);
                }

                // Clear every row from bottom to top
                if (old_max_rows > 0) {
                    var j: usize = 0;
                    while (j < old_max_rows - 1) : (j += 1) {
                        try writer.writeAll(ANSI.Seq.CarriageReturn ++ ANSI.Erase.CursorToEndOfLine ++ ANSI.Cursor.Ct.MoveUp(1));
                    }
                }

                // Clear the top line
                try writer.writeAll(ANSI.Seq.CarriageReturn ++ ANSI.Erase.CursorToEndOfLine);

                // Write prompt
                try writer.writeAll(self.prompt);

                // Write current buffer content
                if (self.repl.mask_mode) {
                    for (self.buf.items) |_| {
                        try writer.writeAll("*");
                    }
                } else switch (mode) {
                    .exit => try writer.writeAll(self.buf.items),
                    .active => if (completion != null or hint != null) {
                        try writer.writeAll(self.buf.items[0..self.pos]);
                        if (completion) |str| {
                            try writer.print("{s}{s}{s}{s}{s}", .{
                                Style.Decoration.StartItalic,
                                Style.Decoration.StartInvert,
                                str,
                                Style.Decoration.EndInvert,
                                Style.Decoration.EndItalic,
                            });
                        } else if (hint) |str| {
                            try writer.print("{s}{s}{s}", .{
                                Style.Decoration.StartItalic,
                                str,
                                Style.Decoration.EndItalic,
                            });
                        }
                        try writer.writeAll(self.buf.items[self.pos..]);
                    } else {
                        try writer.writeAll(self.buf.items);
                    },
                }

                // Reserve a newline if we filled all columns
                if (self.pos > 0 and self.pos == self.buf.items.len and total_width % self.cols == 0) {
                    try writer.writeAll("\r\n");
                    rows += 1;
                    if (rows > self.max_rows) {
                        self.max_rows = rows;
                    }
                }

                // Move cursor to right position:
                const rpos = (prompt_width + pos + self.cols) / self.cols;

                // First, y position (move up if necessary)
                if (rows > rpos) {
                    try ANSI.Cursor.W.MoveUp(writer, rows - rpos);
                }

                try writer.writeByte(ANSI.Scl.CarriageReturn);

                // Then, x position (move right if necessary)
                const col = (prompt_width + pos) % self.cols;
                if (col > 0) {
                    try ANSI.Cursor.W.MoveRight(writer, col);
                }

                self.old_pos = pos;

                try buf.flush();
            }

            fn insert(self: *Self, c: []const u8) !void {
                try self.buf.resize(self.allocator, self.buf.items.len + c.len);
                if (self.buf.items.len > 0 and self.pos < self.buf.items.len - c.len) {
                    std.mem.copyBackwards(
                        u8,
                        self.buf.items[self.pos + c.len .. self.buf.items.len],
                        self.buf.items[self.pos .. self.buf.items.len - c.len],
                    );
                }

                @memcpy(self.buf.items[self.pos..][0..c.len], c);
                self.pos += c.len;
                try self.refresh(.active);
            }

            fn prevCodepointLen(self: *Self, pos: usize) usize {
                if (pos >= 1 and @clz(~self.buf.items[pos - 1]) == 0) {
                    return 1;
                } else if (pos >= 2 and @clz(~self.buf.items[pos - 2]) == 2) {
                    return 2;
                } else if (pos >= 3 and @clz(~self.buf.items[pos - 3]) == 3) {
                    return 3;
                } else if (pos >= 4 and @clz(~self.buf.items[pos - 4]) == 4) {
                    return 4;
                } else {
                    return 0;
                }
            }

            fn moveLeft(self: *Self) !void {
                if (self.pos == 0) return;
                self.pos -= self.prevCodepointLen(self.pos);
                try self.refresh(.active);
            }

            fn moveRight(self: *Self) !void {
                if (self.pos < self.buf.items.len) {
                    const utf8_len = std.unicode.utf8ByteSequenceLength(self.buf.items[self.pos]) catch 1;
                    self.pos += utf8_len;
                    try self.refresh(.active);
                }
            }

            fn moveWordEnd(self: *Self) !void {
                if (self.pos < self.buf.items.len) {
                    while (self.pos < self.buf.items.len and self.buf.items[self.pos] == ' ')
                        self.pos += 1;
                    while (self.pos < self.buf.items.len and self.buf.items[self.pos] != ' ')
                        self.pos += 1;
                    try self.refresh(.active);
                }
            }

            fn moveWordStart(self: *Self) !void {
                if (self.buf.items.len > 0 and self.pos > 0) {
                    while (self.pos > 0 and self.buf.items[self.pos - 1] == ' ')
                        self.pos -= 1;
                    while (self.pos > 0 and self.buf.items[self.pos - 1] != ' ')
                        self.pos -= 1;
                    try self.refresh(.active);
                }
            }

            fn moveHome(self: *Self) !void {
                if (self.pos > 0) {
                    self.pos = 0;
                    try self.refresh(.active);
                }
            }

            fn moveEnd(self: *Self) !void {
                if (self.pos < self.buf.items.len) {
                    self.pos = self.buf.items.len;
                    try self.refresh(.active);
                }
            }

            const HistoryDirection = enum {
                next,
                prev,
            };

            fn historyNext(self: *Self, dir: HistoryDirection) !void {
                if (self.repl.history.hist.items.len > 0) {
                    // Update the current history with the current line
                    const old_index = self.repl.history.current;
                    const current_entry = self.repl.history.hist.items[old_index];
                    self.repl.history.allocator.free(current_entry);
                    self.repl.history.hist.items[old_index] = try self.repl.history.allocator.dupe(u8, self.buf.items);

                    // Update history index
                    const new_index = switch (dir) {
                        .next => if (old_index < self.repl.history.hist.items.len - 1) old_index + 1 else self.repl.history.hist.items.len - 1,
                        .prev => if (old_index > 0) old_index - 1 else 0,
                    };
                    self.repl.history.current = new_index;

                    // Copy history entry to the current line buffer
                    self.buf.deinit(self.allocator);
                    self.buf = .{};
                    try self.buf.appendSlice(self.allocator, self.repl.history.hist.items[new_index]);
                    self.pos = self.buf.items.len;

                    try self.refresh(.active);
                }
            }

            fn delete(self: *Self) !void {
                if (self.buf.items.len == 0 or self.pos >= self.buf.items.len) return;

                const utf8_len = std.unicode.utf8CodepointSequenceLength(self.buf.items[self.pos]) catch 1;
                std.mem.copyForwards(
                    u8,
                    self.buf.items[self.pos .. self.buf.items.len - utf8_len],
                    self.buf.items[self.pos + utf8_len .. self.buf.items.len],
                );
                try self.buf.resize(self.allocator, self.buf.items.len - utf8_len);
                try self.refresh(.active);
            }

            fn backspace(self: *Self) !void {
                if (self.buf.items.len == 0 or self.pos == 0) return;

                const utf8_len = self.prevCodepointLen(self.pos);
                std.mem.copyForwards(
                    u8,
                    self.buf.items[self.pos - utf8_len .. self.buf.items.len - utf8_len],
                    self.buf.items[self.pos..self.buf.items.len],
                );
                self.pos -= utf8_len;
                try self.buf.resize(self.allocator, self.buf.items.len - utf8_len);
                try self.refresh(.active);
            }

            fn swapPrev(self: *Self) !void {
                //    aaa bb            =>   bb aaa
                //   ^   ^  ^- pos_end      ^  ^   ^- pos_end
                //   |   |                  |  |
                //   |   pos_1              |  pos_2
                // pos_begin            pos_begin

                const pos_end = self.pos;
                const b_len = self.prevCodepointLen(pos_end);
                const pos_1 = pos_end - b_len;
                const a_len = self.prevCodepointLen(pos_1);
                const pos_begin = pos_1 - a_len;
                const pos_2 = pos_begin + b_len;

                if (a_len == 0 or b_len == 0) return;

                // save both codepoints
                var tmp: [8]u8 = undefined;
                @memcpy(
                    tmp[0 .. a_len + b_len],
                    self.buf.items[pos_begin..pos_end],
                );
                // write b from tmp
                @memcpy(
                    self.buf.items[pos_begin..pos_2],
                    tmp[a_len .. a_len + b_len],
                );
                // write a from tmp
                @memcpy(
                    self.buf.items[pos_2..pos_end],
                    tmp[0..a_len],
                );

                try self.refresh(.active);
            }

            fn deletePrevWord(self: *Self) !void {
                if (self.buf.items.len == 0 or self.pos == 0) return;

                const old_pos = self.pos;
                while (self.pos > 0 and self.buf.items[self.pos - 1] == ' ')
                    self.pos -= 1;
                while (self.pos > 0 and self.buf.items[self.pos - 1] != ' ')
                    self.pos -= 1;

                const diff = old_pos - self.pos;
                const new_len = self.buf.items.len - diff;
                std.mem.copyForwards(
                    u8,
                    self.buf.items[self.pos..new_len],
                    self.buf.items[old_pos..self.buf.items.len],
                );
                try self.buf.resize(self.allocator, new_len);
                try self.refresh(.active);
            }

            fn deleteNextWord(self: *Self) !void {
                if (self.buf.items.len == 0 or self.pos == self.buf.items.len) return;

                var iter = self.pos;
                while (iter < self.buf.items.len and self.buf.items[iter] == ' ')
                    iter += 1;
                while (iter < self.buf.items.len and self.buf.items[iter] != ' ')
                    iter += 1;

                const diff = iter - self.pos;
                const new_len = self.buf.items.len - diff;

                std.mem.copyForwards(
                    u8,
                    self.buf.items[self.pos..new_len],
                    self.buf.items[iter..self.buf.items.len],
                );
                try self.buf.resize(self.allocator, new_len);
                try self.refresh(.active);
            }

            fn killLineForward(self: *Self) !void {
                try self.buf.resize(self.allocator, self.pos);
                try self.refresh(.active);
            }

            fn killLineBackward(self: *Self) !void {
                const new_len = self.buf.items.len - self.pos;
                std.mem.copyForwards(
                    u8,
                    self.buf.items[0..new_len],
                    self.buf.items[self.pos..self.buf.items.len],
                );
                self.pos = 0;
                try self.buf.resize(self.allocator, new_len);
                try self.refresh(.active);
            }
        };

        const max_line_len = 4096;

        pub const History = struct {
            allocator: Allocator,
            hist: ArrayListUnmanaged([]const u8) = .{},
            max_len: usize = 10_000,
            current: usize = 0,

            const Self = @This();

            /// Creates a new empty history
            pub fn empty(allocator: Allocator) Self {
                return Self{
                    .allocator = allocator,
                };
            }

            /// Deinitializes the history
            pub fn deinit(self: *Self) void {
                for (self.hist.items) |x| self.allocator.free(x);
                self.hist.deinit(self.allocator);
            }

            /// Ensures that at most self.max_len items are in the history
            fn truncate(self: *Self) void {
                if (self.hist.items.len > self.max_len) {
                    const surplus = self.hist.items.len - self.max_len;
                    for (self.hist.items[0..surplus]) |x| self.allocator.free(x);
                    std.mem.copyForwards(
                        []const u8,
                        self.hist.items[0..self.max_len],
                        self.hist.items[surplus..],
                    );
                    self.hist.shrinkAndFree(self.allocator, self.max_len);
                }
            }

            /// Adds this line to the history. Does not take ownership of the line, but
            /// instead copies it
            pub fn add(self: *Self, line: []const u8) !void {
                if (self.hist.items.len < 1 or !std.mem.eql(u8, line, self.hist.items[self.hist.items.len - 1])) {
                    try self.hist.append(self.allocator, try self.allocator.dupe(u8, line));
                    self.truncate();
                }
            }

            /// Removes the last item (newest item) of the history
            pub fn pop(self: *Self) void {
                self.allocator.free(self.hist.pop());
            }

            /// Loads the history from a file
            pub fn load(self: *Self, path: []const u8) !bool {
                const file = std.fs.cwd().openFile(path, .{}) catch |err| {
                    switch (err) {
                        error.FileNotFound => return false,
                        else => return err,
                    }
                };
                defer file.close();

                const reader = file.reader();
                while (reader.readUntilDelimiterAlloc(self.allocator, '\n', max_line_len)) |line| {
                    try self.hist.append(self.allocator, line);
                } else |err| {
                    switch (err) {
                        error.EndOfStream => {},
                        else => return err,
                    }
                }

                self.truncate();

                return true;
            }

            /// Saves the history to a file
            pub fn save(self: *Self, path: []const u8) !void {
                const file = try std.fs.cwd().createFile(path, .{});
                defer file.close();

                try self.write(file);
            }

            pub fn write(self: *Self, writer: anytype) !void {
                for (self.hist.items) |line| {
                    try writer.writeAll(line);
                    try writer.writeAll("\n");
                }
            }

            /// Sets the maximum number of history items. If more history
            /// items than len exist, this will truncate the history to the
            /// len most recent items.
            pub fn setMaxLen(self: *Self, len: usize) !void {
                self.max_len = len;
                self.truncate();
            }

            pub fn clear(self: *Self) void {
                for (self.hist.items) |x| self.allocator.free(x);
                self.hist.clearRetainingCapacity();
            }
        };
    };
}

test {
    log.debug("history test start\n", .{});
    var hist = Builder(struct {const Error = error {};}).History.empty(std.testing.allocator);
    defer hist.deinit();

    try hist.add("Hello");
    hist.pop();
    log.debug("history test complete\n", .{});
}
