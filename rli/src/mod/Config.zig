const std = @import("std");
const zig_config = @import("config");

pub const VERSION: std.SemanticVersion = zig_config.version;

pub const LOG_LEVEL: std.log.Level = @enumFromInt(@intFromEnum(zig_config.logLevel));

pub const LOG_SCOPES: []const u8 = zig_config.logScopes;

pub const USE_EMOJI_DEFAULT: bool = zig_config.useEmoji;
pub var USE_EMOJI = USE_EMOJI_DEFAULT;

pub const USE_ANSI_STYLES_DEFAULT: bool = zig_config.useAnsiStyles;
pub var USE_ANSI_STYLES = USE_ANSI_STYLES_DEFAULT;

pub var REPL_DISABLE_RAW_MODE: bool = false;

pub const REPL_DUMP_STDIN_DEFAULT: bool = zig_config.replDumpStdIn;
pub var REPL_DUMP_STDIN = REPL_DUMP_STDIN_DEFAULT;

pub const REPL_HISTORY_PATH_DEFAULT: []const u8 = zig_config.replHistoryPath;
pub var REPL_HISTORY_PATH = REPL_HISTORY_PATH_DEFAULT;

pub const MAX_DEPTH_DEFAULT: usize = zig_config.maxComptimeDepth;
pub var MAX_DEPTH = MAX_DEPTH_DEFAULT;

pub const MAX_DEPTH_MIN: usize = 8;

comptime {
    if (MAX_DEPTH_DEFAULT < MAX_DEPTH_MIN) {
        @compileError("invalid maxComptimeDepth option set");
    }
}
