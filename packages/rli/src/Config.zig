const std = @import("std");
const zig_config = @import("config");

pub const VERSION: std.SemanticVersion = zig_config.version;

pub var USE_EMOJI = true;

pub var USE_ANSI_STYLES = true;

pub var REPL_DISABLE_RAW_MODE = false;

pub var REPL_DUMP_STDIN = false;

pub var REPL_HISTORY_PATH: []const u8 = ".rli-repl-history";

pub var MAX_DEPTH: usize = 1024;
