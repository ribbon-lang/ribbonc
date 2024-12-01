const std = @import("std");
const zig_config = @import("config");

pub const VERSION: std.SemanticVersion = zig_config.version;

pub const LOG_LEVEL: std.log.Level = @enumFromInt(@intFromEnum(zig_config.logLevel));

pub const LOG_SCOPES: []const u8 = zig_config.logScopes;

pub const USE_EMOJI_DEFAULT: bool = zig_config.useEmoji;
pub var USE_EMOJI = USE_EMOJI_DEFAULT;

pub const USE_ANSI_STYLES_DEFAULT: bool = zig_config.useAnsiStyles;
pub var USE_ANSI_STYLES = USE_ANSI_STYLES_DEFAULT;

pub const MAXIMUM_INLINING: bool = zig_config.maximumInlining;
pub const INLINING_BRANCH_QUOTA: comptime_int = if (MAXIMUM_INLINING) 25_000 else 5_000;
pub const INLINING_CALL_CONV: std.builtin.CallingConvention =
    if (MAXIMUM_INLINING) std.builtin.CallingConvention.Inline
    else std.builtin.CallingConvention.Unspecified;
pub const INLINING_CALL_MOD: std.builtin.CallModifier =
    if (MAXIMUM_INLINING) std.builtin.CallModifier.always_inline
    else std.builtin.CallModifier.auto;

test {
    std.testing.refAllDeclsRecursive(@This());
}
