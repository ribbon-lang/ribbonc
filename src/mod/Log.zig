const std = @import("std");
const Config = @import("Config");

pub const std_options: std.Options = .{
    .log_level = Config.LOG_LEVEL,
    .logFn = filteredLogFn,
};

pub fn log(
    comptime message_level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    if (comptime !std.log.logEnabled(message_level, scope)) return;

    std.options.logFn(message_level, scope, format, args);
}

pub fn scoped(comptime scope: @Type(.enum_literal)) type {
    if (comptime !@hasDecl(@import("root"), "std_options")) {
        @compileError("std_options not found in root file");
    }

    return struct {
        pub fn err(
            comptime format: []const u8,
            args: anytype,
        ) void {
            @branchHint(.cold);
            log(.err, scope, format, args);
        }

        pub fn warn(
            comptime format: []const u8,
            args: anytype,
        ) void {
            log(.warn, scope, format, args);
        }

        pub fn info(
            comptime format: []const u8,
            args: anytype,
        ) void {
            log(.info, scope, format, args);
        }

        pub fn debug(
            comptime format: []const u8,
            args: anytype,
        ) void {
            log(.debug, scope, format, args);
        }

        pub fn print(
            comptime level: std.log.Level,
            comptime format: []const u8,
            args: anytype,
        ) void {
            log(level, scope, format, args);
        }
    };
}

pub fn unfilteredLogFn(
    comptime level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    nosuspend std.debug.print(@tagName(scope) ++ "[" ++ @tagName(level) ++ "]: " ++ format ++ "\n", args);
}

pub fn filteredLogFn(
    comptime level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    comptime var iter = std.mem.tokenizeScalar(u8, Config.LOG_SCOPES, ',');

    inline while (comptime iter.next()) |scopeStr| {
        if (comptime std.mem.eql(u8, @tagName(scope), scopeStr)) {
            return unfilteredLogFn(level, scope, format, args);
        }
    }
}
