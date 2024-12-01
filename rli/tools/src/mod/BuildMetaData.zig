const std = @import("std");

pub fn make(comptime TypeUtils: type) type {
    return struct {
        pub const paths = .{
            .withTooling = &[_][]const u8{ ".", "tools/src" },
            .sourceOnly = &[_][]const u8{ ".", "tools/src/mod" },
        };

        pub const commands = TypeUtils.structConcat(.{
            basicCommands,
            testCommands,
            fullCommands,
            verifyCommands,
        });

        pub const commandNames = std.meta.fieldNames(@TypeOf(commands));
        pub const basicCommandNames = std.meta.fieldNames(@TypeOf(basicCommands));
        pub const testCommandNames = std.meta.fieldNames(@TypeOf(testCommands));
        pub const fullCommandNames = std.meta.fieldNames(@TypeOf(fullCommands));
        pub const verifyCommandNames = std.meta.fieldNames(@TypeOf(verifyCommands));

        pub const basicCommands = .{
            .run = "Build and run a quick debug test version of rli only (No headers, readme, lib ...)",
            .quick = "Build a quick debug test version of rli only (No headers, readme, lib ...)",
            .full = listCommand(fullCommandNames),
            .verify = listCommand(verifyCommandNames),
            .release = "Build the release versions of rli for all targets",
        };

        pub const testCommands = .{
            .@"unit-tests" = "Run unit tests",
            .@"cli-tests" = "Run cli tests",
            .@"c-tests" = "Run C tests",
        };

        pub const fullCommands = .{
            .@"test" = listCommand(testCommandNames),
            .readme = "Generate `./README.md`",
            .header = "Generate `./include/rli.h`",
        };

        pub const verifyCommands = .{
            .@"verify-readme" = "Verify that `./README.md` is up to date",
            .@"verify-header" = "Verify that `./include/rli.h` is up to date",
            .@"verify-tests" = "Verify that all tests pass (this is an alias for `test`)",
        };

        pub const options = .{
            .logLevel = .{ std.log.Level, "Logging output level to display", .err },
            .logScopes = .{ []const u8, "Logging scopes to display", "rli,repl" },
            .useEmoji = .{ bool, "Use emoji in the output", true },
            .useAnsiStyles = .{ bool, "Use ANSI styles in the output", true },
            .replDumpStdIn = .{ bool, "(REPL) Default setting for dumping stdin to a file", false },
            .replHistoryPath = .{ []const u8, "(REPL) Default path to the history file", ".rli-repl-history" },
            .maxComptimeDepth = .{ usize, "(Compiler Eval) Default maximum call depth", 1024 },
        };

        pub const buildOptions = .{
            .forceNewSnapshot = .{ bool, "(Tests) Force a new snapshot to be created instead of referring to an existing one", false },
            .stripDebugInfo = .{ ?bool, "Override for optimization-specific settings for stripping debug info from the binary" },
        };

        pub const releaseTargets: []const std.Target.Query = &.{
            .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .gnu },
            .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .musl },
            .{ .cpu_arch = .x86_64, .os_tag = .windows, .abi = .gnu },
            .{ .cpu_arch = .x86_64, .os_tag = .windows, .abi = .msvc },
            .{ .cpu_arch = .x86_64, .os_tag = .macos },
            .{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .gnu },
            .{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .musl },
            .{ .cpu_arch = .aarch64, .os_tag = .windows, .abi = .gnu },
            .{ .cpu_arch = .aarch64, .os_tag = .windows, .abi = .msvc },
            .{ .cpu_arch = .aarch64, .os_tag = .macos },
        };

        pub const packageDeps = .{
            .bdwgc = .{
                .parameters = .{
                    .install_headers = false,
                    .pointer_mask = @as(usize, 0x00_00_FF_FF_FF_FF_FF_FF),
                },
            },
            .clap = .{},
            .Utils = .{},
        };

        fn listCommand(comptime names: []const []const u8) []const u8 {
            comptime {
                var cmdList: []const u8 = "Runs the following commands: ";

                for (names, 0..) |name, i| {
                    cmdList = cmdList ++ name;
                    if (i < names.len - 1) {
                        cmdList = cmdList ++ ", ";
                    }
                }

                return cmdList;
            }
        }
    };
}
