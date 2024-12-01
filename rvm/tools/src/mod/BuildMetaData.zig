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
            .run = "Build and run a quick debug test version of rvm only (No headers, readme, lib ...)",
            .quick = "Build a quick debug test version of rvm only (No headers, readme, lib ...)",
            .full = listCommand(fullCommandNames),
            .verify = listCommand(verifyCommandNames),
            .check = "Run semantic analysis on all files referenced by a unit test; do not build artifacts (Useful with `zls` build on save)",
            .release = "Build the release versions of Rvm for all targets",
        };

        pub const testCommands = .{
            .@"unit-tests" = "Run unit tests",
            .@"cli-tests" = "Run cli tests",
            .@"c-tests" = "Run C tests",
        };

        pub const fullCommands = .{
            .@"test" = listCommand(testCommandNames),
            .readme = "Generate `./README.md`",
            .header = "Generate `./include/rvm.h`",
        };

        pub const verifyCommands = .{
            .@"verify-readme" = "Verify that `./README.md` is up to date",
            .@"verify-header" = "Verify that `./include/rvm.h` is up to date",
            .@"verify-tests" = "Verify that all tests pass (this is an alias for `test`)",
        };

        pub const options = .{
            .logLevel = .{ std.log.Level, "Logging output level to display", .err },
            .logScopes = .{ []const u8, "Logging scopes to display", "rvm" },
            .useEmoji = .{ bool, "Use emoji in the output", true },
            .useAnsiStyles = .{ bool, "Use ANSI styles in the output", true },
        };

        pub const buildOptions = .{
            .forceNewSnapshot = .{ bool, "(Tests) Force a new snapshot to be created instead of referring to an existing one", false },
            .stripDebugInfo = .{ ?bool, "Override for optimization-specific settings for stripping debug info from the binary. This will default to `true` when `-Doptimize` is not set to `Debug`" },
            .maximumInlining = .{ ?bool, "Override for optimization-specific settings for inlining as much as possible in the interpreter. This will default to `true` when `-Doptimize` is not set to `Debug`" },
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
            .clap = .{},
            .Rbc = .{
                .modules = .{ "Core", "Builder" }
            },
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
