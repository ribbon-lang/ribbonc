const std = @import("std");
const Build = std.Build;

pub usingnamespace @import("src/root.zig");

pub fn build(b: *Build) !void {
    const defaultTarget = b.standardTargetOptions(.{});
    const defaultOptimize = b.standardOptimizeOption(.{});

    _ = b.addModule("Type", .{
        .root_source_file = b.path("src/root.zig"),
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });
}
