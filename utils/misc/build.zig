const std = @import("std");

pub fn build(b: *std.Build) void {
    const defaultTarget = b.standardTargetOptions(.{});
    const defaultOptimize = b.standardOptimizeOption(.{});

    _ = b.addModule("Misc", .{
        .root_source_file = b.path("src/root.zig"),
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });
}
