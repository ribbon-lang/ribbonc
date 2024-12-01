const std = @import("std");
const Build = std.Build;

pub usingnamespace @import("src/root.zig");

const zgModules = .{
    "GenCatData",
    "PropsData",
    "CaseData",
    "CaseFold",
    "DisplayWidth",
};

pub fn build(b: *Build) !void {
    const defaultTarget = b.standardTargetOptions(.{});
    const defaultOptimize = b.standardOptimizeOption(.{});

    const nativeTarget = b.resolveTargetQuery(.{});

    const zg = b.dependency("zg", .{
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    const mod = b.addModule("Text", .{
        .root_source_file = b.path("src/root.zig"),
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    inline for (zgModules) |modName| {
        mod.addImport(modName, zg.module(modName));
    }

    const testStep = b.step("test", "run tests");

    const testExe = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = nativeTarget,
        .optimize = .Debug,
    });

    inline for (zgModules) |modName| {
        testExe.root_module.addImport(modName, zg.module(modName));
    }

    testStep.dependOn(&b.addRunArtifact(testExe).step);
}
