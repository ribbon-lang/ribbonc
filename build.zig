const std = @import("std");
const Build = std.Build;

const log = std.log.scoped(.build);

pub fn build(b: *Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const Rli = b.dependency("Rli", .{
        .target = target,
        .optimize = optimize,
    });

    const Utils = b.dependency("Utils", .{
        .target = target,
        .optimize = optimize,
    });

    const ribbonExe = b.addExecutable(.{
        .name = "ribbon",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const testExe = b.addTest(.{
        .name = "test-exe",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    ribbonExe.root_module.addImport("Utils", Utils.module("Utils"));
    ribbonExe.root_module.addImport("Rli", Rli.module("Rli"));

    testExe.root_module.addImport("Utils", Utils.module("Utils"));
    testExe.root_module.addImport("Rli", Rli.module("Rli"));

    const installStep = b.default_step;
    const runStep = b.step("run", "Run the exe");
    const checkStep = b.step("check", "Semantic analysis");
    const testStep = b.step("test", "Run all tests");

    const runRibbon = b.addRunArtifact(ribbonExe);
    if (b.args) |args| runRibbon.addArgs(args);

    installStep.dependOn(&b.addInstallArtifact(ribbonExe, .{}).step);
    runStep.dependOn(&runRibbon.step);
    testStep.dependOn(&b.addRunArtifact(testExe).step);
    checkStep.dependOn(&testExe.step);
}
