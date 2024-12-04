const std = @import("std");
const Build = std.Build;
const Builder = @import("Utils").Module.Build.Module;
const Snapshot = Builder.Snapshot;
const Manifest = Builder.Manifest;
const TypeUtils = @import("Utils").Module.Type;

const log = std.log.scoped(.build);

pub fn build(b: *Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const forceNewSnapshot = b.option(bool, "forceNewSnapshot", "Force new snapshots to be generated instead of referring to existing ones when validating tests") orelse false;

    const manifest = try Manifest.readFile(b.allocator, "./build.zig.zon");

    const config = b.addOptions();
    config.addOption(std.SemanticVersion, "version", manifest.version);

    const bdwgc = b.dependency("bdwgc", .{
        .target = target,
        .optimize = optimize,
        .install_headers = false,
        .pointer_mask = @as(usize, 0x00_00_FF_FF_FF_FF_FF_FF),
    });

    const clap = b.dependency("clap", .{
        .target = target,
        .optimize = optimize,
    });

    const Utils = b.dependency("Utils", .{
        .target = target,
        .optimize = optimize,
    });

    const Config = b.createModule(.{
        .root_source_file = b.path("src/Config.zig"),
        .target = target,
        .optimize = optimize,
    });

    const Rli = b.addModule("Rli", .{
        .root_source_file = b.path("src/mod/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const testRli = b.addTest(.{
        .name = "test-rli",
        .root_source_file = b.path("src/mod/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const rliExe = b.addExecutable(.{
        .name = "rli",
        .root_source_file = b.path("src/bin/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const testExe = b.addTest(.{
        .name = "test-exe",
        .root_source_file = b.path("src/bin/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    Config.addOptions("config", config);

    Rli.addImport("Config", Config);
    Rli.addImport("Utils", Utils.module("Utils"));
    Rli.addImport("bdwgc", bdwgc.module("bdwgc"));

    testRli.root_module.addImport("Config", Config);
    testRli.root_module.addImport("Utils", Utils.module("Utils"));
    testRli.root_module.addImport("bdwgc", bdwgc.module("bdwgc"));

    rliExe.root_module.addImport("Rli", Rli);
    rliExe.root_module.addImport("Config", Config);
    rliExe.root_module.addImport("clap", clap.module("clap"));
    rliExe.root_module.addImport("Utils", Utils.module("Utils"));

    testExe.root_module.addImport("Rli", &testRli.root_module);
    testExe.root_module.addImport("Config", Config);
    testExe.root_module.addImport("clap", clap.module("clap"));
    testExe.root_module.addImport("Utils", Utils.module("Utils"));

    const installStep = b.default_step;
    const runStep = b.step("run", "Run the exe");
    const unitTestsStep = b.step("unit-tests", "Run the unit tests");
    const checkStep = b.step("check", "Semantic analysis");
    const cliTestsStep = b.step("cli-tests", "Run the CLI tests");
    const testStep = b.step("test", "Run all tests");

    const runRli = b.addRunArtifact(rliExe);
    if (b.args) |args| runRli.addArgs(args);

    installStep.dependOn(&b.addInstallArtifact(rliExe, .{}).step);
    runStep.dependOn(&runRli.step);
    unitTestsStep.dependOn(&b.addRunArtifact(testExe).step);
    unitTestsStep.dependOn(&b.addRunArtifact(testRli).step);
    testStep.dependOn(unitTestsStep);
    testStep.dependOn(cliTestsStep);
    checkStep.dependOn(&testExe.step);
    checkStep.dependOn(&testRli.step);

    const bin = Utils.artifact("snapshot-writer");
    const run = b.addRunArtifact(bin);
    const out = run.captureStdOut();
    const write = b.addUpdateSourceFiles();
    write.addCopyFileToSource(out, "./tests/.snapshot");

    const map = try Snapshot.Map.readMap(b.allocator, "./tests/.snapshot")
         orelse try Snapshot.Map.init(b.allocator);

    var snapshotHelper = Builder.Snapshot.Helper { .owner = b, .map = map, .run = run, .write = write };

    snapshotHelper.runWith(cliTestsStep);

    try cliTest(b, &snapshotHelper, forceNewSnapshot, rliExe, cliTestsStep, .pass);
    try cliTest(b, &snapshotHelper, forceNewSnapshot, rliExe, cliTestsStep, .fail);

    snapshotHelper.finalize();
}


const ScriptTestKind = enum {
    pass,
    fail,
};

fn matchArg(against: []const u8, arg: []const u8) bool {
    return std.mem.containsAtLeast(u8, against, 1, arg);
}

fn cliTest(b: *Build, snapshotHelper: *Snapshot.Helper, forceNew: bool, bin: *Build.Step.Compile, step: *Build.Step, kind: ScriptTestKind) !void {
    const kindName = @tagName(kind);

    const kindPath = b.fmt("tests/{s}", .{kindName});
    const kindDir = try std.fs.cwd().makeOpenPath(kindPath, .{ .iterate = true });

    var iter = kindDir.iterate();

    testLoop: while (try iter.next()) |tEntry| {
        if (tEntry.kind == .file) {
            const localPath = b.fmt("{s}/{s}", .{ kindName, tEntry.name });
            if (b.args) |args| {
                for (args) |arg| {
                    if (matchArg(localPath, arg)) {
                        break;
                    }
                } else {
                    continue :testLoop;
                }
            }
            const tPath = b.fmt("./tests/{s}", .{localPath});
            const name = std.fs.path.stem(tEntry.name);
            const tTest = Build.Step.Run.create(b, b.fmt("{s} {s}", .{ kindName, name }));
            tTest.addArtifactArg(bin);
            tTest.addFileInput(b.path(tPath));
            tTest.addArg(tPath);

            const snapshotName = b.fmt("{s}:{s}", .{ kindName, name });

            tTest.expectExitCode(switch (kind) {
                .pass => 0,
                .fail => 1,
            });

            const expectedOutput =
                if (forceNew) null
                else snapshotHelper.get(snapshotName);

            const run = if (expectedOutput) |expect| expect: {
                const text = try expect.toText(b.allocator);
                tTest.expectStdOutEqual(text.out);
                tTest.expectStdErrEqual(text.err);

                break :expect &tTest.step;
            } else new: {
                const snapshot = Snapshot.LazyPair{ .out = tTest.captureStdOut(), .err = tTest.captureStdErr() };
                try snapshotHelper.put(snapshotName, snapshot);

                const warn = b.addSystemCommand(&[_][]const u8{ "echo", b.fmt("manual validation required, new snapshots added; capturing output of test [{s}] to the following paths:\n", .{tPath}) });
                warn.addFileArg(snapshot.out);
                warn.addFileArg(snapshot.err);
                step.dependOn(&warn.step);
                break :new &warn.step;
            };

            // const stdOut = tTest.captureStdOut();
            // const stdErr = tTest.captureStdErr();

            // const dumpOut = b.addSystemCommand(&.{ "cat" });
            // dumpOut.addFileArg(stdOut);
            // const dumpErr = b.addSystemCommand(&.{ "cat" });
            // dumpErr.addFileArg(stdErr);

            // dumpOut.step.dependOn(run);
            // dumpErr.step.dependOn(&dumpOut.step);

            // step.dependOn(&dumpErr.step);

            step.dependOn(run);
        }
    }
}
