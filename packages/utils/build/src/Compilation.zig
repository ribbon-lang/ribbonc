const std = @import("std");
const Build = std.Build;

const lib = @import("./root.zig");
const SourceTree = lib.SourceTree;
const TypeUtils = @import("TypeUtils");
const HeaderGenUtils = lib.HeaderGenUtils;
const Snapshot = lib.Snapshot;

const log = std.log.scoped(.compilation);


const Set = @This();
owner: *Build,
name: []const u8,
tree: *const SourceTree.Map,
isTest: bool,
fileGen: bool,
tests: std.ArrayList([]const u8),
files: std.ArrayList([]const u8),

meta: Meta,
vis: UnitVisibility,
triple: []const u8,
target: Build.ResolvedTarget,
optimize: std.builtin.OptimizeMode,
strip: bool,
units: UnitMap,
packages: std.StringHashMap(Package),

pub fn init(
    b: *Build,
    name: []const u8,
    tree: *const SourceTree.Map,
    packages: anytype,
    details: BuildDetails,
) anyerror!*Set {
    const set = try b.allocator.create(Set);
    const nativeTarget = b.resolveTargetQuery(.{});

    set.owner = b;
    set.name = name;
    set.tree = tree;
    set.isTest = details.tests;
    set.fileGen = details.fileGen;
    set.tests = std.ArrayList([]const u8).init(b.allocator);
    set.files = std.ArrayList([]const u8).init(b.allocator);
    set.units = UnitMap.init(b.allocator);
    set.packages = std.StringHashMap(Package).init(b.allocator);

    set.meta = details.meta;

    set.vis = details.vis;
    set.triple = try details.target.query.zigTriple(b.allocator);
    std.debug.assert( details.meta == .generative
                   or std.mem.eql(u8, set.triple, try nativeTarget.query.zigTriple(b.allocator))
                   );
    set.target = details.target;
    set.optimize = details.optimize;
    set.strip = details.strip;

    inline for (comptime std.meta.fieldNames(@TypeOf(packages))) |packageName| {
        const packageInfo = @field(packages, packageName);
        const InfoT = @TypeOf(packageInfo);

        if (comptime InfoT == *Build.Step.Options) {
            _ = try createUnit(.{
                .set = set,
                .name = std.fmt.comptimePrint("{s}", .{packageName}),
                .dependencies = &[0][]const u8 {},
                .data = .{ .config = packageInfo },
            });
            continue;
        }

        const package = set.owner.dependency(
            packageName,
            TypeUtils.structConcat(.{
                .{
                    .target = set.target,
                    .optimize = set.optimize,
                },
                if (@hasField(InfoT, "parameters")) @field(packageInfo, "parameters")
                else .{}
            })
        );

        try set.packages.put(packageName, package);

        const modules = comptime
            if (@hasField(InfoT, "modules")) @field(packageInfo, "modules")
            else .{packageName};

        inline for (0..modules.len) |i| {
            const moduleName = modules[i];

            const namespacedName = comptime
                if (std.mem.eql(u8, packageName, moduleName)) std.fmt.comptimePrint("{s}", .{packageName})
                else std.fmt.comptimePrint("{s}:{s}", .{packageName, moduleName});

            _ = try createUnit(.{
                .set = set,
                .name = namespacedName,
                .dependencies = &[0][]const u8 {},
                .data = .{ .dependency = .{
                    .build = package.module(moduleName),
                    .package = package,
                } },
            });
        }
    }

    var treeIter = set.tree.keyIterator();
    while (treeIter.next()) |nodeName| {
        _ = try acquireUnit(set, nodeName.*);
    }

    var unitIter = set.units.keyIterator();
    while (unitIter.next()) |unitName| {
        const unit = set.units.get(unitName.*).?;

        std.debug.assert(!unit.isUninit());

        if (!unit.isLinkable()) continue;

        try linkDependencies(set, unit);
    }

    return set;
}

pub fn getHeader(self: *const Set, headerName: []const u8) !File {
    const headerUnitName = try std.fmt.allocPrint(self.owner.allocator, HEADER_PREFIX ++ "{s}", .{headerName});

    return self.getFile(headerUnitName);
}

pub fn getFile(self: *const Set, unitName: []const u8) !File {
    if (self.units.get(unitName)) |unit| {
        switch (unit.data) {
            .file => |x| return x,
            else => {
                log.err("expected unit `{s}` to be a file unit, got {s}", .{unitName, @tagName(unit.data)});
                return error.UnexpectedUnitData;
            }
        }
    } else {
        log.err("cannot find file unit `{s}`", .{unitName});
        return error.MissingFile;
    }
}

pub fn getTest(self: *const Set, unitName: []const u8) !Test {
    if (self.units.get(unitName)) |unit| {
        switch (unit.data) {
            .@"test" => |x| return x,
            else => {
                log.err("expected unit `{s}` to be a test, got {s}", .{unitName, @tagName(unit.data)});
                return error.UnexpectedUnitData;
            }
        }
    } else {
        log.err("cannot find test unit `{s}`", .{unitName});
        return error.MissingTest;
    }
}

pub fn getModule(self: *const Set, unitName: []const u8) !Module {
    if (self.units.get(unitName)) |unit| {
        switch (unit.data) {
            .module => |x| return x,
            else => {
                log.err("expected unit `{s}` to be a module, got {s}", .{unitName, @tagName(unit.data)});
                return error.UnexpectedUnitData;
            }
        }
    } else {
        log.err("cannot find module unit `{s}`", .{unitName});
        return error.MissingModule;
    }
}

pub fn getBinary(self: *const Set, unitName: []const u8) !Binary {
    if (self.units.get(unitName)) |unit| {
        switch (unit.data) {
            .binary => |x| return x,
            else => {
                log.err("expected unit `{s}` to be a binary, got {s}", .{unitName, @tagName(unit.data)});
                return error.UnexpectedUnitData;
            }
        }
    } else {
        log.err("cannot find binary unit `{s}`", .{unitName});
        return error.MissingBinary;
    }
}

pub fn getLibrary(self: *const Set, unitName: []const u8) !Library {
    if (self.units.get(unitName)) |unit| {
        switch (unit.data) {
            .library => |x| return x,
            else => {
                log.err("expected unit `{s}` to be a library, got {s}", .{unitName, @tagName(unit.data)});
                return error.UnexpectedUnitData;
            }
        }
    } else {
        log.err("cannot find library unit `{s}`", .{unitName});
        return error.MissingLibrary;
    }
}

pub fn getPackage(self: *const Set, packageName: []const u8) !Package {
    return self.packages.get(packageName) orelse {
        log.err("cannot find package `{s}`", .{packageName});
        return error.MissingPackage;
    };
}

pub fn getDependency(self: *const Set, unitName: []const u8) !Dependency {
    if (self.units.get(unitName)) |unit| {
        switch (unit.data) {
            .dependency => |x| return x,
            else => {
                log.err("expected unit `{s}` to be a dependency, got {s}", .{unitName, @tagName(unit.data)});
                return error.UnexpectedUnitData;
            }
        }
    } else {
        log.err("cannot find dependency unit `{s}`", .{unitName});
        return error.MissingDependency;
    }
}

pub fn getConfig(self: *const Set, unitName: []const u8) !Config {
    if (self.units.get(unitName)) |unit| {
        switch (unit.data) {
            .config => |x| return x,
            else => {
                log.err("expected unit `{s}` to be a config, got {s}", .{unitName, @tagName(unit.data)});
                return error.UnexpectedUnitData;
            }
        }
    } else {
        log.err("cannot find config unit `{s}`", .{unitName});
        return error.MissingConfig;
    }
}



pub fn findUnit(self: *const Set, unitName: []const u8) ?*Unit {
    return self.units.get(unitName);
}

pub fn getLibJoiner(self: *Set) Binary {
    switch (self.meta) {
        .generative => |set| {
            return set.getLibJoiner();
        },
        .native => |builder| {
            return builder.artifact("libjoiner");
        },
    }
}

pub fn getTemplater(self: *Set) Binary {
    switch (self.meta) {
        .generative => |set| {
            return set.getTemplater();
        },
        .native => |builder| {
            return builder.artifact("templater");
        },
    }
}

pub fn getSnapshotWriter(self: *Set) Binary {
    switch (self.meta) {
        .generative => |set| {
            return set.getSnapshotWriter();
        },
        .native => |builder| {
            return builder.artifact("snapshot-writer");
        },
    }
}

pub fn getSnapshotHelper(self: *Set, snapshotPath: []const u8) !Snapshot.Helper {
    const bin = self.getSnapshotWriter();
    const run = self.owner.addRunArtifact(bin);
    const out = run.captureStdOut();
    const write = self.owner.addUpdateSourceFiles();
    write.addCopyFileToSource(out, snapshotPath);

    const map = try Snapshot.Map.readMap(self.owner.allocator, snapshotPath)
         orelse try Snapshot.Map.init(self.owner.allocator);

    return .{ .owner = self.owner, .map = map, .run = run, .write = write };
}

pub fn createHeaderGen(self: *Set, source: *Unit) !*Unit {
    switch (self.meta) {
        .generative => |set| {
            return set.createHeaderGen(source);
        },
        .native => |_| {
            var src = source;

            if (self != source.set) {
                // we need to recreate the source in the meta set so that its linked properly and set to the native target
                src = acquireUnit(self, source.name) catch |err| {
                    if (err == error.MissingNode) {
                        log.err("cannot find meta source unit `{s}`", .{source.name});
                    }
                    return err;
                };
            }

            const name = try std.fmt.allocPrint(self.owner.allocator, "HeaderGen:{s}", .{source.name});

            if (self.units.get(name)) |unit| {
                return unit;
            }

            const module = try extractModule(source);

            const unit = try createUnit(.{
                .set = self,
                .name = name,
                .dependencies = &[0][]const u8 {},
                .data = .{ .binary = try lib.makeHeaderGen(self.owner, module) },
            });

            return unit;
        },
    }
}

pub fn format(self: *const Set, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) anyerror!void {
    const templ =
        \\metaSet: {?},
        \\vis: {s},
        \\target: {s},
        \\optimize: {s},
        \\strip: {},
        \\
        ;
    try writer.print(templ, .{
        self.metaSet,
        @tagName(self.vis),
        self.triple,
        @tagName(self.optimize),
        self.strip,
    });

    try writer.writeAll("units:\n");
    var iter = self.units.valueIterator();
    while (iter.next()) |unit| {
        try writer.print("  {s} {s}:\n", .{@tagName(unit.*.data), unit.*.name});
        try writer.writeAll("    dependencies:\n");
        for (unit.*.dependencies) |dep| {
            try writer.print("      {s}\n", .{dep});
        }
    }

    try writer.writeAll("tests:\n");
    for (self.tests.items) |t| {
        try writer.print("  {s}\n", .{t});
    }

    try writer.writeAll("files:\n");
    for (self.files.items) |a| {
        try writer.print("  {s}\n", .{a});
    }
}

pub const HEADER_PREFIX = "@Header:";

pub fn makeHeaderFileName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
    var baseName = name;
    if (std.mem.startsWith(u8, name, HEADER_PREFIX)) {
        baseName = name[HEADER_PREFIX.len..];
    }

    return try std.fmt.allocPrint(allocator, "{s}.h", .{baseName});
}

pub fn isHeaderFileName(name: []const u8) bool {
    return std.mem.endsWith(u8, name, ".h")
        or std.mem.startsWith(u8, name, HEADER_PREFIX)
         ;
}


pub const UnitVisibility = SourceTree.EntryVis;

pub const DependencyList = std.ArrayList([]const u8);

pub const UnitMap = std.StringHashMap(*Unit);

pub const Unit = struct {
    set: *Set,
    name: []const u8,
    data: UnitData,
    dependencies: []const []const u8,

    fn isLinkable(self: *const Unit) bool {
        return switch (self.data) {
            .@"test" => true,
            .module => true,
            .library => true,
            .binary => true,
            .dependency => false,
            .config => false,
            .file => false,
            .uninit => false,
        };
    }

    fn isUninit(self: *const Unit) bool {
        return self.data == .uninit;
    }

    pub fn format(self: *const Unit, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) anyerror!void {
        const templ =
            \\name: {s},
            \\data: {s},
            \\
            ;
        try writer.print(templ, .{self.name, @tagName(self.data)});

        try writer.writeAll("dependencies:\n");
        for (self.dependencies) |dep| {
            try writer.print("  {s}\n", .{dep});
        }
    }
};

pub const UnitData = union(enum) {
    @"test": Test,
    module: Module,
    library: Library,
    binary: Binary,
    dependency: Dependency,
    config: Config,
    file: File,
    uninit: void,
};

pub const Test = *Build.Step.Compile;
pub const Module = *Build.Module;
pub const Library = *Build.Step.Compile;
pub const Binary = *Build.Step.Compile;

pub const Dependency = struct {
    build: *Build.Module,
    package: Package,
};

pub const Config = *Build.Step.Options;

pub const File = Build.LazyPath;

pub const Meta = union(enum) {
    native: Package,
    generative: *Set,
};

pub const Package = *Build.Dependency;

pub const BuildDetails = struct {
    meta: Meta,
    vis: UnitVisibility,
    target: Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    strip: bool,
    fileGen: bool,
    tests: bool,
};


fn createUnit(unit: Unit) anyerror!*Unit {
    if (unit.set.units.contains(unit.name)) return error.DuplicateUnit;
    const outUnit = try unit.set.units.allocator.create(Unit);
    try unit.set.units.put(unit.name, outUnit);
    outUnit.* = unit;
    return outUnit;
}

fn acquireUnit(set: *Set, nodeName: []const u8) anyerror!*Unit {
    if (set.findUnit(nodeName)) |unit| return unit;

    const node: *SourceTree.Entry = set.tree.get(nodeName) orelse {
        return error.MissingNode;
    };

    const unit = try createUnit(.{
        .set = set,
        .name = node.name,
        .dependencies = node.dependencies.keys(),
        .data = .uninit,
    });

    var path = set.owner.path(node.path);
    if (node.templateData) |templateData| {
        const templater = set.getTemplater();

        const runTemplater = set.owner.addRunArtifact(templater);

        for (templateData.deps) |dep| {
            { // HACK
                // this is a workaround for the fact that you cannot currently add
                // directories as fileInputs to a run artifact
                const stat = std.fs.cwd().statFile(dep) catch |err| {
                    log.err("cannot stat template dependency `{s}` for template `{s}`, error {s}", .{dep, node.path, @errorName(err)});
                    return err;
                };

                if (stat.kind != .file) {
                    runTemplater.has_side_effects = true;
                    break;
                }
            }

            runTemplater.addFileInput(set.owner.path(dep));
        }

        runTemplater.addFileInput(path);
        runTemplater.addArg(node.path);

        runTemplater.addArg("-no-static");

        for (templateData.params) |binaryName| {
            const bin = try acquireTemplaterBinary(set, binaryName);

            runTemplater.addArg(binaryName);
            runTemplater.addArtifactArg(bin);
        }

        path = runTemplater.captureStdOut();
    }

    unit.data = if (set.isTest and node.hasTests) .{
        .@"test" = set.owner.addTest(.{
            .name = node.name,
            .root_source_file = path,
            .target = set.target,
            .optimize = set.optimize,
            .strip = set.strip,
        }),
    } else switch (node.kind) {
        .module => mod: {
            const module = set.owner.createModule(.{
                .root_source_file = path,
                .target = set.target,
                .optimize = set.optimize,
                .strip = set.strip,
            });

            if (node.vis.concat(set.vis) == .public) {
                try set.owner.modules.put(set.owner.dupe(node.name), module);
            }

            break :mod .{ .module = module };
        },
        .library => .{
            .library = set.owner.addStaticLibrary(.{
                .name = node.name,
                .root_source_file = path,
                .target = set.target,
                .optimize = set.optimize,
                .strip = set.strip,
            })
        },
        .binary => .{
            .binary = set.owner.addExecutable(.{
                .name = node.name,
                .root_source_file = path,
                .target = set.target,
                .optimize = set.optimize,
                .strip = set.strip,
            })
        },
        .document => .{
            .file = path,
        },
    };

    if (unit.data == .@"test") {
        try set.tests.append(unit.name);
    }

    if (set.fileGen and node.hasHeaderGenData) {
        const headerGen = set.createHeaderGen(unit) catch |err| {
            log.err("cannot create header generator for unit `{s}`, error {s}", .{node.name, @errorName(err)});
            return err;
        };

        const runHeaderGen = set.owner.addRunArtifact(headerGen.data.binary);

        runHeaderGen.addFileArg(set.owner.path(node.path));

        runHeaderGen.addArg("-no-static");

        const output = runHeaderGen.captureStdOut();

        const headerUnit = try createUnit(.{
            .set = set,
            .name = try std.fmt.allocPrint(set.owner.allocator, HEADER_PREFIX ++ "{s}", .{node.name}),
            .dependencies = try set.owner.allocator.dupe([]const u8, &[2][]const u8 {unit.name, headerGen.name}),
            .data = .{ .file = output },
        });

        try set.files.append(headerUnit.name);
    } else if (unit.data == .file) {
        try set.files.append(unit.name);
    }

    return unit;
}


fn acquireTemplaterBinary(self: *Set, name: []const u8) !Binary {
    switch (self.meta) {
        .generative => |set| {
            return set.acquireTemplaterBinary(name);
        },
        .native => |_| {
            const pfxBinaryName = try std.fmt.allocPrint(self.owner.allocator, "Templater:{s}", .{name});
            const unit = acquireUnit(self, pfxBinaryName) catch |err| {
                if (err == error.MissingNode) {
                    log.err("Cannot find template parameter unit `{s}`", .{pfxBinaryName});
                }
                return err;
            };

            if (unit.data != .binary) {
                log.err("Expected template parameter unit to be a binary, got {s}", .{@tagName(unit.data)});
                return error.InvalidUnitType;
            }

            return unit.data.binary;
        },
    }
}

fn extractFileName(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var sub = path;
    var ext: []const u8 = "";

    if (std.mem.lastIndexOfAny(u8, path, "\\/")) |lsi| {
        sub = path[lsi + 1..];
    }

    if (std.mem.lastIndexOf(u8, sub, ".")) |ldi| {
        ext = sub[ldi..];
        sub = sub[0..ldi];
    }

    const TEMPLATE_SUFFIX = ".template";
    if (std.mem.endsWith(u8, sub, TEMPLATE_SUFFIX)) {
        sub = sub[0..sub.len - TEMPLATE_SUFFIX.len];
    }

    return try std.fmt.allocPrint(allocator, "{s}{s}", .{sub, ext});
}

fn extractModule(unit: *Unit) anyerror!Module {
    return switch (unit.data) {
        .@"test" => |x| &x.root_module,
        .module => |x| x,
        .library => |x| &x.root_module,
        .binary => |x| &x.root_module,
        .config => return error.UnexpectedConfig,
        .file => return error.UnexpectedFile,
        .dependency => return error.UnexpectedDependency,
        .uninit => return error.UnexpectedUninit,
    };
}

fn extractStep(unit: *Unit) anyerror!*Build.Step {
    return switch (unit.data) {
        .@"test" => |x| &x.step,
        .module => return error.UnexpectedModule,
        .library => |x| &x.step,
        .binary => |x| &x.step,
        .config => return error.UnexpectedConfig,
        .file => return error.UnexpectedFile,
        .dependency => return error.UnexpectedDependency,
        .uninit => return error.UnexpectedUninit,
    };
}

fn linkDependencies(set: *Set, unit: *Unit) anyerror!void {
    const module = try extractModule(unit);

    try linkDependenciesRaw(set, unit.name, module, unit.dependencies);
}

fn linkDependenciesRaw(set: *Set, name: []const u8, module: Module, dependencies: []const []const u8) anyerror!void {
    for (dependencies) |depName| {
        const dep: *Unit = acquireUnit(set, depName) catch |err| {
            if (err == error.MissingNode) {
                log.err("cannot find dependency `{s}` for unit `{s}`", .{depName, name});
                log.info("  available dependencies were:", .{});
                var keyIter = set.units.keyIterator();
                while (keyIter.next()) |unitName| {
                    log.info("    {s}", .{unitName.*});
                }
            }
            return err;
        };

        switch (dep.data) {
            .@"test" => |x| module.addImport(dep.name, &x.root_module),
            .module => |x| module.addImport(dep.name, x),
            .dependency => |x| module.addImport(dep.name, x.build),
            .config => |x| module.addOptions(dep.name, x),
            .uninit => {
                log.err("cannot link uninitialized dependency `{s}` for unit `{s}`", .{dep.name, name});
                return error.UnexpectedUninit;
            },
            else => continue,
        }
    }
}
