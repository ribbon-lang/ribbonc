const std = @import("std");

const log = std.log.scoped(.sourcetree);

const lib = @import("./root.zig");
const Templater = lib.Templater;
const HeaderGenUtils = lib.HeaderGenUtils;


const BEGIN_TEST = "test";
const BEGIN_IMPORT = "@import(\"";
const END_IMPORT = "\")";
pub var STD_NAMES = [_][]const u8{
    "std",
    "builtin",
    "root",
    "#HEADER_GENERATION_SOURCE_MODULE#",
};

pub var BUILD_IGNORE_FILE_NAME = ".buildignore";

pub var HEADER_GEN_QUERY = "pub const @\"" ++ HeaderGenUtils.DATA_SOURCE_NAME ++ "\"";

pub const EntryVis = enum {
    public,
    private,
    pub const DEFAULT: EntryVis = .public;
    pub const PREFIX = .{
        .public = "public",
        .private = "private",
    };
    pub fn concat(self: EntryVis, other: EntryVis) EntryVis {
        if (self == .private or other == .private) {
            return .private;
        } else {
            return .public;
        }
    }
};

pub const EntryKind = enum {
    binary,
    library,
    module,
    document,
    pub const SUFFIX = .{
        .binary = "bin",
        .library = "lib",
        .module = "mod",
        .document = "doc",
    };
};


pub const Entry = struct {
    parent: ?[]const u8,
    vis: EntryVis,
    kind: EntryKind,
    name: []const u8,
    path: []const u8,
    hasTests: bool,
    hasHeaderGenData: bool,
    files: std.ArrayList([]const u8),
    dependencies: std.StringArrayHashMap(void),
    templateData: ?Templater.Data,

    pub fn isTemplate(self: Entry) bool {
        return self.templateData != null;
    }

    pub fn format(self: Entry, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s} {s} {s}{s}\n", .{ @tagName(self.vis), @tagName(self.kind), self.name, if (self.isTemplate()) " Template" else "" });

        try writer.print("    parent: {s}\n", .{if (self.parent) |p| p else "{root}"});

        try writer.print("    path: `{s}`\n", .{self.path});

        try writer.print("    hasTests: {}\n", .{self.hasTests});
        try writer.print("    hasHeaderGenData: {}\n", .{self.hasHeaderGenData});

        try writer.writeAll("    files: [");
        if (self.files.items.len > 0) {
            try writer.writeAll("\n");
            for (self.files.items) |file| {
                try writer.print("        `{s}`\n", .{file});
            }
            try writer.writeAll("    ]\n");
        } else {
            try writer.writeAll("]\n");
        }

        try writer.writeAll("    dependencies: [");
        if (self.dependencies.count() > 0) {
            try writer.writeAll("\n");
            for (self.dependencies.keys()) |dep| {
                try writer.print("        {s}\n", .{dep});
            }
            try writer.writeAll("    ]\n");
        } else {
            try writer.writeAll("]\n");
        }

        if (self.templateData) |data| {
            try writer.writeAll("    template params: [");
            if (data.params.len > 0) {
                try writer.writeAll("\n");
                for (data.params) |param| {
                    try writer.print("        {s}\n", .{param});
                }
                try writer.writeAll("    ]\n");
            } else {
                try writer.writeAll("]\n");
            }

            try writer.writeAll("    template deps: [");
            if (data.deps.len > 0) {
                try writer.writeAll("\n");
                for (data.deps) |dep| {
                    try writer.print("        {s}\n", .{dep});
                }
                try writer.writeAll("    ]\n");
            } else {
                try writer.writeAll("]\n");
            }
        }
    }
};

pub const Map = std.StringHashMap(*Entry);

const Ignore = std.ArrayList([]const u8);

const Traversal = union(enum) {
    root: EntryVis,
    base: Base,
    specific: *Entry,

    pub const Base = struct {
        vis: EntryVis,
        kind: EntryKind,

        pub fn format(self: Base, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.print("{s} {s}", .{ @tagName(self.vis), @tagName(self.kind) });
        }
    };

    pub fn getVis(self: Traversal) EntryVis {
        return switch (self) {
            .root => |x| x,
            .base => |x| x.vis,
            .specific => |x| x.vis,
        };
    }

    pub fn getKind(self: Traversal) ?EntryKind {
        return switch (self) {
            .root => null,
            .base => |x| x.kind,
            .specific => |x| x.kind,
        };
    }

    pub fn withVis(self: Traversal, vis: EntryVis) Traversal {
        return switch (self) {
            .root => |x| Traversal { .root = x },
            .base => |x| Traversal { .base = .{ .vis = vis, .kind = x.kind } },
            .specific => |x| Traversal { .base = .{ .vis = vis, .kind = x.kind } },
        };
    }

    pub fn withKind(self: Traversal, kind: EntryKind) Traversal {
        return switch (self) {
            .root => |x| Traversal { .base = .{ .vis = x, .kind = kind } },
            .base => |x| Traversal { .base = .{ .vis = x.vis, .kind = kind } },
            .specific => |x| Traversal { .base = .{ .vis = x.vis, .kind = kind } },
        };
    }
};

const IgnoringIter = struct {
    inner: std.fs.Dir.Iterator,
    ignore: *const std.StringHashMap(void),

    pub fn init(dir: std.fs.Dir, ignore: *const std.StringHashMap(void)) IgnoringIter {
        return IgnoringIter{
            .inner = dir.iterate(),
            .ignore = ignore,
        };
    }

    pub fn next(self: *IgnoringIter) anyerror!?std.fs.Dir.Entry {
        while (try self.inner.next()) |entry| {
            if (isDotFile(entry.name)) continue;
            if (isRoot(entry.name)) continue;
            if (isIgnore(entry.name)) continue;
            if (self.ignore.contains(entry.name)) continue;
            return entry;
        }

        return null;
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);

    if (args.len < 2) {
        return error.NotEnoughArguments;
    }

    const map = try getMap(allocator, args[1..]);
    const output = std.io.getStdOut().writer();

    var iter = map.keyIterator();

    while (iter.next()) |name| {
        try output.print("{}", .{map.get(name.*).?});
    }
}

pub fn getMap(allocator: std.mem.Allocator, rootPaths: []const []const u8) !*Map {
    const map = try allocator.create(Map);
    map.* = Map.init(allocator);

    const cwd = std.fs.cwd();

    const cwdPath = cwd.realpathAlloc(allocator, ".") catch |err| {
        log.err("Could not get the real path of current working directory, error `{s}`", .{@errorName(err)});
        return err;
    };

    const cwdWithSlash = try std.fmt.allocPrint(allocator, "{s}/", .{cwdPath});

    for (rootPaths) |rootPath| {
        var rootDir = std.fs.cwd().openDir(rootPath, .{ .iterate = true }) catch |err| {
            log.err("Could not open root path [{s}], error `{s}`", .{rootPath, @errorName(err)});
            return err;
        };
        defer rootDir.close();

        try traverse(
            map,
            cwdWithSlash,
            null,
            rootDir,
            "",
            if (getKind(rootPath)) |kind| Traversal{ .base = .{ .kind = kind, .vis = EntryVis.DEFAULT } }
            else Traversal{ .root = EntryVis.DEFAULT }
        );
    }
    return map;
}

fn traverse(
    rootMap: *Map,
    rootPath: []const u8,
    parentEntry: ?*Entry,
    dir: std.fs.Dir,
    super: []const u8,
    t: Traversal
) anyerror!void {
    var ignore = std.StringHashMap(void).init(rootMap.allocator);

    if (try getIgnore(rootMap.allocator, dir)) |subPath| {
        const file = dir.openFile(subPath, .{ .mode = .read_only }) catch |err| {
            log.err("Could not open ignore file [{s}], error `{s}`", .{ subPath, @errorName(err) });
            return err;
        };
        defer file.close();

        const src = try readFile(rootMap.allocator, file);

        parseIgnore(src, &ignore) catch |err| {
            log.err("Could not parse ignore file [{s}], error `{s}`", .{ subPath, @errorName(err) });
            return err;
        };
    }

    var parent2 = parentEntry;

    const traversal =
        if (try getRoot(rootMap.allocator, dir)) |subPath| traversal: {
            if (t == .base) {
                const filePath = try relativizePath(rootPath, dir.realpathAlloc(rootMap.allocator, subPath) catch |err| {
                    log.err("Could not get the real path of file [{s}], error `{s}`", .{ subPath, @errorName(err) });
                    return err;
                });

                const file = dir.openFile(subPath, .{ .mode = .read_only }) catch |err| {
                    log.warn("Skipping file [{s}], it could not be opened because of error `{s}`", .{ subPath, @errorName(err) });
                    return;
                };
                defer file.close();

                const name = try getName(rootMap.allocator, super, subPath);

                const src = try readFile(rootMap.allocator, file);
                const hasTests = detectTests(src);
                const hasHeaderGenData = detectHeaderGenData(src);

                var deps = std.StringArrayHashMap(void).init(rootMap.allocator);
                try getDependencies(src, &deps);

                const entry = try rootMap.allocator.create(Entry);
                entry.* = .{
                    .parent = if (parentEntry) |p| p.name else null,
                    .vis = t.base.vis,
                    .name = name,
                    .templateData = null,
                    .kind = t.base.kind,
                    .hasTests = hasTests,
                    .hasHeaderGenData = hasHeaderGenData,
                    .path = filePath,
                    .files = std.ArrayList([]const u8).init(rootMap.allocator),
                    .dependencies = deps,
                };

                if (isTemplatePath(subPath)) {
                    entry.templateData = Templater.queryData(rootMap.allocator, filePath) catch |err| {
                        log.err("Could not get template data for file [{s}], error `{s}`", .{ filePath, @errorName(err) });
                        return err;
                    };
                }

                try insert(rootMap, parentEntry, entry);

                parent2 = entry;

                break :traversal Traversal { .specific = entry };
            } else {
                log.err("Unexpected root file [{s}], it is not properly categorized", .{subPath});
                return error.UnexpectedRootFile;
            }
        } else t;

    var iter = IgnoringIter.init(dir, &ignore);

    return iterate(rootMap, rootPath, parent2, dir, super, traversal, &iter);
}

fn iterate(rootMap: *Map, rootPath: []const u8, parentEntry: ?*Entry, dir: std.fs.Dir, super: []const u8, t: Traversal, iter: anytype) anyerror!void {
    while (try iter.next()) |it| {
        switch (it.kind) {
            .directory => {
                var subDir = dir.openDir(it.name, .{ .iterate = true }) catch |err| {
                    log.warn("Skipping directory [{s}], it could not be opened because of error `{s}`", .{ it.name, @errorName(err) });
                    continue;
                };
                defer subDir.close();

                const strippedName = getStripped(it.name);

                const vis = getVis(it.name) orelse t.getVis();
                const kind = getKind(it.name) orelse t.getKind();

                const super2 =
                    if (super.len > 0 and strippedName.len > 0 and !isLowerStr(strippedName))
                        try std.fmt.allocPrint(rootMap.allocator, "{s}:{s}", .{super, strippedName})
                    else if (strippedName.len > 0 and !isLowerStr(strippedName)) strippedName
                    else super;

                const traversal = (if (kind) |k| t.withKind(k) else t).withVis(vis);

                try traverse(rootMap, rootPath, parentEntry, subDir, super2, traversal);
            },
            .file => {
                if (!hasUsableFileExtension(it.name)) continue;

                const file = dir.openFile(it.name, .{ .mode = .read_only }) catch |err| {
                    log.err("File [{s}] could not be opened, error {s}", .{ it.name, @errorName(err) });
                    return err;
                };
                defer file.close();

                const filePath = try relativizePath(rootPath, dir.realpathAlloc(rootMap.allocator, it.name) catch |err| {
                    log.err("Could not get the real path of file [{s}], error {s}", .{ it.name, @errorName(err) });
                    return err;
                });

                const overrideVis = getVis(it.name);
                const overrideKind = getFileKind(it.name);

                switch (t) {
                    .root => {
                        if (overrideKind) |ovK| {
                            try processBaseFile(rootMap, parentEntry, super, overrideVis orelse EntryVis.DEFAULT, ovK, it.name, filePath, file);
                        } else {
                            log.warn("Skipping file [{s}], it is not properly categorized", .{it.name});
                            continue;
                        }
                    },
                    .base => |base| {
                        try processBaseFile(rootMap, parentEntry, super, overrideVis orelse base.vis, overrideKind orelse base.kind, it.name, filePath, file);
                    },
                    .specific => |entry| {
                        if (overrideVis != null or overrideKind != null) {
                            log.err("File-specific visibility/kind override found in file [{s}], this is not supported in directories with a root file", .{it.name});
                            return error.UnexpectedOverride;
                        }

                        try entry.files.append(filePath);

                        const src = try readFile(rootMap.allocator, file);
                        const hasTests = detectTests(src);
                        const hasHeaderGenData = detectHeaderGenData(src);

                        if (!entry.hasTests and hasTests) {
                            log.warn("Entry [{s}] has tests in file [{s}] (and possibly others), but its root does not", .{entry.name, filePath});
                        }

                        if (!entry.hasHeaderGenData and hasHeaderGenData) {
                            log.warn("Entry [{s}] has header generation data in file [{s}] (and possibly others), but its root does not", .{entry.name, filePath});
                        }

                        entry.hasTests = entry.hasTests or hasTests;
                        entry.hasHeaderGenData = entry.hasHeaderGenData or hasHeaderGenData;

                        try getDependencies(src, &entry.dependencies);
                    },
                }
            },
            else => {
                log.warn("Skipping entry [{s}], it is not a file or directory", .{it.name});
                continue;
            },
        }
    }
}

fn processBaseFile(rootMap: *Map, parentEntry: ?*Entry, super: []const u8, vis: EntryVis, kind: EntryKind, fileName: []const u8, filePath: []const u8, file: std.fs.File) !void {
    const src = try readFile(rootMap.allocator, file);

    const name = try getName(rootMap.allocator, super, fileName);

    const hasTests = detectTests(src);
    const hasHeaderGenData = detectHeaderGenData(src);

    var deps = std.StringArrayHashMap(void).init(rootMap.allocator);
    try getDependencies(src, &deps);

    const entry = try rootMap.allocator.create(Entry);
    entry.* = .{
        .parent = if (parentEntry) |p| p.name else null,
        .vis = vis,
        .kind = kind,
        .templateData = null,
        .name = name,
        .path = filePath,
        .hasTests = hasTests,
        .hasHeaderGenData = hasHeaderGenData,
        .files = std.ArrayList([]const u8).init(rootMap.allocator),
        .dependencies = deps,
    };

    if (isTemplatePath(fileName)) {
        entry.templateData = Templater.queryData(rootMap.allocator, filePath) catch |err| {
            log.err("Could not get template parameters for file [{s}], error {s}", .{ filePath, @errorName(err) });
            return err;
        };
    }

    try insert(rootMap, parentEntry, entry);
}

fn insert(rootMap: *Map, parentEntry: ?*Entry, entry: *Entry) anyerror!void {
    if (rootMap.get(entry.name)) |old| {
        log.err("Duplicate entry [{s}]:\n{} vs\n {}", .{ entry.name, old, entry });
        return error.DuplicateEntry;
    }

    try rootMap.put(entry.name, entry);

    if (parentEntry) |p| {
        try p.dependencies.put(entry.name, {});
    }
}

fn isTemplatePath(path: []const u8) bool {
    return std.mem.indexOf(u8, path, ".template") != null;
}

fn relativizePath(rootPath: []const u8, childPath: []const u8) ![]const u8 {
    std.debug.assert(std.mem.startsWith(u8, childPath, rootPath));
    return childPath[rootPath.len..];
}

fn getStem(path: []const u8) []const u8 {
    var stem = std.fs.path.stem(path);

    if (std.mem.indexOfScalar(u8, stem, '.')) |i| {
        stem = stem[0..i];
    }

    return stem;
}

fn getName(allocator: std.mem.Allocator, super: []const u8, path: []const u8) ![]const u8 {
    const stem = getStripped(getStem(path));

    const out = try
        if (stem.len == 0 or std.mem.eql(u8, stem, "root")) allocator.dupe(u8, super)
        else
            if (super.len > 0) std.fmt.allocPrint(allocator, "{s}:{s}", .{ super, stem })
            else allocator.dupe(u8, stem);

    return out;
}

fn readFile(allocator: std.mem.Allocator, file: std.fs.File) ![]const u8 {
    return try file.reader().readAllAlloc(allocator, std.math.maxInt(usize));
}

fn detectHeaderGenData(src: []const u8) bool {
    return std.mem.indexOf(u8, src, HEADER_GEN_QUERY) != null;
}

fn detectTests(src: []const u8) bool {
    var i: usize = 0;
    while(i < src.len) {
        var subStr = src[i..];
        if (std.mem.indexOf(u8, subStr, BEGIN_TEST)) |j| {
            if (j == 0 or std.ascii.isWhitespace(subStr[j - 1])) {
                subStr = subStr[j + BEGIN_TEST.len..];

                if (std.ascii.isWhitespace(subStr[0])) {
                    subStr = subStr[1..];

                    if (subStr[0] == '"' or subStr[0] == '{') {
                        return true;
                    }
                }
            }

            i += j + BEGIN_TEST.len;
        } else break;
    }

    return false;
}

fn getDependencies(src: []const u8, deps: *std.StringArrayHashMap(void)) anyerror!void {
    var i: usize = 0;
    while (i < src.len) {
        var subStr = src[i..];

        if (std.mem.indexOf(u8, subStr, BEGIN_IMPORT)) |j| {
            const lineStart = getLineStart(src, i + j);

            var shouldIgnore = ignoreLine(lineStart);

            subStr = subStr[j + BEGIN_IMPORT.len ..];

            const k = std.mem.indexOf(u8, subStr, END_IMPORT) orelse {
                return error.UnparseableImport;
            };

            subStr = subStr[0..k];

            shouldIgnore = ignoreImport(subStr) or shouldIgnore;

            if (!shouldIgnore) {
                const dep = try deps.allocator.dupe(u8, subStr);
                try deps.put(dep, {});
            }

            i += BEGIN_IMPORT.len + j + k + END_IMPORT.len;
        } else break;
    }
}

fn ignoreLine(line: []const u8) bool {
    return std.mem.indexOf(u8, line, "//") != null   // ignore comment
        or std.mem.indexOf(u8, line, "\\\\") != null // ignore multiline string
        or std.mem.startsWith(u8, line, "#!");       // ignore shebang
}

fn getLineStart(src: []const u8, i: usize) []const u8 {
    var j = i;
    while (j > 0 and src[j - 1] != '\n') {
        j -= 1;
    }

    return src[j..i];
}

fn parseIgnore(src: []const u8, ignore: *std.StringHashMap(void)) anyerror!void {
    var iter = std.mem.tokenizeAny(u8, src, " \t\n\r");

    while (iter.next()) |path| {
        try ignore.put(path, {});
    }
}

fn getStripped(name: []const u8) []const u8 {
    var stripped = name;

    if (getVis(name)) |v| {
        switch (v) {
            .public => stripped = stripped[EntryVis.PREFIX.public.len..],
            .private => stripped = stripped[EntryVis.PREFIX.private.len..],
        }

        if (std.mem.startsWith(u8, stripped, "-")) {
            stripped = stripped[1..];
        }
    }

    if (getKind(name)) |k| {
        switch (k) {
            .binary => stripped = stripped[0..stripped.len - EntryKind.SUFFIX.binary.len],
            .library => stripped = stripped[0..stripped.len - EntryKind.SUFFIX.library.len],
            .module => stripped = stripped[0..stripped.len - EntryKind.SUFFIX.module.len],
            .document => stripped = stripped[0..stripped.len - EntryKind.SUFFIX.document.len],
        }

        if (std.mem.endsWith(u8, stripped, "-")) {
            stripped = stripped[0..stripped.len - 1];
        }
    }

    return stripped;
}

fn getVis(name: []const u8) ?EntryVis {
    if (std.mem.startsWith(u8, name, EntryVis.PREFIX.public)) {
        return EntryVis.public;
    } else if (std.mem.startsWith(u8, name, EntryVis.PREFIX.private)) {
        return EntryVis.private;
    } else {
        return null;
    }
}

fn getKind(name: []const u8) ?EntryKind {
    if (std.mem.endsWith(u8, name, EntryKind.SUFFIX.binary)) {
        return EntryKind.binary;
    } else if (std.mem.endsWith(u8, name, EntryKind.SUFFIX.library)) {
        return EntryKind.library;
    } else if (std.mem.endsWith(u8, name, EntryKind.SUFFIX.module)) {
        return EntryKind.module;
    } else if (std.mem.endsWith(u8, name, EntryKind.SUFFIX.document)) {
        return EntryKind.document;
    } else {
        return null;
    }
}

fn getFileKind(name: []const u8) ?EntryKind {
    if (std.mem.endsWith(u8, name, ".md")) {
        return EntryKind.document;
    } else {
        return getKind(getStem(name));
    }
}

fn getIgnore(allocator: std.mem.Allocator, dir: std.fs.Dir) anyerror!?[]const u8 {
    var iter = dir.iterate();

    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;

        if (isIgnore(entry.name)) {
            return try allocator.dupe(u8, entry.name);
        }
    }

    return null;
}

fn getRoot(allocator: std.mem.Allocator, dir: std.fs.Dir) anyerror!?[]const u8 {
    var iter = dir.iterate();

    var out: ?[]const u8 = null;

    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;

        if (isRoot(entry.name)) {
            if (out) |_| {
                return error.MultipleRootFiles;
            } else {
                out = try allocator.dupe(u8, entry.name);
            }
        }
    }

    return out;
}

fn isIgnore(path: []const u8) bool {
    return std.mem.eql(u8, path, BUILD_IGNORE_FILE_NAME);
}

fn isDotFile(path: []const u8) bool {
    return std.mem.startsWith(u8, path, ".");
}

fn isRoot(path: []const u8) bool {
    return std.mem.startsWith(u8, path, "root")
       and hasUsableFileExtension(path)
         ;
}

fn hasUsableFileExtension(path: []const u8) bool {
    return std.mem.endsWith(u8, path, ".zig")
        or std.mem.endsWith(u8, path, ".md")
         ;
}

fn ignoreImport(path: []const u8) bool {
    return isStandardImport(path)
        or isLocalFile(path)
        ;
}

fn isLowerStr(str: []const u8) bool {
    for (str) |ch| {
        if (!std.ascii.isLower(ch)) {
            return false;
        }
    }

    return true;
}

fn isStandardImport(path: []const u8) bool {
    for (STD_NAMES) |name| {
        if (std.mem.eql(u8, path, name)) {
            return true;
        }
    }

    return false;
}

fn isLocalFile(path: []const u8) bool {
    return std.mem.endsWith(u8, path, ".zig");
}

fn indentStr(str: *std.ArrayList(u8), indent: []const u8) !void {
    var i: usize = indent.len;

    try str.insertSlice(0, indent);

    while (i < str.items.len) {
        const byteLen = try std.unicode.utf8ByteSequenceLength(str.items[i]);
        const ch = try std.unicode.utf8Decode(str.items[i..i + byteLen]);

        i += byteLen;

        if (i == str.items.len) break;

        if (ch == '\n') {
            try str.insertSlice(i, indent);
            i += indent.len;
        }
    }
}
