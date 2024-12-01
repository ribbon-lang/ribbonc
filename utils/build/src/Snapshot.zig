const std = @import("std");
const Build = std.Build;
const Char = u21;

const log = std.log.scoped(.@"snapshot-map");

pub const Snapshot = union(enum) {
    Text: StrPair,
    Files: StrPair,
    Lazy: LazyPair,

    pub fn toText(self: Snapshot, allocator: std.mem.Allocator) !StrPair {
        switch (self) {
            Snapshot.Text => |x| return x,
            Snapshot.Files => |x| return .{
                .out = try std.fs.cwd().readFileAlloc(allocator, x.out, std.math.maxInt(usize)),
                .err = try std.fs.cwd().readFileAlloc(allocator, x.err, std.math.maxInt(usize)),
            },
            Snapshot.Lazy => |x| return .{
                .out = try std.fs.cwd().readFileAlloc(allocator, try std.fmt.allocPrint(allocator, "{}", .{x.out}), std.math.maxInt(usize)),
                .err = try std.fs.cwd().readFileAlloc(allocator, try std.fmt.allocPrint(allocator, "{}", .{x.err}), std.math.maxInt(usize)),
            },
        }
    }

    pub const NameIterator = struct {
        inner: std.StringHashMap(Snapshot).KeyIterator,

        pub fn next(self: *NameIterator) ?[]const u8 {
            return if (self.inner.next()) |n| n.* else null;
        }
    };

    pub const StrPair = struct {
        out: []const u8,
        err: []const u8,
    };

    pub const LazyPair = struct {
        out: Build.LazyPath,
        err: Build.LazyPath,
    };


    pub const Map = struct {
        inner: std.StringHashMap(Snapshot),

        pub fn init(allocator: std.mem.Allocator) !Map {
            return .{ .inner = std.StringHashMap(Snapshot).init(allocator) };
        }

        pub fn contains(map: *const Map, name: []const u8) bool {
            return map.inner.contains(name);
        }

        pub fn put(map: *Map, name: []const u8, snapshot: Snapshot) !void {
            try map.inner.put(name, snapshot);
        }

        pub fn get(map: *const Map, name: []const u8) ?Snapshot {
            return map.inner.get(name);
        }

        pub fn nameIterator(map: *const Map) NameIterator {
            return .{ .inner = map.inner.keyIterator() };
        }

        pub fn readMap(allocator: std.mem.Allocator, filepath: []const u8) !?Map {
            const source = std.fs.cwd().readFileAlloc(allocator, filepath, std.math.maxInt(usize)) catch return null;

            var parser = Parser.init(source);

            var map = std.StringHashMap(Snapshot).init(allocator);

            while (parser.isValid()) {
                try parser.expect("## ");
                const name = try parser.takeUntil('\n');

                try parser.expect("out[");
                const outLen = try parser.number();
                try parser.expect("]:\n```\n");
                const out = try parser.take(outLen);
                try parser.expect("\n```\n");

                try parser.expect("err[");
                const errLen = try parser.number();
                try parser.expect("]:\n```\n");
                const err = try parser.take(errLen);
                try parser.expect("\n```\n\n");

                try map.put(name, Snapshot{ .Text = .{ .out = out, .err = err } });
            }

            if (!parser.isDone()) {
                return error.UnexpectedInput;
            }

            return .{ .inner = map };
        }

        pub fn writeMap(map: *const Map, writer: anytype) !void {
            var iter = map.nameIterator();

            var names = std.ArrayList([]const u8).init(map.inner.allocator);
            defer names.deinit();

            while (iter.next()) |name| {
                try names.append(name);
            }

            std.mem.sort([]const u8, names.items, @as(void, {}), struct {
                fn fun(_: void, a: []const u8, b: []const u8) bool {
                    return std.mem.lessThan(u8, a, b);
                }
            }.fun);

            for (names.items) |name| {
                const snapshot = try map.inner.get(name).?.toText(map.inner.allocator);

                try writer.print(
                    \\## {s}
                    \\out[{}]:
                    \\```
                    \\{s}
                    \\```
                    \\err[{}]:
                    \\```
                    \\{s}
                    \\```
                    \\
                    \\
                , .{
                    name,
                    snapshot.out.len,
                    snapshot.out,
                    snapshot.err.len,
                    snapshot.err,
                });
            }
        }
    };

    pub const Parser = struct {
        source: []const u8,
        pos: usize,

        pub fn init(source: []const u8) Parser {
            return Parser{
                .source = source,
                .pos = 0,
            };
        }

        pub fn expect(self: *Parser, expected: []const u8) !void {
            if (self.pos + expected.len > self.source.len) {
                return error.UnexpectedEof;
            }

            if (!std.mem.startsWith(u8, self.source[self.pos..], expected)) {
                log.err("expected `{s}`, got `{s}`", .{ expected, self.source[self.pos..@min(self.source.len, self.pos + expected.len)] });
                log.err("ie: {any} vs {any}", .{ expected, self.source[self.pos..@min(self.source.len, self.pos + expected.len)] });
                return error.InvalidToken;
            }

            self.pos += expected.len;
        }

        pub fn take(self: *Parser, len: usize) ![]const u8 {
            if (self.pos + len > self.source.len) {
                return error.UnexpectedEof;
            }

            const res = self.source[self.pos..(self.pos + len)];
            self.pos += len;

            return res;
        }

        pub fn takeUntil(self: *Parser, delim: Char) ![]const u8 {
            const start = self.pos;
            while (self.isValid()) {
                const res = try decode1(self.source[self.pos..]);

                if (res.ch == delim) {
                    const out = self.source[start..self.pos];

                    self.pos += res.len;

                    return out;
                } else {
                    self.pos += res.len;
                }
            }
            return error.UnexpectedEof;
        }

        pub fn nextIf(self: *Parser, comptime predicate: fn (Char) bool) !bool {
            if (!self.isValid()) return false;

            const res = try decode1(self.source[self.pos..]);

            if (!predicate(res.ch)) {
                return false;
            }

            self.pos += res.len;

            return true;
        }

        pub fn nextWhile(self: *Parser, comptime predicate: fn (Char) bool) ![]const u8 {
            const start = self.pos;
            while (try self.nextIf(predicate)) {}

            return self.source[start..self.pos];
        }

        pub fn number(self: *Parser) !usize {
            const digits = try self.nextWhile(isDecimal);

            return try std.fmt.parseInt(usize, digits, 10);
        }

        pub fn isValid(self: *Parser) bool {
            return self.pos < self.source.len;
        }

        pub fn isDone(self: *Parser) bool {
            return self.pos == self.source.len;
        }

        fn decode1(str: []const u8) !struct { ch: Char, len: u3 } {
            const len = try std.unicode.utf8ByteSequenceLength(str[0]);
            const ch = try std.unicode.utf8Decode(str[0..len]);
            return .{ .ch = ch, .len = len };
        }

        fn isDecimal(ch: Char) bool {
            return ch >= '0' and ch <= '9';
        }
    };

    pub const Helper = struct {
        owner: *Build,
        map: Map,
        run: *Build.Step.Run,
        write: *Build.Step.UpdateSourceFiles,

        pub fn get(self: *const Helper, name: []const u8) ?Snapshot {
            return self.map.get(name);
        }

        pub fn put(self: *Helper, name: []const u8, pair: LazyPair) !void {
            try self.map.put(name, .{ .Lazy = pair });
        }

        pub fn runWith(self: *const Helper, step: *Build.Step) void {
            step.dependOn(&self.write.step);
        }

        pub fn finalize(self: *const Helper) void {
            var names = self.map.nameIterator();

            while (names.next()) |name| {
                switch (self.map.get(name).?) {
                    .Text => |x| {
                        self.run.addArg("-t");
                        self.run.addArg(name);
                        self.run.addArg(x.out);
                        self.run.addArg(x.err);
                    },
                    .Files => |x| {
                        self.run.addArg("-f");
                        self.run.addArg(name);
                        self.run.addFileArg(self.owner.path(x.out));
                        self.run.addFileArg(self.owner.path(x.err));
                    },
                    .Lazy => |x| {
                        self.run.addArg("-f");
                        self.run.addArg(name);
                        self.run.addFileArg(x.out);
                        self.run.addFileArg(x.err);
                    },
                }
            }
        }
    };
};
