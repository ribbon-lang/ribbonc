const std = @import("std");
const TypeUtils = @import("Utils").Type;

const Rml = @import("root.zig");
const OOM = Rml.OOM;
const Ordering = Rml.Ordering;


pub const Origin = struct {
    filename: []const u8,
    range: ?Range = null,

    pub fn fromStr(rml: *Rml, str: []const u8) OOM! Origin {
        return Origin { .filename = try rml.storage.interner.get(str) };
    }

    pub fn fromComptimeStr(comptime str: []const u8) Origin {
        return Origin { .filename = str };
    }

    pub fn fromComptimeFmt(comptime fmt: []const u8, comptime args: anytype) Origin {
        return Origin { .filename = comptime std.fmt.comptimePrint(fmt, args) };
    }

    pub fn compare(self: Origin, other: Origin) Ordering {
        var res = Rml.compare(self.filename, other.filename);

        if (res == Ordering.Equal) {
            res = Rml.compare(self.range, other.range);
        }

        return res;
    }

    pub fn format(self: *const Origin, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("[{s}", .{self.filename});

        if (self.range) |range| {
            try writer.print(":{" ++ fmt ++ "}", .{range});
        }

        try writer.print("]", .{});
    }

    pub fn hashWith(self: Origin, hasher: anytype) void {
        Rml.hashWith(hasher, self.filename);
        Rml.hashWith(hasher, self.range);
    }
};

pub const Range = struct {
    start: ?Pos = null,
    end: ?Pos = null,

    pub fn init(start: ?Pos, end: ?Pos) Range {
        return Range{ .start = start, .end = end };
    }

    pub fn add(a: Range, b: Range) Range {
        return Range{
            .start = if (a.start) |as| (if (b.start) |bs| as.min(bs) else as) else null,
            .end = if (a.end) |ae| (if (b.end) |be| ae.max(be) else ae) else null,
        };
    }

    pub fn addPos(self: Range, pos: Pos) Range {
        return Range{
            .start = if (self.start) |s| s.add(pos) else null,
            .end = if (self.end) |e| e.add(pos) else null,
        };
    }

    pub fn compare(self: Range, other: Range) Ordering {
        var res = Rml.compare(self.start, other.start);

        if (res == Ordering.Equal) {
            res = Rml.compare(self.end, other.end);
        }

        return res;
    }

    pub fn format(self: Range, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const g = "{" ++ fmt ++ "}";

        if (self.start) |start| {
            try writer.print(g, .{start});
            if (self.end) |end| {
                if (start.line == end.line and !std.mem.eql(u8, fmt, "offset")) {
                    try writer.print("-{}", .{end.column});
                } else {
                    try writer.print(" to " ++ g, .{end});
                }
            }
        } else if (self.end) |end| {
            try writer.print("?-" ++ g, .{end});
        } else {
            try writer.print("?-?", .{});
        }
    }

    pub fn hashWith(self: Range, hasher: anytype) void {
        Rml.hashWith(hasher, self.start);
        Rml.hashWith(hasher, self.end);
    }
};

pub const Pos = struct {
    line: u32 = 0,
    column: u32 = 0,
    offset: u32 = 0,

    pub fn init(line: u32, column: u32, offset: u32) Pos {
        return Pos{ .line = line, .column = column, .offset = offset };
    }

    pub fn add(a: Pos, b: Pos) Pos {
        return Pos{
            .line = a.line + b.line,
            .column = a.column + b.column,
            .offset = a.offset + b.offset,
        };
    }

    pub fn min(a: Pos, b: Pos) Pos {
        return Pos{
            .line = @min(a.line, b.line),
            .column = @min(a.column, b.column),
            .offset = @min(a.offset, b.offset),
        };
    }

    pub fn max(a: Pos, b: Pos) Pos {
        return Pos{
            .line = @max(a.line, b.line),
            .column = @max(a.column, b.column),
            .offset = @max(a.offset, b.offset),
        };
    }

    pub fn compare(self: Pos, other: Pos) Ordering {
        var res = Rml.compare(self.line, other.line);

        if (res == Ordering.Equal) {
            res = Rml.compare(self.column, other.column);

            if (res == Ordering.Equal) {
                res = Rml.compare(self.offset, other.offset);
            }
        }

        return res;
    }

    pub fn format(self: Pos, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (comptime std.mem.eql(u8, fmt, "offset")) {
            try writer.print("{d}:{d}:{d}", .{ self.line, self.column, self.offset });
        } else {
            try writer.print("{d}:{d}", .{ self.line, self.column });
        }
    }

    pub fn hashWith(self: Pos, hasher: anytype) void {
        Rml.hashWith(hasher, self.line);
        Rml.hashWith(hasher, self.column);
        Rml.hashWith(hasher, self.offset);
    }
};
