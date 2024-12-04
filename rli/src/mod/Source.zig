const std = @import("std");

const Extern = @import("Utils").Extern;

const MiscUtils = @import("Utils").Misc;
const Ordering = MiscUtils.Ordering;

const Rli = @import("root.zig");
const Context = Rli.Context;

pub const Range = struct {
    start: ?Pos,
    end: ?Pos,

    const Self = @This();

    pub fn init(start: ?Pos, end: ?Pos) Self {
        return Self{ .start = start, .end = end };
    }

    pub fn add(a: Self, b: Self) Self {
        return Self{
            .start = if (a.start) |as| (if (b.start) |bs| as.min(bs) else as) else null,
            .end = if (a.end) |ae| (if (b.end) |be| ae.max(be) else ae) else null,
        };
    }

    pub fn addPos(self: Self, pos: Pos) Self {
        return Self{
            .start = if (self.start) |s| s.add(pos) else null,
            .end = if (self.end) |e| e.add(pos) else null,
        };
    }

    pub fn compare(self: Self, other: Self) Ordering {
        var res = MiscUtils.compare(self.start, other.start);

        if (res == Ordering.Equal) {
            res = MiscUtils.compare(self.end, other.end);
        }

        return res;
    }

    pub fn format(self: Self, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
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

    pub fn hashWith(self: Self, hasher: anytype) void {
        MiscUtils.hashWith(hasher, self.start);
        MiscUtils.hashWith(hasher, self.end);
    }

    pub fn default() Self {
        return Self{ .start = null, .end = null };
    }
};

pub const Pos = extern struct {
    line: u32,
    column: u32,
    offset: u32,

    const Self = @This();

    pub fn init(line: u32, column: u32, offset: u32) Self {
        return Self{ .line = line, .column = column, .offset = offset };
    }

    pub fn add(a: Self, b: Self) Self {
        return Self{
            .line = a.line + b.line,
            .column = a.column + b.column,
            .offset = a.offset + b.offset,
        };
    }

    pub fn min(a: Self, b: Self) Self {
        return Self{
            .line = @min(a.line, b.line),
            .column = @min(a.column, b.column),
            .offset = @min(a.offset, b.offset),
        };
    }

    pub fn max(a: Self, b: Self) Self {
        return Self{
            .line = @max(a.line, b.line),
            .column = @max(a.column, b.column),
            .offset = @max(a.offset, b.offset),
        };
    }

    pub fn compare(self: Self, other: Self) Ordering {
        var res = MiscUtils.compare(self.line, other.line);

        if (res == Ordering.Equal) {
            res = MiscUtils.compare(self.column, other.column);

            if (res == Ordering.Equal) {
                res = MiscUtils.compare(self.offset, other.offset);
            }
        }

        return res;
    }

    pub fn format(self: Self, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (comptime std.mem.eql(u8, fmt, "offset")) {
            try writer.print("{d}:{d}:{d}", .{ self.line, self.column, self.offset });
        } else {
            try writer.print("{d}:{d}", .{ self.line, self.column });
        }
    }

    pub fn hashWith(self: Self, hasher: anytype) void {
        MiscUtils.hashWith(hasher, self.line);
        MiscUtils.hashWith(hasher, self.column);
        MiscUtils.hashWith(hasher, self.offset);
    }

    pub fn default() Self {
        return Self{ .line = 0, .column = 0, .offset = 0 };
    }
};

pub const Comment = struct {
    kind: Kind = .plain,
    text: []const u8,

    pub const Kind = enum {
        plain,
        documentation,
    };
};

pub const Attr = struct {
    context: *Context,
    filename: []const u8,
    range: ?Range,
    comments: []const Comment,

    const Self = @This();

    pub fn clone(self: *Self) !*Self {
        return try self.context.bindAttrExistingFile(self.filename, self.range, &.{});
    }

    pub fn compare(self: Self, other: Self) Ordering {
        var res = MiscUtils.compare(self.context, other.context);

        if (res == Ordering.Equal) {
            res = MiscUtils.compare(self.filename, other.filename);
        }

        if (res == Ordering.Equal) {
            res = MiscUtils.compare(self.range, other.range);
        }

        return res;
    }

    pub fn format(self: *const Self, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("[{s}", .{self.filename});

        if (self.range) |range| {
            try writer.print(":{" ++ fmt ++ "}", .{range});
        }

        try writer.print("]", .{});
    }

    pub fn hashWith(self: Self, hasher: anytype) void {
        MiscUtils.hashWith(hasher, self.filename);
        MiscUtils.hashWith(hasher, self.range);
    }
};
