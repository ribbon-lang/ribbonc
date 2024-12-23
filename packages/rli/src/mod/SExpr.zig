const std = @import("std");

const Extern = @import("Utils").Extern;

const TextUtils = @import("Utils").Text;

const MiscUtils = @import("Utils").Misc;
const Unit = MiscUtils.Unit;
const Ordering = MiscUtils.Ordering;

const Rli = @import("root.zig");
const Context = Rli.Context;
const Source = Rli.Source;
const Interpreter = Rli.Interpreter;

pub const SExpr = extern struct {
    head: *Head.Unknown,
    data: Data,

    pub const Err = error{};

    pub fn HashMapOf(comptime T: type) type {
        return std.ArrayHashMap(SExpr, T, HashContext, true);
    }

    pub const HashMap = HashMapOf(SExpr);
    pub const HashSet = HashMapOf(void);


    pub const Iterator = struct {
        list: SExpr,

        pub fn next(self: *Iterator) !?SExpr {
            if (self.list.isNil()) {
                return null;
            }

            const xp = self.list.castCons() orelse return error.TypeError;
            const car = xp.car;
            self.list = xp.cdr;

            return car;
        }

        pub fn peek(self: *Iterator) ?SExpr {
            if (self.list.isNil()) {
                return null;
            }

            const xp = self.list.castCons() orelse return null;
            return xp.car;
        }

        pub fn isDone(self: *Iterator) bool {
            return self.list.isNil();
        }
    };

    pub fn iter(self: SExpr) Iterator {
        return Iterator{ .list = self };
    }

    pub const HashContext = struct {
        pub fn hash(_: @This(), key: SExpr) u32 {
            var hasher = Extern.Hasher.initFnv1a32();
            MiscUtils.hashWith(&hasher, key);
            return hasher.final();
        }

        pub fn eql(_: @This(), a: SExpr, b: SExpr, _: usize) bool {
            return MiscUtils.equal(a, b);
        }
    };

    pub const Tag = enum(u8) {
        Nil,
        Bool,
        Int,
        Float,
        Char,
        Symbol,
        String,
        Cons,
        Function,
        Builtin,
        ExternData,
        ExternFunction,

        const Self = @This();

        pub fn isContextual(self: Self) bool {
            switch (self) {
                .Nil, .Bool, .Int, .Float, .Char => return false,
                .Symbol, .String, .Cons, .Function, .Builtin, .ExternData, .ExternFunction => return true,
            }
        }

        pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self) {
                .Nil => try writer.print("Nil", .{}),
                .Bool => try writer.print("Bool", .{}),
                .Int => try writer.print("Int", .{}),
                .Float => try writer.print("Float", .{}),
                .Char => try writer.print("Char", .{}),
                .Symbol => try writer.print("Symbol", .{}),
                .String => try writer.print("String", .{}),
                .Cons => try writer.print("Cons", .{}),
                .Function => try writer.print("Function", .{}),
                .Builtin => try writer.print("Builtin", .{}),
                .ExternData => try writer.print("ExternData", .{}),
                .ExternFunction => try writer.print("ExternFunction", .{}),
            }
        }

        pub fn hashWith(self: Self, hasher: anytype) void {
            return MiscUtils.hashWith(hasher, @intFromEnum(self));
        }

        pub fn toSlice(self: Self) []const u8 {
            switch (self) {
                .Nil => return "Nil",
                .Bool => return "Bool",
                .Int => return "Int",
                .Float => return "Float",
                .Char => return "Char",
                .Symbol => return "Symbol",
                .String => return "String",
                .Cons => return "Cons",
                .Function => return "Function",
                .Builtin => return "Builtin",
                .ExternData => return "ExternData",
                .ExternFunction => return "ExternFunction",
            }
        }

        pub fn fromType(comptime T: type) Tag {
            return switch (T) {
                Unit => Tag.Nil,
                bool => Tag.Bool,
                i64 => Tag.Int,
                f64 => Tag.Float,
                TextUtils.Char => Tag.Char,
                Types.Symbol => Tag.Symbol,
                Types.String => Tag.String,
                Types.Cons => Tag.Cons,
                Types.Function => Tag.Function,
                Types.Builtin => Tag.Builtin,
                Types.ExternData => Tag.ExternData,
                Types.ExternFunction => Tag.ExternFunction,

                else => @compileError("invalid SExpr object type"),
            };
        }
    };

    pub const Head = struct {
        pub const Unknown = opaque {};
        pub fn init(comptime T: type, object: *ObjType(T)) *Unknown {
            return @ptrFromInt(@intFromPtr(object) | (@as(u64, @intFromEnum(Tag.fromType(T))) << 48));
        }

        pub fn extractPtr(comptime T: type, head: *Unknown) *T {
            return @ptrFromInt(@intFromPtr(head) & 0x00_00_FF_FF_FF_FF_FF_FF);
        }

        pub fn extractTag(head: *Unknown) Tag {
            return @enumFromInt(@intFromPtr(head) >> 48);
        }

        fn ObjType(comptime T: type) type {
            return switch (T) {
                Unit,
                bool,
                i64,
                f64,
                TextUtils.Char,
                Types.Prim,
                => Types.Prim,

                Types.Symbol => Types.Symbol,
                Types.String => Types.String,
                Types.Cons => Types.Cons,
                Types.Function => Types.Function,
                Types.Builtin => Types.Builtin,
                Types.ExternData => Types.ExternData,
                Types.ExternFunction => Types.ExternFunction,

                else => @compileError("invalid SExpr object type"),
            };
        }
    };

    pub const Data = extern union {
        nil: Unit,
        boolean: bool,
        integral: i64,
        floating: f64,
        character: u32,
    };

    comptime {
        if (@sizeOf(Data) != 8) {
            @compileError("Data size is not 8 bytes");
        }
    }

    pub const Types = struct {
        pub const Symbol = extern struct {
            attr: *const Source.Attr,
            buffer: Extern.UStr,

            const Self = @This();

            pub fn compare(self: Self, other: Self) Ordering {
                return MiscUtils.compare(@intFromPtr(self.buffer.ptr), @intFromPtr(other.buffer.ptr));
            }

            pub fn format(self: *const Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                try writer.print("{s}", .{self.toSlice()});
            }

            pub fn toSlice(self: *const Self) []const u8 {
                return self.buffer.toNative();
            }

            pub fn hashWith(self: Self, hasher: anytype) void {
                return MiscUtils.hashWith(hasher, @intFromPtr(self.buffer.ptr));
            }
        };

        pub const String = extern struct {
            attr: *const Source.Attr,
            buffer: Extern.UStr,

            const Self = @This();

            pub fn compare(self: Self, other: Self) Ordering {
                return MiscUtils.compare(self.toSlice(), other.toSlice());
            }

            pub fn format(self: *const Self, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                if (comptime std.mem.eql(u8, fmt, "display")) {
                    try writer.print("{s}", .{self.toSlice()});
                } else {
                    try writer.print("\"{}\"", .{TextUtils.EscapeFormatter.init(self.toSlice())});
                }
            }

            pub fn toSlice(self: *const Self) []const u8 {
                return self.buffer.toNative();
            }

            pub fn hashWith(self: Self, hasher: anytype) void {
                return MiscUtils.hashWith(hasher, self.toSlice());
            }
        };

        pub const Cons = extern struct {
            attr: *const Source.Attr,
            car: SExpr,
            cdr: SExpr,

            const Self = @This();

            pub fn compare(self: Self, other: Self) Ordering {
                var res = MiscUtils.compare(self.car, other.car);
                if (res == Ordering.Equal) {
                    res = MiscUtils.compare(self.cdr, other.cdr);
                }
                return res;
            }

            pub fn format(self: *const Self, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                const fmt2 = comptime fmt2: {
                    if (std.mem.eql(u8, fmt, "attr") or std.mem.eql(u8, fmt, "display")) {
                        break :fmt2 "{}";
                    } else {
                        break :fmt2 "{" ++ fmt ++ "}";
                    }
                };

                if (comptime std.mem.eql(u8, fmt, ".")) {
                    return writer.print("(" ++ fmt2 ++ " . " ++ fmt2 ++ ")", .{ self.car, self.cdr });
                } else if (self.car.castSymbol()) |sym| {
                    if (self.cdr.castCons()) |cons| {
                        if (std.mem.eql(u8, sym.toSlice(), "quote")) {
                            return writer.print("'" ++ fmt2, .{cons.car});
                        } else if (std.mem.eql(u8, sym.toSlice(), "quasiquote")) {
                            return writer.print("`" ++ fmt2, .{cons.car});
                        } else if (std.mem.eql(u8, sym.toSlice(), "unquote")) {
                            return writer.print("," ++ fmt2, .{cons.car});
                        } else if (std.mem.eql(u8, sym.toSlice(), "unquote-splicing")) {
                            return writer.print(",@" ++ fmt2, .{cons.car});
                        }
                    }
                }

                try writer.print("(" ++ fmt2, .{self.car});

                var current = self.cdr;
                while (current.castCons()) |cons| {
                    try writer.print(" " ++ fmt2, .{cons.car});
                    current = cons.cdr;
                } else {
                    if (!current.isNil()) {
                        try writer.print(" . " ++ fmt2, .{current});
                    }
                }

                try writer.print(")", .{});
            }

            pub fn isList(self: *const Self) bool {
                return self.cdr.isList();
            }

            pub fn listLen(self: *const Self) ?usize {
                if (self.cdr.listLen()) |cdrLen| {
                    return cdrLen + 1;
                } else {
                    return null;
                }
            }

            pub fn hashWith(self: Self, hasher: anytype) void {
                MiscUtils.hashWith(hasher, self.car);
                MiscUtils.hashWith(hasher, self.cdr);
            }
        };

        pub const Function = extern struct {
            attr: *const Source.Attr,
            kind: Kind,
            args: SExpr,
            env: SExpr,
            body: SExpr,
            id: u64,

            const Self = @This();

            pub const Kind = enum(u8) {
                Lambda,
                Macro,

                pub fn format(self: Kind, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                    switch (self) {
                        .Lambda => try writer.print("Lambda", .{}),
                        .Macro => try writer.print("Macro", .{}),
                    }
                }

                pub fn toSlice(self: Kind) []const u8 {
                    switch (self) {
                        .Lambda => return "Lambda",
                        .Macro => return "Macro",
                    }
                }
            };

            pub fn compare(self: Self, other: Self) Ordering {
                return MiscUtils.compare(self.id, other.id);
            }

            pub fn format(self: *const Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
                _ = fmt;
                _ = options;
                try writer.print("[{} {d}]", .{ self.kind, self.id });
            }

            pub fn hashWith(self: Self, hasher: anytype) void {
                MiscUtils.hashWith(hasher, self.id);
            }
        };

        pub const Builtin = extern struct {
            attr: *const Source.Attr,
            name: Extern.UStr,
            proc: u64,

            const Self = @This();

            pub const Proc = *const fn (*Interpreter, *const Source.Attr, SExpr) Interpreter.Result!SExpr;

            pub fn getProc(self: Self) Proc {
                return @ptrFromInt(self.proc);
            }

            pub fn compare(self: Self, other: Self) Ordering {
                return MiscUtils.compare(self.proc, other.proc);
            }

            pub fn format(self: *const Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                try writer.print("[Builtin {s}]", .{self.nameSlice()});
            }

            pub fn nameSlice(self: *const Self) []const u8 {
                return self.name.toNative();
            }

            pub fn hashWith(self: Self, hasher: anytype) void {
                MiscUtils.hashWith(hasher, self.proc);
            }
        };

        pub const ExternData = extern struct {
            attr: *const Source.Attr,
            ptr: *anyopaque,
            vtable: *const VTable(anyopaque),
            typeName: Extern.UStr,

            const Self = @This();

            pub fn VTable(comptime T: type) type {
                return extern struct {
                    compare: CompareProc = null,
                    format: FormatProc = null,
                    hashWith: HasherProc = null,
                    finalizer: FinalizerProc = null,

                    pub const CompareProc = ?*const fn (*const T, *const T) callconv(.C) Ordering;
                    pub const FormatProc = ?*const fn (*const T, Extern.Writer) callconv(.C) bool;
                    pub const HasherProc = ?*const fn (*const T, *Extern.Hasher) callconv(.C) void;
                    pub const FinalizerProc = ?*const fn (*T) callconv(.C) void;
                };
            }

            pub fn compare(self: Self, other: Self) Ordering {
                var res = MiscUtils.compare(@intFromPtr(self.ptr), @intFromPtr(other.ptr));

                if (res != Ordering.Equal) {
                    res = MiscUtils.compare(@intFromPtr(self.vtable), @intFromPtr(other.vtable));

                    if (res == Ordering.Equal) {
                        if (self.vtable.compare) |callback| {
                            res = callback(self.ptr, other.ptr);
                        }
                    }
                }

                return res;
            }

            pub fn format(self: *const Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                if (self.vtable.format) |callback| {
                    if (!callback(self.ptr, Extern.Writer.init(&writer))) return error.InvalidArgument;
                } else {
                    try writer.print("[{s}]", .{self.typeNameSlice()});
                }
            }

            pub fn hashWith(self: Self, hasher: *Extern.Hasher) void {
                MiscUtils.hashWith(hasher, @intFromPtr(self.vtable));

                if (self.vtable.hashWith) |callback| {
                    callback(self.ptr, hasher);
                } else {
                    MiscUtils.hashWith(hasher, @intFromPtr(self.ptr));
                }
            }

            pub fn finalizer(self: *const Self) void {
                if (self.vtable.finalizer) |callback| {
                    callback(self.ptr);
                }
            }

            pub fn typeNameSlice(self: *const Self) []const u8 {
                return self.typeName.toNative();
            }

            pub fn castPtr(self: *const Self, comptime T: type) ?*T {
                if (std.mem.eql(u8, self.typeNameSlice(), @typeName(T))) {
                    return @ptrCast(@alignCast(self.ptr));
                } else {
                    return null;
                }
            }
        };

        pub const ExternFunction = extern struct {
            attr: *const Source.Attr,
            name: Extern.UStr,
            proc: Proc,

            const Self = @This();

            pub const Proc = *const fn (*Interpreter, *const Source.Attr, *Interpreter.ExternMessage, *SExpr, SExpr) callconv(.C) bool;

            pub fn compare(self: Self, other: Self) Ordering {
                return MiscUtils.compare(@intFromPtr(self.proc), @intFromPtr(other.proc));
            }

            pub fn format(self: *const Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                try writer.print("[ExternFunction {s}]", .{self.nameSlice()});
            }

            pub fn hashWith(self: Self, hasher: anytype) void {
                MiscUtils.hashWith(hasher, @intFromPtr(self.proc));
            }

            pub fn nameSlice(self: Self) []const u8 {
                return self.name.toNative();
            }
        };

        pub const Prim = extern struct {
            attr: *const Source.Attr,

            const Self = @This();

            pub fn compare(self: Self, other: Self) Ordering {
                return MiscUtils.compare(self.attr, other.attr);
            }

            pub fn format(self: *const Self, comptime fmt: []const u8, opt: std.fmt.FormatOptions, writer: anytype) !void {
                try self.attr.format(fmt, opt, writer);
            }

            pub fn hashWith(self: Self, hasher: anytype) void {
                MiscUtils.hashWith(hasher, self.attr);
            }
        };
    };

    pub fn Nil(attr: *const Source.Attr) !SExpr {
        const prim = try attr.context.new(Types.Prim{ .attr = attr });

        return SExpr{
            .head = Head.init(Unit, prim),
            .data = .{ .nil = .{} },
        };
    }

    pub fn Bool(attr: *const Source.Attr, value: bool) !SExpr {
        const prim = try attr.context.new(Types.Prim{ .attr = attr });

        return SExpr{
            .head = Head.init(bool, prim),
            .data = .{ .boolean = value },
        };
    }

    pub fn Int(attr: *const Source.Attr, value: i64) !SExpr {
        const prim = try attr.context.new(Types.Prim{ .attr = attr });

        return SExpr{
            .head = Head.init(i64, prim),
            .data = .{ .integral = value },
        };
    }

    pub fn Float(attr: *const Source.Attr, value: f64) !SExpr {
        const prim = try attr.context.new(Types.Prim{ .attr = attr });

        return SExpr{
            .head = Head.init(f64, prim),
            .data = .{ .floating = value },
        };
    }

    pub fn Char(attr: *const Source.Attr, value: TextUtils.Char) !SExpr {
        if (!TextUtils.isValid(value)) return error.BadEncoding;

        const prim = try attr.context.new(Types.Prim{ .attr = attr });

        return SExpr{
            .head = Head.init(TextUtils.Char, prim),
            .data = .{ .character = value },
        };
    }

    pub fn Symbol(attr: *const Source.Attr, value: []const u8) !SExpr {
        if (!TextUtils.isValidStr(value)) return error.BadEncoding;
        return SymbolUnchecked(attr, value);
    }

    pub fn SymbolFmt(attr: *const Source.Attr, comptime fmt: []const u8, args: anytype) !SExpr {
        const len = std.fmt.count(fmt, args);
        if (len > Context.SYMBOL_MAX_LEN) return error.OutOfMemory;
        const tempBuf = [1]u8{0} ** Context.SYMBOL_MAX_LEN;
        const bytes = std.fmt.bufPrint(tempBuf, fmt, args) catch return error.OutOfMemory;
        return SymbolUnchecked(attr, bytes);
    }

    pub fn SymbolUnchecked(attr: *const Source.Attr, value: []const u8) !SExpr {
        const buf = try attr.context.bindSymbol(value);
        const symbol = try attr.context.new(Types.Symbol{ .attr = attr, .buffer = Extern.UStr.fromNative(buf) });

        return SExpr{
            .head = Head.init(Types.Symbol, symbol),
            .data = .{ .nil = .{} },
        };
    }

    pub fn GenSymbol(attr: *const Source.Attr) !SExpr {
        const buf = try attr.context.genSymbol();
        return SymbolUnchecked(attr, buf);
    }

    pub fn String(attr: *const Source.Attr, value: []const u8) !SExpr {
        if (!TextUtils.isValidStr(value)) return error.BadEncoding;
        return StringUnchecked(attr, value);
    }

    pub fn StringFmt(attr: *const Source.Attr, comptime fmt: []const u8, args: anytype) !SExpr {
        const len = std.fmt.count(fmt, args);
        const buf = try attr.context.newBuffer(u8, len);
        _ = std.fmt.bufPrint(buf, fmt, args) catch return error.OutOfMemory;
        return StringPreallocated(attr, buf);
    }

    pub fn StringUnchecked(attr: *const Source.Attr, value: []const u8) !SExpr {
        const buf = try attr.context.newBuffer(u8, value);
        return StringPreallocatedUnchecked(attr, buf);
    }

    pub fn StringPreallocated(attr: *const Source.Attr, buf: []const u8) !SExpr {
        if (!TextUtils.isValidStr(buf)) return error.BadEncoding;
        return StringPreallocatedUnchecked(attr, buf);
    }

    pub fn StringPreallocatedUnchecked(attr: *const Source.Attr, buf: []const u8) !SExpr {
        const string = try attr.context.new(Types.String{ .attr = attr, .buffer = Extern.UStr.fromNative(buf) });
        return SExpr{
            .head = Head.init(Types.String, string),
            .data = .{ .nil = .{} },
        };
    }

    pub fn ListTail(attr: *const Source.Attr, values: []const SExpr, tail: SExpr) !SExpr {
        var cdr = tail;

        if (values.len > 0) {
            var i = values.len - 1;

            while (true) {
                const val = values[i];

                cdr = try Cons(attr, val, cdr);

                if (i == 0) break;

                i -= 1;
            }
        }

        return cdr;
    }

    pub fn List(attr: *const Source.Attr, values: []const SExpr) !SExpr {
        return ListTail(attr, values, try Nil(attr));
    }


    pub fn Quote(sexpr: SExpr) !SExpr {
        const at = try sexpr.getAttr().clone();
        return try List(at, &.{ try Symbol(at, "quote"), sexpr });
    }

    pub fn ToQuote(sexpr: SExpr) !SExpr {
        const at = try sexpr.getAttr().clone();
        return try List(at, &.{ try Symbol(at, "to-quote"), sexpr });
    }


    pub fn Quasi(sexpr: SExpr) !SExpr {
        const at = try sexpr.getAttr().clone();
        return try List(at, &.{ try Symbol(at, "quasiquote"), sexpr });
    }


    pub fn Unquote(sexpr: SExpr) !SExpr {
        const at = try sexpr.getAttr().clone();
        return try List(at, &.{ try Symbol(at, "unquote"), sexpr });
    }


    pub fn UnquoteSplicing(sexpr: SExpr) !SExpr {
        const at = try sexpr.getAttr().clone();
        return try List(at, &.{ try Symbol(at, "unquote-splicing"), sexpr });
    }


    pub fn MappedList(at: *const Source.Attr, args: anytype, comptime callback: anytype) (error{OutOfMemory, TypeError} || @typeInfo(@typeInfo(@TypeOf(callback)).@"fn".return_type.?).error_union.error_set)!SExpr {
        switch (@TypeOf(args)) {
            SExpr => {
                const xp =
                    if (args.castCons()) |c| c
                    else if (args.isNil()) return args
                    else {
                        return error.TypeError;
                    };
                const newCar = try callback(xp.car);
                const newCdr = try MappedList(at, xp.cdr, callback);
                return try Cons(at, newCar, newCdr);
            },
            else => {
                const car = if (args.len > 0) try callback(args[0]) else {
                    return try Nil(at);
                };
                const cdr = try MappedList(at, args[1..], callback);
                return try Cons(at, car, cdr);
            }
        }
    }

    pub fn Cons(attr: *const Source.Attr, car: SExpr, cdr: SExpr) !SExpr {
        std.debug.assert(car.isInContext(attr.context));
        std.debug.assert(cdr.isInContext(attr.context));

        const cons = try attr.context.new(Types.Cons{ .attr = attr, .car = car, .cdr = cdr });

        return SExpr{
            .head = Head.init(Types.Cons, cons),
            .data = .{ .nil = .{} },
        };
    }

    pub fn fromExistingPtr(v: anytype) SExpr {
        return SExpr{
            .head = Head.init(@TypeOf(v), v),
            .data = .{ .nil = .{} },
        };
    }

    pub fn Function(attr: *const Source.Attr, kind: Types.Function.Kind, args: SExpr, env: SExpr, body: SExpr) !SExpr {
        std.debug.assert(args.isInContext(attr.context));
        std.debug.assert(env.isInContext(attr.context));
        std.debug.assert(body.isInContext(attr.context));

        const id = attr.context.genId();

        const function = try attr.context.new(Types.Function{ .attr = attr, .id = id, .kind = kind, .args = args, .env = env, .body = body });

        return SExpr{
            .head = Head.init(Types.Function, function),
            .data = .{ .nil = .{} },
        };
    }

    pub fn Builtin(attr: *const Source.Attr, name: []const u8, proc: Types.Builtin.Proc) !SExpr {
        const builtin = try attr.context.new(Types.Builtin{ .attr = attr, .name = Extern.UStr.fromNative(name), .proc = @intFromPtr(proc) });

        return SExpr{
            .head = Head.init(Types.Builtin, builtin),
            .data = .{ .nil = .{} },
        };
    }

    pub fn ExternData(comptime T: type, attr: *const Source.Attr, ptr: *T, vtable: *const Types.ExternData.VTable(T)) !SExpr {
        return ExternDataErased(attr, @ptrCast(ptr), @ptrCast(vtable), @typeName(T));
    }

    pub fn ExternDataErased(attr: *const Source.Attr, ptr: *anyopaque, vtable: *const Types.ExternData.VTable(anyopaque), typeName: []const u8) !SExpr {
        const buf = try attr.context.newBuffer(u8, typeName);
        const externData = try attr.context.new(Types.ExternData{ .attr = attr, .ptr = ptr, .vtable = vtable, .typeName = Extern.UStr.fromNative(buf) });

        attr.context.setFinalizer(externData, Types.ExternData.finalizer);

        return SExpr{
            .head = Head.init(Types.ExternData, externData),
            .data = .{ .nil = .{} },
        };
    }

    pub fn ExternFunction(attr: *const Source.Attr, name: []const u8, proc: Types.ExternFunction.Proc) !SExpr {
        const buf = try attr.context.newBuffer(u8, name);
        const externFunction = try attr.context.new(Types.ExternFunction{ .attr = attr, .name = Extern.UStr.fromNative(buf), .proc = proc });

        return SExpr{
            .head = Head.init(Types.ExternFunction, externFunction),
            .data = .{ .nil = .{} },
        };
    }

    pub fn compare(self: SExpr, other: SExpr) Ordering {
        const tagOrd = MiscUtils.compare(@intFromEnum(self.getTag()), @intFromEnum(other.getTag()));
        if (tagOrd != Ordering.Equal) {
            if (self.getTag() == Tag.Int and other.getTag() == Tag.Float) {
                return MiscUtils.compare(@as(f64, @floatFromInt(self.data.integral)), other.data.floating);
            } else if (self.getTag() == Tag.Float and other.getTag() == Tag.Int) {
                return MiscUtils.compare(self.data.floating, @as(f64, @floatFromInt(other.data.integral)));
            } else {
                return tagOrd;
            }
        }

        switch (self.getTag()) {
            Tag.Nil => return Ordering.Equal,
            Tag.Bool => return MiscUtils.compare(self.data.boolean, other.data.boolean),
            Tag.Int => return MiscUtils.compare(self.data.integral, other.data.integral),
            Tag.Float => return MiscUtils.compare(self.data.floating, other.data.floating),
            Tag.Char => return MiscUtils.compare(self.data.character, other.data.character),
            Tag.Symbol => return MiscUtils.compare(self.forcePtr(Types.Symbol), other.forcePtr(Types.Symbol)),
            Tag.String => return MiscUtils.compare(self.forcePtr(Types.String), other.forcePtr(Types.String)),
            Tag.Cons => return MiscUtils.compare(self.forcePtr(Types.Cons), other.forcePtr(Types.Cons)),
            Tag.Function => return MiscUtils.compare(self.forcePtr(Types.Function), other.forcePtr(Types.Function)),
            Tag.Builtin => return MiscUtils.compare(self.forcePtr(Types.Builtin), other.forcePtr(Types.Builtin)),
            Tag.ExternData => return MiscUtils.compare(self.forcePtr(Types.ExternData), other.forcePtr(Types.ExternData)),
            Tag.ExternFunction => return MiscUtils.compare(self.forcePtr(Types.ExternFunction), other.forcePtr(Types.ExternFunction)),
        }
    }

    pub fn compareAddress(self: SExpr, other: SExpr) Ordering {
        const tagOrd = MiscUtils.compare(@intFromEnum(self.getTag()), @intFromEnum(other.getTag()));
        if (tagOrd != Ordering.Equal) {
            return tagOrd;
        }

        switch (self.getTag()) {
            Tag.Nil => return Ordering.Equal,
            Tag.Bool => return MiscUtils.compare(self.data.boolean, other.data.boolean),
            Tag.Int => return MiscUtils.compare(self.data.integral, other.data.integral),
            Tag.Float => return MiscUtils.compare(self.data.floating, other.data.floating),
            Tag.Char => return MiscUtils.compare(self.data.character, other.data.character),
            Tag.Symbol => return MiscUtils.compareAddress(self.forcePtr(Types.Symbol), other.forcePtr(Types.Symbol)),
            Tag.String => return MiscUtils.compareAddress(self.forcePtr(Types.String), other.forcePtr(Types.String)),
            Tag.Cons => return MiscUtils.compareAddress(self.forcePtr(Types.Cons), other.forcePtr(Types.Cons)),
            Tag.Function => return MiscUtils.compareAddress(self.forcePtr(Types.Function), other.forcePtr(Types.Function)),
            Tag.Builtin => return MiscUtils.compareAddress(self.forcePtr(Types.Builtin), other.forcePtr(Types.Builtin)),
            Tag.ExternData => return MiscUtils.compareAddress(self.forcePtr(Types.ExternData), other.forcePtr(Types.ExternData)),
            Tag.ExternFunction => return MiscUtils.compareAddress(self.forcePtr(Types.ExternFunction), other.forcePtr(Types.ExternFunction)),
        }
    }

    pub fn getTag(self: SExpr) Tag {
        return Head.extractTag(self.head);
    }

    pub fn forcePtr(self: SExpr, comptime T: type) *T {
        return Head.extractPtr(T, self.head);
    }

    pub fn format(self: SExpr, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const fmt2 = "{" ++ fmt ++ "}";
        switch (self.getTag()) {
            Tag.Nil => try writer.print("()", .{}),
            Tag.Bool => try writer.print("{}", .{self.data.boolean}),
            Tag.Int => try writer.print("{}", .{self.data.integral}),
            Tag.Float => try writer.print("{}", .{self.data.floating}),
            Tag.Char => {
                var buf = [1]u8{0} ** 4;
                const esc = TextUtils.escape(@truncate(self.data.character), .Single, &buf) catch return error.InvalidArgument;
                try writer.print("'{s}'", .{esc});
            },
            Tag.Symbol => try writer.print(fmt2, .{self.forcePtr(Types.Symbol)}),
            Tag.String => try writer.print(fmt2, .{self.forcePtr(Types.String)}),
            Tag.Cons => try writer.print(fmt2, .{self.forcePtr(Types.Cons)}),
            Tag.Function => try writer.print(fmt2, .{self.forcePtr(Types.Function)}),
            Tag.Builtin => try writer.print(fmt2, .{self.forcePtr(Types.Builtin)}),
            Tag.ExternData => try writer.print(fmt2, .{self.forcePtr(Types.ExternData)}),
            Tag.ExternFunction => try writer.print(fmt2, .{self.forcePtr(Types.ExternFunction)}),
        }

        if (comptime std.mem.eql(u8, fmt, "attr")) {
            try writer.print(" @ {}", .{self.getAttr()});
        }
    }

    pub fn isNil(self: SExpr) bool {
        return self.getTag() == Tag.Nil;
    }

    pub fn isBool(self: SExpr) bool {
        return self.getTag() == Tag.Bool;
    }

    pub fn isNumber(self: SExpr) bool {
        return self.isInt() or self.isFloat();
    }

    pub fn isInt(self: SExpr) bool {
        return self.getTag() == Tag.Int;
    }

    pub fn isFloat(self: SExpr) bool {
        return self.getTag() == Tag.Float;
    }

    pub fn isChar(self: SExpr) bool {
        return self.getTag() == Tag.Char;
    }

    pub fn isSymbol(self: SExpr) bool {
        return self.getTag() == Tag.Symbol;
    }

    pub fn isExactSymbol(self: SExpr, value: []const u8) bool {
        return if (self.isSymbol()) MiscUtils.equal(value, self.forcePtr(Types.Symbol).toSlice()) else false;
    }

    pub fn isString(self: SExpr) bool {
        return self.getTag() == Tag.String;
    }

    pub fn isExactString(self: SExpr, value: []const u8) bool {
        return if (self.isString()) MiscUtils.equal(value, self.forcePtr(Types.String).toSlice()) else false;
    }

    pub fn isCons(self: SExpr) bool {
        return self.getTag() == Tag.Cons;
    }

    pub fn isList(self: SExpr) bool {
        return self.isNil() or if (self.castCons()) |cons| cons.isList() else false;
    }

    pub fn isFunction(self: SExpr) bool {
        return self.getTag() == Tag.Function;
    }

    pub fn isLambda(self: SExpr) bool {
        return if (self.castFunction()) |fun| fun.kind == .Lambda else false;
    }

    pub fn isMacro(self: SExpr) bool {
        return if (self.castFunction()) |fun| fun.kind == .Macro else false;
    }

    pub fn isBuiltin(self: SExpr) bool {
        return self.getTag() == Tag.Builtin;
    }

    pub fn isExternData(self: SExpr) bool {
        return self.getTag() == Tag.ExternData;
    }

    pub fn isNamedExternData(self: SExpr, typeName: []const u8) bool {
        return if (self.castExternData()) |ext| std.mem.eql(u8, ext.typeNameSlice(), typeName) else false;
    }

    pub fn isExactExternData(self: SExpr, comptime T: type) bool {
        return self.isNamedExternData(@typeName(T));
    }

    pub fn isExternFunction(self: SExpr) bool {
        return self.getTag() == Tag.ExternFunction;
    }

    pub fn isNamedExternFunction(self: SExpr, name: []const u8) bool {
        return if (self.castExternFunction()) |ext| std.mem.eql(u8, ext.nameSlice(), name) else false;
    }

    pub fn isCallable(self: SExpr) bool {
        return self.isFunction() or self.isBuiltin() or self.isExternFunction();
    }

    pub fn isInContext(self: SExpr, context: *const Context) bool {
        return self.getContext().id == context.id;
    }

    pub fn getContext(self: SExpr) *Context {
        return self.getAttr().context;
    }

    pub fn setAttr(self: SExpr, attr: *const Source.Attr) void {
        // NOTE: this requires that all objects place attr at the top of their struct and that they have guaranteed, unpacked layout
        self.forcePtr(Types.Prim).attr = attr;
    }

    pub fn getAttr(self: SExpr) *const Source.Attr {
        // NOTE: this requires that all objects place attr at the top of their struct and that they have guaranteed, unpacked layout
        return self.forcePtr(Types.Prim).attr;
    }

    pub fn forceNil(self: SExpr) Unit {
        std.debug.assert(self.isNil());
        return .{};
    }

    pub fn forceBool(self: SExpr) bool {
        std.debug.assert(self.isBool());
        return self.data.boolean;
    }

    pub fn forceInt(self: SExpr) i64 {
        std.debug.assert(self.isInt());
        return self.data.integral;
    }

    pub fn forceFloat(self: SExpr) f64 {
        std.debug.assert(self.isFloat());
        return self.data.floating;
    }

    pub fn forceChar(self: SExpr) TextUtils.Char {
        std.debug.assert(self.isChar());
        return @truncate(self.data.character);
    }

    pub fn forceSymbol(self: SExpr) *Types.Symbol {
        std.debug.assert(self.isSymbol());
        return self.forcePtr(Types.Symbol);
    }

    pub fn forceSymbolSlice(self: SExpr) []const u8 {
        return self.forceSymbol().toSlice();
    }

    pub fn forceString(self: SExpr) *Types.String {
        std.debug.assert(self.isString());
        return self.forcePtr(Types.String);
    }

    pub fn forceStringSlice(self: SExpr) []const u8 {
        return self.forceString().toSlice();
    }

    pub fn forceCons(self: SExpr) *Types.Cons {
        std.debug.assert(self.isCons());
        return self.forcePtr(Types.Cons);
    }

    pub fn forceFunction(self: SExpr) *Types.Function {
        std.debug.assert(self.isFunction());
        return self.forcePtr(Types.Function);
    }

    pub fn forceBuiltin(self: SExpr) *Types.Builtin {
        std.debug.assert(self.isBuiltin());
        return self.forcePtr(Types.Builtin);
    }

    pub fn forceExternData(self: SExpr) *Types.ExternData {
        std.debug.assert(self.isExternData());
        return self.forcePtr(Types.ExternData);
    }

    pub fn forceNamedExternData(self: SExpr, typeName: []const u8) *Types.ExternData {
        std.debug.assert(self.isNamedExternData(typeName));
        return self.forcePtr(Types.ExternData);
    }

    pub fn forceExternDataPtr(self: SExpr) *anyopaque {
        std.debug.assert(self.isExternData());
        return self.forcePtr(Types.ExternData).ptr;
    }

    pub fn forceExternDataNamedPtr(self: SExpr, typeName: []const u8) *anyopaque {
        std.debug.assert(self.isNamedExternData(typeName));
        return self.forcePtr(Types.ExternData).ptr;
    }

    pub fn forceExternDataExactPtr(self: SExpr, comptime T: type) *T {
        std.debug.assert(self.isExactExternData(T));
        return @ptrCast(@alignCast(self.forcePtr(Types.ExternData).ptr));
    }

    pub fn forceExternFunction(self: SExpr) *Types.ExternFunction {
        std.debug.assert(self.isExternFunction());
        return self.forcePtr(Types.ExternFunction);
    }

    pub fn forceNamedExternFunction(self: SExpr, name: []const u8) *Types.ExternFunction {
        std.debug.assert(self.isNamedExternFunction(name));
        return self.forcePtr(Types.ExternFunction);
    }

    pub fn forceExternFunctionProc(self: SExpr) Types.ExternFunction.Proc {
        return self.forceExternFunction().proc;
    }

    pub fn forceExternFunctionNamedProc(self: SExpr, name: []const u8) Types.ExternFunction.Proc {
        return self.forceNamedExternFunction(name).proc;
    }

    pub fn castNil(self: SExpr) ?Unit {
        if (self.isNil()) {
            return .{};
        } else {
            return null;
        }
    }

    pub fn castBool(self: SExpr) ?bool {
        if (self.isBool()) {
            return self.data.boolean;
        } else {
            return null;
        }
    }

    pub fn castInt(self: SExpr) ?i64 {
        if (self.isInt()) {
            return self.data.integral;
        } else {
            return null;
        }
    }

    pub fn castFloat(self: SExpr) ?f64 {
        if (self.isFloat()) {
            return self.data.floating;
        } else {
            return null;
        }
    }

    pub fn castChar(self: SExpr) ?TextUtils.Char {
        if (self.isChar()) {
            return @intCast(self.data.character);
        } else {
            return null;
        }
    }

    pub fn castSymbol(self: SExpr) ?*Types.Symbol {
        if (self.isSymbol()) {
            return self.forcePtr(Types.Symbol);
        } else {
            return null;
        }
    }

    pub fn castSymbolSlice(self: SExpr) ?[]const u8 {
        if (self.isSymbol()) {
            return self.forceSymbolSlice();
        } else {
            return null;
        }
    }

    pub fn castString(self: SExpr) ?*Types.String {
        if (self.isString()) {
            return self.forcePtr(Types.String);
        } else {
            return null;
        }
    }

    pub fn castStringSlice(self: SExpr) ?[]const u8 {
        if (self.isString()) {
            return self.forceStringSlice();
        } else {
            return null;
        }
    }

    pub fn castCons(self: SExpr) ?*Types.Cons {
        if (self.isCons()) {
            return self.forcePtr(Types.Cons);
        } else {
            return null;
        }
    }

    pub fn castFunction(self: SExpr) ?*Types.Function {
        if (self.isFunction()) {
            return self.forcePtr(Types.Function);
        } else {
            return null;
        }
    }

    pub fn castBuiltin(self: SExpr) ?*Types.Builtin {
        if (self.isBuiltin()) {
            return self.forcePtr(Types.Builtin);
        } else {
            return null;
        }
    }

    pub fn castExternData(self: SExpr) ?*Types.ExternData {
        if (self.isExternData()) {
            return self.forcePtr(Types.ExternData);
        } else {
            return null;
        }
    }

    pub fn castNamedExternData(self: SExpr, typeName: []const u8) ?*Types.ExternData {
        if (self.isNamedExternData(typeName)) {
            return self.forcePtr(Types.ExternData);
        } else {
            return null;
        }
    }

    pub fn castExternDataPtr(self: SExpr) ?*anyopaque {
        if (self.isExternData()) {
            return self.forceExternDataPtr();
        } else {
            return null;
        }
    }

    pub fn castExternDataNamedPtr(self: SExpr, typeName: []const u8) ?*anyopaque {
        if (self.isNamedExternData(typeName)) {
            return self.forceExternDataNamedPtr(typeName);
        } else {
            return null;
        }
    }

    pub fn castExternDataExactPtr(self: SExpr, comptime T: type) ?*T {
        if (self.isExactExternData(T)) {
            return self.forceExternDataExactPtr(T);
        } else {
            return null;
        }
    }

    pub fn castExternFunction(self: SExpr) ?*Types.ExternFunction {
        if (self.isExternFunction()) {
            return self.forcePtr(Types.ExternFunction);
        } else {
            return null;
        }
    }

    pub fn castNamedExternFunction(self: SExpr, name: []const u8) ?*Types.ExternFunction {
        if (self.isNamedExternFunction(name)) {
            return self.forcePtr(Types.ExternFunction);
        } else {
            return null;
        }
    }

    pub fn castExternFunctionProc(self: SExpr) ?Types.ExternFunction.Proc {
        if (self.isExternFunction()) {
            return self.forceExternFunctionProc();
        } else {
            return null;
        }
    }

    pub fn castExternFunctionNamedProc(self: SExpr, name: []const u8) ?Types.ExternFunction.Proc {
        if (self.isNamedExternFunction(name)) {
            return self.forceExternFunctionNamedProc(name);
        } else {
            return null;
        }
    }

    pub fn coerceNativeBool(self: SExpr) bool {
        if (self.isBool()) {
            return self.data.boolean;
        } else {
            return !self.isNil();
        }
    }

    pub fn coerceNativeInt(self: SExpr) ?i64 {
        if (self.isInt()) {
            return self.data.integral;
        } else if (self.isChar()) {
            return @intCast(self.data.character);
        } else if (self.isFloat()) {
            return @intFromFloat(self.data.floating);
        } else {
            return null;
        }
    }

    pub fn coerceNativeFloat(self: SExpr) ?f64 {
        if (self.isInt()) {
            return @floatFromInt(self.data.integral);
        } else if (self.isChar()) {
            return @floatFromInt(self.data.character);
        } else if (self.isFloat()) {
            return self.data.floating;
        } else {
            return null;
        }
    }

    pub fn coerceNativeChar(self: SExpr) ?TextUtils.Char {
        if (self.isInt()) {
            if (self.data.integral >= 0 and self.data.integral <= 0x10FFFF) {
                return @intCast(self.data.integral);
            } else {
                return null;
            }
        } else if (self.isChar()) {
            return @intCast(self.data.character);
        } else if (self.isFloat()) {
            const i: i64 = @intFromFloat(self.data.floating);
            if (i >= 0 and i <= 0x10FFFF) {
                return @intCast(i);
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    pub fn listLen(self: SExpr) ?usize {
        return if (self.isNil()) return 0 else if (self.castCons()) |cons| cons.listLen() else null;
    }

    pub fn hashWith(self: SExpr, hasher: anytype) void {
        MiscUtils.hashWith(hasher, self.getTag());

        switch (self.getTag()) {
            Tag.Nil => return MiscUtils.hashWith(hasher, @as(usize, 0)),
            Tag.Bool => return MiscUtils.hashWith(hasher, self.data.boolean),
            Tag.Int => return MiscUtils.hashWith(hasher, self.data.integral),
            Tag.Float => return MiscUtils.hashWith(hasher, self.data.floating),
            Tag.Char => return MiscUtils.hashWith(hasher, self.data.character),
            Tag.Symbol => return MiscUtils.hashWith(hasher, self.forcePtr(Types.Symbol)),
            Tag.String => return MiscUtils.hashWith(hasher, self.forcePtr(Types.String)),
            Tag.Cons => return MiscUtils.hashWith(hasher, self.forcePtr(Types.Cons)),
            Tag.Function => return MiscUtils.hashWith(hasher, self.forcePtr(Types.Function)),
            Tag.Builtin => return MiscUtils.hashWith(hasher, self.forcePtr(Types.Builtin)),
            Tag.ExternData => return MiscUtils.hashWith(hasher, self.forcePtr(Types.ExternData)),
            Tag.ExternFunction => return MiscUtils.hashWith(hasher, self.forcePtr(Types.ExternFunction)),
        }
    }


    pub fn from(at: *const Source.Attr, value: anytype) Interpreter.Error!SExpr {
        const T = @TypeOf(value);

        if (std.meta.hasMethod(T, "toSExpr")) {
            return value.toSExpr(at);
        }

        switch (T) {
            void, Unit => return Nil(at),
            bool => return Bool(at, value),
            i64 => return Int(at, value),
            f64 => return Float(at, value),
            *bool => return Bool(at, value.*),
            *i64 => return Int(at, value.*),
            *f64 => return Float(at, value.*),
            TextUtils.Char => return Char(at, value),
            []const u8 => return String(at, value),
            SExpr => return value,
            []SExpr => return fromBuffer(SExpr, at, value),
            *Types.Cons, *Types.Function, *Types.String, *Types.Symbol, *Types.ExternData, *Types.ExternFunction => return fromExistingPtr(value),
            Types.ExternFunction.Proc => return ExternFunction(at, "extern", value),
            Types.Builtin.Proc => return Builtin(at, "extern", value),
            else => switch (@typeInfo(T)) {
                .null, .undefined => return Nil(at),
                .optional => if (value) |v| return from(at, v) else return Nil(at),
                .error_set => return Cons(at, try Symbol(at, "error"), try Int(at, @intFromError(value))),
                .error_union => if (value) |v| return from(at, v) else |e| return from(e),
                .int => return Int(at, @intCast(value)),
                .float => return Float(at, @floatCast(value)),
                .vector => fromBuffer(at, &MiscUtils.arrayFromVector(value)),
                .array => fromBuffer(at, &value),
                .pointer => |info| switch (comptime info.size) {
                    .One, .Many, .C => return ExternData(T, at, @ptrCast(value), null),
                    .Slice => return fromBuffer(info.child, at, value),
                },
                .@"fn" => return wrapFn(at, "extern", value),
                .@"struct" => |info| {
                    const att = try at.clone();
                    var fields = try SExpr.Nil(att);
                    inline for (info.fields) |field| {
                        const pair = try SExpr.Cons(att, try Symbol(att, field.name), try from(at, @field(value, field.name)));
                        fields = try SExpr.Cons(att, pair, fields);
                    }
                    return try SExpr.Cons(at, try SExpr.Symbol(att, @typeName(T)), fields);
                },
                .@"union" => |info| if (info.tag_type) |TT| {
                    const att = try at.clone();
                    return SExpr.Cons(at, try from(att, @as(TT, value)), try from(att, @field(value, @tagName(value))));
                } else @compileError("cannot convert union without tag type to SExpr"),
                .@"enum", .enum_literal => return try SExpr.Symbol(at, @tagName(value)),
                else => @compileError("cannot convert `" ++ @typeName(T) ++ "` to SExpr"),
            }
        }
    }

    fn fromBuffer(comptime T: type, at: *const Source.Attr, value: []const T) Interpreter.Error!SExpr {
        const att = try at.clone();
        var tail = try Nil(att);
        for (0..value.len) |i| {
            const j = value.len - i - 1;
            tail = try Cons(att, try from(att, value[j]), tail);
        }
        tail.setAttr(at);
        return tail;
    }

    fn wrapFn(at: *const Source.Attr, name: []const u8, value: anytype) Interpreter.Error!SExpr {
        const T = @TypeOf(value);
        const info = @typeInfo(T).@"fn";

        return Builtin(at, name, struct {
            fn wrapper(interpreter: *Interpreter, attr: *const Source.Attr, args: SExpr) Interpreter.Result!SExpr {
                const eArgs = interpreter.evalN(info.params.len, args);
                const zArgs: std.meta.ArgsTuple(T) = undefined;

                for (info.params, 0..) |param, i| {
                    zArgs[i] = try eArgs[i].to(param.type);
                }

                const result = @call(.auto, value, zArgs);

                return SExpr.from(attr, result);
            }
        }.wrapper);
    }

    pub fn toError(self: SExpr, comptime E: type) (Interpreter.Error || E) {
        const symbol = self.castSymbolSlice() orelse return error.TypeError;

        inline for (@typeInfo(E).error_set.?) |err| {
            if (std.mem.eql(u8, symbol, err.name)) {
                return @field(E, err.name);
            }
        }

        return error.TypeError;
    }

    pub fn to(self: *const SExpr, comptime T: type) Interpreter.Error!T {
        if (std.meta.hasFn(T, "fromSExpr")) {
            return T.fromSExpr(self);
        }

        const tInfo = @typeInfo(T);
        if (tInfo == .pointer and tInfo.pointer.size == .One
        and std.meta.hasFn(tInfo.pointer.child, "fromSExpr")) {
            return T.fromSExpr(self);
        }

        switch (T) {
            void => { _ = self.castNil(); return; },
            Unit => { _ = self.castNil(); return .{}; },
            bool => return self.castBool() orelse error.TypeError,
            i64 => return self.castInt() orelse error.TypeError,
            f64 => return self.castFloat() orelse error.TypeError,
            *bool => return if (self.isBool()) &self.data.boolean else return error.TypeError,
            *i64 => return if (self.isInt()) &self.data.integral else return error.TypeError,
            *f64 => return if (self.isFloat()) &self.data.floating else return error.TypeError,
            TextUtils.Char => return self.castChar() orelse error.TypeError,
            []const u8 => return self.castStringSlice() orelse error.TypeError,
            SExpr => return self.*,
            []SExpr => return self.toBuffer(SExpr),
            *Types.Cons => return self.castCons() orelse error.TypeError,
            *Types.Function => return self.castFunction() orelse error.TypeError,
            *Types.Builtin => return self.castBuiltin() orelse error.TypeError,
            *Types.Symbol => return self.castSymbol() orelse error.TypeError,
            *Types.String => return self.castString() orelse error.TypeError,
            *Types.ExternData => return self.castExternData() orelse error.TypeError,
            *Types.ExternFunction => return self.castExternFunction() orelse error.TypeError,
            Types.ExternFunction.Proc => return self.castExternFunctionProc() orelse error.TypeError,
            Types.Builtin.Proc => return (self.castBuiltin() orelse return error.TypeError).getProc(),
            else => switch (@typeInfo(T)) {
                .null => return null,
                .optional => |info| if (self.isNil()) return null else return try self.to(info.payload),
                .error_set => return @errorFromInt(self.castInt() orelse return error.TypeError),
                .error_union => {
                    if (self.castCons()) |cons| {
                        if (cons.car.isExactSymbol("error")) {
                            return @errorFromInt(cons.cdr.castInt() orelse return error.TypeError);
                        }
                    }
                    return try self.to(T);
                },
                .int => return @intCast(self.castInt() orelse return error.TypeError),
                .float => return @floatCast(self.castFloat() orelse return error.TypeError),
                .vector => return MiscUtils.vectorFromArray(try toArray(self, T)),
                .array => return try toArray(self, T),
                .pointer => |info| switch (info.size) {
                    .One, .Many, .C => return @ptrCast(self.castExternDataPtr() orelse return error.TypeError),
                    .Slice => return try toBuffer(self, T),
                },
                .@"fn" => return @ptrCast(self.castExternFunctionProc() orelse return error.TypeError),
                .@"struct" => |info| {
                    const xp = self.castCons() orelse return error.TypeError;
                    const typeName = xp.car.castSymbolSlice() orelse return error.TypeError;
                    if (!std.mem.eql(u8, typeName, @typeName(T))) return error.TypeError;

                    const fields = xp.cdr;

                    var result: T = undefined;
                    for (info.fields) |field| {
                        @field(result, field.name) = try (try fields.getField(field.name) orelse return error.TypeError).to(field.type);
                    }

                    return result;
                },
                .@"union" => |info| if (info.tag_type) |_| {
                    const pair = try self.castCons() orelse return error.TypeError;
                    const sym = try pair.car.castSymbolSlice() orelse return error.TypeError;
                    inline for (info.fields) |field| {
                        if (std.mem.eql(u8, field.name, sym)) {
                            return @unionInit(T, field.name, try pair.cdr.to(field.type));
                        }
                    }
                } else @compileError("cannot convert union without tag type from SExpr"),
                .@"enum" => |info| {
                    const str = self.castSymbolSlice() orelse return error.TypeError;
                    inline for (info.fields) |field| {
                        if (std.mem.eql(u8, str, field.name)) {
                            return @field(T, field.name);
                        }
                    }
                    return error.TypeError;
                },
                else => @compileError("cannot convert SExpr to `" ++ @typeName(T) ++ "`"),
            }
        }
    }

    fn toArray(self: *const SExpr, comptime N: usize, comptime T: type) Interpreter.Error![N]T {
        var it = self.iter();
        var result: [N]T = undefined;
        for (0..N) |i| {
            result[i] = ((try it.next()) orelse return error.TypeError).to(T);
        }
        return result;
    }

    fn toBuffer(self: *const SExpr, comptime T: type) Interpreter.Error![]T {
        var buffer = std.ArrayList(T).init(self.getContext().allocator);
        var it = self.iter();
        while (try it.next()) |elem| {
            try buffer.append(try elem.to(T));
        }
        return buffer.toOwnedSlice();
    }

    fn getField(self: *const SExpr, name: []const u8) Interpreter.Error!?SExpr {
        var it = self.iter();
        while (try it.next()) |elem| {
            if (elem.castCons()) |cons| {
                if (cons.car.isExactSymbol(name)) {
                    return cons.cdr;
                }
            }
        }
        return null;
    }

    fn getIndex(self: *const SExpr, index: usize) Interpreter.Error!?SExpr {
        var it = self.iter();
        for (0..index) |_| {
            try it.next() orelse return null;
        }
        return it.next();
    }
};
