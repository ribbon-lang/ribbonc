const std = @import("std");
const MiscUtils = @import("Utils").Misc;
const TypeUtils = @import("Utils").Type;

const Rml = @import("root.zig");
const bindgen = Rml.bindgen;
const Writer = Rml.Writer;
const Error = Rml.Error;
const Ordering = Rml.Ordering;
const OOM = Rml.OOM;
const log = Rml.log;
const TypeId = Rml.TypeId;
const map = Rml.map;
const Origin = Rml.Origin;


pub const refcount = std.log.scoped(.refcount);

pub const OBJ_ALIGN = 16;

pub const ObjData = extern struct { data: u8 align(OBJ_ALIGN) };
pub fn ptr(comptime T: type) type { return *align(OBJ_ALIGN) T; }
pub fn const_ptr(comptime T: type) type { return *const align(OBJ_ALIGN) T; }

pub const PropertySet = map.MemoryUnmanaged(ObjData, ObjData);

pub const Header = struct {
    rml: *Rml,
    type_id: TypeId,
    vtable: *const VTable,
    origin: Origin,
    ref_count: usize,
    weak_ref_count: usize,
    properties: PropertySet,

    pub fn onInit(self: ptr(Header), comptime T: type, rml: *Rml, origin: Origin) void {
        refcount.debug("Header/onInit {} {} {s} @ {} : #{x}", .{1, 1, @typeName(T), origin, @intFromPtr(self.getObjMemory())});
        self.* = Header {
            .rml = rml,
            .type_id = TypeId.of(T),
            .vtable = VTable.of(T),
            .origin = origin,
            .ref_count = 1,
            .weak_ref_count = 1,
            .properties = .{},
        };
    }

    pub fn onDeinit(self: ptr(Header)) void {
        refcount.debug("Header/onDeinit {} {} {s} @ {} : #{x}", .{self.ref_count, self.weak_ref_count, TypeId.name(self.type_id), self.origin, @intFromPtr(self.getObjMemory())});
        std.debug.assert(self.ref_count == 0);

        self.vtable.onDeinit(self);

        self.properties.deinit(self.rml);

        self.decrWeakRefCount();
    }

    pub fn onDestroy(self: ptr(Header)) void {
        refcount.debug("Header/onDestroy {} {} {s} @ {} : #{x}", .{self.ref_count, self.weak_ref_count, TypeId.name(self.type_id), self.origin, @intFromPtr(self.getObjMemory())});
        std.debug.assert(self.weak_ref_count == 0);
        self.vtable.onDestroy(self);
    }

    pub fn onCompare(self: ptr(Header), other: ptr(Header)) Ordering {
        const obj = other.getObject();
        defer obj.deinit();

        return self.vtable.onCompare(self, obj);
    }

    pub fn onFormat(self: ptr(Header), writer: Writer) Error! void {
        return self.vtable.onFormat(self, writer);
    }

    pub fn getObject(self: ptr(Header)) Object {
        return getObj(self.getData());
    }

    pub fn getObjMemory(self: ptr(Header)) *ObjMemory(ObjData) {
        return @fieldParentPtr("header", @as(ptr(TypeUtils.ToBytes(Header)), @ptrCast(self)));
    }

    pub fn getData(self: ptr(Header)) ptr(ObjData) {
        return self.getObjMemory().getData();
    }

    pub fn incrRefCount(self: ptr(Header)) void {
        std.debug.assert(self.ref_count > 0);

        self.ref_count += 1;

        refcount.debug("incr {} {} {s} @ {} : #{x}", .{self.ref_count, self.weak_ref_count, TypeId.name(self.type_id), self.origin, @intFromPtr(self.getObjMemory())});
    }

    pub fn decrRefCount(self: ptr(Header)) void {
        std.debug.assert(self.ref_count > 0);

        self.ref_count -= 1;

        refcount.debug("decr {} {} {s} @ {} : #{x}", .{self.ref_count, self.weak_ref_count, TypeId.name(self.type_id), self.origin, @intFromPtr(self.getObjMemory())});

        if (self.ref_count == 0) self.onDeinit();
    }

    pub fn incrWeakRefCount(self: ptr(Header)) void {
        std.debug.assert(self.weak_ref_count > 0);

        self.weak_ref_count += 1;

        refcount.debug("incr weak {} {} {s} @ {} : #{x}", .{self.ref_count, self.weak_ref_count, TypeId.name(self.type_id), self.origin, @intFromPtr(self.getObjMemory())});
    }

    pub fn decrWeakRefCount(self: ptr(Header)) void {
        std.debug.assert(self.weak_ref_count > 0);

        self.weak_ref_count -= 1;

        refcount.debug("decr weak {} {} {s} @ {} : #{x}", .{self.ref_count, self.weak_ref_count, TypeId.name(self.type_id), self.origin, @intFromPtr(self.getObjMemory())});

        if (self.weak_ref_count == 0) self.onDestroy();
    }
};


pub const VTable = struct {
    obj_memory: ObjMemoryFunctions,
    obj_data: ObjDataFunctions,

    pub const ObjMemoryFunctions = struct {
        onDestroy: ?*const fn (*anyopaque) void = null,
    };

    pub const ObjDataFunctions = struct {
        onCompare: ?*const fn (const_ptr(ObjData), Rml.Object) Ordering = null,
        onFormat: ?*const fn (const_ptr(ObjData), Writer) Error! void = null,
        onDeinit: ?*const fn (const_ptr(ObjData)) void = null,
    };

    pub fn of(comptime T: type) *const VTable {
        if (comptime T == ObjData) return undefined;

        const x = struct {
            const vtable = VTable {
                .obj_memory = obj_memory: {
                    var functionSet: ObjMemoryFunctions = .{};

                    for (std.meta.fields(ObjMemoryFunctions)) |field| {
                        const funcName = field.name;

                        const G = @typeInfo(@typeInfo(field.type).optional.child).pointer.child;
                        const gInfo = @typeInfo(G).@"fn";

                        const F = @TypeOf(@field(ObjMemory(T), funcName));
                        const fInfo = @typeInfo(F).@"fn";

                        std.debug.assert(!fInfo.is_generic);
                        std.debug.assert(!fInfo.is_var_args);
                        std.debug.assert(fInfo.return_type.? == gInfo.return_type.?);
                        std.debug.assert(fInfo.params.len == gInfo.params.len);

                        @field(functionSet, funcName) = @ptrCast(&@field(ObjMemory(T), funcName));
                    }

                    break :obj_memory functionSet;
                },
                .obj_data = obj_data: {
                    var functionSet: ObjDataFunctions = .{};

                    const support = bindgen.Support(T);
                    for (std.meta.fields(ObjDataFunctions)) |field| {
                        const funcName = field.name;

                        const def =
                            if (TypeUtils.supportsDecls(T) and @hasDecl(T, funcName)) &@field(T, funcName)
                            else if (@hasDecl(support, funcName)) &@field(support, funcName)
                            else @compileError("no " ++ @typeName(T) ++ "." ++ funcName ++ " found");

                        const G = @typeInfo(@typeInfo(field.type).optional.child).pointer.child;
                        const gInfo = @typeInfo(G).@"fn";

                        const F = @typeInfo(@TypeOf(def)).pointer.child;
                        if (@typeInfo(F) != .@"fn") {
                            @compileError("wtf1 " ++ @typeName(T) ++ " " ++ @typeName(@TypeOf(def)));
                        }
                        const fInfo = @typeInfo(F).@"fn";

                        std.debug.assert(!fInfo.is_generic);
                        std.debug.assert(!fInfo.is_var_args);
                        std.debug.assert(fInfo.return_type.? == gInfo.return_type.?);
                        std.debug.assert(fInfo.params.len == gInfo.params.len);

                        @field(functionSet, funcName) = @ptrCast(def);
                    }

                    break :obj_data functionSet;
                },
            };
        };

        return &x.vtable;
    }

    pub fn onCompare(self: *const VTable, header: ptr(Header), other: Object) Ordering {
        const data = header.getData();
        log.debug("VTable/onCompare {s}", .{TypeId.name(header.type_id)});
        return self.obj_data.onCompare.?(data, other);
    }

    pub fn onFormat(self: *const VTable, header: ptr(Header), writer: Writer) Error! void {
        const data = header.getData();
        log.debug("VTable/onFormat {s}", .{TypeId.name(header.type_id)});
        return self.obj_data.onFormat.?(data, writer);
    }

    pub fn onDeinit(self: *const VTable, header: ptr(Header)) void {
        const data = header.getData();
        log.debug("VTable/onDeinit {s}", .{TypeId.name(header.type_id)});
        return self.obj_data.onDeinit.?(data);
    }

    pub fn onDestroy(self: *const VTable, header: ptr(Header)) void {
        const data = header.getObjMemory();
        log.debug("VTable/onDestroy {s}", .{TypeId.name(header.type_id)});
        return self.obj_memory.onDestroy.?(data);
    }
};

pub const ObjectMemory = ObjMemory(ObjData);
pub fn ObjMemory (comptime T: type) type {
    return extern struct {
        const Self = @This();

        // this sucks but we need extern to guarantee layout here & don't want it on Header / T
        header: TypeUtils.ToBytes(Header) align(OBJ_ALIGN),
        data: TypeUtils.ToBytes(T) align(OBJ_ALIGN),

        pub fn onInit(self: *Self, rml: *Rml, origin: Origin, data: T) void {
            Header.onInit(@ptrCast(&self.header), T, rml, origin);
            self.data = std.mem.toBytes(data);
            rml.storage.object_count += 1;
        }

        pub fn getHeader(self: *Self) ptr(Header) {
            return @ptrCast(&self.header);
        }

        pub fn getData(self: ptr(Self)) ptr(T) {
            return @ptrCast(&self.data);
        }

        pub fn onDestroy(self: ptr(Self)) void {
            log.debug("(ObjMemory {s})/onDestroy", .{@typeName(T)});
            const rml = self.getHeader().rml;
            rml.storage.object.destroy(self);
            rml.storage.object_count -= 1;
        }
    };
}

pub const Weak = Wk(ObjData);
pub fn Wk(comptime T: type) type {
    return struct {
        const Self = @This();

        memory: ?ptr(ObjMemory(T)),

        pub const Null = Self { .memory = null };

        pub fn upgradeUnchecked(self: Self) Obj(T) {
            const m = self.memory.?;
            m.getHeader().incrRefCount();
            return .{.data = @ptrCast(&m.data)};
        }

        pub fn upgrade(self: Self) ?Obj(T) {
            return if (self.memory) |m| (
                if (m.getHeader().ref_count > 0) self.upgradeUnchecked()
                else null
            ) else null;
        }

        pub fn deinit(self: Self) void {
            if (self.memory) |m| {
                refcount.debug("Wk({s})/deinit", .{TypeId.name(m.getHeader().type_id)});
                m.getHeader().decrWeakRefCount();
            }
        }
    };
}

pub fn ref (comptime T: type) type {
    return struct {
        obj: Object,
        data: if (@typeInfo(T) == .pointer) T else *T,
    };
}

pub const Object = Obj(ObjData);
pub fn Obj(comptime T: type) type {
    std.debug.assert(@alignOf(T) <= OBJ_ALIGN);

    return struct {
        const Self = @This();

        data: ptr(T),

        pub const init = if (T == ObjData) null else if (std.meta.hasMethod(T, "onInit")) struct {
            const OnInit: type = @TypeOf(T.onInit);
            const Args = TypeUtils.DropSelf(T, std.meta.ArgsTuple(OnInit));

            pub fn init(rml: *Rml, origin: Origin, args: Args) OOM! Self {
                const memory = try rml.storage.object.create(ObjMemory(T));
                errdefer rml.storage.object.destroy(memory);

                const causesErrors = comptime TypeUtils.causesErrors(OnInit);

                if (comptime TypeUtils.hasSelf(T, std.meta.ArgsTuple(OnInit))) {
                    memory.onInit(rml, origin, undefined);
                    const r = @call(.auto, T.onInit, .{@as(ptr(T), @ptrCast(&memory.data))} ++ args);
                    if (causesErrors) try r;
                } else {
                    const r = @call(.auto, T.onInit, args);
                    memory.onInit(rml, origin, if (causesErrors) try r else r);
                }

                return Self {.data = @ptrCast(&memory.data) };
            }
        }.init else struct {
            pub fn init(rml: *Rml, origin: Origin) OOM! Self {
                const memory = try rml.storage.object.create(ObjMemory(T));
                errdefer rml.storage.object.destroy(memory);

                memory.onInit(rml, origin, TypeUtils.zero(T));

                return Self { .data = @ptrCast(&memory.data) };
            }
        }.init;

        pub fn clone(self: Self) Self {
            self.getHeader().incrRefCount();
            return Self { .data = self.data };
        }

        pub fn downgrade(self: Self) Wk(T) {
            self.getHeader().incrWeakRefCount();
            return .{ .memory = self.getMemory() };
        }

        pub fn typeErase(self: Self) Object {
            self.getHeader().incrRefCount();
            return self.typeEraseLeak();
        }

        pub fn typeEraseLeak(self: Self) Object {
            return .{ .data = @alignCast(@ptrCast(self.data)) };
        }

        pub fn wrap(rml: *Rml, origin: Origin, val: T) OOM! Self {
            const memory = try rml.storage.object.create(ObjMemory(T));
            errdefer rml.storage.object.destroy(memory);

            memory.onInit(rml, origin, val);

            return Self { .data = memory.getData() };
        }

        pub fn compare(self: Self, other: Obj(T)) Ordering {
            return self.getHeader().onCompare(other.getHeader());
        }

        pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) Error! void {
            const w: Rml.writer.Native = if (@TypeOf(writer) == Rml.writer.Native) writer else writer.any();

            const wObj: Writer = try .init(self.getRml(), self.getRml().storage.origin, .{w});
            defer wObj.deinit();

            try self.getHeader().onFormat(wObj);
        }

        pub fn deinit(self: Self) void {
            refcount.debug("deinit Obj({s})", .{TypeId.name(self.getHeader().type_id)});
            self.getHeader().decrRefCount();
        }

        pub fn getMemory(self: Self) *ObjMemory(T) {
            return @fieldParentPtr("data", @as(ptr(TypeUtils.ToBytes(T)), @ptrCast(self.data)));
        }

        pub fn getHeader(self: Self) ptr(Header) {
            return @ptrCast(&getMemory(self).header);
        }

        pub fn getRml(self: Self) *Rml {
            return self.getHeader().rml;
        }
    };
}

pub fn getObj(p: anytype) Obj(@typeInfo(@TypeOf(p)).pointer.child) {
    refcount.debug("getObj", .{});
    const out = Obj(@typeInfo(@TypeOf(p)).pointer.child) { .data = p };
    out.getHeader().incrRefCount();
    return out;
}

pub fn getHeader(p: anytype) ptr(Header) {
    const obj = Obj(@typeInfo(@TypeOf(p)).pointer.child) { .data = p };
    return obj.getHeader();
}

pub fn getTypeId(p: anytype) TypeId {
    const obj = Obj(@typeInfo(@TypeOf(p)).pointer.child) { .data = p };
    return obj.getHeader().type_id;
}

pub fn getRml(p: anytype) *Rml {
    const obj = Obj(@typeInfo(@TypeOf(p)).pointer.child) { .data = p };
    return obj.getRml();
}

pub fn castObj(comptime T: type, obj: Object) ?Obj(T) {
    if (MiscUtils.equal(obj.getHeader().type_id, TypeId.of(T))) return forceObj(T, obj)
    else return null;
}

pub fn forceObj(comptime T: type, obj: Object) Obj(T) {
    obj.getHeader().incrRefCount();
    return .{.data = @ptrCast(obj.data)};
}

pub fn upgradeCast(comptime T: type, weak: Weak) ?Obj(T) {
    return if (weak.upgrade()) |u| {
        defer u.deinit();
        return castObj(T, u);
    } else null;
}

pub fn downgradeCast(obj: anytype) Weak {
    const e: Object = obj.typeErase();
    defer e.deinit();
    return e.downgrade();
}
