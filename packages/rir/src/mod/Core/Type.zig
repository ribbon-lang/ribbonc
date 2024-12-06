const std = @import("std");
const MiscUtils = @import("Utils").Misc;

const Core = @import("root.zig");


pub const Type = union(enum) {
    void: void,
    bool: void,
    u8: void, u16: void, u32: void, u64: void,
    s8: void, s16: void, s32: void, s64: void,
    f32: void, f64: void,
    pointer: Core.TypeId,
    array: Type.Array,
    product: []Core.TypeId,
    sum: Sum,
    raw_sum: []Core.TypeId,
    function: Type.Function,

    block: void,
    handler_set: void,
    evidence: Type.Function,

    pub const Map = struct {
        inner: std.ArrayHashMapUnmanaged(Core.Type, void, MiscUtils.SimpleHashContext, true) = .{},

        pub fn init() !Map {
            return Map { };
        }

        pub fn deinit(self: *Map, allocator: std.mem.Allocator) void {
            for (self.inner.keys()) |t| {
                t.deinit(allocator);
            }

            self.inner.deinit(allocator);
        }

        /// Calls `Core.Type.clone` on the input, if the type is not found in the map
        pub fn typeId(self: *Map, allocator: std.mem.Allocator, ty: Core.Type) !Core.TypeId {
            if (self.inner.getIndex(ty)) |index| {
                return @truncate(index);
            }

            const index = self.inner.count();

            if (index >= Core.MAX_TYPES) {
                return error.TooManyTypes;
            }

            try self.inner.put(allocator, try ty.clone(allocator), {});

            return @truncate(index);
        }

        /// Does not call `Core.Type.clone` on the input
        pub fn typeIdPreallocated(self: *Map, allocator: std.mem.Allocator, ty: Core.Type) !Core.TypeId {
            if (self.inner.getIndex(ty)) |index| {
                return @truncate(index);
            }

            const index = self.inner.count();

            if (index >= Core.MAX_TYPES) {
                return error.TooManyTypes;
            }

            try self.inner.put(allocator, ty, {});

            return @truncate(index);
        }

        pub fn typeFromNative(self: *Map, comptime T: type, allocator: std.mem.Allocator) !Core.Type {
            switch (T) {
                void => return .void,
                bool => return .bool,
                u8 => return .u8, u16 => return .u16, u32 => return .u32, u64 => return .u64,
                i8 => return .s8, i16 => return .s16, i32 => return .s32, i64 => return .s64,
                f32 => return .f32, f64 => return .f64,

                else => switch (@typeInfo(T)) {
                    .pointer => |info| return Core.Type { .pointer = try self.typeIdFromNative(info.child, allocator) },
                    .array => |info| return Core.Type { .array = .{
                        .length = info.len,
                        .element = try self.typeIdFromNative(info.child, allocator),
                    } },
                    .@"struct" => |info| {
                        var field_types = try allocator.alloc(Core.TypeId, info.fields.len);
                        errdefer allocator.free(field_types);

                        for (info.fields, 0..) |field, i| {
                            field_types[i] = try self.typeIdFromNative(field.type, allocator);
                        }

                        return Core.Type { .product = field_types };
                    },
                    .@"enum" => |info| return self.typeFromNative(info.tag_type, allocator),
                    .@"union" => |info| {
                        var field_types = try allocator.alloc(Core.TypeId, info.fields.len);
                        errdefer allocator.free(field_types);

                        for (info.fields, 0..) |field, i| {
                            field_types[i] = try self.typeIdFromNative(field.type, allocator);
                        }

                        if (info.tag_type) |TT| {
                            return Core.Type { .sum = .{
                                .discriminator = try self.typeIdFromNative(TT, allocator),
                                .types = field_types,
                            } };
                        } else {
                            return Core.Type { .raw_sum = field_types };
                        }
                    },
                    .@"fn" => |info| {
                        const return_type = try self.typeIdFromNative(info.return_type.?, allocator);
                        const termination_type = try self.typeIdFromNative(void, allocator);

                        const effects = try allocator.alloc(Core.EvidenceId, 0);
                        errdefer allocator.free(effects);

                        var parameter_types = try allocator.alloc(Core.TypeId, info.params.len);
                        errdefer allocator.free(parameter_types);

                        inline for (info.params, 0..) |param, i| {
                            parameter_types[i] = try self.typeIdFromNative(param.type.?, allocator);
                        }

                        return Core.Type { .function = .{
                            .return_type = return_type,
                            .termination_type = termination_type,
                            .effects = effects,
                            .parameter_types = parameter_types,
                        } };
                    },

                    else => @compileError("cannot convert type `" ++ @typeName(T) ++ "` to Core.Type"),
                }
            }
        }

        pub fn typeIdFromNative(self: *Map, comptime T: type, allocator: std.mem.Allocator) !Core.TypeId {
            const ty = try self.typeFromNative(T, allocator);
            errdefer ty.deinit(allocator);

            return self.typeIdPreallocated(allocator, ty);
        }

        pub fn getType(self: *Map, id: Core.TypeId) !Core.Type {
            if (id >= self.inner.count()) {
                return error.InvalidType;
            }

            return self.inner.keys()[id];
        }
    };

    const BASIC_TYPE_NAMES = [_][:0]const u8 {
        "void",
        "bool",
        "u8", "u16", "u32", "u64",
        "s8", "s16", "s32", "s64",
        "f32", "f64",
        "block",
    };

    pub const BASIC_TYPE_IDS = type_ids: {
        var type_ids = [1]Core.TypeId { undefined } ** BASIC_TYPE_NAMES.len;

        for (0..BASIC_TYPE_NAMES.len) |i| {
            type_ids[i] = @truncate(i);
        }

        break :type_ids type_ids;
    };

    pub const BASIC_TYPES = types: {
        var types = [1]Type { undefined } ** BASIC_TYPE_NAMES.len;

        for (BASIC_TYPE_NAMES, 0..) |name, i| {
            types[i] = @unionInit(Type, name, {});
        }

        break :types types;
    };

    pub const Sum = struct {
        discriminator: Core.TypeId,
        types: []Core.TypeId,

        pub fn clone(self: *const Type.Sum, allocator: std.mem.Allocator) !Type.Sum {
            return Type.Sum {
                .discriminator = self.discriminator,
                .types = try allocator.dupe(Core.TypeId, self.types),
            };
        }

        pub fn deinit(self: Type.Sum, allocator: std.mem.Allocator) void {
            allocator.free(self.types);
        }
    };

    pub const Array = struct {
        length: usize,
        element: Core.TypeId,
    };

    pub const Function = struct {
        return_type: Core.TypeId,
        termination_type: Core.TypeId,
        effects: []Core.EvidenceId,
        parameter_types: []Core.TypeId,

        pub fn deinit(self: Type.Function, allocator: std.mem.Allocator) void {
            allocator.free(self.effects);
            allocator.free(self.parameter_types);
        }

        pub fn clone(self: *const Type.Function, allocator: std.mem.Allocator) !Type.Function {
            const effects = try allocator.dupe(Core.EvidenceId, self.effects);
            errdefer allocator.free(effects);

            const parameter_types = try allocator.dupe(Core.TypeId, self.parameter_types);
            errdefer allocator.free(parameter_types);

            return Type.Function {
                .return_type = self.return_type,
                .termination_type = self.termination_type,
                .effects = effects,
                .parameter_types = parameter_types,
            };
        }
    };

    pub fn clone(self: *const Type, allocator: std.mem.Allocator) !Type {
        switch (self.*) {
            .product => |field_types| return Type { .product = try allocator.dupe(Core.TypeId, field_types) },
            .sum => |sum| return Type { .sum = try sum.clone(allocator) },
            .raw_sum => |field_types| return Type { .raw_sum = try allocator.dupe(Core.TypeId, field_types) },
            .function => |fun| return Type { .function = try fun.clone(allocator) },
            .evidence => |fun| return Type { .evidence = try fun.clone(allocator) },
            inline else => return self.*,
        }
    }

    pub fn deinit(self: Type, allocator: std.mem.Allocator) void {
        switch (self) {
            .product => |field_types| allocator.free(field_types),
            .sum => |sum| sum.deinit(allocator),
            .raw_sum => |field_types| allocator.free(field_types),
            .function => |fun| fun.deinit(allocator),
            .evidence => |fun| fun.deinit(allocator),
            inline else => {},
        }
    }

    pub fn asFunction(self: Type) !Type.Function {
        switch (self) {
            .function => |fun| return fun,
            .evidence => |fun| return fun,
            inline else => return error.InvalidType,
        }
    }
};
