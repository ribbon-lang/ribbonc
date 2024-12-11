const std = @import("std");
const MiscUtils = @import("Utils").Misc;
const TextUtils = @import("Utils").Text;

const Rml = @import("root.zig");
const Ordering = Rml.Ordering;
const Error = Rml.Error;
const OOM = Rml.OOM;
const log = Rml.log;
const Weak = Rml.Weak;
const Wk = Rml.Wk;
const Writer = Rml.Writer;
const Symbol = Rml.Symbol;
const Object = Rml.Object;
const Origin = Rml.Origin;
const Obj = Rml.Obj;
const ptr = Rml.ptr;
const getObj = Rml.getObj;
const getTypeId = Rml.getTypeId;
const getRml = Rml.getRml;
const castObj = Rml.castObj;
const forceObj = Rml.forceObj;
const upgradeCast = Rml.upgradeCast;


pub const UnboundSymbol = error {UnboundSymbol};
pub const SymbolAlreadyBound = error {SymbolAlreadyBound};

pub const Env = Obj(Memory);

pub const MyId = enum(usize) {_};

pub const Table = Rml.map.MemoryUnmanaged(Rml.symbol.Memory, Rml.ObjData);

pub const Memory = struct {
    parent: Weak = Weak.Null,
    table: Table = .{},

    pub fn onCompare(a: ptr(Memory), other: Object) Ordering {
        var ord = Rml.compare(getTypeId(a), other.getHeader().type_id);

        if (ord == .Equal) {
            const b = forceObj(Memory, other);
            defer b.deinit();

            ord = Rml.compare(a.table, b.data.table);

            if (ord == .Equal) {
                ord = parent: {
                    const ap = upgradeCast(Memory, a.parent);
                    defer if (ap) |x| x.deinit();

                    const bp = upgradeCast(Memory, b.data.parent);
                    defer if (bp) |x| x.deinit();

                    break :parent if (ap == null and bp == null) .Equal
                    else if (ap == null) .Less
                    else if (bp == null) .Greater
                    else Rml.compare(ap.?, bp.?);
                };
            }
        }

        return ord;
    }

    pub fn onFormat(self: ptr(Memory), writer: Writer) Error! void {
        return writer.data.print("{}", .{self.table});
    }

    pub fn onDeinit(self: ptr(Memory)) void {
        self.table.deinit(getRml(self));
        self.parent.deinit();
    }

    pub fn bindNamespace(self: ptr(Memory), namespace: anytype) OOM! void {
        const T = @TypeOf(namespace);
        const rml = getRml(self);
        const origin = Origin.fromComptimeStr("builtin-" ++ @typeName(T));
        inline for (comptime std.meta.fields(T)) |field| {
            const sym: Symbol = try .init(rml, origin, .{field.name});

            if (comptime std.mem.startsWith(u8, @typeName(field.type), "object.Obj")) {
                self.bind(sym, @field(namespace, field.name).typeErase()) catch |err| {
                    if (err == error.OutOfMemory) return error.OutOfMemory
                    else @panic(@errorName(err));
                };
            } else {
                const val: Obj(field.type) = try .wrap(rml, origin, @field(namespace, field.name));
                defer val.deinit();

                self.bind(sym, val.typeErase()) catch |err| {
                    if (err == error.OutOfMemory) return error.OutOfMemory
                    else @panic(@errorName(err));
                };
            }
        }
    }


    /// Set a value associated with a new key
    ///
    /// Returns an error if a value with the same name was already declared in this scope.
    /// Returns an error if Rml is out of memory.
    pub fn bind(self: ptr(Memory), key: Symbol, val: Object) (OOM || SymbolAlreadyBound)! void {
        if (self.containsLocal(key)) return error.SymbolAlreadyBound;

        try self.table.set(getRml(self), key, val);
    }

    /// Set the value associated with a symbol
    ///
    /// Gives an error if a binding does not exist in this or an ancestor env
    pub fn set(self: ptr(Memory), key: Symbol, val: Object) UnboundSymbol! void {
        return if (self.table.native_map.getEntry(key)) |entry| {
            entry.value_ptr.deinit();
            entry.value_ptr.* = val;
        } else if (upgradeCast(Memory, self.parent)) |parent| parent.data.set(key, val)
        else error.UnboundSymbol;
    }

    /// Set the value associated with a symbol
    ///
    /// Gives an error if a binding does not exist in this frame of the env
    pub fn setLocal(self: ptr(Memory), key: Symbol, val: Object) UnboundSymbol! void {
        return if (self.table.native_map.getEntry(key)) |entry| {
            entry.value_ptr.deinit();
            entry.value_ptr.* = val;
        } else error.UnboundSymbol;
    }

    /// Set the value associated with a symbol in the ancestors of this env
    ///
    /// Gives an error if a binding does not exist in the ancestors of this env
    pub fn setInParent(self: ptr(Memory), key: Symbol, val: Object) UnboundSymbol! void {
        const p = upgradeCast(Memory, self.parent) orelse return error.UnboundSymbol;
        defer p.deinit();
        return p.data.set(key, val);
    }

    /// Find the value bound to a symbol in the env
    pub fn get(self: ptr(Memory), key: Symbol) ?Object {
        return self.getLocal(key) orelse self.getInParent(key);
    }

    /// Find the value bound to a symbol in the local frame of the env
    pub fn getLocal(self: ptr(Memory), key: Symbol) ?Object {
        return if (self.table.get(key)) |val| val else null;
    }

    /// Returns the value bound to a symbol in the ancestors of this env
    pub fn getInParent(self: ptr(Memory), key: Symbol) ?Object {
        const p = upgradeCast(Memory, self.parent) orelse return null;
        defer p.deinit();
        return p.data.get(key);
    }

    /// Returns the number of bindings in the env
    pub fn length(self: ptr(Memory)) usize {
        return self.localLength() + self.parentLength();
    }

    /// Returns the number of bindings in the local frame of the env
    pub fn localLength(self: ptr(Memory)) usize {
        return self.table.length();
    }

    /// Returns the number of bindings in the ancestors of this env
    pub fn parentLength(self: ptr(Memory)) usize {
        const p = upgradeCast(Memory, self.parent) orelse return 0;
        defer p.deinit();
        return p.data.length();
    }

    /// Check whether a key is bound in the env
    pub fn contains(self: ptr(Memory), key: Symbol) bool {
        return self.containsLocal(key) or self.containsInParent(key);
    }

    /// Check whether a key is bound in the local frame of the env
    pub fn containsLocal(self: ptr(Memory), key: Symbol) bool {
        return self.table.contains(key);
    }

    /// Check whether a key is bound in the ancestors of this env
    pub fn containsInParent(self: ptr(Memory), key: Symbol) bool {
        const p = upgradeCast(Memory, self.parent) orelse return false;
        defer p.deinit();
        return p.data.contains(key);
    }
};
