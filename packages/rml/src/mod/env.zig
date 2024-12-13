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
const Object = Rml.Object;
const Origin = Rml.Origin;
const Obj = Rml.Obj;
const ptr = Rml.ptr;
const Symbol = Rml.Symbol;
const Writer = Rml.Writer;
const getObj = Rml.getObj;
const getTypeId = Rml.getTypeId;
const getRml = Rml.getRml;
const castObj = Rml.castObj;
const forceObj = Rml.forceObj;
const upgradeCast = Rml.upgradeCast;

pub const SymbolError  = UnboundSymbol || SymbolAlreadyBound;
pub const UnboundSymbol = error {UnboundSymbol};
pub const SymbolAlreadyBound = error {SymbolAlreadyBound};

pub const Domain = Rml.set.TypedSetUnmanaged(Symbol);

pub const MyId = enum(usize) {_};

pub const Env = struct {
    parent: Weak = Weak.Null,
    table: Rml.map.TableUnmanaged = .{},

    pub fn onCompare(a: ptr(Env), other: Object) Ordering {
        var ord = Rml.compare(getTypeId(a), other.getTypeId());

        if (ord == .Equal) {
            const b = forceObj(Env, other);
            defer b.deinit();

            ord = Rml.compare(a.table, b.data.table);

            if (ord == .Equal) {
                ord = parent: {
                    const ap = upgradeCast(Env, a.parent);
                    defer if (ap) |x| x.deinit();

                    const bp = upgradeCast(Env, b.data.parent);
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

    pub fn onFormat(self: ptr(Env), writer: Obj(Writer)) Error! void {
        return writer.data.print("{}", .{self.table});
    }

    pub fn onDeinit(self: ptr(Env)) void {
        self.table.deinit(getRml(self));
        self.parent.deinit();
    }

    pub fn bindNamespace(self: ptr(Env), namespace: anytype) OOM! void {
        const T = @TypeOf(namespace);
        const rml = getRml(self);
        const origin = Origin.fromComptimeStr("builtin-" ++ @typeName(T));
        inline for (comptime std.meta.fields(T)) |field| {
            const sym: Obj(Symbol) = try .init(rml, origin, .{field.name});

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


    pub fn copyFrom(self: ptr(Env), env: Obj(Env)) (OOM || SymbolAlreadyBound)! void {
        var it = env.data.table.iter();
        while (it.next()) |entry| {
            try self.bind(entry.key_ptr.clone(), entry.value_ptr.clone());
        }
    }


    /// Set a value associated with a new key
    ///
    /// Returns an error if a value with the same name was already declared in this scope.
    /// Returns an error if Rml is out of memory.
    pub fn bind(self: ptr(Env), key: Obj(Symbol), val: Object) (OOM || SymbolAlreadyBound)! void {
        if (self.containsLocal(key)) return error.SymbolAlreadyBound;

        try self.table.set(getRml(self), key, val);
    }

    /// Set the value associated with a symbol
    ///
    /// Gives an error if a binding does not exist in this or an ancestor env
    pub fn set(self: ptr(Env), key: Obj(Symbol), val: Object) UnboundSymbol! void {
        return if (self.table.native_map.getEntry(key)) |entry| {
            entry.value_ptr.deinit();
            entry.value_ptr.* = val;
        } else if (upgradeCast(Env, self.parent)) |parent| parent.data.set(key, val)
        else error.UnboundSymbol;
    }

    /// Set the value associated with a symbol
    ///
    /// Gives an error if a binding does not exist in this frame of the env
    pub fn setLocal(self: ptr(Env), key: Obj(Symbol), val: Object) UnboundSymbol! void {
        return if (self.table.native_map.getEntry(key)) |entry| {
            entry.value_ptr.deinit();
            entry.value_ptr.* = val;
        } else error.UnboundSymbol;
    }

    /// Set the value associated with a symbol in the ancestors of this env
    ///
    /// Gives an error if a binding does not exist in the ancestors of this env
    pub fn setInParent(self: ptr(Env), key: Obj(Symbol), val: Object) UnboundSymbol! void {
        const p = upgradeCast(Env, self.parent) orelse return error.UnboundSymbol;
        defer p.deinit();
        return p.data.set(key, val);
    }

    /// Find the value bound to a symbol in the env
    pub fn get(self: ptr(Env), key: Obj(Symbol)) ?Object {
        return self.getLocal(key) orelse self.getInParent(key);
    }

    /// Find the value bound to a symbol in the local frame of the env
    pub fn getLocal(self: ptr(Env), key: Obj(Symbol)) ?Object {
        return if (self.table.get(key)) |val| val else null;
    }

    /// Returns the value bound to a symbol in the ancestors of this env
    pub fn getInParent(self: ptr(Env), key: Obj(Symbol)) ?Object {
        const p = upgradeCast(Env, self.parent) orelse return null;
        defer p.deinit();
        return p.data.get(key);
    }

    /// Returns the number of bindings in the env
    pub fn length(self: ptr(Env)) usize {
        return self.localLength() + self.parentLength();
    }

    /// Returns the number of bindings in the local frame of the env
    pub fn localLength(self: ptr(Env)) usize {
        return self.table.length();
    }

    /// Returns the number of bindings in the ancestors of this env
    pub fn parentLength(self: ptr(Env)) usize {
        const p = upgradeCast(Env, self.parent) orelse return 0;
        defer p.deinit();
        return p.data.length();
    }

    /// Check whether a key is bound in the env
    pub fn contains(self: ptr(Env), key: Obj(Symbol)) bool {
        return self.containsLocal(key) or self.containsInParent(key);
    }

    /// Check whether a key is bound in the local frame of the env
    pub fn containsLocal(self: ptr(Env), key: Obj(Symbol)) bool {
        return self.table.contains(key);
    }

    /// Check whether a key is bound in the ancestors of this env
    pub fn containsInParent(self: ptr(Env), key: Obj(Symbol)) bool {
        const p = upgradeCast(Env, self.parent) orelse return false;
        defer p.deinit();
        return p.data.contains(key);
    }
};
