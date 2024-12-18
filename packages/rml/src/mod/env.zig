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

pub const Env = struct {
    table: Rml.map.TypedMapUnmanaged(Rml.Symbol, Rml.Cell) = .{},

    pub fn onCompare(a: ptr(Env), other: Object) Ordering {
        var ord = Rml.compare(getTypeId(a), other.getTypeId());

        if (ord == .Equal) {
            const b = forceObj(Env, other);
            defer b.deinit();

            ord = Rml.compare(a.table, b.data.table);
        }

        return ord;
    }

    pub fn onFormat(self: ptr(Env), writer: Obj(Writer)) Error! void {
        return writer.data.print("{}", .{self.table});
    }

    pub fn onDeinit(self: ptr(Env)) void {
        self.table.deinit(getRml(self));
    }

    pub fn bindNamespace(self: ptr(Env), namespace: anytype) OOM! void {
        const T = @TypeOf(namespace);
        const rml = getRml(self);
        const origin = Origin.fromComptimeStr("builtin-" ++ @typeName(T));
        inline for (comptime std.meta.fields(T)) |field| {
            const sym: Obj(Symbol) = try .new(rml, origin, .{field.name});

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


    pub fn dupe(self: ptr(Env), origin: ?Origin) OOM! Obj(Env) {
        const x: Obj(Env) = try .new(getRml(self), origin orelse Rml.getOrigin(self));
        errdefer x.deinit();

        x.data.copyFromEnv(self) catch return error.OutOfMemory;

        return x;
    }


    pub fn copyFromEnv(self: ptr(Env), other: ptr(Env)) (OOM || SymbolAlreadyBound)! void {
        var it = other.table.iter();
        while (it.next()) |entry| {
            if (self.table.contains(entry.key_ptr.*)) return error.SymbolAlreadyBound;
            try self.table.set(getRml(self), entry.key_ptr.clone(), entry.value_ptr.clone());
        }
    }

    pub fn copyFromTable(self: ptr(Env), table: *const Rml.map.TableUnmanaged) (OOM || SymbolAlreadyBound)! void {
        var it = table.iter();
        while (it.next()) |entry| {
            try self.bind(entry.key_ptr.clone(), entry.value_ptr.clone());
        }
    }

    pub fn overwriteFromTable(self: ptr(Env), table: *const Rml.map.TableUnmanaged) OOM! void {
        var it = table.iter();
        while (it.next()) |entry| {
            try self.bindOrSet(entry.key_ptr.clone(), entry.value_ptr.clone());
        }
    }

    /// Set a cell associated with a key
    pub fn bindOrSetCell(self: ptr(Env), key: Obj(Symbol), cell: Obj(Rml.Cell)) OOM! void {
        return self.setCell(key, cell)
         catch self.bindCell(key, cell)
         catch error.OutOfMemory;
    }

    pub fn bindCell(self: ptr(Env), key: Obj(Symbol), cell: Obj(Rml.Cell)) (OOM || SymbolAlreadyBound)! void {
        if (self.contains(key)) return error.SymbolAlreadyBound;

        try self.table.set(getRml(self), key, cell);
    }

    pub fn setCell(self: ptr(Env), key: Obj(Symbol), cell: Obj(Rml.Cell)) UnboundSymbol! void {
        return if (self.table.native_map.getEntry(key)) |entry| {
            entry.value_ptr.deinit();
            entry.value_ptr.* = cell;
        } else error.UnboundSymbol;
    }

    /// Set a value associated with a key
    pub fn bindOrSet(self: ptr(Env), key: Obj(Symbol), val: Object) OOM! void {
        return self.set(key, val)
         catch self.bind(key, val)
         catch error.OutOfMemory;
    }

    /// Set a value associated with a new key
    ///
    /// Returns an error if a value with the same name was already declared in this scope.
    /// Returns an error if Rml is out of memory.
    pub fn bind(self: ptr(Env), key: Obj(Symbol), val: Object) (OOM || SymbolAlreadyBound)! void {
        if (self.contains(key)) return error.SymbolAlreadyBound;

        const cell = try Rml.wrap(getRml(self), key.getOrigin(), Rml.Cell {.value = val});
        errdefer cell.deinit();

        try self.table.set(getRml(self), key, cell);
    }

    /// Set the value associated with a symbol
    ///
    /// Gives an error if a binding does not exist in this env
    pub fn set(self: ptr(Env), key: Obj(Symbol), val: Object) UnboundSymbol! void {
        return if (self.table.native_map.getEntry(key)) |entry| {
            entry.value_ptr.deinit();
            entry.value_ptr.data.set(val);
        } else error.UnboundSymbol;
    }

    /// Find the cell bound to a symbol in the env
    pub fn getCell(self: ptr(Env), key: Obj(Symbol)) ?Obj(Rml.Cell) {
        return self.table.get(key);
    }

    /// Find the value bound to a symbol in the env
    pub fn get(self: ptr(Env), key: Obj(Symbol)) ?Object {
        if (self.table.get(key)) |cell| {
            defer cell.deinit();
            return cell.data.get();
        }

        return null;
    }

    /// Returns the number of bindings in the env
    pub fn length(self: ptr(Env)) usize {
        return self.table.length();
    }

    /// Check whether a key is bound in the env
    pub fn contains(self: ptr(Env), key: Obj(Symbol)) bool {
        return self.table.contains(key);
    }

    /// Get a slice of the local keys of this Env
    pub fn keys(self: ptr(Env)) []Obj(Symbol) {
        return self.table.keys();
    }
};
