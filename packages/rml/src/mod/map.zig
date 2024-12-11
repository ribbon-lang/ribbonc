const std = @import("std");

const Rml = @import("root.zig");
const Error = Rml.Error;
const Ordering = Rml.Ordering;
const OOM = Rml.OOM;
const const_ptr = Rml.const_ptr;
const ptr = Rml.ptr;
const Obj = Rml.Obj;
const Object = Rml.Object;
const Writer = Rml.Writer;
const getHeader = Rml.getHeader;
const getObj = Rml.getObj;
const getRml = Rml.getRml;
const forceObj = Rml.forceObj;


pub const Map = TypedMap(Rml.object.ObjData, Rml.object.ObjData);

pub fn TypedMap (comptime K: type, comptime V: type) type {
    return struct {
        const Self = @This();

        pub const NativeIter = NativeMap.Iterator;
        pub const NativeMap = std.ArrayHashMapUnmanaged(Obj(K), Obj(V), Rml.SimpleHashContext, true);


        unmanaged: MemoryUnmanaged(K, V) = .{},


        pub fn onCompare(a: ptr(Self), other: Object) Ordering {
            var ord = Rml.compare(getHeader(a).type_id, other.getHeader().type_id);
            if (ord == .Equal) {
                const b = forceObj(Self, other);
                defer b.deinit();

                ord = a.unmanaged.compare(b.data.unmanaged);
            }
            return ord;
        }

        pub fn onFormat(self: const_ptr(Self), writer: Rml.Obj(Writer)) Error! void {
            return writer.data.print("{}", .{self.unmanaged});
        }

        pub fn onDeinit(self: ptr(Self)) void {
            const rml = getRml(self);
            self.unmanaged.deinit(rml);
        }

        /// Set the value associated with a key
        pub fn set(self: ptr(Self), key: Obj(K), v: Obj(V)) OOM! void {
            const rml = getRml(self);
            return self.unmanaged.set(rml, key, v);
        }

        /// Find the value associated with a key
        pub fn get(self: ptr(Self), key: Obj(K)) ?Object {
            return self.unmanaged.get(key);
        }

        /// Returns the number of key-value pairs in the map
        pub fn length(self: ptr(Self)) usize {
            return self.unmanaged.length();
        }

        /// Check whether a key is stored in the map
        pub fn contains(self: ptr(Self), key: Obj(K)) bool {
            return self.unmanaged.contains(key);
        }

        /// Returns the backing array of keys in this map. Modifying the map may invalidate this array.
        /// Modifying this array in a way that changes key hashes or key equality puts the map into an unusable state until reIndex is called.
        pub fn keys(self: ptr(Self)) []Object {
            return self.unmanaged.keys();
        }

        /// Returns the backing array of values in this map.
        /// Modifying the map may invalidate this array.
        /// It is permitted to modify the values in this array.
        pub fn values(self: ptr(Self)) []Obj(V) {
            return self.unmanaged.values();
        }

        /// Recomputes stored hashes and rebuilds the key indexes.
        /// If the underlying keys have been modified directly,
        /// call this method to recompute the denormalized metadata
        /// necessary for the operation of the methods of this map that lookup entries by key.
        pub fn reIndex(self: ptr(Self)) OOM! void {
            const rml = getRml(self);
            return self.unmanaged.reIndex(rml);
        }
    };
}

pub fn MemoryUnmanaged (comptime K: type, comptime V: type) type {
    return struct {
        const Self = @This();

        native_map: NativeMap = .{},

        pub const NativeIter = NativeMap.Iterator;
        pub const NativeMap = std.ArrayHashMapUnmanaged(Obj(K), Obj(V), Rml.SimpleHashContext, true);

        pub fn compare(self: Self, other: Self) Ordering {
            var ord = Rml.compare(self.keys().len, other.keys().len);

            if (ord == .Equal) {
                ord = Rml.compare(self.keys(), other.keys());
            }

            if (ord == .Equal) {
                ord = Rml.compare(self.values(), other.values());
            }

            return ord;
        }

        pub fn format(self: *const Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) Error! void {
            var it = self.iter();
            while (it.next()) |entry| {
                writer.print("({} {})", .{entry.key_ptr.*, entry.value_ptr.*}) catch |err| return Rml.errorCast(err);
                if (it.index < it.len) {
                    writer.writeAll(" ") catch |err| return Rml.errorCast(err);
                }
            }
        }

        pub fn deinit(self: *Self, rml: *Rml) void {
            var it = self.native_map.iterator();

            while (it.next()) |entry| {
                entry.key_ptr.deinit();
                entry.value_ptr.deinit();
            }
            self.native_map.deinit(rml.storage.object);
        }

        /// Set the value associated with a key
        pub fn set(self: *Self, rml: *Rml, key: Obj(K), val: Obj(V)) OOM! void {
            if (self.native_map.getEntry(key)) |entry| {
                entry.key_ptr.deinit();
                entry.key_ptr.* = key;

                entry.value_ptr.deinit();
                entry.value_ptr.* = val;
            } else {
                try self.native_map.put(rml.storage.object, key, val);
            }
        }

        /// Find the value associated with a key
        pub fn get(self: *const Self, key: Obj(K)) ?Obj(V) {
            return if (self.native_map.get(key)) |v| v.clone() else null;
        }

        /// Returns the number of key-value pairs in the map
        pub fn length(self: *const Self) usize {
            return self.native_map.count();
        }

        /// Check whether a key is stored in the map
        pub fn contains(self: *const Self, key: Obj(K)) bool {
            return self.native_map.contains(key);
        }

        /// Returns an iterator over the pairs in this map. Modifying the map may invalidate this iterator.
        pub fn iter(self: *const Self) NativeIter {
            return self.native_map.iterator();
        }

        /// Returns the backing array of keys in this map. Modifying the map may invalidate this array.
        /// Modifying this array in a way that changes key hashes or key equality puts the map into an unusable state until reIndex is called.
        pub fn keys(self: *const Self) []Obj(K) {
            return self.native_map.keys();
        }

        /// Returns the backing array of values in this map.
        /// Modifying the map may invalidate this array.
        /// It is permitted to modify the values in this array.
        pub fn values(self: *const Self) []Obj(V) {
            return self.native_map.values();
        }

        /// Recomputes stored hashes and rebuilds the key indexes.
        /// If the underlying keys have been modified directly,
        /// call this method to recompute the denormalized metadata
        /// necessary for the operation of the methods of this map that lookup entries by key.
        pub fn reIndex(self: *Self, rml: *Rml) OOM! void {
            return self.native_map.reIndex(rml.storage.object);
        }

        pub fn clone(self: *Self, rml: *Rml) OOM! Self {
            const newMap = Self { .native_map = try self.native_map.clone(rml.storage.object) };
            var it = newMap.iter();
            while (it.next()) |entry| {
                entry.key_ptr.getHeader().incrRefCount();
                entry.value_ptr.getHeader().incrRefCount();
            }
            return newMap;
        }

        pub fn copyFrom(self: *Self, rml: *Rml, other: *Self) OOM! void {
            var it = other.iter();
            while (it.next()) |entry| {
                try self.set(rml, entry.key_ptr.clone(), entry.value_ptr.clone());
            }
        }
    };
}
