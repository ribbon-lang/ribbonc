const Core = @import("root.zig");

pub const Set = []const Binding;

pub const Binding = struct {
    id: Core.EvidenceIndex,
    handler: Core.FunctionIndex,
};
