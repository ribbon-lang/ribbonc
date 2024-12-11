const std = @import("std");

const Rml = @import("root.zig");



/// Print any number of arguments followed by a new line
pub fn @"print-ln"(interpreter: Rml.Interpreter, callOrigin: Rml.Origin, args: []const Rml.Object) Rml.Result! Rml.Object {
    const rml = interpreter.getRml();

    const stdout = std.io.getStdOut();
    const nativeWriter = stdout.writer();

    const writer: Rml.Writer = try .init(rml, callOrigin, .{nativeWriter.any()});
    defer writer.deinit();

    try writer.data.print("{}: ", .{callOrigin});

    for (args) |arg| try arg.getHeader().onFormat(writer);

    try writer.data.writeAll("\n");

    const nil: Rml.Nil = try .init(rml, callOrigin);
    defer nil.deinit();

    return nil.typeErase();
}



/// Print any number of arguments
pub fn print(interpreter: Rml.Interpreter, callOrigin: Rml.Origin, args: []const Rml.Object) Rml.Result! Rml.Object {
    const rml = interpreter.getRml();

    const stdout = std.io.getStdOut();
    const nativeWriter = stdout.writer();

    const writer: Rml.Writer = try .init(rml, callOrigin, .{nativeWriter.any()});
    defer writer.deinit();

    for (args) |arg| try arg.getHeader().onFormat(writer);

    const nil: Rml.Nil = try .init(rml, callOrigin);
    defer nil.deinit();

    return nil.typeErase();
}



/// Alias for `+`
pub const add = @"+";
/// Sum any number of arguments of type `int | float | char`;
/// if only one argument is provided, return the argument's absolute value
pub fn @"+"(interpreter: Rml.Interpreter, callOrigin: Rml.Origin, args: []const Rml.Object) Rml.Result! Rml.Object {
    if (args.len == 0) try interpreter.data.abort(callOrigin, error.InvalidArgumentCount, "expected at least 1 argument, found 0", .{});

    var sum: Rml.Object = args[0].clone();
    defer sum.deinit();

    if (args.len == 1) {
        if (Rml.castObj(Rml.int, sum)) |int| {
            defer int.deinit();

            const result: Rml.Int = try .wrap(int.getRml(), callOrigin, @intCast(@abs(int.data.*)));
            defer result.deinit();

            return result.typeErase();
        } else if (Rml.castObj(Rml.float, sum)) |float| {
            defer float.deinit();

            const result: Rml.Float = try .wrap(float.getRml(), callOrigin, @abs(float.data.*));
            defer result.deinit();

            return result.typeErase();
        } if (Rml.castObj(Rml.char, sum)) |char| {
            defer char.deinit();

            const result: Rml.Char = try .wrap(char.getRml(), callOrigin, char.data.*);
            defer result.deinit();

            return result.typeErase();
        } else {
            try interpreter.data.abort(callOrigin, error.TypeError, "expected int | float | char, found {s}", .{Rml.TypeId.name(sum.getHeader().type_id)});
        }
    }

    return arithCastReduce(interpreter, callOrigin, &sum, args, struct {
        pub fn int(a: Rml.int, b: Rml.int) Rml.int { return a + b; }
        pub fn float(a: Rml.float, b: Rml.float) Rml.float { return a + b; }
        pub fn char(a: Rml.char, b: Rml.char) Rml.char { return a + b; }
    });
}



/// Alias for `-`
pub const sub = @"-";
/// Subtract any number of arguments of type `int | float | char`;
/// if only one argument is provided, return the argument's negative value
pub fn @"-"(interpreter: Rml.Interpreter, callOrigin: Rml.Origin, args: []const Rml.Object) Rml.Result! Rml.Object {
    if (args.len == 0) try interpreter.data.abort(callOrigin, error.InvalidArgumentCount, "expected at least 1 argument, found 0", .{});

    var sum: Rml.Object = args[0].clone();
    defer sum.deinit();

    if (args.len == 1) {
        if (Rml.castObj(Rml.int, sum)) |int| {
            defer int.deinit();

            const result: Rml.Int = try .wrap(int.getRml(), callOrigin, -int.data.*);
            defer result.deinit();

            return result.typeErase();
        } else if (Rml.castObj(Rml.float, sum)) |float| {
            defer float.deinit();

            const result: Rml.Float = try .wrap(float.getRml(), callOrigin, -float.data.*);
            defer result.deinit();

            return result.typeErase();
        } if (Rml.castObj(Rml.char, sum)) |char| { // ???
            defer char.deinit();

            const result: Rml.Char = try .wrap(char.getRml(), callOrigin, char.data.*);
            defer result.deinit();

            return result.typeErase();
        } else {
            try interpreter.data.abort(callOrigin, error.TypeError, "expected int | float | char, found {s}", .{Rml.TypeId.name(sum.getHeader().type_id)});
        }
    }

    return arithCastReduce(interpreter, callOrigin, &sum, args, struct {
        pub fn int(a: Rml.int, b: Rml.int) Rml.int { return a - b; }
        pub fn float(a: Rml.float, b: Rml.float) Rml.float { return a - b; }
        pub fn char(a: Rml.char, b: Rml.char) Rml.char { return a - b; }
    });
}


/// Alias for `/`
pub const div = @"/";
/// Divide any number of arguments of type `int | float | char`;
/// if only one argument is provided, it is an error
pub fn @"/"(interpreter: Rml.Interpreter, callOrigin: Rml.Origin, args: []const Rml.Object) Rml.Result! Rml.Object {
    if (args.len < 2) try interpreter.data.abort(callOrigin, error.InvalidArgumentCount, "expected at least 2 arguments, found {}", .{args.len});

    var sum: Rml.Object = args[0].clone();
    defer sum.deinit();

    return arithCastReduce(interpreter, callOrigin, &sum, args, struct {
        pub fn int(a: Rml.int, b: Rml.int) Rml.int { return @divFloor(a, b); }
        pub fn float(a: Rml.float, b: Rml.float) Rml.float { return a / b; }
        pub fn char(a: Rml.char, b: Rml.char) Rml.char { return @divFloor(a, b); }
    });
}


/// Alias for `*`
pub const mul = @"*";
/// Multiply any number of arguments of type `int | float | char`;
/// if only one argument is provided, it is an error
pub fn @"*"(interpreter: Rml.Interpreter, callOrigin: Rml.Origin, args: []const Rml.Object) Rml.Result! Rml.Object {
    if (args.len < 2) try interpreter.data.abort(callOrigin, error.InvalidArgumentCount, "expected at least 2 arguments, found {}", .{args.len});

    var sum: Rml.Object = args[0].clone();
    defer sum.deinit();

    return arithCastReduce(interpreter, callOrigin, &sum, args, struct {
        pub fn int(a: Rml.int, b: Rml.int) Rml.int { return a * b; }
        pub fn float(a: Rml.float, b: Rml.float) Rml.float { return a * b; }
        pub fn char(a: Rml.char, b: Rml.char) Rml.char { return a * b; }
    });
}


/// Perform remainder division on any number of arguments of type `int | float | char`;
/// if only one argument is provided, it is an error
pub fn @"rem"(interpreter: Rml.Interpreter, callOrigin: Rml.Origin, args: []const Rml.Object) Rml.Result! Rml.Object {
    if (args.len < 2) try interpreter.data.abort(callOrigin, error.InvalidArgumentCount, "expected at least 2 arguments, found {}", .{args.len});

    var sum: Rml.Object = args[0].clone();
    defer sum.deinit();

    return arithCastReduce(interpreter, callOrigin, &sum, args[1..], struct {
        pub fn int(a: Rml.int, b: Rml.int) Rml.int { return @rem(a, b); }
        pub fn float(a: Rml.float, b: Rml.float) Rml.float { return @rem(a, b); }
        pub fn char(a: Rml.char, b: Rml.char) Rml.char { return @rem(a, b); }
    });
}



fn arithCastReduce(
    interpreter: Rml.Interpreter,
    callOrigin: Rml.Origin, acc: *Rml.Object, args: []const Rml.Object,
    comptime Ops: type,
) Rml.Result! Rml.Object {
    const offset = 1;
    for (args, 0..) |arg, i| {
        if (Rml.castObj(Rml.int, acc.*)) |int| {
            defer int.deinit();

            if (Rml.castObj(Rml.int, arg)) |int2| { // int + int = int
                defer int2.deinit();

                const int3: Rml.Int = try .wrap(int2.getRml(), callOrigin, @field(Ops, "int")(int.data.*, int2.data.*));
                defer int3.deinit();

                acc.deinit();
                acc.* = int3.typeErase();
            } else if (Rml.castObj(Rml.float, arg)) |float| { // int + float = float
                defer float.deinit();

                const float2: Rml.Float = try .wrap(float.getRml(), callOrigin, @field(Ops, "float")(@as(Rml.float, @floatFromInt(int.data.*)), float.data.*));
                defer float2.deinit();

                acc.deinit();
                acc.* = float2.typeErase();
            } else if (Rml.castObj(Rml.char, arg)) |char| { // int + char = int
                defer char.deinit();

                const int2: Rml.Int = try .wrap(char.getRml(), callOrigin, @field(Ops, "int")(int.data.*, @as(Rml.int, @intCast(char.data.*))));
                defer int2.deinit();

                acc.deinit();
                acc.* = int2.typeErase();
            } else {
                try interpreter.data.abort(callOrigin, error.TypeError, "expected int | float | char for argument {}, found {s}", .{i + offset, Rml.TypeId.name(arg.getHeader().type_id)});
            }
        } else if (Rml.castObj(Rml.float, acc.*)) |float| {
            defer float.deinit();

            if (Rml.castObj(Rml.int, arg)) |int| { // float + int = float
                defer int.deinit();

                const float2: Rml.Float = try .wrap(int.getRml(), callOrigin, @field(Ops, "float")(float.data.*, @as(Rml.float, @floatFromInt(int.data.*))));
                defer float2.deinit();

                acc.deinit();
                acc.* = float2.typeErase();
            } else if (Rml.castObj(Rml.float, arg)) |float2| { // float + float = float
                defer float2.deinit();

                const float3: Rml.Float = try .wrap(float2.getRml(), callOrigin, @field(Ops, "float")(float.data.*, float2.data.*));
                defer float3.deinit();

                acc.deinit();
                acc.* = float3.typeErase();
            } else if (Rml.castObj(Rml.char, arg)) |char| { // float + char = float
                defer char.deinit();

                const float2: Rml.Float = try .wrap(char.getRml(), callOrigin, @field(Ops, "float")(float.data.*, @as(Rml.float, @floatFromInt(char.data.*))));
                defer float2.deinit();

                acc.deinit();
                acc.* = float2.typeErase();
            } else {
                try interpreter.data.abort(callOrigin, error.TypeError, "expected int | float | char for argument {}, found {s}", .{i + offset, Rml.TypeId.name(arg.getHeader().type_id)});
            }
        } else if (Rml.castObj(Rml.char, acc.*)) |char| {
            defer char.deinit();

            if (Rml.castObj(Rml.int, arg)) |int| { // char + int = int
                defer int.deinit();

                const int2: Rml.Int = try .wrap(char.getRml(), callOrigin, @field(Ops, "int")(@as(Rml.int, @intCast(char.data.*)), int.data.*));
                defer int2.deinit();

                acc.deinit();
                acc.* = int2.typeErase();
            } else if (Rml.castObj(Rml.float, arg)) |float| { // char, float = float
                defer float.deinit();

                const float2: Rml.Float = try .wrap(float.getRml(), callOrigin, @field(Ops, "float")(@as(Rml.float, @floatFromInt(char.data.*)), float.data.*));
                defer float2.deinit();

                acc.deinit();
                acc.* = float2.typeErase();
            } else if (Rml.castObj(Rml.char, arg)) |char2| { // char, char = char
                defer char2.deinit();

                const char3: Rml.Char = try .wrap(char2.getRml(), callOrigin, @field(Ops, "char")(char.data.*, char2.data.*));
                defer char3.deinit();

                acc.deinit();
                acc.* = char3.typeErase();
            } else {
                try interpreter.data.abort(callOrigin, error.TypeError, "expected int | float | char for argument {}, found {s}", .{i + offset, Rml.TypeId.name(arg.getHeader().type_id)});
            }
        } else {
            try interpreter.data.abort(callOrigin, error.TypeError, "expected int | float | char for argument {}, found {s}", .{i, Rml.TypeId.name(acc.getHeader().type_id)});
        }
    }

    return acc.clone();
}

