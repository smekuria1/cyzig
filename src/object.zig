const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("./ast.zig");
const Enviornment = @import("./environment.zig").Environment;
pub const BuiltinFn = *const fn (allocator: Allocator, args: []?Object) Object;
pub const builtFns = std.StaticStringMap(Object.Builtin).initComptime(
    .{
        .{
            "len",
            Object.Builtin{
                .Fn = len,
            },
        },
        .{
            "rest", Object.Builtin{
                .Fn = rest,
            },
        },
    },
);

fn rest(allocator: Allocator, args: []?Object) Object {
    if (args.len != 1) {
        const message = std.fmt.allocPrint(allocator, "wrong number of arguments. got={d}, want=1", .{args.len}) catch unreachable;
        return Object{ .eror = Object.Error{ .message = message } };
    }
    if (args[0]) |value| {
        switch (value) {
            .array => |arr| {
                const length = arr.elements.len;
                if (length > 0) {
                    var newElements = std.ArrayList(?Object).init(allocator);
                    newElements.appendSlice(arr.elements[1..length]) catch unreachable;
                    return Object{ .array = Object.Array{
                        .elements = newElements.items,
                        .allocator = allocator,
                    } };
                }
            },
            else => {
                return Object.Error.newError(allocator, "argument to `rest` must be ARRAY_OBJ got", getObjType(value)) catch unreachable;
            },
        }
    }
    return Object{ .nil = Object.Nil{} };
}

fn len(allocator: Allocator, args: []?Object) Object {
    if (args.len != 1) {
        const message = std.fmt.allocPrint(allocator, "wrong number of arguments. got={d}, want=1", .{args.len}) catch unreachable;
        return Object{ .eror = Object.Error{ .message = message } };
    }
    if (args[0]) |value| {
        switch (value) {
            .string => {
                return Object{ .integer = Object.Integer{ .allocator = allocator, .value = @intCast(value.string.value.len) } };
            },
            .array => {
                return Object{ .integer = Object.Integer{ .allocator = allocator, .value = @intCast(value.array.elements.len) } };
            },
            else => {
                return Object.Error.newError(allocator, "argument to `len` not supported got", getObjType(value)) catch unreachable;
            },
        }
    }
    return Object{ .nil = Object.Nil{} };
}

fn getObjType(o: Object) ObjectType {
    return switch (o) {
        inline else => |case| {
            return case.oType();
        },
    };
}
pub const ObjectType = enum(u8) {
    INTEGER_OBJ,
    BOOLEAN_OBJ,
    NULL_OBJ,
    RETURN_OBJ,
    ERROR_OBJ,
    FUNCTION_OBJ,
    STRING_OBJ,
    BUILTIN_OBJ,
    ARRAY_OBJ,
};
pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    returnval: ReturnValue,
    nil: Nil,
    eror: Error,
    function: Function,
    string: String,
    builtin: Builtin,
    array: Array,

    pub fn stringer(self: Object) ![]const u8 {
        switch (self) {
            inline else => |case| {
                return case.inspect();
            },
        }
    }

    pub const Array = struct {
        elements: []?Object,
        stop: bool = false,
        allocator: Allocator,

        pub fn oType(self: Array) ObjectType {
            _ = self;
            return ObjectType.ARRAY_OBJ;
        }

        pub fn inspect(self: Array) Allocator.Error![]const u8 {
            var list = std.ArrayList(u8).init(self.allocator);
            defer list.deinit();
            _ = try list.writer().write("[");
            for (0.., self.elements) |i, value| {
                if (value) |obj| {
                    const str = try obj.stringer();
                    _ = try list.writer().write(str);
                    if (i >= self.elements.len - 1) {
                        continue;
                    }
                    _ = try list.writer().write(",");
                }
            }
            _ = try list.writer().write("]");
            return list.toOwnedSlice();
        }
    };
    pub const Builtin = struct {
        Fn: BuiltinFn,
        stop: bool = false,

        pub fn inspect(self: Builtin) ![]const u8 {
            _ = self;
            return "builtin function";
        }

        pub fn oType(self: Builtin) ObjectType {
            _ = self;
            return ObjectType.BUILTIN_OBJ;
        }
    };
    pub const Function = struct {
        enviornment: *Enviornment,
        parameters: ?std.ArrayList(Ast.Identifier),
        body: ?Ast.BlockStatement,
        allocatorr: Allocator,
        stop: bool = false,

        pub fn inspect(self: Function) Allocator.Error![]u8 {
            var list = std.ArrayList(u8).init(self.allocatorr);
            var paramlist = std.ArrayList([]const u8).init(self.allocatorr);
            defer paramlist.deinit();

            for (0.., self.parameters.?.items) |i, value| {
                paramlist.append(value.string()) catch unreachable;
                if (i >= self.parameters.?.items.len - 1) {
                    continue;
                }
                paramlist.append(",") catch unreachable;
            }
            _ = list.writer().write("fn") catch unreachable;
            _ = list.writer().write("(") catch unreachable;
            const commalist = paramlist.toOwnedSlice() catch unreachable;
            for (commalist) |value| {
                _ = list.writer().write(value) catch unreachable;
            }
            self.allocatorr.free(commalist);
            _ = list.writer().write(") {") catch unreachable;
            const bodystring = self.body.?.string() catch unreachable;
            _ = list.writer().write(bodystring) catch unreachable;
            self.allocatorr.free(bodystring);
            _ = list.writer().write("}") catch unreachable;
            return list.toOwnedSlice();
        }
        pub fn oType(self: Function) ObjectType {
            _ = self;
            return ObjectType.FUNCTION_OBJ;
        }
    };

    pub const Integer = struct {
        stop: bool = false,
        allocator: Allocator,
        value: i64,

        pub fn inspect(self: Integer) ![]const u8 {
            return std.fmt.allocPrint(self.allocator, "{d}", .{self.value});
        }

        pub fn oType(self: Integer) ObjectType {
            _ = self;
            return ObjectType.INTEGER_OBJ;
        }
    };
    pub const String = struct {
        stop: bool = false,
        allocator: Allocator,
        value: []const u8,

        pub fn inspect(self: String) ![]const u8 {
            return std.fmt.allocPrint(self.allocator, "\"{s}\"", .{self.value});
        }

        pub fn oType(self: String) ObjectType {
            _ = self;
            return ObjectType.STRING_OBJ;
        }
    };
    pub const Error = struct {
        message: []const u8,
        stop: bool = true,
        pub fn inspect(self: Error) ![]const u8 {
            return self.message;
        }

        pub fn oType(self: Error) ObjectType {
            _ = self;
            return ObjectType.ERROR_OBJ;
        }

        pub fn newError(allocator: Allocator, message: []const u8, value: anytype) Allocator.Error!Object {
            const allocMessage = try std.fmt.allocPrint(allocator, "{s} {any}", .{ message, value });
            return Object{ .eror = Object.Error{ .message = allocMessage } };
        }
    };

    pub const ReturnValue = struct {
        allocator: Allocator,
        value: *Object,
        stop: bool = false,

        pub fn inspect(self: ReturnValue) ![]const u8 {
            switch (self.value.*) {
                inline else => |case| {
                    return case.inspect();
                },
            }
        }

        pub fn oType(self: ReturnValue) ObjectType {
            _ = self;
            return ObjectType.RETURN_OBJ;
        }
    };

    pub const Boolean = struct {
        value: bool,
        stop: bool = false,

        pub fn inspect(self: Boolean) ![]const u8 {
            if (self.value == true) {
                return "true";
            }
            return "false";
        }

        pub fn oType(self: Boolean) ObjectType {
            _ = self;
            return ObjectType.BOOLEAN_OBJ;
        }
    };

    pub const Nil = struct {
        stop: bool = false,
        pub fn inspect(self: Nil) ![]const u8 {
            _ = self;
            return "nil";
        }

        pub fn oType(self: Nil) ObjectType {
            _ = self;
            return ObjectType.NULL_OBJ;
        }
    };
};
