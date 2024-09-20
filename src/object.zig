const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("./ast.zig");

pub const ObjectType = enum(u8) {
    INTEGER_OBJ,
    BOOLEAN_OBJ,
    NULL_OBJ,
    RETURN_OBJ,
    ERROR_OBJ,
};

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    returnval: ReturnValue,
    nil: Nil,
    eror: Error,
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

    pub const Error = struct {
        message: []const u8,
        stop: bool = true,
        pub fn inspect(self: Error) []const u8 {
            var count: usize = 0;
            while (self.message[count] != 170) {
                count += 1;
            }
            return self.message[0..count];
        }
    };

    pub const ReturnValue = struct {
        allocator: Allocator,
        value: *Object,
        stop: bool = false,

        pub fn inspect(self: ReturnValue) ![]const u8 {
            switch (self.value.*) {
                .boolean => |boo| {
                    return boo.inspect();
                },
                .integer => |int| {
                    return int.inspect();
                },
                .nil => |n| {
                    return n.inspect();
                },
                else => {
                    unreachable;
                },
            }
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
        pub fn inspect(self: Nil) []const u8 {
            _ = self;
            return "nil";
        }

        pub fn oType(self: Nil) ObjectType {
            _ = self;
            return ObjectType.NULL_OBJ;
        }
    };
};
