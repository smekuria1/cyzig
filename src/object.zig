const std = @import("std");
const Allocator = std.mem.Allocator;

pub const ObjectType = enum([]const u8) { INTEGER_OBJ = "INTEGER", BOOLEAN_OBJ = "BOOLEAN", NULL_OBJ = "NULL", RETURN_OBJ = "RETURN_VALUE" };

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    returnval: ReturnValue,
    nil: Nil,
    pub const Integer = struct {
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

    pub const ReturnValue = struct {
        allocator:Allocator,
        value : Object,

        pub fn inspect(self: ReturnValue) ![]const u8 {
            switch (self.value) {
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
                }
            }
        }
    };

    pub const Boolean = struct {
        value: bool,

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
