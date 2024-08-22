const std = @import("std");
const Allocator = std.mem.Allocator;

pub const ObjectType = enum([]const u8) {
    INTEGER_OBJ = "INTEGER",
    BOOLEAN_OBJ = "BOOLEAN",
    NULL_OBJ = "NULL",
};

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    nil: Nil,
    pub const Integer = struct {
        allocator: Allocator,
        value: i64,

        pub fn inspect(self: *Integer) ![]const u8 {
            return std.fmt.allocPrint(self.allocatorField, "{d}", .{self.value});
        }

        pub fn oType(self: *Integer) ObjectType {
            _ = self;
            return ObjectType.INTEGER_OBJ;
        }
    };

    pub const Boolean = struct {
        allocator: Allocator,
        value: bool,

        pub fn inspect(self: *Boolean) ![]const u8 {
            return std.fmt.allocPrint(self.allocator, "{}", .{self.value});
        }

        pub fn oType(self: *Boolean) ObjectType {
            _ = self;
            return ObjectType.BOOLEAN_OBJ;
        }
    };

    pub const Nil = struct {
        allocator: Allocator,

        pub fn inspect(self: *Nil) ![]const u8 {
            _ = self;
            return "nil";
        }

        pub fn oType(self: *Nil) ObjectType {
            _ = self;
            return ObjectType.NULL_OBJ;
        }
    };
};
