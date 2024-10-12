const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("./ast.zig");
const Enviornment = @import("./environment.zig").Environment;

pub const ObjectType = enum(u8) {
    INTEGER_OBJ,
    BOOLEAN_OBJ,
    NULL_OBJ,
    RETURN_OBJ,
    ERROR_OBJ,
    FUNCTION_OBJ,
};

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    returnval: ReturnValue,
    nil: Nil,
    eror: Error,
    function: Function,

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

    pub const Error = struct {
        message: []const u8,
        stop: bool = true,
        pub fn inspect(self: Error) []const u8 {
            return self.message;
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
