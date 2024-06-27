const std = @import("std");
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;
const print = std.debug.print;

pub const Node = union(enum) {
    const self = @This();
    statement: Statement,
    expression: Expression,
};

pub const Statement = union(enum) {
    letStatement: LetStatement,
    returnStatement: ReturnStatement,
    expression: ExpressionStatement,

    pub const LetStatement = struct {
        const Self = @This();
        token: Token,
        name: Identifier,
        value: ?Expression,

        pub fn printValue(self: *Self) void {
            print("{} {} \n", .{ self.token.literal, self.value.?.identifier.value });
        }

        pub fn string(self: *Self) []const u8 {
            var buf: [1024]u8 = undefined;
            _ = std.fmt.bufPrint(&buf, "{s} {s} =", .{ self.token.literal, self.name.tokenLiteral() }) catch unreachable;
            if (self.value != null) {
                _ = std.fmt.bufPrint(&buf, "{s}", .{self.value.?.identifier.string()}) catch unreachable;
            }
            _ = std.fmt.bufPrint(&buf, ";", .{}) catch unreachable;

            return &buf;
        }
    };

    pub const ReturnStatement = struct {
        const Self = @This();
        token: Token,
        returnValue: ?Expression,

        pub fn string(self: *Self) []const u8 {
            var buf: [1024]u8 = undefined;
            _ = std.fmt.bufPrint(&buf, "{s} + ", .{self.token.literal}) catch unreachable;

            if (self.returnValue != null) {
                _ = std.fmt.bufPrint(&buf, "{s}", .{self.returnValue.?.identifier.string()}) catch unreachable;
            }

            _ = std.fmt.bufPrint(&buf, ";", .{}) catch unreachable;

            return &buf;
        }
    };

    pub const ExpressionStatement = struct {
        const Self = @This();
        token: Token,
        expression: ?Expression,

        pub fn string(self: *Self) []const u8 {
            if (self.expression != null) {
                return self.expression.?.identifier.string();
            }
            return "";
        }
    };
};

pub const Expression = union(enum) {
    identifier: Identifier,
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn init(token: Token) Identifier {
        return .{
            .token = token,
            .value = token.literal,
        };
    }

    pub fn tokenLiteral(self: Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *Identifier) []const u8 {
        return self.value;
    }
};

pub const Program = struct {
    allocator: std.mem.Allocator,
    statements: std.ArrayList(Statement),

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{
            .allocator = allocator,
            .statements = std.ArrayList(Statement).init(allocator),
        };
    }

    pub fn deinit(self: *Program) void {
        self.statements.deinit();
    }

    pub fn tokenLiteral(self: *Program) []const u8 {
        if (self.statements.items.len > 0) {
            return self.statements[0].tokenLiteral;
        } else {
            return "";
        }
    }

    pub fn string(self: *Program) ![]const u8 {
        var buff = std.ArrayList(u8).init(self.allocator);
        defer buff.deinit();
        for (self.statements.items) |value| {
            switch (value) {
                .letStatement => try std.fmt.bufPrint(&buff.writer(), "{s}", .{value.letStatement.string()}),
                .returnStatement => try std.fmt.bufPrint(&buff.writer(), "{s}", .{value.returnStatement.string()}),
                .expression => try std.fmt.bufPrint(&buff.writer(), "{s}", .{value.expression.string()}),
            }
        }

        return buff.toOwnedSlice();
    }
};

pub fn GenericAst(comptime T: type) type {
    return struct {
        list: std.ArrayList(T),
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) !GenericAst(T) {
            return .{
                .allocator = allocator,
                .list = std.ArrayList(T).init(allocator),
            };
        }

        pub fn deinit(self: *GenericAst(T)) void {
            self.list.deinit();
        }

        pub fn append(self: *GenericAst(T), value: T) !void {
            try self.list.append(value);
        }
    };
}
