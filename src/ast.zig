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
        ident: Token,
        value: ?Expression,

        pub fn printValue(self: *Self) void {
            print("Token: {}\n", .{self.token});
        }
    };

    pub const ReturnStatement = struct {
        const Self = @This();
        token: Token,
        value: ?Expression,
    };

    pub const ExpressionStatement = struct {
        token: Token,
        value: ?Expression,
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

    pub fn tokenLiteral(self: *Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn printValue(self: *Identifier) !void {
        print("{s}", .{self.value});
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
