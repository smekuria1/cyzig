const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;
const std = @import("std");

const Node = struct {
    ptr: *anyopaque,
    impl: *const Interface,

    pub const Interface = struct {
        tokenLiteral: *const fn (ctx: *anyopaque) []const u8,
    };

    pub fn tokenLiteral(self: Node) []const u8 {
        return self.impl.tokenLiteral(self.ptr);
    }
};

const Statement = struct {
    node: Node,
};

const Expression = struct {
    node: Node,
};

const Program = struct {
    statements: []Statement,

    pub fn tokenLiteral(ctx: *anyopaque) []const u8 {
        const self: *Program = @ptrCast(@alignCast(ctx));

        if (self.statements.len > 0) {
            return self.statements[0].node.tokenLiteral();
        } else {
            return "";
        }
    }

    pub fn create(allocator: std.mem.Allocator) !*Program {
        const instance = try allocator.create(Program);
        instance.*.statements = try allocator.alloc(Statement, 1);
    }
};

const letStatement = struct {
    token: Token,
    name: *Identifier,
    value: Expression,

    pub fn tokenLiteral(self: *@This()) []const u8 {
        return self.token.literal;
    }
};

const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn tokenLiteral(self: *@This()) []const u8 {
        return self.token.literal;
    }
};

test "Test Nodes" {}
