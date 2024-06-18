const std = @import("std");

const Token = @import("token.zig").Token;

pub const Node = union(enum) {
    Statement: *Statement,
    Expression: *Expression,

    pub fn tokenLiteral(self: Node) []const u8 {
        return switch (self) {
            .Statement => |stmt| stmt.tokenLiteral(),
            .Expression => |expr| expr.tokenLiteral(),
        };
    }
};

pub const Statement = union(enum) {
    LetStatement: *LetStatement,

    pub fn tokenLiteral(self: Statement) []const u8 {
        return switch (self) {
            .LetStatement => |letStmt| letStmt.tokenLiteral(),
        };
    }
};

pub const Expression = union(enum) {
    Identifier: *Identifier,

    pub fn tokenLiteral(self: Expression) []const u8 {
        return switch (self) {
            .Identifier => |ident| ident.tokenLiteral(),
        };
    }
};

pub const Program = struct {
    statements: []Node,

    pub fn init(self: *Program, allocator: std.mem.Allocator) void {
        self.statements = allocator.alloc(Node, 0) catch unreachable;
    }

    pub fn deinit(self: *Program) void {
        self.statements = self.allocator.realloc(self.statements, 0) catch unreachable;
    }

    pub fn tokenLiteral(self: *Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }
};

pub const LetStatement = struct {
    token: Token,
    name: *Identifier,
    value: Expression,

    pub fn tokenLiteral(self: *LetStatement) []const u8 {
        return self.token.literal;
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn tokenLiteral(self: *Identifier) []const u8 {
        return self.token.literal;
    }
};

test "AST example" {
    const allocator = std.testing.allocator;

    var ident = Identifier{
        .token = Token{ .literal = "myVar", .tType = .IDENT },
        .value = "myVar",
    };

    var letStmt = LetStatement{
        .token = Token{ .literal = "let", .tType = .LET },
        .name = &ident,
        .value = Expression{ .Identifier = &ident },
    };

    var program = try allocator.create(Program);

    program.statements = try allocator.alloc(Node, 1);

    program.statements[0] = Node{ .Statement = &letStmt };

    std.debug.print("{s}\n", .{program.tokenLiteral()});
}
