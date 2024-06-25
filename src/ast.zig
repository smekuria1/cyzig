const std = @import("std");
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;
const print = std.debug.print;

pub const Node = union(enum) {
    Statement: Statement,
    Expression: Expression,

    pub fn tokenLiteral(self: *Node) []const u8 {
        switch (self.*) {
            inline else => |*case| case.tokenLiteral(),
        }
    }
};

pub const Statement = union(enum) {
    LetStatement: LetStatement,

    pub fn tokenLiteral(self: *Statement) []const u8 {
        switch (self.*) {
            inline else => |*case| case.tokenLiteral(),
        }
    }
};

pub const Expression = struct {};

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

pub const Program = struct {
    statements: GenericAst(*LetStatement),
    allocator: std.mem.Allocator,

    pub fn tokenLiteral(self: *Program) []const u8 {
        if (self.Statements.len > 0) {
            return self.Statements[0].tokenLiteral();
        } else {
            return "";
        }
    }

    pub fn init(allocator: std.mem.Allocator) *Program {
        var program = allocator.create(Program) catch unreachable;
        const astslice = GenericAst(*LetStatement).init(allocator) catch unreachable;
        program.statements = astslice;
        program.allocator = allocator;

        return program;
    }

    pub fn deinit(self: *Program) void {
        var count: usize = 0;
        for (self.statements.list.items) |value| {
            self.allocator.destroy(value.name);
            self.allocator.destroy(value);
            count += 1;
        }
        // self.statements.deinit();
        self.statements.list.deinit();
        self.allocator.destroy(self);
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
