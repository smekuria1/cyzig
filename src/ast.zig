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
        for (self.statements.items) |value| {
            if (count > self.statements.pos) {
                break;
            }
            self.allocator.destroy(value.name);
            self.allocator.destroy(value);
            count += 1;
        }
        // self.statements.deinit();
        self.allocator.free(self.statements.items);
        self.allocator.destroy(self);
    }
};

pub fn GenericAst(comptime T: type) type {
    return struct {
        pos: usize,
        items: []T,
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) !GenericAst(T) {
            return .{
                .pos = 0,
                .allocator = allocator,
                .items = try allocator.alloc(T, 1),
            };
        }

        pub fn deinit(self: *GenericAst(T)) void {
            self.allocator.free(self.items);
        }

        pub fn append(self: *GenericAst(T), value: T) !void {
            const pos = self.pos;
            const len = self.items.len;
            print("\nin apppend value {any}\n", .{value});
            if (pos == 0 and len == 1) {
                self.items[pos] = value;
                self.pos = pos + 1;
                return;
            }
            if (pos == len) {
                var larger = try self.allocator.alloc(T, len * 2);

                @memcpy(larger[0..len], self.items);

                self.allocator.free(self.items);
                self.items = larger;
            }

            self.items[pos] = value;
            self.pos = pos + 1;
        }
    };
}
