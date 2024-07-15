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

        pub fn string(self: Self, writer: *std.ArrayList(u8).Writer) []const u8 {
            // var buf: [1024]u8 = undefined;
            // _ = std.fmt.bufPrint(&buf, "{s} {s} =", .{ self.token.literal, self.name.tokenLiteral() }) catch unreachable;
            _ = writer.write(self.token.literal) catch unreachable;
            _ = writer.write(" ") catch unreachable;
            _ = writer.write(self.name.tokenLiteral()) catch unreachable;
            if (self.value != null) {
                // _ = std.fmt.bufPrint(&buf, "{s}", .{self.value.?.identifier.string()}) catch unreachable;
                _ = writer.write(" = ") catch unreachable;
                const tmp = self.value.?.identifier.tokenLiteral();
                // print("Writing Expression {s}\n", .{tmp});
                _ = writer.write(tmp) catch unreachable;
            }
            _ = writer.write(";\n") catch unreachable;
            // _ = std.fmt.bufPrint(&buf, ";", .{}) catch unreachable;

            return writer.context.items;
        }
    };

    pub const ReturnStatement = struct {
        const Self = @This();
        token: Token,
        returnValue: ?Expression,

        pub fn string(self: Self, writer: *std.ArrayList(u8).Writer) []const u8 {
            // var buf: [1024]u8 = undefined;
            // _ = std.fmt.bufPrint(&buf, "{s} + ", .{self.token.literal}) catch unreachable;
            writer.writeAll(self.token.literal) catch unreachable;
            writer.writeAll(" ") catch unreachable;

            if (self.returnValue != null) {
                // _ = std.fmt.bufPrint(&buf, "{s}", .{self.returnValue.?.identifier.string()}) catch unreachable;
                writer.writeAll(self.returnValue.?.identifier.string()) catch unreachable;
            }

            // _ = std.fmt.bufPrint(&buf, ";", .{}) catch unreachable;
            writer.writeAll(";\n") catch unreachable;
            return writer.context.items;
        }
    };

    pub const ExpressionStatement = struct {
        const Self = @This();
        token: Token,
        expression: ?Expression,

        pub fn string(self: Self, writer: *std.ArrayList(u8).Writer) []const u8 {
            if (self.expression != null) {
                switch (self.expression.?) {
                    .identifier => {
                        const tmp = self.expression.?.identifier.string();
                        _ = writer.write(tmp) catch unreachable;
                        return writer.context.items;
                    },
                    .integerLiteral => {
                        const tmp = self.expression.?.integerLiteral.string();
                        _ = writer.write(tmp) catch unreachable;
                        return writer.context.items;
                    },
                    .prefixExp => {
                        _ = self.expression.?.prefixExp.string(writer);

                        return writer.context.items;
                    },
                }
            }
            return "";
        }
    };
};

pub const Expression = union(enum) {
    identifier: Identifier,
    integerLiteral: IntegerLiteral,
    prefixExp: PrefixExpression,
};

pub const IntegerLiteral = struct {
    token: Token,
    value: i64,

    // pub fn init(token: Token) IntegerLiteral {
    //     return .{
    //         .token = token,
    //         .value = @intCast(),
    //     };
    // }

    pub fn tokenLiteral(self: IntegerLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: IntegerLiteral) []const u8 {
        // print("In integerLiteral String {}\n", .{self.token});
        return self.token.literal;
    }
};

pub const PrefixExpression = struct {
    token: Token,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: PrefixExpression, writer: *std.ArrayList(u8).Writer) []const u8 {
        _ = writer.write("(") catch unreachable;
        _ = writer.write(self.operator) catch unreachable;
        // print("\nIn PrefixExp Right token {}\n", .{self.right.*.integerLiteral});
        // print("^^ Operator {s}\n", .{self.operator});
        _ = switch (self.right.*) {
            .identifier => |id| writer.write(id.string()) catch unreachable,
            .integerLiteral => |int| writer.write(int.string()) catch unreachable,
            else => {
                _ = writer.write(self.tokenLiteral()) catch unreachable;
            },
        };
        _ = writer.write(")") catch unreachable;

        return writer.context.items;
    }
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

    pub fn string(self: Identifier) []const u8 {
        print("In identifier String {}\n", .{self});
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

    pub fn string(self: *Program) !std.ArrayList(u8) {
        var buff = std.ArrayList(u8).init(self.allocator);
        var writer = buff.writer();
        for (self.statements.items) |value| {
            switch (value) {
                .letStatement => {
                    // print("In letstatement {s}\n", .{value.letStatement.value.?.identifier.string()});
                    _ = value.letStatement.string(&writer);
                },
                .returnStatement => {
                    // print("In returnstatement {any}\n", .{value});
                    _ = value.returnStatement.string(&writer);
                },
                .expression => {
                    // print("In expression {s}\n", .{value.expression.expression.?.identifier.value});
                    _ = value.expression.string(&writer);
                },
            }
        }
        return buff;
    }
};

pub fn GenericAst(comptime T: type) type {
    return struct {
        list: []T,
        pos: usize = 0,
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) !GenericAst(T) {
            return .{
                .allocator = allocator,
                .list = try allocator.alloc(T, 32),
            };
        }

        pub fn deinit(self: *GenericAst(T)) void {
            self.allocator.free(self.list);
        }

        // pub fn append(self: *GenericAst(T), value: T) void {
        //     try self.list.append(value);
        // }

        pub fn insert(self: *GenericAst(T), value: T, pos: usize) void {
            self.list[pos] = value;
        }
    };
}
