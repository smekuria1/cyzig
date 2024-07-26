const std = @import("std");
const assert = std.debug.assert;
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

        pub fn string(self: Self, list: *std.ArrayList(u8)) []const u8 {
            _ = list.writer().write(self.token.literal) catch unreachable;
            _ = list.writer().write(" ") catch unreachable;
            _ = list.writer().write(self.name.tokenLiteral()) catch unreachable;
            if (self.value) |value| {
                _ = list.writer().write(" = ") catch unreachable;
                //TODO: add Prefix printing abstract it out before tho
                switch (value) {
                    .identifier => {
                        const tmp = self.value.?.identifier.string();
                        _ = list.writer().write(tmp) catch unreachable;
                        return list.writer().context.items;
                    },
                    .integerLiteral => {
                        const tmp = self.value.?.integerLiteral.string();
                        _ = list.writer().write(tmp) catch unreachable;
                        return list.writer().context.items;
                    },
                    else => {
                        return "";
                    },
                }
            }
            _ = list.writer().write(";\n") catch unreachable;
            // _ = std.fmt.bufPrint(&buf, ";", .{}) catch unreachable;

            return list.writer().context.items;
        }
    };

    pub const ReturnStatement = struct {
        const Self = @This();
        token: Token,
        returnValue: ?Expression,

        pub fn string(self: Self, list: *std.ArrayList(u8)) []const u8 {
            // var buf: [1024]u8 = undefined;
            // _ = std.fmt.bufPrint(&buf, "{s} + ", .{self.token.literal}) catch unreachable;
            list.writer().writeAll(self.token.literal) catch unreachable;
            list.writer().writeAll(" ") catch unreachable;

            if (self.returnValue != null) {
                // _ = std.fmt.bufPrint(&buf, "{s}", .{self.returnValue.?.identifier.string()}) catch unreachable;
                list.writer().writeAll(self.returnValue.?.identifier.string()) catch unreachable;
            }

            // _ = std.fmt.bufPrint(&buf, ";", .{}) catch unreachable;
            list.writer().writeAll(";\n") catch unreachable;
            return list.writer().context.items;
        }
    };

    pub const ExpressionStatement = struct {
        const Self = @This();
        token: Token,
        expression: ?Expression,

        pub fn string(self: Self, list: *std.ArrayList(u8)) []const u8 {
            if (self.expression != null) {
                switch (self.expression.?) {
                    .identifier => {
                        const tmp = self.expression.?.identifier.string();
                        _ = list.writer().write(tmp) catch unreachable;
                        return list.writer().context.items;
                    },
                    .integerLiteral => {
                        const tmp = self.expression.?.integerLiteral.string();
                        _ = list.writer().write(tmp) catch unreachable;
                        return list.writer().context.items;
                    },
                    .prefixExp => {
                        const prefix = self.expression.?.prefixExp;
                        _ = list.writer().write("(") catch unreachable;
                        // print("In ExpStmt before acesss String prefix {any}\n", .{prefix});
                        std.log.debug("using prefix {any}", .{prefix});
                        _ = list.writer().write(prefix.operator) catch unreachable;
                        // print("In ExpStmt after acesss String prefix {any}\n", .{prefix});
                        std.log.debug("using prefix {any}", .{prefix});
                        _ = switch (prefix.right.*) {
                            .identifier => |id| list.writer().write(id.string()) catch unreachable,
                            .integerLiteral => |int| list.writer().write(int.string()) catch unreachable,
                            else => {
                                _ = list.writer().write(prefix.tokenLiteral()) catch unreachable;
                            },
                        };
                        _ = list.writer().write(")") catch unreachable;

                        return list.writer().context.items;
                    },
                    .infixExp => {
                        const infix = self.expression.?.infixExp;

                        _ = list.writer().write("(") catch unreachable;
                        std.log.debug("using infix {any}", .{infix});
                        _ = switch (infix.left.*) {
                            .identifier => |id| list.writer().write(id.string()) catch unreachable,
                            .integerLiteral => |int| list.writer().write(int.string()) catch unreachable,
                            else => {
                                _ = list.writer().write(infix.tokenLiteral()) catch unreachable;
                            },
                        };
                        _ = list.writer().write(infix.operator) catch unreachable;
                        _ = switch (infix.right.*) {
                            .identifier => |id| list.writer().write(id.string()) catch unreachable,
                            .integerLiteral => |int| list.writer().write(int.string()) catch unreachable,
                            else => {
                                _ = list.writer().write(infix.tokenLiteral()) catch unreachable;
                            },
                        };
                        _ = list.writer().write(")") catch unreachable;

                        return list.writer().context.items;
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
    infixExp: InfixExpression,
};

pub const PrefixExpression = struct {
    token: Token,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: PrefixExpression) []const u8 {
        return self.token.literal;
    }
};

pub const InfixExpression = struct {
    token: Token,
    left: *Expression,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: InfixExpression) []const u8 {
        return self.token.literal;
    }
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
        // print("In integerLiteral String {any}\n", .{self.token});
        return self.token.literal;
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
        // print("In identifier String {}\n", .{self});
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
        for (self.statements.items) |value| {
            switch (value) {
                .letStatement => {
                    // print("In letstatement {s}\n", .{value.letStatement.value.?.identifier.string()});
                    _ = value.letStatement.string(&buff);
                },
                .returnStatement => {
                    // print("In returnstatement {any}\n", .{value});
                    _ = value.returnStatement.string(&buff);
                },
                .expression => {
                    // print("In expression {s}\n", .{value.expression.expression.?.identifier.value});
                    _ = value.expression.string(&buff);
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
        filled: std.AutoHashMap(usize, bool),
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) !GenericAst(T) {
            return .{
                .allocator = allocator,
                .filled = std.AutoHashMap(usize, bool).init(allocator),
                .list = try allocator.alloc(T, 32),
            };
        }

        pub fn deinit(self: *GenericAst(T)) void {
            self.allocator.free(self.list);
            self.filled.deinit();
        }

        // pub fn append(self: *GenericAst(T), value: T) void {
        //     try self.list.append(value);
        // }

        pub fn insert(self: *GenericAst(T), value: T, pos: usize) void {
            self.list[pos] = value;
            self.filled.put(pos, true) catch unreachable;
        }
    };
}
