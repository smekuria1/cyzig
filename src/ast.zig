const std = @import("std");
const assert = std.debug.assert;
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;
const print = std.debug.print;
const Allocator = std.mem.Allocator;

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
        expression: ?*Expression,

        pub fn string(self: Self, list: *std.ArrayList(u8)) []const u8 {
            if (self.expression != null) {
                switch (self.expression.?.*) {
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
                        const prefixString = prefix.string() catch unreachable;
                        _ = list.writer().write(prefixString) catch unreachable;
                        prefix.allocator.free(prefixString);
                        return "";
                    },
                    .infixExp => {
                        const infix = self.expression.?.infixExp;
                        const infixStrng = infix.string() catch unreachable;
                        _ = list.writer().write(infixStrng) catch unreachable;
                        infix.allocator.free(infixStrng);
                        return "";
                    },
                    .boolean => {
                        const tmp = self.expression.?.boolean.string();
                        _ = list.writer().write(tmp) catch unreachable;
                        return list.writer().context.items;
                    },
                    .ifexp => {
                        const ifexpression = self.expression.?.ifexp;
                        const ifString = ifexpression.string() catch unreachable;
                        _ = list.writer().write(ifString) catch unreachable;
                        ifexpression.allocator.free(ifString);
                        return "";
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
    boolean: Boolean,
    ifexp: IfExpression,

    pub fn init(allocator: Allocator) *Expression {
        const exp = allocator.create(Expression) catch unreachable;
        return exp;
    }

    pub fn deinit(self: *Expression, allocator: Allocator) void {
        switch (self.*) {
            .infixExp => |in| {
                in.left.deinit(allocator);
                in.right.deinit(allocator);
                return allocator.destroy(self);
            },
            .prefixExp => |pf| {
                pf.right.deinit(allocator);
                return allocator.destroy(self);
            },
            .identifier => {
                return allocator.destroy(self);
            },
            .integerLiteral => {
                return allocator.destroy(self);
            },
            .boolean => {
                return allocator.destroy(self);
            },
            .ifexp => |ifexpr| {
                if (ifexpr.consequence) |con| {
                    for (con.statements.items) |value| {
                        switch (value) {
                            .expression => |exp| {
                                exp.expression.?.deinit(allocator);
                            },
                            else => {},
                        }
                    }
                    con.statements.deinit();
                }
                if (ifexpr.alternative) |alt| {
                    for (alt.statements.items) |value| {
                        switch (value) {
                            .expression => |exp| {
                                exp.expression.?.deinit(allocator);
                            },
                            else => {},
                        }
                    }
                    alt.statements.deinit();
                }
                ifexpr.condition.deinit(allocator);
                return allocator.destroy(self);
            },
        }
    }
};

pub const PrefixExpression = struct {
    allocator: Allocator,
    token: Token,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: PrefixExpression) Allocator.Error![]u8 {
        var list = std.ArrayList(u8).init(self.allocator);
        _ = list.writer().write("(") catch unreachable;
        _ = list.writer().write(self.operator) catch unreachable;
        _ = switch (self.right.*) {
            .identifier => |id| list.writer().write(id.string()) catch unreachable,
            .integerLiteral => |int| list.writer().write(int.string()) catch unreachable,
            .infixExp => |in| {
                const infixStrng = in.string() catch unreachable;
                _ = list.writer().write(infixStrng) catch unreachable;
                in.allocator.free(infixStrng);
            },
            .prefixExp => |pf| {
                const prefixStrng = pf.string() catch unreachable;
                _ = list.writer().write(prefixStrng) catch unreachable;
                pf.allocator.free(prefixStrng);
            },
            .boolean => |boo| list.writer().write(boo.string()) catch unreachable,
            .ifexp => |ifexpression| {
                const ifString = ifexpression.string() catch unreachable;
                _ = list.writer().write(ifString) catch unreachable;
                ifexpression.allocator.free(ifString);
            },
        };

        _ = list.writer().write(")") catch unreachable;

        return list.toOwnedSlice();
    }
};

pub const InfixExpression = struct {
    allocator: Allocator,
    token: Token,
    left: *Expression,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: InfixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: InfixExpression) Allocator.Error![]u8 {
        var list = std.ArrayList(u8).init(self.allocator);
        _ = list.writer().write("(") catch unreachable;
        switch (self.left.*) {
            .identifier => |id| _ = list.writer().write(id.string()) catch unreachable,
            .integerLiteral => |int| _ = list.writer().write(int.string()) catch unreachable,
            .prefixExp => |pf| {
                const prefixStrng = pf.string() catch unreachable;
                _ = list.writer().write(prefixStrng) catch unreachable;
                pf.allocator.free(prefixStrng);
            },
            .infixExp => |in| {
                const infixStrng = in.string() catch unreachable;
                _ = list.writer().write(infixStrng) catch unreachable;
                in.allocator.free(infixStrng);
            },
            .boolean => |boo| _ = list.writer().write(boo.string()) catch unreachable,
            .ifexp => |ifexpression| {
                const ifString = ifexpression.string() catch unreachable;
                _ = list.writer().write(ifString) catch unreachable;
                ifexpression.allocator.free(ifString);
            },
        }
        _ = list.writer().write(" ") catch unreachable;
        _ = list.writer().write(self.operator) catch unreachable;
        _ = list.writer().write(" ") catch unreachable;
        switch (self.right.*) {
            .identifier => |id| _ = list.writer().write(id.string()) catch unreachable,
            .integerLiteral => |int| _ = list.writer().write(int.string()) catch unreachable,
            .prefixExp => |pf| {
                const prefixString = pf.string() catch unreachable;
                _ = list.writer().write(prefixString) catch unreachable;
                pf.allocator.free(prefixString);
            },
            .infixExp => |in| {
                const infixStrng = in.string() catch unreachable;
                _ = list.writer().write(infixStrng) catch unreachable;
                in.allocator.free(infixStrng);
            },
            .boolean => |boo| _ = list.writer().write(boo.string()) catch unreachable,
            .ifexp => |ifexpression| {
                const ifString = ifexpression.string() catch unreachable;
                _ = list.writer().write(ifString) catch unreachable;
                ifexpression.allocator.free(ifString);
            },
        }

        _ = list.writer().write(")") catch unreachable;

        return list.toOwnedSlice();
    }
};

pub const IfExpression = struct {
    allocator: Allocator,
    token: Token,
    condition: *Expression,
    consequence: ?BlockStatement,
    alternative: ?BlockStatement,

    pub fn tokenLiteral(self: IfExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: IfExpression) Allocator.Error![]u8 {
        var list = std.ArrayList(u8).init(self.allocator);
        _ = list.writer().write("if") catch unreachable;
        switch (self.condition.*) {
            .identifier => {
                const tmp = self.condition.identifier.string();
                _ = list.writer().write(tmp) catch unreachable;
                // return list.writer().context.items;
            },
            .integerLiteral => {
                const tmp = self.condition.integerLiteral.string();
                _ = list.writer().write(tmp) catch unreachable;
                // return list.writer().context.items;
            },
            .prefixExp => {
                const prefix = self.condition.prefixExp;
                const prefixString = prefix.string() catch unreachable;
                _ = list.writer().write(prefixString) catch unreachable;
                prefix.allocator.free(prefixString);
            },
            .infixExp => {
                const infix = self.condition.infixExp;
                const infixStrng = infix.string() catch unreachable;
                _ = list.writer().write(infixStrng) catch unreachable;
                infix.allocator.free(infixStrng);
            },
            .boolean => {
                const tmp = self.condition.boolean.string();
                _ = list.writer().write(tmp) catch unreachable;
                // return list.writer().context.items;
            },
            .ifexp => |ifexpression| {
                const ifString = ifexpression.string() catch unreachable;
                _ = list.writer().write(ifString) catch unreachable;
                ifexpression.allocator.free(ifString);
            },
        }
        _ = list.writer().write(" ") catch unreachable;
        const conString = self.consequence.?.string() catch unreachable;
        _ = list.writer().write(conString) catch unreachable;
        self.allocator.free(conString);

        if (self.alternative) |alt| {
            _ = list.writer().write(" else ") catch unreachable;
            const altString = alt.string() catch unreachable;
            _ = list.writer().write(altString) catch unreachable;
            self.allocator.free(altString);
        }

        return list.toOwnedSlice();
    }
};

pub const FunctionLiteral = struct {
    allocator: Allocator,
    token: Token,
    parameters: std.ArrayList(Identifier),
    body: ?BlockStatement,

    pub fn tokenLiteral(self: FunctionLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: FunctionLiteral) Allocator![]u8 {
        var list = std.ArrayList(u8).init(self.allocator);
        var params: [1024]u8 = undefined;
        assert(self.parameters.items.len <= 1024);
        for (0.., self.parameters.items) |i, value| {
            params[i] = value.string();
        }
        // params[self.parameters.items.len + 1] = "-";

        _ = list.writer().write(self.tokenLiteral()) catch unreachable;
        _ = list.writer().write("(") catch unreachable;
        //TODO: add comma separator
        _ = list.writer().write(params[0..self.parameters.items.len]) catch unreachable;
        _ = list.writer().write(")") catch unreachable;

        const bodystring = self.body.?.string() catch unreachable;
        _ = list.writer().write(bodystring) catch unreachable;
        self.allocator.free(bodystring);

        return list.toOwnedSlice();
    }
};

pub const BlockStatement = struct {
    allocator: Allocator,
    token: Token,
    statements: std.ArrayList(Statement),

    pub fn tokenLiteral(self: BlockStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: BlockStatement) Allocator.Error![]u8 {
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
        return buff.toOwnedSlice();
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
pub const Boolean = struct {
    token: Token,
    value: bool,

    pub fn tokenLiteral(self: Boolean) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: Boolean) []const u8 {
        return self.token.literal;
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
        for (self.statements.items) |value| {
            switch (value) {
                .expression => {
                    const expr = value.expression.expression;
                    if (expr) |expression| {
                        expression.deinit(self.allocator);
                    }
                },
                else => {
                    continue;
                },
            }
        }
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
