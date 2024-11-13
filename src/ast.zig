const std = @import("std");
const assert = std.debug.assert;
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const Pretty = @import("./pretty.zig");

pub const Node = union(enum) {
    const self = @This();
    statement: Statement,
    expression: Expression,
    program: Program,
    block: BlockStatement,
};

pub const Statement = union(enum) {
    letStatement: LetStatement,
    returnStatement: ReturnStatement,
    expression: ExpressionStatement,

    pub const LetStatement = struct {
        const Self = @This();
        token: Token,
        name: Identifier,
        value: ?*Expression,

        pub fn printValue(self: *Self) void {
            print("{} {} \n", .{ self.token.literal, self.value.?.identifier.value });
        }

        // pub fn deinit(self: *Self) void {
        //     self.value.?.deinit(self.allocator);
        // }

        pub fn string(self: Self, list: *std.ArrayList(u8)) []const u8 {
            _ = list.writer().write(self.token.literal) catch unreachable;
            _ = list.writer().write(" ") catch unreachable;
            _ = list.writer().write(self.name.tokenLiteral()) catch unreachable;
            if (self.value) |value| {
                _ = list.writer().write(" = ") catch unreachable;
                //TODO: add Prefix printing abstract it out before tho
                switch (value.*) {
                    .identifier => {
                        const tmp = self.value.?.identifier.string() catch unreachable;
                        _ = list.writer().write(tmp) catch unreachable;
                        self.value.?.identifier.allocator.free(tmp);
                        return list.writer().context.items;
                    },
                    .integerLiteral => {
                        const tmp = self.value.?.integerLiteral.string() catch unreachable;
                        _ = list.writer().write(tmp) catch unreachable;
                        self.value.?.integerLiteral.allocator.free(tmp);
                        return list.writer().context.items;
                    },
                    .prefixExp => {
                        const prefix = self.value.?.*.prefixExp;
                        const prefixString = prefix.string() catch unreachable;
                        _ = list.writer().write(prefixString) catch unreachable;
                        prefix.allocator.free(prefixString);
                        return "";
                    },
                    .infixExp => {
                        const infix = self.value.?.*.infixExp;
                        const infixStrng = infix.string() catch unreachable;
                        _ = list.writer().write(infixStrng) catch unreachable;
                        infix.allocator.free(infixStrng);
                        return "";
                    },
                    .boolean => {
                        const tmp = self.value.?.*.boolean.string() catch unreachable;
                        _ = list.writer().write(tmp) catch unreachable;
                        self.value.?.boolean.allocator.free(tmp);
                        return list.writer().context.items;
                    },
                    .ifexp => {
                        const ifexpression = self.value.?.*.ifexp;
                        const ifString = ifexpression.string() catch unreachable;
                        _ = list.writer().write(ifString) catch unreachable;
                        ifexpression.allocator.free(ifString);
                    },
                    .function => {
                        const fun = self.value.?.*.function;
                        const funString = fun.string() catch unreachable;
                        _ = list.writer().write(funString) catch unreachable;
                        fun.allocator.free(funString);
                    },
                    .callExpression => {
                        const callexp = self.value.?.*.callExpression;
                        const callString = callexp.string() catch unreachable;
                        _ = list.writer().write(callString) catch unreachable;
                        callexp.allocator.free(callString);
                    },
                    .stringLiteral => {
                        const str = self.value.?.stringLiteral;
                        _ = list.writer().write(str.value) catch unreachable;
                        return list.writer().context.items;
                    },
                    .arrayLiteral => {
                        const arr = self.value.?.arrayLiteral;
                        const arrString = arr.string() catch unreachable;
                        _ = list.writer().write(arrString) catch unreachable;
                        arr.allocator.free(arrString);
                    },
                    .indexExpression => {
                        const index = self.value.?.indexExpression;
                        const indexString = index.string() catch unreachable;
                        _ = list.writer().write(indexString) catch unreachable;
                        index.allocator.free(indexString);
                    },
                    .hashLiteral => {
                        const hash = self.value.?.hashLiteral;
                        const hashString = hash.string() catch unreachable;
                        _ = list.writer().write(hashString) catch unreachable;
                        hash.allocator.free(hashString);
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
        returnValue: ?*Expression,

        pub fn string(self: Self, list: *std.ArrayList(u8)) []const u8 {
            // var buf: [1024]u8 = undefined;
            // _ = std.fmt.bufPrint(&buf, "{s} + ", .{self.token.literal}) catch unreachable;
            list.writer().writeAll(self.token.literal) catch unreachable;
            list.writer().writeAll(" ") catch unreachable;

            if (self.returnValue) |value| {
                switch (value.*) {
                    .identifier => {
                        const tmp = self.returnValue.?.identifier.string() catch unreachable;
                        _ = list.writer().write(tmp) catch unreachable;
                        self.returnValue.?.identifier.allocator.free(tmp);
                        return list.writer().context.items;
                    },
                    .integerLiteral => {
                        const tmp = self.returnValue.?.integerLiteral.string() catch unreachable;
                        _ = list.writer().write(tmp) catch unreachable;
                        self.returnValue.?.integerLiteral.allocator.free(tmp);
                        return list.writer().context.items;
                    },
                    .prefixExp => {
                        const prefix = self.returnValue.?.*.prefixExp;
                        const prefixString = prefix.string() catch unreachable;
                        _ = list.writer().write(prefixString) catch unreachable;
                        prefix.allocator.free(prefixString);
                        return "";
                    },
                    .infixExp => {
                        const infix = self.returnValue.?.*.infixExp;
                        const infixStrng = infix.string() catch unreachable;
                        _ = list.writer().write(infixStrng) catch unreachable;
                        infix.allocator.free(infixStrng);
                        return "";
                    },
                    .boolean => {
                        const tmp = self.returnValue.?.*.boolean.string() catch unreachable;
                        _ = list.writer().write(tmp) catch unreachable;
                        self.returnValue.?.boolean.allocator.free(tmp);
                        return list.writer().context.items;
                    },
                    .ifexp => {
                        const ifexpression = self.returnValue.?.*.ifexp;
                        const ifString = ifexpression.string() catch unreachable;
                        _ = list.writer().write(ifString) catch unreachable;
                        ifexpression.allocator.free(ifString);
                    },
                    .function => {
                        const fun = self.returnValue.?.*.function;
                        const funString = fun.string() catch unreachable;
                        _ = list.writer().write(funString) catch unreachable;
                        fun.allocator.free(funString);
                    },
                    .callExpression => {
                        const callexp = self.returnValue.?.*.callExpression;
                        const callString = callexp.string() catch unreachable;
                        _ = list.writer().write(callString) catch unreachable;
                        callexp.allocator.free(callString);
                    },
                    .stringLiteral => {
                        const str = self.returnValue.?.stringLiteral;
                        _ = list.writer().write(str.value) catch unreachable;
                        return list.writer().context.items;
                    },
                    .arrayLiteral => {
                        const arr = self.returnValue.?.arrayLiteral;
                        const arrString = arr.string() catch unreachable;
                        _ = list.writer().write(arrString) catch unreachable;
                        arr.allocator.free(arrString);
                    },
                    .indexExpression => {
                        const index = self.returnValue.?.indexExpression;
                        const indexString = index.string() catch unreachable;
                        _ = list.writer().write(indexString) catch unreachable;
                        index.allocator.free(indexString);
                    },
                    .hashLiteral => {
                        const hash = self.returnValue.?.hashLiteral;
                        const hashString = hash.string() catch unreachable;
                        _ = list.writer().write(hashString) catch unreachable;
                        hash.allocator.free(hashString);
                    },
                }
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
                        const tmp = self.expression.?.identifier.string() catch unreachable;
                        _ = list.writer().write(tmp) catch unreachable;
                        self.expression.?.identifier.allocator.free(tmp);
                        return list.writer().context.items;
                    },
                    .integerLiteral => {
                        const tmp = self.expression.?.integerLiteral.string() catch unreachable;
                        _ = list.writer().write(tmp) catch unreachable;
                        self.expression.?.integerLiteral.allocator.free(tmp);
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
                        const tmp = self.expression.?.boolean.string() catch unreachable;
                        _ = list.writer().write(tmp) catch unreachable;
                        self.expression.?.boolean.allocator.free(tmp);
                        return list.writer().context.items;
                    },
                    .ifexp => {
                        const ifexpression = self.expression.?.ifexp;
                        const ifString = ifexpression.string() catch unreachable;
                        _ = list.writer().write(ifString) catch unreachable;
                        ifexpression.allocator.free(ifString);
                    },
                    .function => {
                        const fun = self.expression.?.function;
                        const funString = fun.string() catch unreachable;
                        _ = list.writer().write(funString) catch unreachable;
                        fun.allocator.free(funString);
                    },
                    .callExpression => {
                        const callexp = self.expression.?.callExpression;
                        const callString = callexp.string() catch unreachable;
                        _ = list.writer().write(callString) catch unreachable;
                        callexp.allocator.free(callString);
                    },

                    .stringLiteral => {
                        const str = self.expression.?.stringLiteral;
                        _ = list.writer().write(str.value) catch unreachable;
                        return list.writer().context.items;
                    },
                    .arrayLiteral => {
                        const arr = self.expression.?.arrayLiteral;
                        const arrString = arr.string() catch unreachable;
                        _ = list.writer().write(arrString) catch unreachable;
                        arr.allocator.free(arrString);
                    },
                    .indexExpression => {
                        const index = self.expression.?.indexExpression;
                        const indexString = index.string() catch unreachable;
                        _ = list.writer().write(indexString) catch unreachable;
                        index.allocator.free(indexString);
                    },
                    .hashLiteral => {
                        const hash = self.expression.?.hashLiteral;
                        const hashString = hash.string() catch unreachable;
                        _ = list.writer().write(hashString) catch unreachable;
                        hash.allocator.free(hashString);
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
    function: FunctionLiteral,
    callExpression: CallExpression,
    stringLiteral: StringLiteral,
    arrayLiteral: ArrayLiteral,
    indexExpression: IndexExpression,
    hashLiteral: HashLiteral,

    pub fn init(allocator: Allocator) *Expression {
        const exp = allocator.create(Expression) catch unreachable;
        return exp;
    }

    pub fn string(self: *Expression) Allocator.Error![]u8 {
        switch (self.*) {
            inline else => |case| {
                return case.string();
            },
        }
    }

    pub fn deinit(self: *Expression, allocator: Allocator) void {
        switch (self.*) {
            .infixExp => |in| {
                in.left.deinit(allocator);
                in.right.deinit(allocator);
                return allocator.destroy(self);
            },
            .prefixExp => |pf| {
                // Pretty.print(allocator, self, .{ .max_depth = 40 }) catch unreachable;
                // std.debug.print("Self in deinit {any} \n", .{self});
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
                                if (exp.expression) |expr| {
                                    expr.deinit(allocator);
                                }
                            },
                            .returnStatement => |ret| {
                                if (ret.returnValue) |retval| {
                                    retval.deinit(allocator);
                                }
                            },
                            .letStatement => |let| {
                                if (let.value) |letval| {
                                    letval.deinit(allocator);
                                }
                            },
                        }
                    }
                    con.statements.deinit();
                }
                if (ifexpr.alternative) |alt| {
                    for (alt.statements.items) |value| {
                        switch (value) {
                            .expression => |exp| {
                                if (exp.expression) |expr| {
                                    expr.deinit(allocator);
                                }
                            },
                            .returnStatement => |ret| {
                                if (ret.returnValue) |retval| {
                                    retval.deinit(allocator);
                                }
                            },
                            .letStatement => |let| {
                                if (let.value) |letval| {
                                    letval.deinit(allocator);
                                }
                            },
                        }
                    }
                    alt.statements.deinit();
                }
                ifexpr.condition.deinit(allocator);
                return allocator.destroy(self);
            },
            .function => |fun| {
                if (fun.parameters) |params| {
                    params.deinit();
                }
                if (fun.body) |body| {
                    for (body.statements.items) |value| {
                        switch (value) {
                            .expression => |exp| {
                                exp.expression.?.deinit(allocator);
                            },
                            else => {},
                        }
                    }
                    body.statements.deinit();
                }
                return allocator.destroy(self);
            },
            .callExpression => |call| {
                call.function.deinit(allocator);
                for (call.arguments.?.items) |value| {
                    value.deinit(allocator);
                }
                call.arguments.?.deinit();

                return allocator.destroy(self);
            },
            .arrayLiteral => |arr| {
                if (arr.elements) |elements| {
                    for (elements.items) |value| {
                        value.deinit(allocator);
                    }
                    elements.deinit();
                }
                return allocator.destroy(self);
            },
            .indexExpression => |indexExpr| {
                if (indexExpr.index) |index| {
                    index.deinit(allocator);
                }
                indexExpr.left.deinit(allocator);
                return allocator.destroy(self);
            },
            .hashLiteral => |hash| {
                // hash.pairs.deinit();
                var iter = hash.pairs.iterator();
                while (iter.next()) |entry| {
                    entry.key_ptr.*.deinit(allocator);
                    entry.value_ptr.*.deinit(allocator);
                }
                //TODO: FIX THIS SHIT
                var x = @constCast(&hash.pairs);
                x.deinit();
                return allocator.destroy(self);
            },
            else => {
                return allocator.destroy(self);
            },
        }
    }
};

pub const HashLiteral = struct {
    token: Token,
    allocator: Allocator,
    pairs: std.AutoHashMap(*Expression, *Expression),

    pub fn tokenLiteral(self: HashLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: HashLiteral) Allocator.Error![]u8 {
        var list = std.ArrayList(u8).init(self.allocator);
        var iter = self.pairs.iterator();
        try list.writer().writeAll("{");
        while (iter.next()) |entry| {
            const k = try entry.key_ptr.*.string();
            const v = try entry.value_ptr.*.string();

            _ = try list.writer().write(k);
            _ = try list.writer().write(":");
            _ = try list.writer().write(v);
            _ = try list.writer().write(",");
            self.allocator.free(k);
            self.allocator.free(v);
        }
        try list.writer().writeAll("}");
        return list.toOwnedSlice();
    }
};

pub const StringLiteral = struct {
    token: Token,
    value: []const u8,
    allocator: Allocator,

    pub fn tokenLiteral(self: StringLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: StringLiteral) Allocator.Error![]u8 {

        // Allocate a mutable array with the same length as self.value
        const buffer = try self.allocator.alloc(u8, self.value.len);
        // Copy contents from self.value ([]const u8) to buffer ([]u8)
        @memcpy(buffer, self.value);
        return buffer;
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
        try list.writer().writeAll("(");
        try list.writer().writeAll(self.operator);
        const rightstr = try self.right.string();
        try list.writer().writeAll(rightstr);
        self.allocator.free(rightstr);
        // _ = switch (self.right.*) {
        //     .identifier => |id| {
        //         const tmp = try id.string();
        //         try list.writer().writeAll(tmp);
        //         id.allocator.free(tmp);
        //     },
        //     .integerLiteral => |int| {
        //         const tmp = try int.string();
        //         try list.writer().writeAll(tmp);
        //         int.allocator.free(tmp);
        //     },
        //     .infixExp => |in| {
        //         const infixStrng = in.string() catch unreachable;
        //         _ = list.writer().write(infixStrng) catch unreachable;
        //         in.allocator.free(infixStrng);
        //     },
        //     .prefixExp => |pf| {
        //         const prefixStrng = pf.string() catch unreachable;
        //         _ = list.writer().write(prefixStrng) catch unreachable;
        //         pf.allocator.free(prefixStrng);
        //     },
        //     .boolean => |boo| {
        //         const tmp = try boo.string();
        //         try list.writer().writeAll(tmp);
        //         boo.allocator.free(tmp);
        //     },
        //     .ifexp => |ifexpression| {
        //         const ifString = ifexpression.string() catch unreachable;
        //         _ = list.writer().write(ifString) catch unreachable;
        //         ifexpression.allocator.free(ifString);
        //     },
        //     .function => |fun| {
        //         const funString = fun.string() catch unreachable;
        //         _ = list.writer().write(funString) catch unreachable;
        //         fun.allocator.free(funString);
        //     },
        //     .callExpression => |call| {
        //         const callstring = call.string() catch unreachable;
        //         _ = list.writer().write(callstring) catch unreachable;
        //         call.allocator.free(callstring);
        //     },
        //     .stringLiteral => |str| {
        //         _ = list.writer().write(str.value) catch unreachable;
        //     },
        //     .arrayLiteral => |arr| {
        //         const arrString = arr.string() catch unreachable;
        //         _ = list.writer().write(arrString) catch unreachable;
        //         arr.allocator.free(arrString);
        //     },
        //     .indexExpression => |index| {
        //         const indexString = index.string() catch unreachable;
        //         _ = list.writer().write(indexString) catch unreachable;
        //         index.allocator.free(indexString);
        //     },
        //     .hashLiteral => |hash| {
        //         const hashStr = hash.string() catch unreachable;
        //         _ = list.writer().write(hashStr) catch unreachable;
        //         hash.allocator.free(hashStr);
        //     },
        // };
        //
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
        const leftstr = try self.left.string();
        try list.writer().writeAll(leftstr);
        self.allocator.free(leftstr);
        // switch (self.left.*) {
        //     .identifier => |id| {
        //         const tmp = try id.string();
        //         try list.writer().writeAll(tmp);
        //         id.allocator.free(tmp);
        //     },
        //     .integerLiteral => |int| {
        //         const tmp = try int.string();
        //         try list.writer().writeAll(tmp);
        //         int.allocator.free(tmp);
        //     },
        //     .infixExp => |in| {
        //         const infixStrng = in.string() catch unreachable;
        //         _ = list.writer().write(infixStrng) catch unreachable;
        //         in.allocator.free(infixStrng);
        //     },
        //     .prefixExp => |pf| {
        //         const prefixStrng = pf.string() catch unreachable;
        //         _ = list.writer().write(prefixStrng) catch unreachable;
        //         pf.allocator.free(prefixStrng);
        //     },
        //     .boolean => |boo| {
        //         const tmp = try boo.string();
        //         try list.writer().writeAll(tmp);
        //         boo.allocator.free(tmp);
        //     },
        //     .ifexp => |ifexpression| {
        //         const ifString = ifexpression.string() catch unreachable;
        //         _ = list.writer().write(ifString) catch unreachable;
        //         ifexpression.allocator.free(ifString);
        //     },
        //     .function => |fun| {
        //         const funString = fun.string() catch unreachable;
        //         _ = list.writer().write(funString) catch unreachable;
        //         fun.allocator.free(funString);
        //     },
        //     .callExpression => |call| {
        //         const callstring = call.string() catch unreachable;
        //         _ = list.writer().write(callstring) catch unreachable;
        //         call.allocator.free(callstring);
        //     },
        //     .stringLiteral => |str| {
        //         _ = list.writer().write(str.value) catch unreachable;
        //     },
        //     .arrayLiteral => |arr| {
        //         const arrString = arr.string() catch unreachable;
        //         _ = list.writer().write(arrString) catch unreachable;
        //         arr.allocator.free(arrString);
        //     },
        //     .indexExpression => |index| {
        //         const indexString = index.string() catch unreachable;
        //         _ = list.writer().write(indexString) catch unreachable;
        //         index.allocator.free(indexString);
        //     },
        //     .hashLiteral => |hash| {
        //         const hashStr = hash.string() catch unreachable;
        //         _ = list.writer().write(hashStr) catch unreachable;
        //         hash.allocator.free(hashStr);
        //     },
        // }
        try list.writer().writeAll(" ");
        try list.writer().writeAll(self.operator);
        try list.writer().writeAll(" ");
        const rightstr = try self.right.string();
        try list.writer().writeAll(rightstr);
        self.allocator.free(rightstr);
        // switch (self.right.*) {
        //     .identifier => |id| {
        //         const tmp = try id.string();
        //         try list.writer().writeAll(tmp);
        //         id.allocator.free(tmp);
        //     },
        //     .integerLiteral => |int| {
        //         const tmp = try int.string();
        //         try list.writer().writeAll(tmp);
        //         int.allocator.free(tmp);
        //     },
        //     .infixExp => |in| {
        //         const infixStrng = in.string() catch unreachable;
        //         _ = list.writer().write(infixStrng) catch unreachable;
        //         in.allocator.free(infixStrng);
        //     },
        //     .prefixExp => |pf| {
        //         const prefixStrng = pf.string() catch unreachable;
        //         _ = list.writer().write(prefixStrng) catch unreachable;
        //         pf.allocator.free(prefixStrng);
        //     },
        //     .boolean => |boo| {
        //         const tmp = try boo.string();
        //         try list.writer().writeAll(tmp);
        //         boo.allocator.free(tmp);
        //     },
        //     .ifexp => |ifexpression| {
        //         const ifString = ifexpression.string() catch unreachable;
        //         _ = list.writer().write(ifString) catch unreachable;
        //         ifexpression.allocator.free(ifString);
        //     },
        //     .function => |fun| {
        //         const funString = fun.string() catch unreachable;
        //         _ = list.writer().write(funString) catch unreachable;
        //         fun.allocator.free(funString);
        //     },
        //     .callExpression => |call| {
        //         const callstring = call.string() catch unreachable;
        //         _ = list.writer().write(callstring) catch unreachable;
        //         call.allocator.free(callstring);
        //     },
        //     .stringLiteral => |str| {
        //         _ = list.writer().write(str.value) catch unreachable;
        //     },
        //     .arrayLiteral => |arr| {
        //         const arrString = arr.string() catch unreachable;
        //         _ = list.writer().write(arrString) catch unreachable;
        //         arr.allocator.free(arrString);
        //     },
        //     .indexExpression => |index| {
        //         const indexString = index.string() catch unreachable;
        //         _ = list.writer().write(indexString) catch unreachable;
        //         index.allocator.free(indexString);
        //     },
        //     .hashLiteral => |hash| {
        //         const hashStr = hash.string() catch unreachable;
        //         _ = list.writer().write(hashStr) catch unreachable;
        //         hash.allocator.free(hashStr);
        //     },
        // }

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
                const tmp = self.condition.identifier.string() catch unreachable;
                _ = list.writer().write(tmp) catch unreachable;
                // return list.writer().context.items;
            },
            .integerLiteral => {
                const tmp = self.condition.integerLiteral.string() catch unreachable;
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
                const tmp = self.condition.boolean.string() catch unreachable;
                _ = list.writer().write(tmp) catch unreachable;
                // return list.writer().context.items;
            },
            .ifexp => |ifexpression| {
                const ifString = ifexpression.string() catch unreachable;
                _ = list.writer().write(ifString) catch unreachable;
                ifexpression.allocator.free(ifString);
            },
            .function => |fun| {
                const funString = fun.string() catch unreachable;
                _ = list.writer().write(funString) catch unreachable;
                fun.allocator.free(funString);
            },
            .callExpression => |call| {
                const callstring = call.string() catch unreachable;
                _ = list.writer().write(callstring) catch unreachable;
                call.allocator.free(callstring);
            },
            .stringLiteral => |str| {
                _ = list.writer().write(str.value) catch unreachable;
            },
            .arrayLiteral => |arr| {
                const arrString = arr.string() catch unreachable;
                _ = list.writer().write(arrString) catch unreachable;
                arr.allocator.free(arrString);
            },
            .indexExpression => |index| {
                const indexString = index.string() catch unreachable;
                _ = list.writer().write(indexString) catch unreachable;
                index.allocator.free(indexString);
            },
            .hashLiteral => |hash| {
                const hashStr = hash.string() catch unreachable;
                _ = list.writer().write(hashStr) catch unreachable;
                hash.allocator.free(hashStr);
            },
        }
        _ = list.writer().write(" ") catch unreachable;
        if (self.consequence) |conq| {
            const conString = conq.string() catch unreachable;
            _ = list.writer().write(conString) catch unreachable;
            self.allocator.free(conString);
        }

        if (self.alternative) |alt| {
            _ = list.writer().write(" else ") catch unreachable;
            const altString = alt.string() catch unreachable;
            _ = list.writer().write(altString) catch unreachable;
            self.allocator.free(altString);
        }

        return list.toOwnedSlice();
    }
};

pub const IndexExpression = struct {
    token: Token,
    left: *Expression,
    index: ?*Expression,
    allocator: Allocator,

    pub fn string(self: IndexExpression) Allocator.Error![]u8 {
        var list = std.ArrayList(u8).init(self.allocator);

        _ = list.writer().write("(") catch unreachable;
        const leftstr = try self.left.string();
        try list.writer().writeAll(leftstr);
        self.allocator.free(leftstr);
        // switch (self.left.*) {
        //     .identifier => {
        //         const tmp = self.left.identifier.string() catch unreachable;
        //         _ = list.writer().write(tmp) catch unreachable;
        //         // return list.writer().context.items;
        //     },
        //     .integerLiteral => {
        //         const tmp = self.left.integerLiteral.string() catch unreachable;
        //         _ = list.writer().write(tmp) catch unreachable;
        //         // return list.writer().context.items;
        //     },
        //     .prefixExp => {
        //         const prefix = self.left.prefixExp;
        //         const prefixString = prefix.string() catch unreachable;
        //         _ = list.writer().write(prefixString) catch unreachable;
        //         prefix.allocator.free(prefixString);
        //     },
        //     .infixExp => {
        //         const infix = self.left.infixExp;
        //         const infixStrng = infix.string() catch unreachable;
        //         _ = list.writer().write(infixStrng) catch unreachable;
        //         infix.allocator.free(infixStrng);
        //     },
        //     .boolean => {
        //         const tmp = self.left.boolean.string() catch unreachable;
        //         _ = list.writer().write(tmp) catch unreachable;
        //         // return list.writer().context.items;
        //     },
        //     .ifexp => {
        //         const ifexpression = self.left.ifexp;
        //         const ifString = ifexpression.string() catch unreachable;
        //         _ = list.writer().write(ifString) catch unreachable;
        //         ifexpression.allocator.free(ifString);
        //     },
        //     .function => {
        //         const fun = self.left.function;
        //         const funString = fun.string() catch unreachable;
        //         _ = list.writer().write(funString) catch unreachable;
        //         fun.allocator.free(funString);
        //     },
        //     .callExpression => {
        //         const callexp = self.left.callExpression;
        //         const callString = callexp.string() catch unreachable;
        //         _ = list.writer().write(callString) catch unreachable;
        //         callexp.allocator.free(callString);
        //     },
        //     .stringLiteral => {
        //         const str = self.left.stringLiteral;
        //         _ = list.writer().write(str.value) catch unreachable;
        //         // return list.writer().context.items;
        //     },
        //     .arrayLiteral => {
        //         const arr = self.left.arrayLiteral;
        //         const arrString = arr.string() catch unreachable;
        //         _ = list.writer().write(arrString) catch unreachable;
        //         arr.allocator.free(arrString);
        //     },
        //     .indexExpression => {
        //         const index = self.left.indexExpression;
        //         const indexString = index.string() catch unreachable;
        //         _ = list.writer().write(indexString) catch unreachable;
        //         index.allocator.free(indexString);
        //     },
        //     .hashLiteral => |hash| {
        //         const hashStr = hash.string() catch unreachable;
        //         _ = list.writer().write(hashStr) catch unreachable;
        //         hash.allocator.free(hashStr);
        //     },
        // }
        _ = list.writer().write("[") catch unreachable;
        if (self.index) |index| {
            const indexstr = try index.string();
            try list.writer().writeAll(indexstr);
            self.allocator.free(indexstr);
        }
        // if (self.index) |index| {
        //     switch (index.*) {
        //         .identifier => {
        //             const tmp = index.identifier.string() catch unreachable;
        //             _ = list.writer().write(tmp) catch unreachable;
        //             // return list.writer().context.items;
        //         },
        //         .integerLiteral => {
        //             const tmp = index.integerLiteral.string() catch unreachable;
        //             _ = list.writer().write(tmp) catch unreachable;
        //             // return list.writer().context.items;
        //         },
        //         .prefixExp => {
        //             const prefix = index.prefixExp;
        //             const prefixString = prefix.string() catch unreachable;
        //             _ = list.writer().write(prefixString) catch unreachable;
        //             prefix.allocator.free(prefixString);
        //         },
        //         .infixExp => {
        //             const infix = index.infixExp;
        //             const infixStrng = infix.string() catch unreachable;
        //             _ = list.writer().write(infixStrng) catch unreachable;
        //             infix.allocator.free(infixStrng);
        //         },
        //         .boolean => {
        //             const tmp = index.boolean.string() catch unreachable;
        //             _ = list.writer().write(tmp) catch unreachable;
        //             // return list.writer().context.items;
        //         },
        //         .ifexp => {
        //             const ifexpression = index.ifexp;
        //             const ifString = ifexpression.string() catch unreachable;
        //             _ = list.writer().write(ifString) catch unreachable;
        //             ifexpression.allocator.free(ifString);
        //         },
        //         .function => {
        //             const fun = index.function;
        //             const funString = fun.string() catch unreachable;
        //             _ = list.writer().write(funString) catch unreachable;
        //             fun.allocator.free(funString);
        //         },
        //         .callExpression => {
        //             const callexp = index.callExpression;
        //             const callString = callexp.string() catch unreachable;
        //             _ = list.writer().write(callString) catch unreachable;
        //             callexp.allocator.free(callString);
        //         },
        //         .stringLiteral => {
        //             const str = index.stringLiteral;
        //             _ = list.writer().write(str.value) catch unreachable;
        //             // return list.writer().context.items;
        //         },
        //         .arrayLiteral => {
        //             const arr = index.arrayLiteral;
        //             const arrString = arr.string() catch unreachable;
        //             _ = list.writer().write(arrString) catch unreachable;
        //             arr.allocator.free(arrString);
        //         },
        //         .indexExpression => {
        //             const in = index.indexExpression;
        //             const indexString = in.string() catch unreachable;
        //             _ = list.writer().write(indexString) catch unreachable;
        //             in.allocator.free(indexString);
        //         },
        //         .hashLiteral => |hash| {
        //             const hashStr = hash.string() catch unreachable;
        //             _ = list.writer().write(hashStr) catch unreachable;
        //             hash.allocator.free(hashStr);
        //         },
        //     }
        // }
        _ = list.writer().write("]") catch unreachable;
        _ = list.writer().write(")") catch unreachable;
        return list.toOwnedSlice();
    }
};

pub const ArrayLiteral = struct {
    token: Token,
    elements: ?std.ArrayList(*Expression),
    allocator: Allocator,

    pub fn tokenLiteral(self: ArrayLiteral) []const u8 {
        self.token.literal;
    }
    pub fn string(self: ArrayLiteral) Allocator.Error![]u8 {
        var list = std.ArrayList(u8).init(self.allocator);
        _ = list.writer().write("[") catch unreachable;

        if (self.elements) |elements| {
            for (0.., elements.items) |i, value| {
                const elemstr = try value.string();
                try list.writer().writeAll(elemstr);
                self.allocator.free(elemstr);
                // switch (value.*) {
                //     .identifier => |id| _ = list.writer().write(id.string() catch unreachable) catch unreachable,
                //     .integerLiteral => |int| _ = list.writer().write(int.string() catch unreachable) catch unreachable,
                //     .prefixExp => |pf| {
                //         const prefixString = pf.string() catch unreachable;
                //         _ = list.writer().write(prefixString) catch unreachable;
                //         //  argList.append(prefixString) catch unreachable;
                //         pf.allocator.free(prefixString);
                //     },
                //     .infixExp => |in| {
                //         const infixStrng = in.string() catch unreachable;
                //         _ = list.writer().write(infixStrng) catch unreachable;
                //         // argList.append(infixStrng) catch unreachable;
                //         in.allocator.free(infixStrng);
                //     },
                //     .boolean => |boo| _ = list.writer().write(boo.string() catch unreachable) catch unreachable,
                //     .ifexp => |ifexpression| {
                //         const ifString = ifexpression.string() catch unreachable;
                //         _ = list.writer().write(ifString) catch unreachable;
                //         // argList.append(ifString) catch unreachable;
                //         ifexpression.allocator.free(ifString);
                //     },
                //     .function => |fun| {
                //         const funString = fun.string() catch unreachable;
                //         _ = list.writer().write(funString) catch unreachable;
                //         // argList.append(funString) catch unreachable;
                //         fun.allocator.free(funString);
                //     },
                //     .callExpression => |call| {
                //         const callstring = call.string() catch unreachable;
                //         _ = list.writer().write(callstring) catch unreachable;
                //         // argList.append(callstring) catch unreachable;
                //         call.allocator.free(callstring);
                //     },
                //     .stringLiteral => |str| {
                //         _ = list.writer().write(str.value) catch unreachable;
                //     },
                //     .arrayLiteral => |arr| {
                //         const arrString = arr.string() catch unreachable;
                //         _ = list.writer().write(arrString) catch unreachable;
                //         arr.allocator.free(arrString);
                //     },
                //     .indexExpression => |index| {
                //         const indexString = index.string() catch unreachable;
                //         _ = list.writer().write(indexString) catch unreachable;
                //         index.allocator.free(indexString);
                //     },
                //     .hashLiteral => |hash| {
                //         const hashStr = hash.string() catch unreachable;
                //         _ = list.writer().write(hashStr) catch unreachable;
                //         hash.allocator.free(hashStr);
                //     },
                // }

                if (i >= elements.items.len - 1) {
                    continue;
                }
                _ = list.writer().write(", ") catch unreachable;
                // argList.append(",") catch unreachable;
            }
        }

        _ = list.writer().write("]") catch unreachable;
        return list.toOwnedSlice();
    }
};

pub const CallExpression = struct {
    allocator: Allocator,
    token: Token,
    function: *Expression,
    arguments: ?std.ArrayList(*Expression),

    pub fn tokenLiteral(self: CallExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: CallExpression) Allocator.Error![]u8 {
        var list = std.ArrayList(u8).init(self.allocator);
        const funcstr = try self.function.string();
        try list.writer().writeAll(funcstr);
        self.allocator.free(funcstr);

        // switch (self.function.*) {
        //     .identifier => {
        //         const tmp = self.function.identifier.string() catch unreachable;
        //         _ = list.writer().write(tmp) catch unreachable;
        //         // return list.writer().context.items;
        //     },
        //     .integerLiteral => {
        //         const tmp = self.function.integerLiteral.string() catch unreachable;
        //         _ = list.writer().write(tmp) catch unreachable;
        //         // return list.writer().context.items;
        //     },
        //     .prefixExp => {
        //         const prefix = self.function.prefixExp;
        //         const prefixString = prefix.string() catch unreachable;
        //         _ = list.writer().write(prefixString) catch unreachable;
        //         prefix.allocator.free(prefixString);
        //     },
        //     .infixExp => {
        //         const infix = self.function.infixExp;
        //         const infixStrng = infix.string() catch unreachable;
        //         _ = list.writer().write(infixStrng) catch unreachable;
        //         infix.allocator.free(infixStrng);
        //     },
        //     .boolean => {
        //         const tmp = self.function.boolean.string() catch unreachable;
        //         _ = list.writer().write(tmp) catch unreachable;
        //     },
        //     .ifexp => |ifexpression| {
        //         const ifString = ifexpression.string() catch unreachable;
        //         _ = list.writer().write(ifString) catch unreachable;
        //         ifexpression.allocator.free(ifString);
        //     },
        //     .function => |fun| {
        //         const funString = fun.string() catch unreachable;
        //         _ = list.writer().write(funString) catch unreachable;
        //         fun.allocator.free(funString);
        //     },
        //     .callExpression => |call| {
        //         const callstring = call.string() catch unreachable;
        //         _ = list.writer().write(callstring) catch unreachable;
        //         call.allocator.free(callstring);
        //     },
        //     .stringLiteral => |str| {
        //         _ = list.writer().write(str.tokenLiteral()) catch unreachable;
        //         // return list.writer().context.items;
        //     },
        //     .arrayLiteral => |arr| {
        //         const arrString = arr.string() catch unreachable;
        //         _ = list.writer().write(arrString) catch unreachable;
        //         arr.allocator.free(arrString);
        //     },
        //     .indexExpression => |index| {
        //         const indexString = index.string() catch unreachable;
        //         _ = list.writer().write(indexString) catch unreachable;
        //         index.allocator.free(indexString);
        //     },
        //     .hashLiteral => |hash| {
        //         const hashStr = hash.string() catch unreachable;
        //         _ = list.writer().write(hashStr) catch unreachable;
        //         hash.allocator.free(hashStr);
        //     },
        // }
        _ = list.writer().write("(") catch unreachable;
        if (self.arguments) |arguments| {
            for (0.., arguments.items) |i, value| {
                const argstr = try value.string();
                try list.writer().writeAll(argstr);
                self.allocator.free(argstr);
                // switch (value.*) {
                //     .identifier => |id| _ = list.writer().write(id.string() catch unreachable) catch unreachable,
                //     .integerLiteral => |int| _ = list.writer().write(int.string() catch unreachable) catch unreachable,
                //     .prefixExp => |pf| {
                //         const prefixString = pf.string() catch unreachable;
                //         _ = list.writer().write(prefixString) catch unreachable;
                //         //  argList.append(prefixString) catch unreachable;
                //         pf.allocator.free(prefixString);
                //     },
                //     .infixExp => |in| {
                //         const infixStrng = in.string() catch unreachable;
                //         _ = list.writer().write(infixStrng) catch unreachable;
                //         // argList.append(infixStrng) catch unreachable;
                //         in.allocator.free(infixStrng);
                //     },
                //     .boolean => |boo| _ = list.writer().write(boo.string() catch unreachable) catch unreachable,
                //     .ifexp => |ifexpression| {
                //         const ifString = ifexpression.string() catch unreachable;
                //         _ = list.writer().write(ifString) catch unreachable;
                //         // argList.append(ifString) catch unreachable;
                //         ifexpression.allocator.free(ifString);
                //     },
                //     .function => |fun| {
                //         const funString = fun.string() catch unreachable;
                //         _ = list.writer().write(funString) catch unreachable;
                //         // argList.append(funString) catch unreachable;
                //         fun.allocator.free(funString);
                //     },
                //     .callExpression => |call| {
                //         const callstring = call.string() catch unreachable;
                //         _ = list.writer().write(callstring) catch unreachable;
                //         // argList.append(callstring) catch unreachable;
                //         call.allocator.free(callstring);
                //     },
                //     .stringLiteral => |str| {
                //         _ = list.writer().write(str.value) catch unreachable;
                //     },
                //     .arrayLiteral => |arr| {
                //         const arrString = arr.string() catch unreachable;
                //         _ = list.writer().write(arrString) catch unreachable;
                //         arr.allocator.free(arrString);
                //     },
                //     .indexExpression => |index| {
                //         const indexString = index.string() catch unreachable;
                //         _ = list.writer().write(indexString) catch unreachable;
                //         index.allocator.free(indexString);
                //     },
                //     .hashLiteral => |hash| {
                //         const hashStr = hash.string() catch unreachable;
                //         _ = list.writer().write(hashStr) catch unreachable;
                //         hash.allocator.free(hashStr);
                //     },
                // }

                if (i >= self.arguments.?.items.len - 1) {
                    continue;
                }
                _ = list.writer().write(", ") catch unreachable;
                // argList.append(",") catch unreachable;
            }
        }

        _ = list.writer().write(")") catch unreachable;

        return list.toOwnedSlice();
    }
};
pub const FunctionLiteral = struct {
    allocator: Allocator,
    token: Token,
    parameters: ?std.ArrayList(Identifier),
    body: ?BlockStatement,

    pub fn tokenLiteral(self: FunctionLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: FunctionLiteral) Allocator.Error![]u8 {
        var list = std.ArrayList(u8).init(self.allocator);
        var paramlist = std.ArrayList([]const u8).init(self.allocator);
        defer paramlist.deinit();
        // var params: []const u8 = undefined;
        // assert(self.parameters.?.items.len <= 1024);
        for (0.., self.parameters.?.items) |i, value| {
            paramlist.append(value.string() catch unreachable) catch unreachable;
            if (i >= self.parameters.?.items.len - 1) {
                continue;
            }
            paramlist.append(",") catch unreachable;
        }
        // params[self.parameters.items.len + 1] = "-";

        _ = list.writer().write(self.tokenLiteral()) catch unreachable;
        _ = list.writer().write("(") catch unreachable;
        //TODO: add comma separator
        const commaList = paramlist.toOwnedSlice() catch unreachable;
        for (commaList) |value| {
            _ = list.writer().write(value) catch unreachable;
        }
        self.allocator.free(commaList);
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
    allocator: Allocator,
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

    pub fn string(self: IntegerLiteral) Allocator.Error![]u8 {
        // print("In integerLiteral String {any}\n", .{self.token});

        // Allocate a mutable array with the same length as self.value
        const buffer = try self.allocator.alloc(u8, self.tokenLiteral().len);
        // Copy contents from self.value ([]const u8) to buffer ([]u8)
        @memcpy(buffer, self.tokenLiteral());
        return buffer;
    }
};

pub const Identifier = struct {
    allocator: Allocator,
    token: Token,
    value: []const u8,

    pub fn tokenLiteral(self: Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: Identifier) Allocator.Error![]u8 {

        // Allocate a mutable array with the same length as self.value
        const buffer = try self.allocator.alloc(u8, self.value.len);
        // Copy contents from self.value ([]const u8) to buffer ([]u8)
        @memcpy(buffer, self.value);
        return buffer;
    }
};
pub const Boolean = struct {
    allocator: Allocator,
    token: Token,
    value: bool,

    pub fn tokenLiteral(self: Boolean) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: Boolean) Allocator.Error![]u8 {

        // Allocate a mutable array with the same length as self.value
        const buffer = try self.allocator.alloc(u8, self.tokenLiteral().len);
        // Copy contents from self.value ([]const u8) to buffer ([]u8)
        @memcpy(buffer, self.tokenLiteral());
        return buffer;
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
                .letStatement => |let| {
                    if (let.value) |val| {
                        val.deinit(self.allocator);
                    }
                },
                .returnStatement => |ret| {
                    if (ret.returnValue) |retVal| {
                        retVal.deinit(self.allocator);
                    }
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
                    _ = buff.writer().write("\n") catch unreachable;
                },
                .returnStatement => {
                    // print("In returnstatement {any}\n", .{value});
                    _ = value.returnStatement.string(&buff);
                    _ = buff.writer().write("\n") catch unreachable;
                },
                .expression => {
                    // print("In expression {s}\n", .{value.expression.expression.?.identifier.value});
                    _ = value.expression.string(&buff);
                    // _ = buff.writer().write("\n") catch unreachable;
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
