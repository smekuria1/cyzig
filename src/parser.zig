const std = @import("std");
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;
const Lexer = @import("./lexer.zig").Lexer;
const Ast = @import("./ast.zig");
const Allocator = std.mem.Allocator;
const Pretty = @import("./pretty.zig");
const string = []const u8;
const prefixParseFn = *const fn (self: *Parser) ?*Ast.Expression;
const infixParseFn = *const fn (slef: *Parser, exp: *Ast.Expression) ?*Ast.Expression;
const Precedence = enum {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
};

fn initPrecedenceTable(table: *std.AutoHashMap(TokenType, Precedence)) void {
    table.put(.EQ, .EQUALS) catch unreachable;
    table.put(.NOT_EQ, .EQUALS) catch unreachable;
    table.put(.LT, .LESSGREATER) catch unreachable;
    table.put(.GT, .LESSGREATER) catch unreachable;
    table.put(.MINUS, .SUM) catch unreachable;
    table.put(.PLUS, .SUM) catch unreachable;
    table.put(.SLASH, .PRODUCT) catch unreachable;
    table.put(.ASTERISK, .PRODUCT) catch unreachable;
}
pub const Parser = struct {
    l: *Lexer,
    curToken: Token,
    peekToken: Token,
    errors: std.ArrayList(string),
    allocator: Allocator,

    precedences: std.AutoHashMap(TokenType, Precedence),
    prefixParseFns: Ast.GenericAst(prefixParseFn),
    infixParseFns: Ast.GenericAst(infixParseFn),

    pub fn init(allocator: Allocator, l: *Lexer) *Parser {
        var parser = allocator.create(Parser) catch unreachable;
        parser.allocator = allocator;
        parser.l = l;
        const err = std.ArrayList(string).init(allocator);
        parser.errors = err;
        parser.nextToken(l.arenaAlloc.allocator());
        parser.nextToken(l.arenaAlloc.allocator());
        parser.prefixParseFns = Ast.GenericAst(prefixParseFn).init(allocator) catch unreachable;
        parser.registerPrefix(.IDENT, parseIdentifier) catch unreachable;
        parser.registerPrefix(.INT, parseIntegerLiteral) catch unreachable;
        parser.registerPrefix(.BANG, parsePrefixExpression) catch unreachable;
        parser.registerPrefix(.MINUS, parsePrefixExpression) catch unreachable;
        parser.registerPrefix(.TRUE, parseBoolean) catch unreachable;
        parser.registerPrefix(.FALSE, parseBoolean) catch unreachable;
        parser.registerPrefix(.LPAREN, parseGroupedExpressions) catch unreachable;

        parser.infixParseFns = Ast.GenericAst(infixParseFn).init(allocator) catch unreachable;
        parser.registerInfix(.PLUS, parseInfixExpression) catch unreachable;
        parser.registerInfix(.MINUS, parseInfixExpression) catch unreachable;
        parser.registerInfix(.SLASH, parseInfixExpression) catch unreachable;
        parser.registerInfix(.ASTERISK, parseInfixExpression) catch unreachable;
        parser.registerInfix(.EQ, parseInfixExpression) catch unreachable;
        parser.registerInfix(.NOT_EQ, parseInfixExpression) catch unreachable;
        parser.registerInfix(.LT, parseInfixExpression) catch unreachable;
        parser.registerInfix(.GT, parseInfixExpression) catch unreachable;

        parser.precedences = std.AutoHashMap(TokenType, Precedence).init(parser.allocator);
        initPrecedenceTable(&parser.precedences);
        return parser;
    }

    pub fn registerPrefix(self: *Parser, tt: TokenType, func: prefixParseFn) !void {
        self.prefixParseFns.insert(func, @intFromEnum(tt));
    }

    pub fn registerInfix(self: *Parser, tt: TokenType, func: infixParseFn) !void {
        self.infixParseFns.insert(func, @intFromEnum(tt));
    }

    pub fn peekPrecedence(self: *Parser) Precedence {
        const prec = self.precedences.get(self.peekToken.tType);
        if (prec != null) {
            return prec.?;
        }
        return Precedence.LOWEST;
    }

    pub fn currPrecedence(self: *Parser) Precedence {
        const prec = self.precedences.get(self.curToken.tType);
        if (prec != null) {
            return prec.?;
        }

        return Precedence.LOWEST;
    }

    pub fn deinit(self: *Parser) void {
        for (self.errors.items) |value| {
            self.allocator.free(value);
        }
        self.errors.deinit();
        self.prefixParseFns.deinit();
        self.infixParseFns.deinit();
        self.precedences.deinit();
        self.allocator.destroy(self);
    }

    pub fn nextToken(self: *Parser, arenaAllocator: Allocator) void {
        self.curToken = self.peekToken;
        self.peekToken = self.l.nextToken(arenaAllocator).?;
    }

    pub fn Errors(self: *Parser) []string {
        return self.errors;
    }

    pub fn checkParserErros(self: *Parser) bool {
        const err = self.errors;
        if (err.items.len <= 0) {
            return false;
        }

        std.debug.print("\nParser had {d} errors\n", .{err.items.len});
        for (err.items) |value| {
            std.debug.print("{s}\n", .{value});
        }
        return true;
    }

    pub fn peekError(self: *Parser, t: TokenType) void {
        const msg = std.fmt.allocPrint(self.allocator, "expected next token to be {}, got {s} instead", .{ t, self.peekToken.literal }) catch unreachable;
        self.errors.append(msg) catch unreachable;
    }

    pub fn parseProgram(self: *Parser) Ast.Program {
        var program = Ast.Program.init(self.allocator);
        const arena = self.l.arenaAlloc.allocator();
        while (self.curToken.tType != .EOF) {
            // std.log.debug("{any} -- {s}\n", .{ self.curToken.tType, self.curToken.literal });
            const stmt = self.parseStatement();
            if (stmt) |s| {
                program.statements.append(s) catch unreachable;
            }
            self.nextToken(arena);
        }

        return program;
    }

    pub fn parseIdentifier(self: *Parser) ?*Ast.Expression {
        const exp = Ast.Expression.init(self.allocator);
        exp.* = Ast.Expression{
            .identifier = Ast.Identifier{
                .token = self.curToken,
                .value = self.curToken.literal,
            },
        };
        return exp;
    }

    pub fn parseBoolean(self: *Parser) *Ast.Expression {
        const exp = Ast.Expression.init(self.allocator);
        exp.* = Ast.Expression{
            .boolean = Ast.Boolean{
                .token = self.curToken,
                .value = self.curTokenIs(.TRUE),
            },
        };
        return exp;
    }

    pub fn parseStatement(self: *Parser) ?Ast.Statement {
        switch (self.curToken.tType) {
            .LET => return self.parseLetStatement(),
            .RETURN => return self.parseReturnStatement(),
            else => return self.parseExpressionStatement(),
        }
    }

    pub fn parseExpressionStatement(self: *Parser) ?Ast.Statement {
        var stmt = Ast.Statement.ExpressionStatement{
            .token = self.curToken,
            .expression = undefined,
        };
        const exp = self.parseExpression(.LOWEST);
        if (exp) |expression| {
            stmt.expression = expression;
        } else {
            stmt.expression = null;
        }

        if (self.peekTokenIs(.SEMICOLON)) {
            self.nextToken(self.l.arenaAlloc.allocator());
        }

        return Ast.Statement{ .expression = stmt };
    }

    pub fn noPrefixParseFnError(self: *Parser, t: TokenType) void {
        const msg = std.fmt.allocPrint(self.allocator, "no prefix parse function for {} found", .{t}) catch unreachable;
        self.errors.append(msg) catch unreachable;
    }

    pub fn parseIntegerLiteral(self: *Parser) ?*Ast.Expression {
        var lit = Ast.IntegerLiteral{
            .token = self.curToken,
            .value = undefined,
        };
        const value = std.fmt.parseInt(i64, self.curToken.literal, 10) catch unreachable;
        errdefer {
            const msg = std.fmt.allocPrint(self.allocator, "Could not parse {} as integer", .{self.curToken.literal}) catch unreachable;
            self.errors.append(msg) catch unreachable;
            lit = null;
        }
        lit.value = value;

        const exp = Ast.Expression.init(self.allocator);
        exp.* = Ast.Expression{
            .integerLiteral = lit,
        };

        return exp;
    }

    pub fn parseInfixExpression(self: *Parser, left: *Ast.Expression) ?*Ast.Expression {
        var expression = Ast.InfixExpression{
            .token = self.curToken,
            .allocator = self.allocator,
            .operator = self.curToken.literal,
            .left = left,
            .right = undefined,
        };

        const prec = self.currPrecedence();
        self.nextToken(self.l.arenaAlloc.allocator());
        const right = self.parseExpression(prec).?;
        expression.right = right;
        // const exp = self.allocator.create(Ast.Expression) catch unreachable;
        const exp = Ast.Expression.init(self.allocator);
        exp.* = Ast.Expression{
            .infixExp = expression,
        };
        return exp;
    }

    pub fn parsePrefixExpression(self: *Parser) ?*Ast.Expression {
        var expression = Ast.PrefixExpression{
            .token = self.curToken,
            .allocator = self.allocator,
            .operator = self.curToken.literal,
            .right = undefined,
        };

        self.nextToken(self.l.arenaAlloc.allocator());
        expression.right = self.parseExpression(.LOWEST).?;
        const exp = Ast.Expression.init(self.allocator);
        exp.* = Ast.Expression{
            .prefixExp = expression,
        };
        return exp;
    }

    pub fn parseGroupedExpressions(self: *Parser) ?*Ast.Expression {
        self.nextToken(self.l.arenaAlloc.allocator());

        const exp = self.parseExpression(.LOWEST);

        if (!self.expectPeek(.RPAREN)) {
            return null;
        }
        if (exp) |expression| {
            //
            return expression;
        }

        return null;
    }

    pub fn printTables(self: *Parser) void {
        var iterator = self.prefixParseFns.filled.iterator();
        while (iterator.next()) |entry| {
            std.debug.print("{any} ---", .{@as(TokenType, @enumFromInt(entry.key_ptr.*))});
            std.debug.print("{any}\n", .{self.prefixParseFns.list[entry.key_ptr.*]});
        }

        var iterator2 = self.infixParseFns.filled.iterator();
        while (iterator2.next()) |entry2| {
            std.debug.print("{any} ---", .{@as(TokenType, @enumFromInt(entry2.key_ptr.*))});
            std.debug.print("{any}\n", .{self.infixParseFns.list[entry2.key_ptr.*]});
        }
    }

    pub fn parseExpression(self: *Parser, prec: Precedence) ?*Ast.Expression {
        // self.printTables();
        /////////TODO:
        // std.debug.print("{any}\n", .{self.curToken.tType});
        // std.debug.print("\nParse Funtion {any}\n", .{self.prefixParseFns.list[6]});
        const prefExists = self.prefixParseFns.filled.get(@intFromEnum(self.curToken.tType));
        if (prefExists == null) {
            self.noPrefixParseFnError(self.curToken.tType);
            return null;
        }
        const prefix = self.prefixParseFns.list[@intFromEnum(self.curToken.tType)];

        var leftexp = prefix(self).?;

        while (!self.peekTokenIs(.SEMICOLON) and @intFromEnum(prec) < @intFromEnum(self.peekPrecedence())) {
            const inExists = self.infixParseFns.filled.get(@intFromEnum(self.peekToken.tType));
            if (inExists == null) {
                return leftexp;
            }
            const infix = self.infixParseFns.list[@intFromEnum(self.peekToken.tType)];

            self.nextToken(self.l.arenaAlloc.allocator());

            leftexp = infix(self, leftexp).?;
        }
        // std.debug.print("In Parse Expression {s}\n", .{leftexp.identifier.string()});
        return leftexp;
    }

    pub fn parseReturnStatement(self: *Parser) ?Ast.Statement {
        const stmt = Ast.Statement.ReturnStatement{
            .token = self.curToken,
            .returnValue = undefined,
        };

        self.nextToken(self.l.arenaAlloc.allocator());

        // TODO: skipping expressions until semicolon
        while (!self.curTokenIs(.SEMICOLON)) {
            self.nextToken(self.l.arenaAlloc.allocator());
        }

        return Ast.Statement{ .returnStatement = stmt };
    }

    pub fn parseLetStatement(self: *Parser) ?Ast.Statement {
        var stmt = Ast.Statement.LetStatement{
            .token = self.curToken,
            .name = undefined,
            .value = null,
        };

        if (!self.expectPeek(.IDENT)) {
            return null;
        }
        stmt.name = Ast.Identifier.init(self.curToken);
        if (!self.expectPeek(.ASSIGN)) {
            return null;
        }

        while (!self.curTokenIs(.SEMICOLON)) {
            self.nextToken(self.l.arenaAlloc.allocator());
        }

        // self.nextToken(self.l.arenaAlloc.allocator());
        // stmt.value = self.parseExpression(.LOWEST);
        // if (self.peekTokenIs(.SEMICOLON)) {
        //     self.nextToken(self.l.arenaAlloc.allocator());
        // }

        return Ast.Statement{ .letStatement = stmt };
    }

    pub fn curTokenIs(self: *Parser, tType: TokenType) bool {
        return self.curToken.tType == tType;
    }

    pub fn peekTokenIs(self: *Parser, tType: TokenType) bool {
        return self.peekToken.tType == tType;
    }

    pub fn expectPeek(self: *Parser, tType: TokenType) bool {
        if (self.peekTokenIs(tType)) {
            self.nextToken(self.l.arenaAlloc.allocator());
            return true;
        } else {
            self.peekError(tType);
            return false;
        }
    }
};

test "TestOperatorPrecedenceParsing" {}

test "TestGroupedExpression" {
    const allocator = std.testing.allocator;
    const input =
        \\(5 + 5) * 2;
        \\!(true == true)
    ;
    var l = Lexer.init(allocator, input);
    defer l.deinit();
    var parser = Parser.init(allocator, l);
    var program = parser.parseProgram();
    defer program.deinit();
    defer parser.deinit();
    try std.testing.expect(!parser.checkParserErros());

    // try Pretty.print(allocator, program.statements.items[0], .{});

    const stringer = try program.string();
    // try Pretty.print(allocator, program.statements.items[0], .{ .max_depth = 30 });
    std.debug.print("Test out {s}\n", .{stringer.items});

    defer stringer.deinit();
    // try std.testing.expectEqualSlices(u8, "(5 + 4)", stringer.items);
}
test "TestBoolean" {
    const allocator = std.testing.allocator;
    const input =
        \\3 > 5 == false;
    ;
    var l = Lexer.init(allocator, input);
    defer l.deinit();
    var parser = Parser.init(allocator, l);
    var program = parser.parseProgram();
    defer program.deinit();
    defer parser.deinit();
    try std.testing.expect(!parser.checkParserErros());

    // try Pretty.print(allocator, program.statements.items[0], .{});

    const stringer = try program.string();
    // try Pretty.print(allocator, program.statements.items[0], .{ .max_depth = 30 });
    std.debug.print("Test out {s}\n", .{stringer.items});

    defer stringer.deinit();
    // try std.testing.expectEqualSlices(u8, "(5 + 4)", stringer.items);
}

// test "TestInfixExpression\n" {
//     const allocator = std.testing.allocator;
//     const input =
//         \\5+4;
//     ;
//     var l = Lexer.init(allocator, input);
//     defer l.deinit();
//     var parser = Parser.init(allocator, l);
//     var program = parser.parseProgram();
//     defer program.deinit();
//     defer parser.deinit();
//     try std.testing.expect(!parser.checkParserErros());

//     // try Pretty.print(allocator, program.statements.items[0], .{});

//     const stringer = try program.string();
//     // try Pretty.print(allocator, program.statements.items[0], .{ .max_depth = 30 });
//     // std.debug.print("Test out {s}\n", .{stringer.items});

//     defer stringer.deinit();
//     try std.testing.expectEqualSlices(u8, "(5 + 4)", stringer.items);
// }

// test "TestPrefixExpression\n" {
//     // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     // const allocator = gpa.allocator();
//     const allocator = std.testing.allocator;
//     const input =
//         \\!20;
//     ;
//     var l = Lexer.init(allocator, input);
//     defer l.deinit();
//     var parser = Parser.init(allocator, l);
//     var program = parser.parseProgram();
//     try std.testing.expect(!parser.checkParserErros());

//     // try Pretty.print(allocator, program.statements.items[0], .{});

//     const stringer = try program.string();
//     // std.debug.print("Test out {s}\n", .{stringer.items});
//     defer program.deinit();
//     defer parser.deinit();
//     defer stringer.deinit();
//     try std.testing.expectEqualSlices(u8, "(!20)", stringer.items);
// }

// test "TestLetStatements Good" {
//     const allocator = std.testing.allocator;
//     const input =
//         \\let x = 5;
//         \\let y = x;
//         \\let foobar = 21321;
//     ;
//     var l = Lexer.init(allocator, input);
//     defer l.deinit();
//     var parser = Parser.init(allocator, l);
//     var program = parser.parseProgram();
//     defer program.deinit();
//     defer parser.deinit();
//     //  try std.testing.expect(parser.checkParserErros());

//     const expected_idents = [_]Ast.Identifier{
//         Ast.Identifier.init(Token{ .tType = .IDENT, .literal = "x" }),
//         Ast.Identifier.init(Token{ .tType = .IDENT, .literal = "y" }),
//         Ast.Identifier.init(Token{ .tType = .IDENT, .literal = "foobar" }),
//     };

//     for (expected_idents, 0..) |ident, i| {
//         const statement = program.statements.items[i];
//         const ls = statement.letStatement;
//         const literal = ls.name.tokenLiteral();

//         // std.debug.print("\n ====== \n statements: {any}\n ", .{statement});
//         try std.testing.expect(ls.token.tType == .LET);

//         // Compare token literal
//         std.testing.expect(std.mem.eql(u8, ident.value, literal)) catch {
//             std.debug.print("Expected: {s}, got: {s}\n", .{ ident.value, literal });
//             return error.literalmismatch;
//         };
//     }
// }

// test "Test ReturnStatement" {
//     const allocator = std.testing.allocator;
//     const input =
//         \\return 5;
//         \\return x;
//         \\return y + x;
//     ;
//     var l = Lexer.init(allocator, input);
//     defer l.deinit();
//     var parser = Parser.init(allocator, l);
//     var program = parser.parseProgram();
//     defer program.deinit();
//     defer parser.deinit();
//     // try Pretty.print(allocator, program, .{});
//     try std.testing.expect(!parser.checkParserErros());
//     try std.testing.expectEqualDeep(3, program.statements.items.len);

//     for (program.statements.items) |value| {
//         try std.testing.expect(checkTokenTypeMatch(value, .RETURN));
//     }
// }

// pub fn checkTokenTypeMatch(stmt: Ast.Statement, kind: TokenType) bool {
//     switch (stmt) {
//         .letStatement => {
//             return stmt.letStatement.token.tType == kind;
//         },
//         .returnStatement => {
//             return stmt.returnStatement.token.tType == kind;
//         },
//         .expression => {
//             return stmt.expression.token.tType == kind;
//         },
//     }
// }
// test "TestString" {
//     const allocator = std.testing.allocator;
//     const input =
//         \\ let myvar = anothervar;
//     ;
//     var l = Lexer.init(allocator, input);
//     defer l.deinit();
//     var parser = Parser.init(allocator, l);
//     var program = parser.parseProgram();

//     // std.debug.print("\n{any}\n", .{program.statements.items});

//     // program.statements.items[0].letStatement.value = Ast.Expression{ .identifier = Ast.Identifier.init(Token{
//     //     .literal = "anothervar",
//     //     .tType = .IDENT,
//     // }) };
//     const stringer = try program.string();
//     // std.debug.print("\n{s}\n", .{stringer.items});
//     defer program.deinit();
//     defer parser.deinit();
//     defer stringer.deinit();
//     // try std.testing.expectEqualSlices(u8, "let myvar = anothervar", stringer.items);
// }

// test "TestIdentifierExpression" {
//     const allocator = std.testing.allocator;
//     const input =
//         \\ foobar;
//     ;
//     var l = Lexer.init(allocator, input);
//     defer l.deinit();
//     var parser = Parser.init(allocator, l);
//     var program = parser.parseProgram();
//     try std.testing.expect(!parser.checkParserErros());

//     // try Pretty.print(allocator, program.statements.items[0], .{});

//     const stringer = try program.string();
//     defer program.deinit();
//     defer parser.deinit();
//     defer stringer.deinit();
//     try std.testing.expectEqualSlices(u8, "foobar", stringer.items);
// }

// test "TestIntegerLiteral\n" {
//     const allocator = std.testing.allocator;
//     const input =
//         \\ 5;
//     ;
//     var l = Lexer.init(allocator, input);
//     defer l.deinit();
//     var parser = Parser.init(allocator, l);
//     var program = parser.parseProgram();
//     try std.testing.expect(!parser.checkParserErros());

//     // try Pretty.print(allocator, program.statements.items[0], .{});

//     const stringer = try program.string();
//     // std.debug.print("{s}\n", .{stringer.items});
//     defer program.deinit();
//     defer parser.deinit();
//     defer stringer.deinit();
//     try std.testing.expectEqualSlices(u8, "5", stringer.items);
// }

// test "TestLetStatements Bad" {
//     const allocator = std.testing.allocator;
//     const input =
//         \\ let x;
//     ;
//     var l = Lexer.init(allocator, input);
//     defer l.deinit();
//     var parser = Parser.init(allocator, l);
//     var program = parser.parseProgram();
//     try std.testing.expect(!parser.checkParserErros());

//     // try Pretty.print(allocator, program.statements.items[0], .{});
//     defer program.deinit();
//     defer parser.deinit();
// }
