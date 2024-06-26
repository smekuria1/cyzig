const std = @import("std");
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;
const Lexer = @import("./lexer.zig").Lexer;
const Ast = @import("./ast.zig");
const Allocator = std.mem.Allocator;
const Pretty = @import("./pretty.zig");
const string = []const u8;
pub const Parser = struct {
    l: *Lexer,
    curToken: Token,
    peekToken: Token,
    errors: std.ArrayList(string),
    allocator: Allocator,

    pub fn init(allocator: Allocator, l: *Lexer) *Parser {
        var parser = allocator.create(Parser) catch unreachable;
        parser.allocator = allocator;
        parser.l = l;
        const err = std.ArrayList(string).init(allocator);
        parser.errors = err;
        parser.nextToken();
        parser.nextToken();

        return parser;
    }

    pub fn deinit(self: *Parser) void {
        for (self.errors.items) |value| {

            self.allocator.free(value);
        }
        self.errors.deinit();
        self.allocator.destroy(self);
    }

    pub fn nextToken(self: *Parser) void {
        self.curToken = self.peekToken;
        self.peekToken = self.l.nextToken().?;
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
        const msg = std.fmt.allocPrint(self.allocator, "expected next token to be {}, got {} instead", .{ t, self.peekToken.tType }) catch unreachable;
        self.errors.append(msg) catch unreachable;

    }

    pub fn parseProgram(self: *Parser) Ast.Program {
        var program = Ast.Program.init(self.allocator);
        while (self.curToken.tType != .EOF) {
            const stmt = self.parseStatement();
            if (stmt) |s| {
                program.statements.append(s) catch unreachable;
            }
            self.nextToken();
        }

        return program;
    }

    pub fn parseStatement(self: *Parser) ?Ast.Statement {
        switch (self.curToken.tType) {
            .LET => return self.parseLetStatement(),
            .RETURN => return self.parseReturnStatement(),
            else => return null,
        }
    }

    pub fn parseReturnStatement(self: *Parser) ?Ast.Statement {
        const stmt = Ast.Statement.ReturnStatement{
            .token = self.curToken,
            .value = undefined,
        };

        self.nextToken();

        // TODO: skipping expressions until semicolon
        while (!self.curTokenIs(.SEMICOLON)) {
            self.nextToken();
        }

        return Ast.Statement{ .returnStatement = stmt };
    }

    pub fn parseLetStatement(self: *Parser) ?Ast.Statement {
        var stmt = Ast.Statement.LetStatement{
            .token = self.curToken,
            .ident = Token{
                .literal = "LET",
                .tType = .LET,
            },
            .value = null,
        };

        if (!self.expectPeek(.IDENT)) {
            return null;
        }
        stmt.ident = self.curToken;
        if (!self.expectPeek(.ASSIGN)) {
            return null;
        }

        while (!self.curTokenIs(.SEMICOLON)) {
            self.nextToken();
        }
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
            self.nextToken();
            return true;
        } else {
            self.peekError(tType);
            return false;
        }
    }
};

test "TestLetStatements Good" {
    const allocator = std.testing.allocator;
    const input =
        \\let x = 5;
        \\let y = x;
        \\let foobar = 21321;
    ;
    var l = Lexer.init(allocator, input);
    defer l.deinit();
    var parser = Parser.init(allocator, l);
    var program = parser.parseProgram();
    //  try std.testing.expect(parser.checkParserErros());
    defer program.deinit();
    defer parser.deinit();

    const expected_idents = [_]Ast.Identifier{
        Ast.Identifier.init(Token{ .tType = .IDENT, .literal = "x" }),
        Ast.Identifier.init(Token{ .tType = .IDENT, .literal = "y" }),
        Ast.Identifier.init(Token{ .tType = .IDENT, .literal = "foobar" }),
    };

    for (expected_idents, 0..) |ident, i| {
        const statement = program.statements.items[i];
        const ls = statement.letStatement;
        const literal = ls.ident.literal;

        // std.debug.print("\n ====== \n statements: {any}\n ", .{statement});
        try std.testing.expect(ls.token.tType == .LET);

        // Compare token literal
        std.testing.expect(std.mem.eql(u8, ident.value, literal)) catch {
            std.debug.print("Expected: {s}, got: {s}\n", .{ ident.value, literal });
            return error.literalmismatch;
        };
    }
}

test "Test ReturnStatement" {
    const allocator = std.testing.allocator;
    const input =
        \\return 5;
        \\return x;
        \\return y + x;

    ;
    var l = Lexer.init(allocator, input);
    defer l.deinit();
    var parser = Parser.init(allocator, l);
    var program = parser.parseProgram();
    defer program.deinit();
    defer parser.deinit();
    // try Pretty.print(allocator, program, .{});
    try std.testing.expect(!parser.checkParserErros());
    try std.testing.expectEqualDeep(3, program.statements.items.len);

    for (program.statements.items) |value| {
        try std.testing.expect(checkTokenTypeMatch(value, .RETURN));
    }
}

pub fn checkTokenTypeMatch(stmt: Ast.Statement, kind: TokenType) bool {
    switch (stmt) {
        .letStatement => {
            return stmt.letStatement.token.tType == kind;
        },
        .returnStatement => {
            return stmt.returnStatement.token.tType == kind;
        },
        .expression => {
            return stmt.expression.token.tType == kind;
        },
    }
}

// test "TestLetStatements Bad" {
//     const allocator = std.testing.allocator;
//     const input =
//         \\let x = 5;
//         \\let y x;
//         \\let x 21321;
//     ;
//     var l = Lexer.init(allocator, input);
//     defer l.deinit();
//     var p = Parser.init(allocator, l);
//     defer p.deinit();
//     const program = p.parseProgram();
//     defer program.deinit();
//     try std.testing.expect(p.checkParserErros());
//     try std.testing.expectEqual(1, program.statements.list.items.len);
//     // try Pretty.print(allocator, program, .{});

// }
