const std = @import("std");
const Token = @import("./token.zig").Token;
const Lexer = @import("./lexer.zig").Lexer;
const Ast = @import("./ast.zig");
const Allocator = std.mem.Allocator;

pub const Parser = struct {
    l: *Lexer,
    curToken: Token,
    peekToken: Token,
    allocator: Allocator,

    pub fn init(allocator: Allocator, l: *Lexer) *Parser {
        var parser = allocator.create(Parser) catch unreachable;
        parser.allocator = allocator;
        parser.l = l;

        parser.nextToken();
        parser.nextToken();

        return parser;
    }

    pub fn deinit(self: *Parser) void {
        self.allocator.destroy(self);
    }

    pub fn nextToken(self: *Parser) void {
        self.curToken = self.peekToken;
        self.peekToken = self.l.nextToken();
    }

    pub fn parseProgram(self: *Parser) *Ast.Program {
        const prog = Ast.Program;

        var program = self.allocator.create(prog) catch unreachable;
        program.init(self.allocator);

        while (self.curToken.tType != .EOF) {
            const stmt = self.parseStatement();
            if (stmt) |s| {
                program.statements.append(s) catch unreachable;
            }
            self.nextToken();
        }

        return program;
    }

    pub fn parseStatement(self: *Parser) *Ast.Statement {
        switch (self.curToken.tType) {
            .LET => return self.parseLetStatement(),
            else => return null,
        }
    }

    pub fn parseLetStatement(self: *Parser) *Ast.LetStatement {
        const stmt = &Ast.LetStatement{
            .token = self.curToken,
            .name = undefined,
            .value = undefined,
        };
        if (!self.expectPeek(.IDENT)) {
            return null;
        }

        stmt.name = Ast.Identifier{
            .token = self.curToken,
            .value = self.curToken.literal,
        };
        if (!self.expectPeek(.ASSIGN)) {
            return null;
        }

        while (!self.curTokenIs(.SEMICOLON)) {
            self.nextToken();
        }
        return stmt;
    }

    pub fn curTokenIs(self: *Parser, tType: Token.TokenType) bool {
        return self.curToken.tType == tType;
    }

    pub fn peekTokenIs(self: *Parser, tType: Token.TokenType) bool {
        return self.peekToken.tType == tType;
    }

    pub fn expectPeek(self: *Parser, tType: Token.TokenType) bool {
        if (self.peekTokenIs(tType)) {
            self.nextToken();
            return true;
        } else {
            return false;
        }
    }
};

test "TestParser" {
    const allocator = std.testing.allocator;
    var l = Lexer.init(allocator, "let x = 5;");
    defer l.deinit();
    var p = Parser.init(allocator, l);
    defer p.deinit();
    const program = p.parseProgram();
    defer program.deinit();
    std.debug.print("{any}\n", .{program});
}
