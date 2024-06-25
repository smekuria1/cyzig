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
    errors: Ast.GenericAst(string),
    allocator: Allocator,

    pub fn init(allocator: Allocator, l: *Lexer) *Parser {
        var parser = allocator.create(Parser) catch unreachable;
        parser.allocator = allocator;
        parser.l = l;
        const err = Ast.GenericAst(string).init(allocator) catch unreachable;
        parser.errors = err;
        parser.nextToken();
        parser.nextToken();

        return parser;
    }

    pub fn deinit(self: *Parser) void {
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
        if (err.list.items.len <= 0) {
            return false;
        }

        std.debug.print("\nParser had {d} errors\n", .{err.list.items.len});
        for (err.list.items) |value| {
            std.debug.print("{any}\n", .{value});
        }
        return true;
    }

    pub fn peekError(self: *Parser, t: TokenType) void {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "expected next token to be {}, got {} instead", .{ t, self.peekToken.tType }) catch unreachable;
        const sliceMsg: []const u8 = msg[0..];
        self.errors.list.append(sliceMsg) catch unreachable;
    }

    pub fn parseProgram(self: *Parser) *Ast.Program {
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

    pub fn parseStatement(self: *Parser) ?*Ast.LetStatement {
        switch (self.curToken.tType) {
            .LET => return self.parseLetStatement(),
            else => return null,
        }
    }

    pub fn parseLetStatement(self: *Parser) ?*Ast.LetStatement {
        var stmt = self.allocator.create(Ast.LetStatement) catch unreachable;
        stmt.token = self.curToken;
        if (!self.expectPeek(.IDENT)) {
            self.allocator.destroy(stmt);
            return null;
        }
        var identifier = self.allocator.create(Ast.Identifier) catch unreachable;
        identifier.token = self.curToken;
        identifier.value = self.curToken.literal;
        stmt.name = identifier;
        if (!self.expectPeek(.ASSIGN)) {
            self.allocator.destroy(identifier);
            self.allocator.destroy(stmt);
            return null;
        }

        while (!self.curTokenIs(.SEMICOLON)) {
            self.nextToken();
        }
        return stmt;
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
        \\let x = 21321;
    ;
    var l = Lexer.init(allocator, input);
    defer l.deinit();
    var p = Parser.init(allocator, l);
    defer p.deinit();
    const program = p.parseProgram();
    defer program.deinit();

    try std.testing.expectEqual(3, program.statements.list.items.len);
    // try Pretty.print(allocator, program, .{});

}

test "TestLetStatements Bad" {
    const allocator = std.testing.allocator;
    const input =
        \\let x = 5;
        \\let y x;
        \\let x 21321;
    ;
    var l = Lexer.init(allocator, input);
    defer l.deinit();
    var p = Parser.init(allocator, l);
    defer p.deinit();
    const program = p.parseProgram();
    defer program.deinit();
    try std.testing.expect(p.checkParserErros());
    try std.testing.expectEqual(1, program.statements.list.items.len);
    // try Pretty.print(allocator, program, .{});

}
