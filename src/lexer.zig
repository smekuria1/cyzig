const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;

pub const Lexer = struct {
    input: []const u8,
    position: usize = 0,
    read_position: usize = 0,
    ch: u8 = 0,
    allocator: Allocator,

    pub fn init(allocator: Allocator, input: []const u8) Lexer {
        var lexer = Lexer{
            .input = input,
            .allocator = allocator,
        };
        lexer.readChar();
        return lexer;
    }

    pub fn deinit(self: *Lexer) void {
        _ = self;
    }

    pub fn readChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn newToken(allocator: Allocator, tType: TokenType, ch: u8) Token {
        const lit = allocator.alloc(u8, 1) catch unreachable;
        lit[0] = ch;
        return Token{
            .tType = tType,
            .literal = lit,
        };
    }

    pub fn nextToken(self: *Lexer) ?Token {
        var tok: Token = undefined;

        switch (self.ch) {
            '=' => {
                tok = newToken(self.allocator, TokenType.ASSIGN, self.ch);
            },
            '+' => {
                tok = newToken(self.allocator, TokenType.PLUS, self.ch);
            },
            ';' => {
                tok = newToken(self.allocator, TokenType.SEMICOLON, self.ch);
            },
            '(' => {
                tok = newToken(self.allocator, TokenType.LPAREN, self.ch);
            },
            ')' => {
                tok = newToken(self.allocator, TokenType.RPAREN, self.ch);
            },
            '{' => {
                tok = newToken(self.allocator, TokenType.LBRACE, self.ch);
            },
            '}' => {
                tok = newToken(self.allocator, TokenType.RBRACE, self.ch);
            },
            0 => {
                tok.literal = "";
                tok.tType = TokenType.EOF;
            },

            else => {
                tok.tType = TokenType.ILLEGAL;
                tok.literal = "ILLEGAL";
            },
        }

        self.readChar();
        // defer self.allocator.free(tok.literal);
        return tok;
    }
};

test "TestNextToken" {
    const allocator = std.testing.allocator;
    const input = "==};";

    var lexer = Lexer.init(allocator, input);
    defer lexer.deinit();

    const expected_tokens = [_]Token{
        // Token{ .tType = .VAR, .literal = @as([]const u8, "var") },
        // Token{ .tType = .IDENT, .literal = "a" },
        Token{ .tType = .ASSIGN, .literal = "=" },
        Token{ .tType = .ASSIGN, .literal = "=" },
        Token{ .tType = .RBRACE, .literal = "}" },
        Token{ .tType = .SEMICOLON, .literal = ";" },
        Token{ .tType = .EOF, .literal = "" },
    };
    var i: usize = 0;
    while (lexer.nextToken()) |token| : (i += 1) {
        if (i >= expected_tokens.len) {
            break;
        }
        std.debug.print("\n{s}\n", .{token.literal});
        try std.testing.expectEqual(expected_tokens[i].tType, token.tType);
    }

    lexer.deinit();
}
