const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;

pub const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    ch: u8,
    keywordMap: std.StringHashMap(TokenType),
    allocator: Allocator,

    pub fn init(allocator: Allocator, input: []const u8) *Lexer {
        var lexer = allocator.create(Lexer) catch unreachable;
        lexer.allocator = allocator;
        lexer.input = input;
        lexer.read_position = 0;
        lexer.position = 0;
        lexer.keywordMap = initKeyWords(allocator);
        lexer.readChar();
        return lexer;
    }

    pub fn deinit(self: *Lexer) void {
        self.keywordMap.deinit();
        self.allocator.destroy(self);
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
        var lit = [_]u8{'1'};
        lit[0] = ch;
        //std.debug.print("In newToken {any}\n", .{lit});
        _ = allocator;
        return Token{
            .tType = tType,
            .literal = &lit,
        };
    }

    pub fn isLetter(ch: u8) bool {
        return 'a' <= ch and ch <= 'z' or 'A' <= ch and ch <= 'Z' or ch == '_';
    }

    pub fn isDigit(ch: u8) bool {
        return '0' <= ch and ch <= '9';
    }
    pub fn readIdentifier(self: *Lexer) []const u8 {
        const postion: usize = self.position;
        while (isLetter(self.ch)) {
            self.readChar();
        }

        return self.input[postion..self.position];
    }

    pub fn readNumber(self: *Lexer) []const u8 {
        const position = self.position;
        while (isDigit(self.ch)) {
            self.readChar();
        }
        return self.input[position..self.position];
    }

    pub fn initKeyWords(allocator: Allocator) std.StringHashMap(TokenType) {
        var keyWords = std.StringHashMap(TokenType).init(allocator);
        keyWords.put("fn", TokenType.FUNCTION) catch unreachable;
        keyWords.put("let", TokenType.LET) catch unreachable;
        keyWords.put("var", TokenType.VAR) catch unreachable;

        return keyWords;
    }

    pub fn lookUpIdentifier(self: *Lexer, ident: []const u8) TokenType {
        const key = self.keywordMap.get(ident);
        if (key) |v| {
            return v;
        }
        return TokenType.IDENT;
    }

    pub fn skipWhiteSpace(self: *Lexer) void {
        while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n' or self.ch == '\r') {
            self.readChar();
        }
    }

    pub fn nextToken(self: *Lexer) ?Token {
        var tok: Token = undefined;
        self.skipWhiteSpace();
        std.debug.print("\nPosition {any}\n", .{self.position});
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
                if (isLetter(self.ch)) {
                    tok.literal = self.readIdentifier();
                    tok.tType = self.lookUpIdentifier(tok.literal);
                    return tok;
                } else if (isDigit(self.ch)) {
                    tok.tType = TokenType.INT;
                    tok.literal = self.readNumber();
                } else {
                    tok = newToken(self.allocator, TokenType.ILLEGAL, self.ch);
                }
            },
        }

        self.readChar();
        //std.debug.print("Token {any}\n", .{tok});
        return tok;
    }
};

test "TestNextToken" {
    const allocator = std.testing.allocator;
    const input = "var a = 75";

    var lexer = Lexer.init(allocator, input);
    defer lexer.deinit();

    const expected_tokens = [_]Token{
        Token{ .tType = .VAR, .literal = @as([]const u8, "var") },
        Token{ .tType = .IDENT, .literal = "a" },
        Token{ .tType = .ASSIGN, .literal = "=" },
        Token{ .tType = .INT, .literal = "7" },
        Token{ .tType = .SEMICOLON, .literal = ";" },
        // Token{ .tType = .EOF, .literal = "" },
    };
    var i: usize = 0;
    while (lexer.nextToken()) |token| : (i += 1) {
        if (i >= expected_tokens.len) {
            break;
        }
        std.debug.print("\n{any} {s} Read Pos {any}\n", .{ token.tType, token.literal, lexer.read_position });
        // try std.testing.expectEqual(expected_tokens[i].tType, token.tType);
    }
}
