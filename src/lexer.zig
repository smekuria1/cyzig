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

    pub fn peekChar(self: *Lexer) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        } else {
            return self.input[self.read_position];
        }
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
        return ch >= '0' and ch <= '9';
    }
    pub fn readIdentifier(self: *Lexer) []const u8 {
        const postion: usize = self.position;
        while (isLetter(self.ch)) {
            // std.debug.print("readIdentifier {c}\n", .{self.ch});
            self.readChar();
        }

        return self.input[postion..self.position];
    }

    pub fn readNumber(self: *Lexer) []const u8 {
        const position = self.position;
        while (isDigit(self.ch)) {
            // std.debug.print("IsDigit {c}\n", .{self.ch});
            self.readChar();
        }
        return self.input[position..self.position];
    }

    pub fn initKeyWords(allocator: Allocator) std.StringHashMap(TokenType) {
        var keyWords = std.StringHashMap(TokenType).init(allocator);
        keyWords.put("fn", TokenType.FUNCTION) catch unreachable;
        keyWords.put("let", TokenType.LET) catch unreachable;
        keyWords.put("var", TokenType.VAR) catch unreachable;
        keyWords.put("true", TokenType.TRUE) catch unreachable;
        keyWords.put("false", TokenType.FALSE) catch unreachable;
        keyWords.put("if", TokenType.IF) catch unreachable;
        keyWords.put("else", TokenType.ELSE) catch unreachable;
        keyWords.put("return", TokenType.RETURN) catch unreachable;

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
        // std.debug.print("\nPosition {any}\n", .{self.position});
        // std.debug.print("Read Position {any}\n", .{self.read_position});
        switch (self.ch) {
            '=' => {
                if (self.peekChar() == '=') {
                    const ch = self.ch;
                    self.readChar();
                    //////////SCUFFED WAY OF CREATING STRING SLICE THINGY PLEASE FIX THIS SHIT
                    var lit = [_]u8{ '1', '1' };
                    lit[0] = ch;
                    lit[1] = self.ch;
                    tok = Token{
                        .literal = &lit,
                        .tType = TokenType.EQ,
                    };
                } else {
                    tok = newToken(self.allocator, TokenType.ASSIGN, self.ch);
                }
            },
            '+' => {
                tok = newToken(self.allocator, TokenType.PLUS, self.ch);
            },
            '-' => {
                tok = newToken(self.allocator, TokenType.MINUS, self.ch);
            },
            '!' => {
                if (self.peekChar() == '=') {
                    const ch = self.ch;
                    self.readChar();
                    //////////SCUFFED WAY OF CREATING STRING SLICE THINGY PLEASE FIX THIS SHIT
                    var lit = [_]u8{ '1', '1' };
                    lit[0] = ch;
                    lit[1] = self.ch;
                    tok = Token{
                        .literal = &lit,
                        .tType = TokenType.NOT_EQ,
                    };
                } else {
                    tok = newToken(self.allocator, TokenType.BANG, self.ch);
                }
            },
            '*' => {
                tok = newToken(self.allocator, TokenType.ASTERISK, self.ch);
            },
            '/' => {
                tok = newToken(self.allocator, TokenType.SLASH, self.ch);
            },
            '<' => {
                tok = newToken(self.allocator, TokenType.LT, self.ch);
            },
            '>' => {
                tok = newToken(self.allocator, TokenType.GT, self.ch);
            },
            ',' => {
                tok = newToken(self.allocator, TokenType.COMMA, self.ch);
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
                    return tok;
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
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\              x + y;
        \\               };    
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\if (5 < 10) {
        \\    return true;
        \\} else {
        \\    return false;
        \\} 
        \\10 == 10;
        \\10 != 9;
        \\
    ;

    var lexer = Lexer.init(allocator, input);
    defer lexer.deinit();

    const expected_tokens = [_]Token{
        Token{ .tType = .LET, .literal = "let" },
        Token{ .tType = .IDENT, .literal = "five" },
        Token{ .tType = .ASSIGN, .literal = "=" },
        Token{ .tType = .INT, .literal = "5" },
        Token{ .tType = .SEMICOLON, .literal = ";" },
        Token{ .tType = .LET, .literal = "let" },
        Token{ .tType = .IDENT, .literal = "ten" },
        Token{ .tType = .ASSIGN, .literal = "=" },
        Token{ .tType = .INT, .literal = "10" },
        Token{ .tType = .SEMICOLON, .literal = ";" },
        Token{ .tType = .LET, .literal = "let" },
        Token{ .tType = .IDENT, .literal = "add" },
        Token{ .tType = .ASSIGN, .literal = "=" },
        Token{ .tType = .FUNCTION, .literal = "fn" },
        Token{ .tType = .LPAREN, .literal = "(" },
        Token{ .tType = .IDENT, .literal = "x" },
        Token{ .tType = .COMMA, .literal = "," },
        Token{ .tType = .IDENT, .literal = "y" },
        Token{ .tType = .RPAREN, .literal = ")" },
        Token{ .tType = .LBRACE, .literal = "{" },
        Token{ .tType = .IDENT, .literal = "x" },
        Token{ .tType = .PLUS, .literal = "+" },
        Token{ .tType = .IDENT, .literal = "y" },
        Token{ .tType = .SEMICOLON, .literal = ";" },
        Token{ .tType = .RBRACE, .literal = "}" },
        Token{ .tType = .SEMICOLON, .literal = ";" },
        Token{ .tType = .LET, .literal = "let" },
        Token{ .tType = .IDENT, .literal = "result" },
        Token{ .tType = .ASSIGN, .literal = "=" },
        Token{ .tType = .IDENT, .literal = "add" },
        Token{ .tType = .LPAREN, .literal = "(" },
        Token{ .tType = .IDENT, .literal = "five" },
        Token{ .tType = .COMMA, .literal = "," },
        Token{ .tType = .IDENT, .literal = "ten" },
        Token{ .tType = .RPAREN, .literal = ")" },
        Token{ .tType = .SEMICOLON, .literal = ";" },
        Token{ .tType = .BANG, .literal = "!" },
        Token{ .tType = .MINUS, .literal = "-" },
        Token{ .tType = .SLASH, .literal = "/" },
        Token{ .tType = .ASTERISK, .literal = "*" },
        Token{ .tType = .INT, .literal = "5" },
        Token{ .tType = .SEMICOLON, .literal = ";" },
        Token{ .tType = .INT, .literal = "5" },
        Token{ .tType = .LT, .literal = "<" },
        Token{ .tType = .INT, .literal = "10" },
        Token{ .tType = .GT, .literal = ">" },
        Token{ .tType = .INT, .literal = "5" },
        Token{ .tType = .SEMICOLON, .literal = ";" },
        Token{ .tType = .IF, .literal = "if" },
        Token{ .tType = .LPAREN, .literal = "(" },
        Token{ .tType = .INT, .literal = "5" },
        Token{ .tType = .LT, .literal = "<" },
        Token{ .tType = .INT, .literal = "10" },
        Token{ .tType = .RPAREN, .literal = ")" },
        Token{ .tType = .LBRACE, .literal = "{" },
        Token{ .tType = .RETURN, .literal = "return" },
        Token{ .tType = .TRUE, .literal = "true" },
        Token{ .tType = .SEMICOLON, .literal = ";" },
        Token{ .tType = .RBRACE, .literal = "}" },

        // Token{ .tType = .EOF, .literal = "" },
    };
    var i: usize = 0;
    while (lexer.nextToken()) |token| : (i += 1) {
        if (i >= expected_tokens.len) {
            break;
        }
        std.debug.print("\n{any} {s} {s} \n", .{ token.tType, token.literal, expected_tokens[i].literal });
        try std.testing.expectEqual(expected_tokens[i].tType, token.tType);
    }
}
