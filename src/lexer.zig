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
    arenaAlloc: std.heap.ArenaAllocator,

    pub fn init(allocator: Allocator, input: []const u8) *Lexer {
        var lexer = allocator.create(Lexer) catch unreachable;
        const arena = std.heap.ArenaAllocator.init(allocator);
        lexer.arenaAlloc = arena;
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
        self.arenaAlloc.deinit();
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
        switch (tType) {
            .NOT_EQ => {
                var lit = allocator.alloc(u8, 2) catch unreachable;
                lit[0] = ch;
                lit[1] = @as(u8, 61);
                return Token{
                    .tType = tType,
                    .literal = lit,
                    .alloc = true,
                };
            },
            .EQ => {
                var lit = allocator.alloc(u8, 2) catch unreachable;
                lit[0] = ch;
                lit[1] = ch;
                return Token{
                    .tType = tType,
                    .literal = lit,
                    .alloc = true,
                };
            },
            inline else => {
                var lit = allocator.alloc(u8, 1) catch unreachable;
                lit[0] = ch;
                return Token{
                    .tType = tType,
                    .literal = lit,
                    .alloc = true,
                };
            },
        }
    }

    pub fn readString(self: *Lexer, arenaAllocator: Allocator) []const u8 {
        const position = self.position + 1;
        while (true) {
            self.readChar();
            if (self.ch == '"' or self.ch == 0) {
                break;
            }
        }
        return std.fmt.allocPrint(arenaAllocator, "{s}", .{self.input[position..self.position]}) catch unreachable;
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

    pub fn nextToken(self: *Lexer, arenaAllocator: Allocator) ?Token {
        var tok: Token = undefined;
        self.skipWhiteSpace();
        // std.debug.print("\nPosition {any}\n", .{self.position});
        // std.debug.print("Read Position {any}\n", .{self.read_position});
        switch (self.ch) {
            '=' => {
                if (self.peekChar() == '=') {
                    const ch = self.ch;
                    self.readChar();
                    tok = newToken(arenaAllocator, .EQ, ch);
                } else {
                    tok = newToken(arenaAllocator, TokenType.ASSIGN, self.ch);
                }
            },
            '+' => {
                tok = newToken(arenaAllocator, TokenType.PLUS, self.ch);
            },
            '-' => {
                tok = newToken(arenaAllocator, TokenType.MINUS, self.ch);
            },
            '!' => {
                if (self.peekChar() == '=') {
                    const ch = self.ch;
                    self.readChar();
                    tok = newToken(arenaAllocator, .NOT_EQ, ch);
                } else {
                    tok = newToken(arenaAllocator, TokenType.BANG, self.ch);
                }
            },
            '*' => {
                tok = newToken(arenaAllocator, TokenType.ASTERISK, self.ch);
            },
            '/' => {
                tok = newToken(arenaAllocator, TokenType.SLASH, self.ch);
            },
            '<' => {
                tok = newToken(arenaAllocator, TokenType.LT, self.ch);
            },
            '>' => {
                tok = newToken(arenaAllocator, TokenType.GT, self.ch);
            },
            ',' => {
                tok = newToken(arenaAllocator, TokenType.COMMA, self.ch);
            },
            ';' => {
                tok = newToken(arenaAllocator, TokenType.SEMICOLON, self.ch);
            },
            '(' => {
                tok = newToken(arenaAllocator, TokenType.LPAREN, self.ch);
            },
            ')' => {
                tok = newToken(arenaAllocator, TokenType.RPAREN, self.ch);
            },
            '{' => {
                tok = newToken(arenaAllocator, TokenType.LBRACE, self.ch);
            },
            '}' => {
                tok = newToken(arenaAllocator, TokenType.RBRACE, self.ch);
            },
            '"' => {
                tok.tType = .STRING;
                tok.literal = self.readString(arenaAllocator);
            },
            '[' => {
                tok = newToken(arenaAllocator, TokenType.LBRACKET, self.ch);
            },
            ']' => {
                tok = newToken(arenaAllocator, TokenType.RBRACKET, self.ch);
            },
            ':' => {
                tok = newToken(arenaAllocator, TokenType.COLON, self.ch);
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
                    tok = newToken(arenaAllocator, TokenType.ILLEGAL, self.ch);
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
        \\10 == 10
        \\10 != 9
        \\ "foobar"
        \\ "foo bar"
        \\ [1,2]
        \\ { "foo" : "bar" }
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
        Token{ .tType = .ELSE, .literal = "else" },
        Token{ .tType = .LBRACE, .literal = "{" },
        Token{ .tType = .RETURN, .literal = "return" },
        Token{ .tType = .FALSE, .literal = "false" },
        Token{ .tType = .SEMICOLON, .literal = ";" },
        Token{ .tType = .RBRACE, .literal = "}" },
        Token{ .tType = .INT, .literal = "10" },
        Token{ .tType = .EQ, .literal = "==" },
        Token{ .tType = .INT, .literal = "9" },
        Token{ .tType = .INT, .literal = "10" },
        Token{ .tType = .NOT_EQ, .literal = "!=" },
        Token{ .tType = .INT, .literal = "9" },
        Token{ .tType = .STRING, .literal = "foobar" },
        Token{ .tType = .STRING, .literal = "foo bar" },
        Token{ .tType = .LBRACKET, .literal = "[" },
        Token{ .tType = .INT, .literal = "1" },
        Token{ .tType = .COMMA, .literal = "," },
        Token{ .tType = .INT, .literal = "2" },
        Token{ .tType = .RBRACKET, .literal = "]" },
        Token{ .tType = .LBRACE, .literal = "{" },
        Token{ .tType = .STRING, .literal = "foo" },
        Token{ .tType = .COLON, .literal = ":" },
        Token{ .tType = .STRING, .literal = "bar" },
        Token{ .tType = .RBRACE, .literal = "}" },

        // Token{ .tType = .EOF, .literal = "" },
    };
    var i: usize = 0;
    const arenaalloc = lexer.arenaAlloc.allocator();
    while (lexer.nextToken(arenaalloc)) |token| : (i += 1) {
        if (i >= expected_tokens.len) {
            break;
        }
        // std.debug.print("\n{any} TokenLit {s} ExpectedLit {s} \n", .{ token.tType, token.literal, expected_tokens[i].literal });
        try std.testing.expectEqual(expected_tokens[i].tType, token.tType);
    }
}
