const std = @import("std");
const Allocator = std.mem.Allocator;
pub const Token = struct {
    tType: TokenType,
    literal: []const u8,
};

pub const TokenType = enum(u8) {
    // Define your token types here
    ILLEGAL,
    EOF,
    IDENT,
    INT, // Operators
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE, // Keywords
    FUNCTION,
    LET,
    VAR,
};


