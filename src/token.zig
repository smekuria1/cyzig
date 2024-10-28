const std = @import("std");
const Allocator = std.mem.Allocator;
pub const Token = struct {
    tType: TokenType,
    literal: []const u8,
    alloc: bool = false,
};

pub const TokenType = enum(u8) {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    //
    LT,
    GT,
    EQ,
    NOT_EQ,
    //
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    // Keywords
    FUNCTION,
    LET,
    VAR,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,

    //Datatypes
    STRING,
};
