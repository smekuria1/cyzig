const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const max_length = 1024;
// We can read any arbitrary number type with number_type

pub fn start(allocator: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();

    const buffer = try allocator.alloc(u8, 1024);
    while (true) {
        std.debug.print("\n--> ", .{});
        if (try stdin.readUntilDelimiterOrEof(buffer[0..], '\n')) |value| {
            // const line = std.mem.trimRight(u8, value[0 .. value.len - 1], "\r");
            var lexer = Lexer.init(allocator, value);

            var token = lexer.nextToken();
            while (token.?.tType != TokenType.EOF) {
                std.debug.print("{any} -- {s}\n", .{ token.?.tType, token.?.literal });
                token = lexer.nextToken();
            }
        }
    }
}
