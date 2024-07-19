const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Parser = @import("./parser.zig").Parser;
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
            var parser = Parser.init(allocator, lexer);
            var program = parser.parseProgram();
            const stringer = try program.string();
            std.debug.print("test out {s}", .{stringer.items});
            var token = lexer.nextToken(lexer.arenaAlloc.allocator());
            while (token.?.tType != TokenType.EOF) {
                std.debug.print("{any} -- {s}\n", .{ token.?.tType, token.?.literal });
                token = lexer.nextToken(lexer.arenaAlloc.allocator());
            }

            defer lexer.deinit();
            defer parser.deinit();
            defer stringer.deinit();
        }
    }
}
