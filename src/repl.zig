const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Parser = @import("./parser.zig").Parser;
const max_length = 1024;
const pretty = @import("./pretty.zig");
const Evaluator = @import("./evaluator.zig");
const Ast = @import("./ast.zig");

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

            if (parser.checkParserErros()) {
                printParseErrors(parser.errors);
                continue;
            }
            // TODO: Work out a way to visualize the AST as a tree.
            // try pretty.print(allocator, program.statements.items[0], .{  });
            const stringer = try program.string();
            // std.debug.print("{s}", .{stringer.items});
            const evaluated = Evaluator.Eval(Ast.Node{ .program = program });
            if (evaluated) |ev| {
                switch (ev) {
                    .boolean => |boo| {
                        const booStr = try boo.inspect();
                        std.debug.print("{s}\n", .{booStr});
                    },
                    .integer => |int| {
                        const intStr = try int.inspect();
                        std.debug.print("{s}\n", .{intStr});
                    },
                    .returnval => |ret| {
                        const retstr = try ret.inspect();
                        std.debug.print("{s}\n", .{retstr});
                    },
                    .nil => |NULL| {
                        std.debug.print("{s}\n", .{NULL.inspect()});
                    },
                }
            }
            defer lexer.deinit();
            defer parser.deinit();
            defer stringer.deinit();
        }
    }
}

fn printParseErrors(errors: std.ArrayList([]const u8)) void {
    std.debug.print("WHOOPSIE YOU MADE AN ERROR\n \t (high chance its the parser freaking out)\n", .{});
    std.debug.print("Parser Errors: \n", .{});
    for (errors.items) |value| {
        std.debug.print("\t{s}\n", .{value});
    }
}
