const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Parser = @import("./parser.zig").Parser;
const max_length = 1024;
const pretty = @import("./pretty.zig");
const Evaluator = @import("./evaluator.zig");
const Ast = @import("./ast.zig");
const Env = @import("./environment.zig").Environment;
pub fn start(allocator: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();

    const buffer = try allocator.alloc(u8, 1024);
    var arena = std.heap.ArenaAllocator.init(allocator);
    const arenaAlloc = arena.allocator();
    const env = try Env.init(arenaAlloc);
    while (true) {
        std.debug.print("\n--> ", .{});
        if (try stdin.readUntilDelimiterOrEof(buffer[0..], '\n')) |value| {
            if (std.mem.eql(u8, value, "q")) {
                break;
            }
            const lexer = Lexer.init(allocator, value);
            var parser = Parser.init(allocator, lexer);
            var program = parser.parseProgram();

            if (parser.checkParserErros()) {
                printParseErrors(parser.errors);
                continue;
            }
            // TODO: Work out a way to visualize the AST as a tree.
            // try pretty.print(allocator, program.statements.items[0], .{  });
            const stringer = try program.string();
            std.debug.print("{s} \n", .{stringer.items});
            const evaluated = Evaluator.Eval(arenaAlloc, Ast.Node{ .program = program }, env);

            if (evaluated) |ev| {
                switch (ev) {
                    .boolean => |boo| {
                        const booStr = try boo.inspect();
                        std.debug.print("Bool {s}\n", .{booStr});
                    },
                    .integer => |int| {
                        const intStr = try int.inspect();
                        std.debug.print("Int {s}\n", .{intStr});
                    },
                    .returnval => |ret| {
                        const retstr = try ret.inspect();
                        std.debug.print("Return {s}\n", .{retstr});
                    },
                    .nil => |NULL| {
                        std.debug.print("Nil {s}\n", .{try NULL.inspect()});
                    },
                    .eror => |err| {
                        std.debug.print("Repl ERROR {s} \n", .{try err.inspect()});
                    },
                    .function => |fun| {
                        const funstring = try fun.inspect();
                        std.debug.print("Function {s}\n", .{funstring});
                        // allocator.free(funstring);
                    },
                    .string => |str| {
                        const strString = try str.inspect();
                        std.debug.print("String {s}\n", .{strString});
                        // allocator.free(strString);
                    },
                    .builtin => |built| {
                        const builtstr = try built.inspect();
                        std.debug.print("builtin {s}\n", .{builtstr});
                    },
                    .array => |array| {
                        const arrStr = try array.inspect();
                        std.debug.print("array {s}\n", .{arrStr});
                        // allocator.free(arrStr);
                    },
                }
            }
            // env.printEnvironment();
            // try pretty.print(allocator, env.store, .{ .max_depth = 100 });
            // std.debug.print("\n {any} \n", .{env.store.get("a")});
            // defer lexer.deinit();
            defer parser.deinit();
            defer stringer.deinit();
        }
    }
    defer arena.deinit();
}

fn printParseErrors(errors: std.ArrayList([]const u8)) void {
    std.debug.print("Main Error is usually the first one\n", .{});
    std.debug.print("Parser Errors: \n", .{});
    for (errors.items) |value| {
        std.debug.print("\t{s}\n", .{value});
    }
}
