const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("./ast.zig");
const Object = @import("./object.zig").Object;
const ObjectType = @import("./object.zig").ObjectType;
const Lexer = @import("./lexer.zig").Lexer;
const Parser = @import("./parser.zig").Parser;

pub fn Eval(node: Ast.Node) ?Object {
    switch (node) {
        .expression => |exp| {
            switch (exp) {
                .integerLiteral => |int| {
                    return Object{ .integer = Object.Integer{
                        .allocator = int.allocator,
                        .value = int.value,
                    } };
                },
                else => {
                    return null;
                },
            }
        },
        else => {
            return null;
        },
    }
}

test "TestEvalIntegerExpression" {
    const allocator = std.testing.allocator;
    const TestStruct = struct {
        input: []const u8,
        expected: i64,
    };
    const testTable = [_]TestStruct{
        TestStruct{ .expected = 5, .input = "5" },
        TestStruct{ .expected = 10, .input = "10" },
    };

    for (testTable) |value| {
        const evaluated = testEval(allocator, value.input);
        _ = testIntegerObject(evaluated, value.expected);
    }
}

fn testEval(allocator: Allocator, input: []const u8) Object {
    var l = Lexer.init(allocator, input);
    defer l.deinit();
    var parser = Parser.init(allocator, l);
    var program = parser.parseProgram();
    defer parser.deinit();
    defer program.deinit();

    if (Eval(Ast.Node{ .program = program })) |eval| {
        return eval;
    }
    return Object{ .nil = Object.Nil{} };
}

fn testIntegerObject(obj: Object, expected: i64) bool {
    switch (obj) {
        .integer => |int| {
            if (int.value != expected) {
                std.debug.print("object has wrong value. got={d}, want {d}\n", .{ int.value, expected });
                return false;
            }
            return true;
        },
        else => {
            std.debug.print("object is not integer got {any}\n", .{obj});
            return false;
        },
    }
}
