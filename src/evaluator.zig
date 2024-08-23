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
        .statement => |stmt| {
            switch (stmt) {
                .expression => |expr| {
                    if (expr.expression) |exp| {
                        return Eval(Ast.Node{ .expression = exp.* });
                    }
                },
                .letStatement => |let| {
                    if (let.value) |exp| {
                        return Eval(Ast.Node{ .expression = exp.* });
                    }
                },
                .returnStatement => |ret| {
                    if (ret.returnValue) |exp| {
                        return Eval(Ast.Node{ .expression = exp.* });
                    }
                },
            }
        },
        .program => |prog| {
            return evalStatements(prog.statements);
        },
    }

    return null;
}
// TODO: FIX this 8/22/2024
fn evalStatements(stmts: std.ArrayList(Ast.Statement)) Object {
    var result: Object = undefined;
    for (stmts.items) |value| {
        const ev = Eval(Ast.Node{ .statement = value });
        if (ev) |evaluated| {
            switch (evaluated) {
                .boolean => result = Object{ .boolean = evaluated.boolean },
                .integer => result = Object{ .integer = evaluated.integer },
                .nil => result = Object{ .nil = evaluated.nil },
            }
        }
    }

    return result;
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
                std.debug.print("\nobject has wrong value. got={d}, want {d}\n", .{ int.value, expected });
                return false;
            }
            return true;
        },
        else => {
            std.debug.print("\nobject is not integer got {any}\n", .{obj});
            return false;
        },
    }
}
