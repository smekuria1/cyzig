const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("./ast.zig");
const Object = @import("./object.zig").Object;
const ObjectType = @import("./object.zig").ObjectType;
const Lexer = @import("./lexer.zig").Lexer;
const Parser = @import("./parser.zig").Parser;
const Pretty = @import("./pretty.zig");

const NULL = Object{ .nil = Object.Nil{} };
const TRUE = Object{ .boolean = Object.Boolean{ .value = true } };
const FALSE = Object{ .boolean = Object.Boolean{ .value = false } };

pub fn Eval(node: Ast.Node) ?Object {
    switch (node) {
        .expression => |exp| {
            switch (exp) {
                .integerLiteral => |int| {
                    return Object{
                        .integer = Object.Integer{
                            .allocator = int.allocator,
                            .value = int.value,
                        },
                    };
                },
                // Not the best way of dealing with native bools
                .boolean => |boo| {
                    return nativeBooltoBoolean(boo.value);
                },
                .prefixExp => |pref| {
                    const evalRight = Eval(Ast.Node{ .expression = pref.right.* });
                    if (evalRight) |right| {
                        return evalPrefixExpression(pref.allocator, pref.operator, right);
                    }
                    return null;
                },
                .infixExp => |infix| {
                    const evalLeft = Eval(Ast.Node{ .expression = infix.left.* });
                    const evalRight = Eval(Ast.Node{ .expression = infix.right.* });
                    if (evalLeft) |left| {
                        if (evalRight) |right| {
                            return evalInfixExpression(infix.allocator, infix.operator, left, right);
                        }
                    }
                },
                .ifexp => |ifexpr| {
                    return evalIfExpression(ifexpr);
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
            return evalProgram(prog.statements.items);
        },
        .block => |block| {
            return evalBlockStatements(block);
        },
    }

    return null;
}

fn nativeBooltoBoolean(input: bool) Object {
    if (input) {
        return TRUE;
    }
    return FALSE;
}

fn evalInfixExpression(allocator: Allocator, operator: []const u8, left: Object, right: Object) Object {
    switch (left) {
        .integer => |leftint| {
            switch (right) {
                .integer => |rightint| {
                    return evalIntegerInfixExpression(allocator, operator, leftint, rightint);
                },
                else => return NULL,
            }
        },
        .boolean => |leftbool| {
            switch (right) {
                .boolean => |rightbool| {
                    return evalBooleanInfixExpression(operator, leftbool, rightbool);
                },
                else => return NULL,
            }
        },
        else => return NULL,
    }
}

fn evalIfExpression(ifexpr: Ast.IfExpression) Object {
    const condition = Eval(Ast.Node{ .expression = ifexpr.condition.* });
    if (condition) |cond| {
        if (isTruthy(cond)) {
            if (ifexpr.consequence) |consq| {
                // Pretty.print(consq.allocator, consq, .{ .max_depth = 30 }) catch unreachable;
                const evaluated = Eval(Ast.Node{ .block = consq });
                if (evaluated) |eval| {
                    return eval;
                }
            }
        } else if (ifexpr.alternative) |alt| {
            const evaluated = Eval(Ast.Node{ .block = alt });
            if (evaluated) |eval| {
                return eval;
            }
        }
    } else {
        return NULL;
    }
    return NULL;
}

fn isTruthy(obj: Object) bool {
    switch (obj) {
        .nil => return false,
        .boolean => |boo| {
            return boo.value;
        },
        else => return true,
    }
}

fn evalBooleanInfixExpression(operator: []const u8, left: Object.Boolean, right: Object.Boolean) Object {
    const leftval = left.value;
    const rightval = right.value;

    switch (operator[0]) {
        '=' => {
            return nativeBooltoBoolean(leftval == rightval);
        },
        '!' => {
            return nativeBooltoBoolean(leftval != rightval);
        },
        else => {
            return NULL;
        },
    }
}

fn evalPrefixExpression(allocator: Allocator, operator: []const u8, right: Object) Object {
    const op = operator[0];
    switch (op) {
        '!' => {
            return evalBangOperatorExpression(right);
        },
        '-' => {
            return evalMinusPrefixOperatorExpression(allocator, right);
        },
        else => {
            return NULL;
        },
    }
}

fn evalIntegerInfixExpression(allocator: Allocator, operator: []const u8, left: Object.Integer, right: Object.Integer) Object {
    const leftval = left.value;
    const rightval = right.value;

    switch (operator[0]) {
        '+' => return Object{
            .integer = Object.Integer{ .allocator = allocator, .value = leftval + rightval },
        },
        '-' => return Object{
            .integer = Object.Integer{ .allocator = allocator, .value = leftval - rightval },
        },
        '*' => return Object{
            .integer = Object.Integer{ .allocator = allocator, .value = leftval * rightval },
        },
        '/' => return Object{
            .integer = Object.Integer{ .allocator = allocator, .value = @divExact(leftval, rightval) },
        },
        '<' => {
            return nativeBooltoBoolean(leftval < rightval);
        },
        '>' => {
            return nativeBooltoBoolean(leftval > rightval);
        },
        '=' => {
            return nativeBooltoBoolean(leftval == rightval);
        },
        '!' => {
            return nativeBooltoBoolean(leftval != rightval);
        },
        else => {
            return NULL;
        },
    }
}

fn evalMinusPrefixOperatorExpression(allocator: Allocator, right: Object) Object {
    switch (right) {
        .integer => {
            const val = right.integer.value;
            return Object{ .integer = Object.Integer{ .allocator = allocator, .value = -val } };
        },
        else => return NULL,
    }
}

fn evalBangOperatorExpression(right: Object) Object {
    switch (right) {
        .boolean => |boo| {
            if (boo.value == true) {
                return FALSE;
            }
            return TRUE;
        },
        .nil => return FALSE,
        else => {
            return FALSE;
        },
    }
}

fn evalProgram(stmts: []Ast.Statement) Object {
    var result: Object = undefined;
    for (stmts) |value| {
        const ev = Eval(Ast.Node{ .statement = value });
        if (ev) |evaluated| {
            switch (evaluated) {
                .boolean => result = Object{ .boolean = evaluated.boolean },
                .integer => result = Object{ .integer = evaluated.integer },
                .nil => result = NULL,
                .returnval => {
                    result = Object{ .returnval = evaluated.returnval };
                    return result;
                },
            }
        }
    }

    return result;
}

fn evalBlockStatements(block: Ast.BlockStatement) Object {
    var result = NULL;

    for (block.statements.items) |stmt| {
        switch (stmt) {
            .expression => |expr| {
                if (expr.expression) |exp| {
                    const evaluated = Eval(Ast.Node{ .expression = exp.* });
                    if (evaluated) |ev| {
                        result = ev;
                    }
                }
            },
            .letStatement => |let| {
                if (let.value) |exp| {
                    const evaluated = Eval(Ast.Node{ .expression = exp.* });
                    if (evaluated) |ev| {
                        result = ev;
                    }
                }
            },
            .returnStatement => |ret| {
                if (ret.returnValue) |exp| {
                    _ = exp;
                    const evaluated = Eval(Ast.Node{ .statement = Ast.Statement{ .returnStatement = ret } });
                    if (evaluated) |ev| {
                        result = ev;
                        return result;
                    }
                }
            },
        }
    }

    return result;
}

fn testNullObject(obj: Object) bool {
    switch (obj) {
        .nil => return true,
        else => {
            std.debug.print("\nobject is not NULL got {any}\n", .{obj});
            return false;
        },
    }
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

fn testBooleanObject(obj: Object, expected: bool) bool {
    switch (obj) {
        .boolean => |boo| {
            if (boo.value != expected) {
                std.debug.print("\nobject has wrong value. got={any}, want {any}\n", .{ boo.value, expected });
                return false;
            }
            return true;
        },
        else => {
            std.debug.print("\nobject is not boolean got {any}\n", .{obj});
            return false;
        },
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

test "TestReturnStatements" {
    const allocator = std.testing.allocator;
    const TestStruct = struct {
        input: []const u8,
        expected: i64,
    };
    const testTable = [_]TestStruct{
        // TestStruct{ .expected = 5, .input = "return 5;" },
        // TestStruct{ .expected = 10, .input = "return 10;" },
        // TestStruct{ .expected = -5, .input = "return -5;" },
        // TestStruct{ .expected = -10, .input = "return -10;" },
        // TestStruct{ .expected = 10, .input = "return 2*5;" },

        TestStruct{
            .expected = 10,
            .input =
            \\ if(1999>200) {
            \\if (120 > 21) {
            \\return 10;
            \\}
            \\return 1;
            \\}
            ,
        },
    };

    for (testTable) |value| {
        const evaluated = testEval(allocator, value.input);
        const result = testIntegerObject(evaluated, value.expected);
        try std.testing.expect(result);
    }
}

// test "TestEvalIntegerExpression" {
//     const allocator = std.testing.allocator;
//     const TestStruct = struct {
//         input: []const u8,
//         expected: i64,
//     };
//     const testTable = [_]TestStruct{
//         TestStruct{ .expected = 5, .input = "5" },
//         TestStruct{ .expected = 10, .input = "10" },
//         TestStruct{ .expected = -5, .input = "-5" },
//         TestStruct{ .expected = -10, .input = "-10" },
//         TestStruct{ .expected = -10, .input = "-10" },
//         // weird way but works for now
//         TestStruct{ .expected = -2, .input = "(-1) + -1" },
//         TestStruct{ .expected = 10, .input = "5 + 5 + 5 + 5 - 10" },
//         TestStruct{ .expected = 37, .input = "3 * (3 * 3) + 10" },
//         TestStruct{ .expected = 50, .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10" },
//     };

//     for (testTable) |value| {
//         const evaluated = testEval(allocator, value.input);
//         const result = testIntegerObject(evaluated, value.expected);
//         try std.testing.expect(result);
//     }
// }
// test "TestBangOperator" {
//     const allocator = std.testing.allocator;
//     const TestStruct = struct {
//         input: []const u8,
//         expected: bool,
//     };
//     const testTable = [_]TestStruct{
//         TestStruct{ .expected = false, .input = "!true" },
//         TestStruct{ .expected = true, .input = "!false" },
//         TestStruct{ .expected = false, .input = "!5" },
//         TestStruct{ .expected = true, .input = "!!true" },
//         TestStruct{ .expected = false, .input = "!!false" },
//         TestStruct{ .expected = true, .input = "!!5" },
//     };

//     for (testTable) |value| {
//         const evaluated = testEval(allocator, value.input);
//         const result = testBooleanObject(evaluated, value.expected);
//         try std.testing.expect(result);
//     }
// }

// test "TestIfElseExpression" {
//     const allocator = std.testing.allocator;
//     const TestStruct = struct {
//         input: []const u8,
//         expected: Object,
//     };
//     const testTable = [_]TestStruct{
//         TestStruct{ .expected = Object{ .integer = Object.Integer{ .allocator = allocator, .value = 10 } }, .input = "if (true) { 10 }" },
//         TestStruct{ .expected = Object{ .integer = Object.Integer{ .allocator = allocator, .value = 10 } }, .input = "if (true) { 10 } else { 20 }" },
//         TestStruct{ .expected = Object{ .integer = Object.Integer{ .allocator = allocator, .value = 20 } }, .input = "if (false) { 10 } else { 20 }" },
//         TestStruct{ .expected = Object{ .integer = Object.Integer{ .allocator = allocator, .value = 10 } }, .input = "if (1) { 10 } else { 20 }" },
//         TestStruct{ .expected = Object{ .integer = Object.Integer{ .allocator = allocator, .value = 10 } }, .input = "if (1 < 2) { 10 }" },
//         TestStruct{ .expected = Object{ .integer = Object.Integer{ .allocator = allocator, .value = 10 } }, .input = "if (1 < 2) { 10 } else { 20 }" },
//         TestStruct{ .expected = Object{ .integer = Object.Integer{ .allocator = allocator, .value = 20 } }, .input = "if (1 > 2) { 10 } else { 20 }" },
//         TestStruct{ .expected = NULL, .input = "if (false) { 10 }" },
//     };
//     var passTest = true;
//     for (testTable) |value| {
//         const evaluated = testEval(allocator, value.input);
//         switch (value.expected) {
//             .integer => {
//                 const result = testIntegerObject(evaluated, value.expected.integer.value);
//                 if (passTest) {
//                     passTest = result;
//                 }
//             },
//             .boolean => {
//                 const result = testBooleanObject(evaluated, value.expected.boolean.value);
//                 if (passTest) {
//                     passTest = result;
//                 }
//             },
//             .nil => {
//                 const result = testNullObject(evaluated);
//                 if (passTest) {
//                     passTest = result;
//                 }
//             },
//             else => passTest = false,
//         }
//     }
//     try std.testing.expect(passTest);
// }
// test "TestEvalBooleanExpression" {
//     const allocator = std.testing.allocator;
//     const TestStruct = struct {
//         input: []const u8,
//         expected: bool,
//     };
//     const testTable = [_]TestStruct{
//         TestStruct{ .expected = true, .input = "true" },
//         TestStruct{ .expected = false, .input = "false" },
//         TestStruct{ .expected = true, .input = "1 < 2" },
//         TestStruct{ .expected = false, .input = "1 > 2" },
//         TestStruct{ .expected = false, .input = "1 == 2" },
//         TestStruct{ .expected = true, .input = "1 != 2" },
//         TestStruct{ .expected = true, .input = "true == true" },
//         TestStruct{ .expected = false, .input = "true == false" },
//         TestStruct{ .expected = false, .input = "true != true" },
//         TestStruct{ .expected = true, .input = "true != false" },
//         TestStruct{ .expected = false, .input = "(1 < 2) != true" },
//         TestStruct{ .expected = false, .input = "(1 > 2) == true" },
//     };

//     for (testTable) |value| {
//         const evaluated = testEval(allocator, value.input);
//         const result = testBooleanObject(evaluated, value.expected);
//         try std.testing.expect(result);
//     }
// }
