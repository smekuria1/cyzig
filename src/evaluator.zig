const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("./ast.zig");
const Object = @import("./object.zig").Object;
const ObjectType = @import("./object.zig").ObjectType;
const Lexer = @import("./lexer.zig").Lexer;
const Parser = @import("./parser.zig").Parser;
const Pretty = @import("./pretty.zig");
const Environment = @import("./environment.zig").Environment;
const NULL = Object{ .nil = Object.Nil{} };
const TRUE = Object{ .boolean = Object.Boolean{ .value = true } };
const FALSE = Object{ .boolean = Object.Boolean{ .value = false } };
pub const stackMax = 4000;
pub var evalCount: usize = 0;

pub fn Eval(node: Ast.Node, environment: *Environment) ?Object {
    if (evalCount > stackMax) {
        return Object{ .eror = Object.Error{
            .message = "stack overflow",
            .stop = true,
        } };
    }
    evalCount += 1;
    std.log.warn("Stack depth {d}\n", .{evalCount});
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
                    const evalRight = Eval(Ast.Node{ .expression = pref.right.* }, environment);
                    if (isError(evalRight)) {
                        return evalRight.?;
                    }
                    if (evalRight) |right| {
                        return evalPrefixExpression(pref.allocator, pref.operator, right);
                    }
                    return null;
                },
                .infixExp => |infix| {
                    const evalLeft = Eval(Ast.Node{ .expression = infix.left.* }, environment);
                    if (isError(evalLeft)) {
                        return evalLeft.?;
                    }
                    const evalRight = Eval(Ast.Node{ .expression = infix.right.* }, environment);
                    if (isError(evalRight)) {
                        return evalRight.?;
                    }
                    if (evalLeft) |left| {
                        if (evalRight) |right| {
                            return evalInfixExpression(infix.allocator, infix.operator, left, right);
                        }
                    }
                },
                .ifexp => |ifexpr| {
                    return evalIfExpression(ifexpr, environment);
                },
                .identifier => |ident| {
                    return evalIdentifierExpression(ident, environment);
                },
                .function => |fun| {
                    //Pretty.print(fun.allocator, fun, .{ .max_depth = 100 }) catch unreachable;
                    const funObj = Object{
                        .function = Object.Function{
                            .enviornment = environment,
                            .body = fun.body,
                            .parameters = fun.parameters,
                            .allocatorr = fun.allocator,
                        },
                    };

                    return funObj;
                },
                .callExpression => |call| {
                    // std.debug.print("calling function {any} \n", .{call.function});
                    const function = Eval(Ast.Node{ .expression = call.function.* }, environment);
                    if (isError(function)) {
                        return function;
                    }

                    const args = evalExpressions(call.allocator, call.arguments, environment) catch unreachable;
                    if (args.len == 1 and isError(args[0])) {
                        return args[0];
                    }
                    return applyFunction(function.?, args);
                },
            }
        },
        .statement => |stmt| {
            switch (stmt) {
                .expression => |expr| {
                    if (expr.expression) |exp| {
                        const obj = Eval(Ast.Node{ .expression = exp.* }, environment);
                        return obj;
                    }
                },
                .letStatement => |let| {
                    if (let.value) |exp| {
                        //std.debug.print("\n{any}\n", .{exp});
                        const obj = Eval(Ast.Node{ .expression = exp.* }, environment);
                        // std.debug.print("let.name in statement eval {s}\n", .{let.name.value});
                        environment.put(let.name.value, obj.?);
                        // std.debug.print("After put statement Store {any} \n", .{environment.store.get(let.name.value).?});
                        return obj;
                    }
                },
                .returnStatement => |ret| {
                    if (ret.returnValue) |exp| {
                        return Eval(Ast.Node{ .expression = exp.* }, environment);
                    }
                },
            }
        },
        .program => |prog| {
            return evalProgram(prog.statements.items, environment);
        },
        .block => |block| {
            return evalBlockStatements(block, environment);
        },
    }

    return null;
}

fn evalExpressions(allocator: Allocator, arguments: ?std.ArrayList(*Ast.Expression), env: *Environment) Allocator.Error![]?Object {
    var objs = std.ArrayList(?Object).init(allocator);
    if (arguments) |args| {
        for (args.items) |exps| {
            const evaluated = Eval(Ast.Node{ .expression = exps.* }, env);
            if (isError(evaluated)) {
                objs.clearRetainingCapacity();
                objs.append(evaluated) catch unreachable;
                return objs.toOwnedSlice();
            }
            objs.append(evaluated) catch unreachable;
        }
        return objs.toOwnedSlice();
    }
    return objs.toOwnedSlice();
}

fn newFuncError(message: []const u8, obj: Object) Object {
    var buf: [1024]u8 = undefined;
    const mess = std.fmt.bufPrint(&buf, "{s} {any}\n", .{ message, obj }) catch unreachable;
    const err = Object{ .eror = Object.Error{
        .message = mess,
        .stop = true,
    } };
    return err;
}

fn applyFunction(function: Object, args: []?Object) Object {
    switch (function) {
        .function => |fun| {
            const extendedEnv = extendFunctionEnv(fun, args);
            // extendedEnv.printEnvironment();
            const evaluated = Eval(Ast.Node{ .block = fun.body.? }, extendedEnv);
            // std.debug.print("Evaluating body {any} \n", .{evaluated.?});
            // extendedEnv.deinit(fun.allocatorr);
            return unwrapReturnValue(evaluated);
        },
        else => |case| {
            return newFuncError("not a function", case);
        },
    }
}

fn extendFunctionEnv(fun: Object.Function, args: []?Object) *Environment {
    var env = Environment.initEnclosed(fun.enviornment) catch unreachable;
    if (fun.parameters) |params| {
        for (0.., params.items) |pId, param| {
            env.put(param.string(), args[pId].?);
        }
    }
    fun.allocatorr.free(args);
    return env;
}
fn unwrapReturnValue(evaluated: ?Object) Object {
    if (evaluated) |eval| {
        if (eval == .returnval) {
            return eval.returnval.value.*;
        }
        return eval;
    }
    return FALSE;
}
fn isError(obj: ?Object) bool {
    if (obj) |o| {
        if (o == .eror) {
            return true;
        }
    }
    return false;
}

fn nativeBooltoBoolean(input: bool) Object {
    if (input) {
        return TRUE;
    }
    return FALSE;
}
fn evalIdentifierExpression(ident: Ast.Identifier, environment: *Environment) Object {
    // std.debug.print("Ident value in eval ident {s}\n", .{ident.value});
    if (environment.get(ident.value)) |val| {
        return val;
    }
    var buf: [32]u8 = undefined;
    const mess = std.fmt.bufPrint(&buf, "ident not found {s}\n", .{ident.value}) catch unreachable;
    return Object{ .eror = Object.Error{ .message = mess } };
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
            return newPrefixError(operator);
        },
    }
}

fn newPrefixError(operator: []const u8) Object {
    var buf: [1024]u8 = undefined;
    const mess = std.fmt.bufPrint(&buf, "Unknown operator: {s}", .{operator}) catch unreachable;
    return Object{ .eror = Object.Error{ .message = mess } };
}

fn evalInfixExpression(allocator: Allocator, operator: []const u8, left: Object, right: Object) Object {
    switch (left) {
        .integer => |leftint| {
            switch (right) {
                .integer => |rightint| {
                    return evalIntegerInfixExpression(allocator, operator, leftint, rightint);
                },
                else => return newInfixError(operator, left, right, true),
            }
        },
        .boolean => |leftbool| {
            switch (right) {
                .boolean => |rightbool| {
                    return evalBooleanInfixExpression(operator, leftbool, rightbool);
                },
                else => return newInfixError(operator, left, right, true),
            }
        },
        else => return newInfixError(operator, left, right, false),
    }
}

fn newInfixError(operator: []const u8, left: Object, right: Object, typemis: bool) Object {
    var buf: [1024]u8 = undefined;
    if (typemis) {
        const mess = std.fmt.bufPrint(&buf, "type mismatch: {any} {s} {any} \n", .{ left.integer.oType(), operator, right.boolean.oType() }) catch unreachable;
        return Object{ .eror = Object.Error{ .message = mess } };
    }
    const mess = std.fmt.bufPrint(&buf, "Unknown operator {any} {s} {any} \n", .{ left, operator, right }) catch unreachable;
    return Object{ .eror = Object.Error{
        .message = mess,
    } };
}

fn evalIfExpression(ifexpr: Ast.IfExpression, environment: *Environment) Object {
    const condition = Eval(Ast.Node{ .expression = ifexpr.condition.* }, environment);
    if (isError(condition)) {
        return condition.?;
    }
    if (condition) |cond| {
        if (isTruthy(cond)) {
            if (ifexpr.consequence) |consq| {
                // Pretty.print(consq.allocator, consq, .{ .max_depth = 30 }) catch unreachable;
                const evaluated = Eval(Ast.Node{ .block = consq }, environment);
                if (isError(evaluated)) {
                    return evaluated.?;
                }
                if (evaluated) |eval| {
                    return eval;
                }
            }
        } else if (ifexpr.alternative) |alt| {
            const evaluated = Eval(Ast.Node{ .block = alt }, environment);
            if (isError(evaluated)) {
                return evaluated.?;
            }
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
            return newInfixError(operator, Object{ .boolean = left }, Object{ .boolean = right }, false);
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
        else => return newPrefixError("-"),
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

fn evalProgram(stmts: []Ast.Statement, environment: *Environment) Object {
    var result: Object = undefined;
    for (stmts) |value| {
        const ev = Eval(Ast.Node{ .statement = value }, environment);
        if (ev) |evaluated| {
            switch (evaluated) {
                .boolean => result = Object{ .boolean = evaluated.boolean },
                .integer => result = Object{ .integer = evaluated.integer },
                .nil => result = NULL,
                .returnval => {
                    result = Object{ .returnval = evaluated.returnval };
                    return result;
                },
                .eror => {
                    result = Object{ .eror = evaluated.eror };
                    return result;
                },
                .function => result = Object{ .function = evaluated.function },
            }
        }
    }

    return result;
}

fn evalBlockStatements(block: Ast.BlockStatement, environment: *Environment) Object {
    var result = NULL;

    for (block.statements.items) |stmt| {
        // std.debug.print("Evaluating Block\n", .{});
        // Pretty.print(block.allocator, stmt, .{ .max_depth = 100 }) catch unreachable;
        switch (stmt) {
            .expression => |expr| {
                if (expr.expression) |exp| {
                    const evaluated = Eval(Ast.Node{ .expression = exp.* }, environment);
                    // std.debug.print("Evaluating expression in Block {any} \n", .{evaluated});
                    if (evaluated) |ev| {
                        result = ev;
                        switch (ev) {
                            inline else => |case| {
                                if (case.stop == true) {
                                    return result;
                                }
                            },
                        }
                    }
                }
            },
            .letStatement => |let| {
                if (let.value) |exp| {
                    const evaluated = Eval(Ast.Node{ .expression = exp.* }, environment);
                    if (evaluated) |ev| {
                        result = ev;
                    }
                }
            },
            .returnStatement => |ret| {
                if (ret.returnValue) |exp| {
                    _ = exp;
                    const evaluated = Eval(Ast.Node{ .statement = Ast.Statement{ .returnStatement = ret } }, environment);
                    if (evaluated) |ev| {
                        result = ev;
                        switch (result) {
                            inline else => |*case| {
                                case.*.stop = true;
                            },
                        }
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
    var arena = std.heap.ArenaAllocator.init(allocator);
    var l = Lexer.init(allocator, input);
    defer l.deinit();
    var parser = Parser.init(allocator, l);
    var program = parser.parseProgram();
    defer parser.deinit();
    defer program.deinit();
    const env = Environment.init(arena.allocator()) catch unreachable;
    defer arena.deinit();
    // Pretty.print(allocator, program.statements.items, .{ .max_depth = 100 }) catch unreachable;
    const object = Eval(Ast.Node{ .program = program }, env);
    if (object) |eval| {
        // Pretty.print(allocator, eval, .{ .max_depth = 100 }) catch unreachable;
        return eval;
    }
    return Object{ .nil = Object.Nil{} };
}

test "Test Recursive Functions" {
    const allocator = std.testing.allocator;
    const TestStruct = struct {
        input: []const u8,
        expected: i64,
    };
    const testTable = [_]TestStruct{
        TestStruct{ .expected = 12, .input = 
        \\let counter = fn(x) {
        \\if (x > 500) {
        \\return true;
        \\} else {
        \\let foobar = 9999;
        \\counter(x + 1);
        \\}
        \\};
        \\counter(0);
        },
    };

    const evaluated = testEval(allocator, testTable[0].input);
    std.debug.print("Evalcount, {d}\n", .{evalCount});
    std.debug.print("Error {s}\n", .{evaluated.eror.message});
    const result = testBooleanObject(evaluated, true);
    try std.testing.expect(result);
}

// test "Test Closures" {
//     const allocator = std.testing.allocator;
//     const TestStruct = struct {
//         input: []const u8,
//         expected: i64,
//     };
//     const testTable = [_]TestStruct{
//         TestStruct{ .expected = 12, .input =
//         \\let newAdder = fn(x) {
//         \\fn(y) { x + y };
//         \\};
//         \\let addTwo = newAdder(2);
//         \\addTwo(10);};
//         },
//     };
//     const evaluated = testEval(allocator, testTable[0].input);
//     const result = testIntegerObject(evaluated, testTable[0].expected);
//     try std.testing.expect(result);
// }
// test "TestFunctionObject" {
//     const allocator = std.testing.allocator;
//     const input = "fn(x) { x + 2; };";
//     // const evaluated = testEval(allocator, input);
//     var l = Lexer.init(allocator, input);
//     defer l.deinit();
//     var parser = Parser.init(allocator, l);
//     var program = parser.parseProgram();
//     defer parser.deinit();
//     defer program.deinit();
//     var arena = std.heap.ArenaAllocator.init(allocator);
//     const env = Environment.init(arena.allocator()) catch unreachable;
//     defer arena.deinit();
//     const object = Eval(Ast.Node{ .program = program }, env);
//     if (object) |eval| {
//         switch (eval) {
//             .function => |fun| {
//                 if (fun.parameters) |params| {
//                     try std.testing.expectEqual(1, params.items.len);
//                     try std.testing.expectEqualSlices(u8, "x", params.items[0].string());
//                 }
//                 const bodyString = try fun.body.?.string();
//                 try std.testing.expectEqualSlices(u8, "(x + 2)", bodyString);
//                 allocator.free(bodyString);
//                 // try Pretty.print(allocator, bodyString, .{ .max_depth = 100 });
//                 // try std.testing.expectEqualSlices(u8, "(x+2)", bodyString);
//
//             },
//             else => |case| {
//                 std.debug.print("Object is not function {any} \n", .{case});
//                 try std.testing.expect(false);
//             },
//         }
//     }
// }
//
// test "Test FunctionApplication" {
//     const allocator = std.testing.allocator;
//     const TestStruct = struct {
//         input: []const u8,
//         expected: i64,
//     };
//
//     const testTable = [_]TestStruct{
//         TestStruct{ .expected = 5, .input = "let identity = fn(x) { x; }; identity(5);" },
//         // TestStruct{ .expected = 5, .input = "fn(x) { x; }(5)" },
//     };
//     for (testTable) |value| {
//         const evaluated = testEval(allocator, value.input);
//         const result = testIntegerObject(evaluated, value.expected);
//         try std.testing.expect(result);
//     }
// }
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
//
//     for (testTable) |value| {
//         const evaluated = testEval(allocator, value.input);
//         // std.debug.print("{any} \n Test out TestEvalIntegerExpression\n", .{evaluated});
//         const result = testIntegerObject(evaluated, value.expected);
//         try std.testing.expect(result);
//     }
// }
// // TODO: FIx Error Handling to print out wrong objects
// test "TestErrorHandler" {
//     const allocator = std.testing.allocator;
//     const TestStruct = struct {
//         expected: []const u8,
//         input: []const u8,
//         typemis: bool,
//     };
//     const testTable = [_]TestStruct{ TestStruct{
//         .expected = "type mismatch",
//         .input = "5 + true",
//         .typemis = true,
//     }, TestStruct{
//         .expected = "Unknown operator",
//         .input = "true + false",
//         .typemis = false,
//     }, TestStruct{
//         .expected = "Unknown operator",
//         .input = "-true",
//         .typemis = false,
//     }, TestStruct{
//         .expected = "ident not found ",
//         .input = "foobar",
//         .typemis = false,
//     } };
//     for (testTable) |value| {
//         const evaluated = testEval(allocator, value.input);
//         switch (evaluated) {
//             .eror => {
//                 if (value.typemis) {
//                     try std.testing.expectEqualSlices(u8, value.expected, evaluated.eror.message[0..13]);
//                     continue;
//                 }
//                 try std.testing.expectEqualSlices(u8, value.expected, evaluated.eror.message[0..16]);
//             },
//             inline else => |case| {
//                 std.debug.print("no error object returned, got {any}\n", .{case});
//                 try std.testing.expect(false);
//             },
//         }
//     }
// }
// test "TestLetStatements" {
//     const allocator = std.testing.allocator;
//     const TestStruct = struct {
//         input: []const u8,
//         expected: i64,
//     };
//
//     const testTable = [_]TestStruct{
//         TestStruct{ .expected = 5, .input =
//         \\let a = 5;
//         \\let b = a > 3;
//         \\a;
//         \\let c = a * 99;
//         \\ if (true) {let p = 20;} else {1};
//         \\ let d = if (c>a) {99} else {100};
//         \\d;
//         \\ d * c * a;
//         \\a;
//         },
//     };
//
//     for (testTable) |value| {
//         const evalualted = testEval(allocator, value.input);
//         const result = testIntegerObject(evalualted, value.expected);
//         // std.debug.print("{any}\n Test Out letStatement \n", .{evalualted});
//         try std.testing.expect(result);
//     }
// }
// test "TestReturnStatements" {
//     const allocator = std.testing.allocator;
//     const TestStruct = struct {
//         input: []const u8,
//         expected: i64,
//     };
//     const testTable = [_]TestStruct{
//         TestStruct{ .expected = 5, .input = "return 5;" },
//         TestStruct{ .expected = 10, .input = "return 10;" },
//         TestStruct{ .expected = -5, .input = "return -5;" },
//         TestStruct{ .expected = -10, .input = "return -10;" },
//         TestStruct{ .expected = 10, .input = "return 2*5;" },
//
//         TestStruct{
//             .expected = 10,
//             .input =
//             \\ if(1999>200) {
//             \\
//             \\if (120 > 21) {return 10} else {return 200}
//             \\
//             \\return 1;
//             \\}
//             ,
//         },
//     };
//
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
//
//     for (testTable) |value| {
//         const evaluated = testEval(allocator, value.input);
//         const result = testBooleanObject(evaluated, value.expected);
//         try std.testing.expect(result);
//     }
// }
//
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
//
//     for (testTable) |value| {
//         const evaluated = testEval(allocator, value.input);
//         const result = testBooleanObject(evaluated, value.expected);
//         try std.testing.expect(result);
//     }
// }
