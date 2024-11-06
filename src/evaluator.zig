const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("./ast.zig");
const Object = @import("./object.zig").Object;
const BuiltinMap = @import("./object.zig").builtFns;
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

pub fn Eval(arenaAlloc: Allocator, node: Ast.Node, environment: *Environment) ?Object {
    if (evalCount > stackMax) {
        return Object{ .eror = Object.Error{
            .message = "stack overflow",
            .stop = true,
        } };
    }
    evalCount += 1;
    // std.log.warn("Stack depth {d}\n", .{evalCount});
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
                    const evalRight = Eval(arenaAlloc, Ast.Node{ .expression = pref.right.* }, environment);
                    if (isError(evalRight)) {
                        return evalRight.?;
                    }
                    if (evalRight) |right| {
                        return evalPrefixExpression(arenaAlloc, pref.operator, right);
                    }
                    return null;
                },
                .infixExp => |infix| {
                    const evalLeft = Eval(arenaAlloc, Ast.Node{ .expression = infix.left.* }, environment);
                    if (isError(evalLeft)) {
                        return evalLeft.?;
                    }
                    const evalRight = Eval(arenaAlloc, Ast.Node{ .expression = infix.right.* }, environment);
                    if (isError(evalRight)) {
                        return evalRight.?;
                    }
                    if (evalLeft) |left| {
                        if (evalRight) |right| {
                            return evalInfixExpression(arenaAlloc, infix.operator, left, right);
                        }
                    }
                },
                .ifexp => |ifexpr| {
                    return evalIfExpression(arenaAlloc, ifexpr, environment);
                },
                .identifier => |ident| {
                    return evalIdentifierExpression(arenaAlloc, ident, environment);
                },
                .function => |fun| {
                    //Pretty.print(fun.allocator, fun, .{ .max_depth = 100 }) catch unreachable;
                    const funObj = Object{
                        .function = Object.Function{
                            .enviornment = environment,
                            .body = fun.body,
                            .parameters = fun.parameters,
                            .allocatorr = arenaAlloc,
                        },
                    };

                    return funObj;
                },
                .callExpression => |call| {
                    // std.debug.print("calling function {any} \n", .{call.function});
                    const function = Eval(arenaAlloc, Ast.Node{ .expression = call.function.* }, environment);
                    if (isError(function)) {
                        return function;
                    }

                    const args = evalExpressions(arenaAlloc, call.arguments, environment) catch unreachable;
                    if (args.len == 1 and isError(args[0])) {
                        return args[0];
                    }
                    return applyFunction(arenaAlloc, function.?, args);
                },
                .stringLiteral => |str| {
                    return Object{ .string = Object.String{
                        .value = str.value,
                        .allocator = str.allocator,
                    } };
                },
                .indexExpression => |indexExpr| {
                    const left = Eval(arenaAlloc, Ast.Node{ .expression = indexExpr.left.* }, environment);
                    if (isError(left)) {
                        return left;
                    }
                    const index = Eval(arenaAlloc, Ast.Node{ .expression = indexExpr.index.?.* }, environment);
                    if (isError(index)) {
                        return index;
                    }
                    return evalIndexExpression(arenaAlloc, left, index);
                },
                .arrayLiteral => |arr| {
                    const elements = evalExpressions(arenaAlloc, arr.elements, environment) catch unreachable;
                    if (elements.len == 1 and isError(elements[0])) {
                        return elements[0];
                    }
                    return Object{ .array = Object.Array{
                        .elements = elements,
                        .allocator = arr.allocator,
                    } };
                },
            }
        },
        .statement => |stmt| {
            switch (stmt) {
                .expression => |expr| {
                    if (expr.expression) |exp| {
                        const obj = Eval(arenaAlloc, Ast.Node{ .expression = exp.* }, environment);
                        return obj;
                    }
                },
                .letStatement => |let| {
                    if (let.value) |exp| {
                        //std.debug.print("\n{any}\n", .{exp});
                        const obj = Eval(arenaAlloc, Ast.Node{ .expression = exp.* }, environment);
                        // std.debug.print("let.name in statement eval {s}\n", .{let.name.value});
                        environment.put(let.name.value, obj.?);
                        // std.debug.print("After put statement Store {any} \n", .{environment.store.get(let.name.value).?});
                        return obj;
                    }
                },
                .returnStatement => |ret| {
                    if (ret.returnValue) |exp| {
                        return Eval(arenaAlloc, Ast.Node{ .expression = exp.* }, environment);
                    }
                },
            }
        },
        .program => |prog| {
            return evalProgram(arenaAlloc, prog.statements.items, environment);
        },
        .block => |block| {
            return evalBlockStatements(arenaAlloc, block, environment);
        },
    }

    return null;
}
fn evalIndexExpression(arenaAlloc: Allocator, left: ?Object, index: ?Object) ?Object {
    if (left) |l| {
        if (index) |in| {
            if (getObjType(l) == .ARRAY_OBJ and getObjType(in) == .INTEGER_OBJ) {
                return evalArrayIndexExpression(l, in);
            }

            const mess = std.fmt.allocPrint(arenaAlloc, "index operator not supported on {any}", .{getObjType(l)}) catch unreachable;
            return Object{ .eror = Object.Error{ .message = mess } };
        }
    }
    return null;
}

fn evalArrayIndexExpression(array: Object, index: Object) ?Object {
    const arrayObj = array.array;
    const idx = index.integer.value;
    var max: i64 = @intCast(arrayObj.elements.len);
    max = max - 1;
    if (idx < 0 or idx > max) {
        return NULL;
    }
    return arrayObj.elements[@intCast(idx)];
}

fn evalExpressions(allocator: Allocator, arguments: ?std.ArrayList(*Ast.Expression), env: *Environment) Allocator.Error![]?Object {
    var objs = std.ArrayList(?Object).init(allocator);
    if (arguments) |args| {
        for (args.items) |exps| {
            const evaluated = Eval(allocator, Ast.Node{ .expression = exps.* }, env);
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

fn newFuncError(allocator: Allocator, message: []const u8, obj: Object) Object {
    // var buf: [1024]u8 = undefined;
    // const mess = std.fmt.bufPrint(&buf, "{s} {any}\n", .{ message, getObjType(obj) }) catch unreachable;
    // const err = Object{ .eror = Object.Error{
    //     .message = mess,
    //     .stop = true,
    // } };
    return Object.Error.newError(allocator, message, getObjType(obj)) catch unreachable;
}

fn applyFunction(allocator: Allocator, function: Object, args: []?Object) Object {
    switch (function) {
        .function => |fun| {
            const extendedEnv = extendFunctionEnv(fun, args);
            // extendedEnv.printEnvironment();
            const evaluated = Eval(allocator, Ast.Node{ .block = fun.body.? }, extendedEnv);
            // std.debug.print("Evaluating body {any} \n", .{evaluated.?});
            // extendedEnv.deinit(fun.allocatorr);
            return unwrapReturnValue(evaluated);
        },
        .builtin => |builtfn| {
            const ret = builtfn.Fn(allocator, args);
            defer allocator.free(args);
            return ret;
        },
        else => |case| {
            return newFuncError(allocator, "not a function", case);
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
fn evalIdentifierExpression(arenaAlloc: Allocator, ident: Ast.Identifier, environment: *Environment) Object {
    // std.debug.print("Ident value in eval ident {s}\n", .{ident.value});
    if (environment.get(ident.value)) |val| {
        return val;
    }
    if (BuiltinMap.get(ident.value)) |bFn| {
        return Object{ .builtin = bFn };
    }
    const mess = std.fmt.allocPrint(arenaAlloc, "ident not found {s}\n", .{ident.value}) catch unreachable;
    return Object{ .eror = Object.Error{ .message = mess } };
}
fn evalPrefixExpression(arenaAlloc: Allocator, operator: []const u8, right: Object) Object {
    const op = operator[0];
    switch (op) {
        '!' => {
            return evalBangOperatorExpression(right);
        },
        '-' => {
            return evalMinusPrefixOperatorExpression(arenaAlloc, right);
        },
        else => {
            return newPrefixError(arenaAlloc, operator, right);
        },
    }
}

fn newPrefixError(arenaAlloc: Allocator, operator: []const u8, right: Object) Object {
    const mess = std.fmt.allocPrint(arenaAlloc, "Unknown operator: {s} {any}", .{ operator, getObjType(right) }) catch unreachable;
    return Object{ .eror = Object.Error{ .message = mess } };
}

fn evalInfixExpression(arenAlloc: Allocator, operator: []const u8, left: Object, right: Object) Object {
    switch (left) {
        .integer => |leftint| {
            switch (right) {
                .integer => |rightint| {
                    return evalIntegerInfixExpression(arenAlloc, operator, leftint, rightint);
                },
                else => return newInfixError(arenAlloc, operator, left, right, true),
            }
        },
        .string => |leftstr| {
            switch (right) {
                .string => |rightstr| {
                    if (std.mem.eql(u8, operator, "+")) {
                        const concated = std.fmt.allocPrint(arenAlloc, "{s}{s}", .{
                            leftstr.value,
                            rightstr.value,
                        }) catch unreachable;
                        return Object{ .string = Object.String{
                            .value = concated,
                            .allocator = arenAlloc,
                        } };
                    }
                    return newInfixError(arenAlloc, operator, left, right, false);
                },
                else => return newInfixError(arenAlloc, operator, left, right, true),
            }
        },
        .boolean => |leftbool| {
            switch (right) {
                .boolean => |rightbool| {
                    return evalBooleanInfixExpression(arenAlloc, operator, leftbool, rightbool);
                },
                else => return newInfixError(arenAlloc, operator, left, right, true),
            }
        },
        else => return newInfixError(arenAlloc, operator, left, right, false),
    }
}

fn newInfixError(allocator: Allocator, operator: []const u8, left: Object, right: Object, typemis: bool) Object {
    // var buf: [1024]u8 = undefined;
    if (typemis) {
        // const mess = std.fmt.bufPrint(&buf, "type mismatch: {any} {s} {any} \n", .{ getObjType(left), operator, getObjType(right) }) catch unreachable;
        return Object.Error.newError(allocator, "type mismatch: ", .{ getObjType(left), operator, getObjType(right) }) catch unreachable;
    }
    // const mess = std.fmt.bufPrint(&buf, "Unknown operator {any} {s} {any} \n", .{ getObjType(left), operator, getObjType(right) }) catch unreachable;
    return Object.Error.newError(allocator, "Unknown operator ", .{ getObjType(left), operator, getObjType(right) }) catch unreachable;
}
fn getObjType(o: Object) ObjectType {
    return switch (o) {
        inline else => |case| {
            return case.oType();
        },
    };
}

fn evalIfExpression(arenalloc: Allocator, ifexpr: Ast.IfExpression, environment: *Environment) Object {
    const condition = Eval(arenalloc, Ast.Node{ .expression = ifexpr.condition.* }, environment);
    if (isError(condition)) {
        return condition.?;
    }
    if (condition) |cond| {
        if (isTruthy(cond)) {
            if (ifexpr.consequence) |consq| {
                // Pretty.print(consq.allocator, consq, .{ .max_depth = 30 }) catch unreachable;
                const evaluated = Eval(arenalloc, Ast.Node{ .block = consq }, environment);
                if (isError(evaluated)) {
                    return evaluated.?;
                }
                if (evaluated) |eval| {
                    return eval;
                }
            }
        } else if (ifexpr.alternative) |alt| {
            const evaluated = Eval(arenalloc, Ast.Node{ .block = alt }, environment);
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

fn evalBooleanInfixExpression(arenAlloc: Allocator, operator: []const u8, left: Object.Boolean, right: Object.Boolean) Object {
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
            return newInfixError(arenAlloc, operator, Object{ .boolean = left }, Object{ .boolean = right }, false);
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
        else => return newPrefixError(allocator, "-", right),
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

fn evalProgram(arenaAlloc: Allocator, stmts: []Ast.Statement, environment: *Environment) Object {
    var result: Object = undefined;
    for (stmts) |value| {
        const ev = Eval(arenaAlloc, Ast.Node{ .statement = value }, environment);
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
                .string => result = Object{ .string = evaluated.string },
                .builtin => result = Object{ .builtin = evaluated.builtin },
                .array => result = Object{ .array = evaluated.array },
            }
        }
    }

    return result;
}

fn evalBlockStatements(arenaAlloc: Allocator, block: Ast.BlockStatement, environment: *Environment) Object {
    var result = NULL;

    for (block.statements.items) |stmt| {
        // std.debug.print("Evaluating Block\n", .{});
        // Pretty.print(block.allocator, stmt, .{ .max_depth = 100 }) catch unreachable;
        switch (stmt) {
            .expression => |expr| {
                if (expr.expression) |exp| {
                    const evaluated = Eval(arenaAlloc, Ast.Node{ .expression = exp.* }, environment);
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
                    const evaluated = Eval(arenaAlloc, Ast.Node{ .expression = exp.* }, environment);
                    if (evaluated) |ev| {
                        result = ev;
                    }
                }
            },
            .returnStatement => |ret| {
                if (ret.returnValue) |exp| {
                    _ = exp;
                    const evaluated = Eval(arenaAlloc, Ast.Node{ .statement = Ast.Statement{ .returnStatement = ret } }, environment);
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
            std.debug.print("\nobject is not boolean got {any}\n", .{getObjType(obj)});
            return false;
        },
    }
}

pub fn TestEvalPair(comptime T: type) type {
    return struct {
        arena: *std.heap.ArenaAllocator,
        object: T,

        pub fn deinit(self: @This()) void {
            const allocator = self.arena.child_allocator;
            self.arena.deinit();
            allocator.destroy(self.arena);
        }
    };
}

fn testEval(allocator: Allocator, input: []const u8) TestEvalPair(Object) {
    var testEvalPair = TestEvalPair(Object){
        .arena = allocator.create(std.heap.ArenaAllocator) catch unreachable,
        .object = Object{ .nil = Object.Nil{} },
    };
    testEvalPair.arena.* = std.heap.ArenaAllocator.init(allocator);
    var l = Lexer.init(allocator, input);
    defer l.deinit();
    var parser = Parser.init(allocator, l);
    var program = parser.parseProgram();
    defer parser.deinit();
    defer program.deinit();
    const arenaAlloc = testEvalPair.arena.allocator();
    const env = Environment.init(arenaAlloc) catch unreachable;
    // Pretty.print(allocator, program.statements.items, .{ .max_depth = 100 }) catch unreachable;
    const object = Eval(arenaAlloc, Ast.Node{ .program = program }, env);
    if (object) |eval| {
        testEvalPair.object = eval;
        return testEvalPair;
    }
    return testEvalPair;
}
test "TestIndexExpression" {
    const allocator = std.testing.allocator;
    const TestStruct = struct {
        input: []const u8,
        expected: Object,
    };
    const testTable = [_]TestStruct{
        TestStruct{ .expected = Object{
            .integer = Object.Integer{ .value = 1, .allocator = allocator },
        }, .input = "[1, 2, 3][0]" },
        TestStruct{ .expected = Object{
            .integer = Object.Integer{ .value = 2, .allocator = allocator },
        }, .input = "[1, 2, 3][1]" },
        TestStruct{ .expected = Object{
            .integer = Object.Integer{ .value = 3, .allocator = allocator },
        }, .input = "[1, 2, 3][2]" },
        TestStruct{ .expected = Object{
            .integer = Object.Integer{ .value = 1, .allocator = allocator },
        }, .input = "let i = 0; [1][i]" },
        TestStruct{ .expected = Object{
            .integer = Object.Integer{ .value = 3, .allocator = allocator },
        }, .input = "let myArray = [1, 2, 3]; myArray[2]" },
        TestStruct{
            .expected = Object{ .nil = Object.Nil{} },
            .input = "[1,2,3][3]",
        },
    };

    for (testTable) |value| {
        const evaluated = testEval(allocator, value.input);
        // std.debug.print("{s} \n", .{value.input});
        if (evaluated.object == .nil) {
            try std.testing.expect(testNullObject(evaluated.object));
            evaluated.deinit();
            continue;
        }
        const result = testIntegerObject(evaluated.object, value.expected.integer.value);
        evaluated.deinit();
        try std.testing.expect(result);
    }
}
test "TestArrayLiterals" {
    const allocator = std.testing.allocator;
    const input: []const u8 = "[1, 2 * 2, 3 + 3]";
    const evaluated = testEval(allocator, input);
    const expected = [3]i32{ 1, 4, 6 };
    switch (evaluated.object) {
        .array => |arr| {
            if (arr.elements.len != 3) {
                std.debug.print("array has wrong number of elements got={d} want {d} \n", .{ arr.elements.len, 3 });
                try std.testing.expect(false);
            }
            for (0.., arr.elements) |i, item| {
                if (item) |obj| {
                    try std.testing.expect(testIntegerObject(obj, expected[i]));
                }
            }
        },
        inline else => {
            std.debug.print("object is not ARRAY_OBJ got {any} \n", .{getObjType(evaluated.object)});
            try std.testing.expect(false);
        },
    }
    evaluated.deinit();
}

test "TestBuiltinFunctions" {
    const allocator = std.testing.allocator;
    const TestStruct = struct {
        input: []const u8,
        expected: i16,
    };
    const TestStructError = struct {
        input: []const u8,
        expected: []const u8,
    };
    const testTable = [_]TestStruct{
        TestStruct{ .expected = 6, .input = "len(\"foobar\")" },
        TestStruct{ .expected = 11, .input = "len(\"Hello World\")" },
        TestStruct{ .expected = 0, .input = "len(\"\")" },
    };
    const errorTestTable = [_]TestStructError{
        TestStructError{
            .expected = "argument to `len` not supported got object.ObjectType.INTEGER_OBJ",
            .input = "len(1)",
        },
        TestStructError{ .expected = "wrong number of arguments. got=2, want=1", .input = 
        \\len("one", "two");
        \\;
        },
    };
    for (testTable) |value| {
        const evaluated = testEval(allocator, value.input);
        const result = testIntegerObject(evaluated.object, value.expected);
        evaluated.deinit();
        try std.testing.expect(result);
    }
    for (errorTestTable) |value| {
        const evaluated = testEval(allocator, value.input);
        if (evaluated.object != .eror) {
            std.debug.print("Object is not error {any} \n", .{getObjType(evaluated.object)});
            evaluated.deinit();
            try std.testing.expect(false);
        }
        try std.testing.expectEqualStrings(value.expected, evaluated.object.eror.message);
        evaluated.deinit();
    }
}
test "TestStringEvaluation" {
    const allocator = std.testing.allocator;
    const TestStruct = struct {
        input: []const u8,
        expected: []const u8,
    };
    const testTable = [_]TestStruct{
        TestStruct{ .expected = "Hello World", .input = 
        \\"Hello World",
        },
    };

    var arena = std.heap.ArenaAllocator.init(allocator);
    var l = Lexer.init(allocator, testTable[0].input);
    defer l.deinit();
    var parser = Parser.init(allocator, l);
    var program = parser.parseProgram();
    defer parser.deinit();
    defer program.deinit();
    const env = Environment.init(arena.allocator()) catch unreachable;
    defer arena.deinit();
    // Pretty.print(allocator, program.statements.items, .{ .max_depth = 100 }) catch unreachable;
    const object = Eval(arena.allocator(), Ast.Node{ .program = program }, env);
    if (object) |evaluated| {
        if (evaluated != .string) {
            std.debug.print("Object is not string {any} \n", .{evaluated});
            try std.testing.expect(false);
        }
        // std.debug.print("evaluated {any} \n", .{evaluated});
        try std.testing.expectEqualStrings(testTable[0].expected, evaluated.string.value);
    }
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
        \\if (x > 100) {
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
    // std.debug.print("Evalcount, {d}\n", .{evalCount});
    // std.debug.print("Error {s}\n", .{evaluated.object.eror.message});
    const result = testBooleanObject(evaluated.object, true);
    evaluated.deinit();
    try std.testing.expect(result);
}

test "Test Closures" {
    const allocator = std.testing.allocator;
    const TestStruct = struct {
        input: []const u8,
        expected: i64,
    };
    const testTable = [_]TestStruct{
        TestStruct{ .expected = 12, .input = 
        \\let newAdder = fn(x) {
        \\fn(y) { x + y };
        \\};
        \\let addTwo = newAdder(2);
        \\addTwo(10);};
        },
    };
    const evaluated = testEval(allocator, testTable[0].input);
    const result = testIntegerObject(evaluated.object, testTable[0].expected);
    evaluated.deinit();
    try std.testing.expect(result);
}
test "TestFunctionObject" {
    const allocator = std.testing.allocator;
    const input = "fn(x) { x + 2; };";
    // const evaluated = testEval(allocator, input);
    var l = Lexer.init(allocator, input);
    defer l.deinit();
    var parser = Parser.init(allocator, l);
    var program = parser.parseProgram();
    defer parser.deinit();
    defer program.deinit();
    var arena = std.heap.ArenaAllocator.init(allocator);
    const env = Environment.init(arena.allocator()) catch unreachable;
    defer arena.deinit();
    const object = Eval(arena.allocator(), Ast.Node{ .program = program }, env);
    if (object) |eval| {
        switch (eval) {
            .function => |fun| {
                if (fun.parameters) |params| {
                    try std.testing.expectEqual(1, params.items.len);
                    try std.testing.expectEqualSlices(u8, "x", params.items[0].string());
                }
                const bodyString = try fun.body.?.string();
                try std.testing.expectEqualSlices(u8, "(x + 2)", bodyString);
                allocator.free(bodyString);
                // try Pretty.print(allocator, bodyString, .{ .max_depth = 100 });
                // try std.testing.expectEqualSlices(u8, "(x+2)", bodyString);

            },
            else => |case| {
                std.debug.print("Object is not function {any} \n", .{case});
                try std.testing.expect(false);
            },
        }
    }
}

test "Test FunctionApplication" {
    const allocator = std.testing.allocator;
    const TestStruct = struct {
        input: []const u8,
        expected: i64,
    };

    const testTable = [_]TestStruct{
        TestStruct{ .expected = 5, .input = "let identity = fn(x) { x; }; identity(5);" },
        // TestStruct{ .expected = 5, .input = "fn(x) { x; }(5)" },
    };
    for (testTable) |value| {
        const evaluated = testEval(allocator, value.input);
        const result = testIntegerObject(evaluated.object, value.expected);
        evaluated.deinit();
        try std.testing.expect(result);
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
        TestStruct{ .expected = -5, .input = "-5" },
        TestStruct{ .expected = -10, .input = "-10" },
        TestStruct{ .expected = -10, .input = "-10" },
        // weird way but works for now
        TestStruct{ .expected = -2, .input = "(-1) + -1" },
        TestStruct{ .expected = 10, .input = "5 + 5 + 5 + 5 - 10" },
        TestStruct{ .expected = 37, .input = "3 * (3 * 3) + 10" },
        TestStruct{ .expected = 50, .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10" },
    };

    for (testTable) |value| {
        const evaluated = testEval(allocator, value.input);
        // std.debug.print("{any} \n Test out TestEvalIntegerExpression\n", .{evaluated});
        const result = testIntegerObject(evaluated.object, value.expected);
        try std.testing.expect(result);
        evaluated.deinit();
    }
}
//TODO: FIx Error Handling to print out wrong objects
test "TestErrorHandler" {
    const allocator = std.testing.allocator;
    const TestStruct = struct {
        expected: []const u8,
        input: []const u8,
        typemis: bool,
    };
    const testTable = [_]TestStruct{
        TestStruct{
            .expected = "type mismatch",
            .input = "5 + true",
            .typemis = true,
        },
        TestStruct{
            .expected = "Unknown operator",
            .input = "true + false",
            .typemis = false,
        },
        TestStruct{
            .expected = "Unknown operator",
            .input = "-true",
            .typemis = false,
        },
        TestStruct{
            .expected = "ident not found ",
            .input = "foobar",
            .typemis = false,
        },
        TestStruct{
            .expected = "Unknown operator",
            .input =
            \\"Hello" - "World";
            ,
            .typemis = false,
        },
    };

    for (testTable) |value| {
        const evaluated = testEval(allocator, value.input);
        switch (evaluated.object) {
            .eror => {
                if (value.typemis) {
                    try std.testing.expectEqualSlices(u8, value.expected, evaluated.object.eror.message[0..13]);
                    evaluated.deinit();
                    continue;
                }
                try std.testing.expectEqualSlices(u8, value.expected, evaluated.object.eror.message[0..16]);
                evaluated.deinit();
            },
            else => |case| {
                std.debug.print("no error object returned, got {any}\n", .{case});
                try std.testing.expect(false);
            },
        }
    }
}
test "TestLetStatements" {
    const allocator = std.testing.allocator;
    const TestStruct = struct {
        input: []const u8,
        expected: i64,
    };

    const testTable = [_]TestStruct{
        TestStruct{ .expected = 5, .input = 
        \\let a = 5;
        \\let b = a > 3;
        \\a;
        \\let c = a * 99;
        \\ if (true) {let p = 20;} else {1};
        \\ let d = if (c>a) {99} else {100};
        \\d;
        \\ d * c * a;
        \\a;
        },
    };

    for (testTable) |value| {
        const evalualted = testEval(allocator, value.input);
        const result = testIntegerObject(evalualted.object, value.expected);
        // std.debug.print("{any}\n Test Out letStatement \n", .{evalualted});
        evalualted.deinit();
        try std.testing.expect(result);
    }
}
test "TestReturnStatements" {
    const allocator = std.testing.allocator;
    const TestStruct = struct {
        input: []const u8,
        expected: i64,
    };
    const testTable = [_]TestStruct{
        TestStruct{ .expected = 5, .input = "return 5;" },
        TestStruct{ .expected = 10, .input = "return 10;" },
        TestStruct{ .expected = -5, .input = "return -5;" },
        TestStruct{ .expected = -10, .input = "return -10;" },
        TestStruct{ .expected = 10, .input = "return 2*5;" },

        TestStruct{
            .expected = 10,
            .input =
            \\ if(1999>200) {
            \\
            \\if (120 > 21) {return 10} else {return 200}
            \\
            \\return 1;
            \\}
            ,
        },
    };

    for (testTable) |value| {
        const evaluated = testEval(allocator, value.input);
        const result = testIntegerObject(evaluated.object, value.expected);
        evaluated.deinit();
        try std.testing.expect(result);
    }
}
test "TestBangOperator" {
    const allocator = std.testing.allocator;
    const TestStruct = struct {
        input: []const u8,
        expected: bool,
    };
    const testTable = [_]TestStruct{
        TestStruct{ .expected = false, .input = "!true" },
        TestStruct{ .expected = true, .input = "!false" },
        TestStruct{ .expected = false, .input = "!5" },
        TestStruct{ .expected = true, .input = "!!true" },
        TestStruct{ .expected = false, .input = "!!false" },
        TestStruct{ .expected = true, .input = "!!5" },
    };

    for (testTable) |value| {
        const evaluated = testEval(allocator, value.input);
        const result = testBooleanObject(evaluated.object, value.expected);
        evaluated.deinit();
        try std.testing.expect(result);
    }
}

test "TestIfElseExpression" {
    const allocator = std.testing.allocator;
    const TestStruct = struct {
        input: []const u8,
        expected: Object,
    };
    const testTable = [_]TestStruct{
        TestStruct{ .expected = Object{ .integer = Object.Integer{ .allocator = allocator, .value = 10 } }, .input = "if (true) { 10 }" },
        TestStruct{ .expected = Object{ .integer = Object.Integer{ .allocator = allocator, .value = 10 } }, .input = "if (true) { 10 } else { 20 }" },
        TestStruct{ .expected = Object{ .integer = Object.Integer{ .allocator = allocator, .value = 20 } }, .input = "if (false) { 10 } else { 20 }" },
        TestStruct{ .expected = Object{ .integer = Object.Integer{ .allocator = allocator, .value = 10 } }, .input = "if (1) { 10 } else { 20 }" },
        TestStruct{ .expected = Object{ .integer = Object.Integer{ .allocator = allocator, .value = 10 } }, .input = "if (1 < 2) { 10 }" },
        TestStruct{ .expected = Object{ .integer = Object.Integer{ .allocator = allocator, .value = 10 } }, .input = "if (1 < 2) { 10 } else { 20 }" },
        TestStruct{ .expected = Object{ .integer = Object.Integer{ .allocator = allocator, .value = 20 } }, .input = "if (1 > 2) { 10 } else { 20 }" },
        TestStruct{ .expected = NULL, .input = "if (false) { 10 }" },
    };
    var passTest = true;
    for (testTable) |value| {
        const evaluated = testEval(allocator, value.input);
        switch (value.expected) {
            .integer => {
                const result = testIntegerObject(evaluated.object, value.expected.integer.value);
                if (passTest) {
                    passTest = result;
                }
            },
            .boolean => {
                const result = testBooleanObject(evaluated.object, value.expected.boolean.value);
                if (passTest) {
                    passTest = result;
                }
            },
            .nil => {
                const result = testNullObject(evaluated.object);
                if (passTest) {
                    passTest = result;
                }
            },
            else => passTest = false,
        }

        evaluated.deinit();
    }
    try std.testing.expect(passTest);
}
test "TestEvalBooleanExpression" {
    const allocator = std.testing.allocator;
    const TestStruct = struct {
        input: []const u8,
        expected: bool,
    };
    const testTable = [_]TestStruct{
        TestStruct{ .expected = true, .input = "true" },
        TestStruct{ .expected = false, .input = "false" },
        TestStruct{ .expected = true, .input = "1 < 2" },
        TestStruct{ .expected = false, .input = "1 > 2" },
        TestStruct{ .expected = false, .input = "1 == 2" },
        TestStruct{ .expected = true, .input = "1 != 2" },
        TestStruct{ .expected = true, .input = "true == true" },
        TestStruct{ .expected = false, .input = "true == false" },
        TestStruct{ .expected = false, .input = "true != true" },
        TestStruct{ .expected = true, .input = "true != false" },
        TestStruct{ .expected = false, .input = "(1 < 2) != true" },
        TestStruct{ .expected = false, .input = "(1 > 2) == true" },
    };

    for (testTable) |value| {
        const evaluated = testEval(allocator, value.input);
        const result = testBooleanObject(evaluated.object, value.expected);
        evaluated.deinit();
        try std.testing.expect(result);
    }
}
