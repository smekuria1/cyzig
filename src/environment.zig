const std = @import("std");
const Object = @import("./object.zig").Object;
const Allocator = std.mem.Allocator;
pub const Environment = struct {
    store: std.StringHashMap(Object),
    outer: ?*Environment,
    allocator: Allocator,

    pub fn init(allocator: Allocator) !*Environment {
        var env = try allocator.create(Environment);
        env.store = std.StringHashMap(Object).init(allocator);
        env.allocator = allocator;
        env.outer = null;

        return env;
    }

    pub fn deinit(self: *Environment, allocator: Allocator) void {
        var iter = self.store.iterator();
        while (iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
        }
        self.store.deinit();
        // if (self.outer) |out| {
        //     return out.deinit(allocator);
        // }
        allocator.destroy(self);
    }

    pub fn initEnclosed(allocator: Allocator, outer: *Environment) !*Environment {
        var env = try init(allocator);
        env.outer = outer;
        return env;
    }

    pub fn deinitEnclosed(self: *Environment, allocator: Allocator) void {
        var iter = self.store.iterator();
        while (iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
        }
        self.store.deinit();
        // if (self.outer) |out| {
        //     return out.deinit(allocator);
        // }
        allocator.destroy(self);
    }

    pub fn put(self: *Environment, key: []const u8, value: Object) void {
        // std.debug.print("Putting in variable {s}\nPutting object {any}\n\n", .{ key, value });
        const allocKey = self.allocator.dupe(u8, key) catch unreachable;
        self.store.put(allocKey, value) catch unreachable;
        // self.printEnvironment();
    }

    pub fn get(self: *Environment, key: []const u8) ?Object {
        // std.debug.print("Getting variable {s}\n\n", .{key});
        // self.printEnvironment();
        if (self.store.get(key)) |obj| {
            return obj;
        } else if (self.outer) |out| {
            if (out.store.get(key)) |outerObj| {
                return outerObj;
            }
        }
        return null;
    }

    pub fn printEnvironment(self: *Environment) void {
        std.debug.print("Size of Map {any} \n ", .{self.store.count()});
        var iterator = self.store.iterator();
        while (iterator.next()) |entry| {
            std.debug.print("Key {s}\n", .{entry.key_ptr.*});
            std.debug.print("Value {any} \n", .{entry.value_ptr.*});
        }
        if (self.outer) |out| {
            std.debug.print("Printing outer\n", .{});
            out.printEnvironment();
        }
    }
};

// pub const EnclosedEnvironment = struct {
//     store: std.StringHashMap(Object),
//     outer: ?*Environment,
//     allocator: Allocator,
//
//     pub fn initEnclosed(allocator: Allocator, outer: *Environment) !*Environment {
//         var env = try allocator.create(Environment);
//         env.store = std.StringHashMap(Object).init(allocator);
//         env.allocator = allocator;
//         env.outer = outer;
//
//         return env;
//     }
//
//     pub fn deinitEnclosed(self: *Environment, allocator: Allocator) void {
//         var iter = self.store.iterator();
//         while (iter.next()) |entry| {
//             allocator.free(entry.key_ptr.*);
//         }
//         self.store.deinit();
//         // if (self.outer) |out| {
//         //     return out.deinit(allocator);
//         // }
//         allocator.destroy(self);
//     }
//
//     pub fn put(self: *Environment, key: []const u8, value: Object) void {
//         // std.debug.print("Putting in variable {s}\nPutting object {any}\n\n", .{ key, value });
//         const allocKey = self.allocator.dupe(u8, key) catch unreachable;
//         self.store.put(allocKey, value) catch unreachable;
//         // self.printEnvironment();
//     }
//
//     pub fn get(self: *Environment, key: []const u8) ?Object {
//         // std.debug.print("Getting variable {s}\n\n", .{key});
//         // self.printEnvironment();
//         if (self.store.get(key)) |obj| {
//             return obj;
//         } else if (self.outer) |out| {
//             if (out.store.get(key)) |outerObj| {
//                 return outerObj;
//             }
//         }
//         return null;
//     }
//
//     pub fn printEnvironment(self: *Environment) void {
//         std.debug.print("Size of Map {any} \n ", .{self.store.count()});
//         var iterator = self.store.iterator();
//         while (iterator.next()) |entry| {
//             std.debug.print("Key {s}\n", .{entry.key_ptr.*});
//             std.debug.print("Value {any} \n", .{entry.value_ptr.*});
//         }
//         if (self.outer) |out| {
//             std.debug.print("Printing outer\n", .{});
//             out.printEnvironment();
//         }
//     }
// };
