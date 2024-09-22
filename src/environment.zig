const std = @import("std");
const Object = @import("./object.zig").Object;
const Allocator = std.mem.Allocator;
pub const Environment = struct {
    store: std.StringHashMap(Object),

    pub fn init(allocator: Allocator) !*Environment {
        var env = try allocator.create(Environment);
        env.store = std.StringHashMap(Object).init(allocator);

        return env;
    }

    pub fn put(self: *Environment, key: []const u8, value: Object) void {
        std.debug.print("Putting in variable {s}\nPutting object {any}\n\n", .{ key, value });
        self.store.putNoClobber(key, value) catch unreachable;
        self.printEnvironment();
    }

    pub fn get(self: *Environment, key: []const u8) ?Object {
        std.debug.print("Getting variable {s}\n\n", .{key});
        self.printEnvironment();
        if (self.store.get(key)) |obj| {
            return obj;
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
    }

    pub fn deinit(self: *Environment, allocator: Allocator) void {
        self.store.deinit();
        allocator.destroy(self);
    }
};
