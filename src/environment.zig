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

    pub fn deinit(self: *Environment, allocator: Allocator) void {
        self.store.deinit();
        allocator.destroy(self);
    }
};
