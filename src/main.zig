const std = @import("std");
const repl = @import("repl.zig").start;

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    std.debug.print("Welcome to cyzig \nWAIIG in ZIG", .{});
    _ = try repl(allocator);
}
