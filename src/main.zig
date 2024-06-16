const std = @import("std");
const repl = @import("repl.zig").start;

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    std.debug.print("Welcome cyzig Monkey's Zig Repl", .{});
    _ = try repl(allocator);
}
