const std = @import("std");
const repl = @import("repl.zig").start;
const run = @import("run.zig").run;

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const args = try std.process.argsAlloc(allocator);
    if (args.len == 2) {
        const filename = args[1];
        std.debug.print("Running file {s}\n", .{filename});
        //TODO: Read file line by line to to stop eval from blowing stack
        const file_content = try std.fs.cwd().readFileAlloc(allocator, filename, std.math.maxInt(usize));
        try run(allocator, file_content);
        defer allocator.free(file_content);
    } else {
        std.debug.print("Welcome to Cyzig's REPL \nWAIIG in Zig \n q to quit", .{});
        _ = try repl(allocator);
    }
    defer std.process.argsFree(allocator, args);
}
