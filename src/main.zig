const std = @import("std");
const Emulator = @import("emulator.zig").Emulator;

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var emulator = try Emulator.init(allocator);
    try emulator.load_cart("./carts/tetris.gb");
    try emulator.run();
    defer emulator.deinit();
}
