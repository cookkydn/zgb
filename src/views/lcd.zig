const ui = @import("ui");
const Emulator = @import("../emulator.zig").Emulator;
const std = @import("std");

pub fn render_lcd(self: *ui.UiContext, emulator: *Emulator, allocator: std.mem.Allocator) !void {
    const hex_str = try std.fmt.allocPrintSentinel(allocator, "PC: 0x{X:0>4}", .{emulator.cpu.registers.pc}, 0);

    defer allocator.free(hex_str);

    try self.draw_text(0, 0, hex_str, ui.Color.WHITE);
}
