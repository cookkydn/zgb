const std = @import("std");
const ui = @import("root.zig");

pub const TextConsole = struct {
    ui_ctx: *ui.UiContext,
    cursor_x: u32,
    cursor_y: u32,
    line_height: u32,
    color: ui.Color,

    pub fn init(ui_ctx: *ui.UiContext, line_height: u32, color: ui.Color) TextConsole {
        return .{
            .ui_ctx = ui_ctx,
            .cursor_x = 20,
            .cursor_y = 20,
            .line_height = line_height,
            .color = color,
        };
    }

    pub fn resetCursor(self: *TextConsole) void {
        self.cursor_x = 20;
        self.cursor_y = 20;
    }

    pub fn print(self: *TextConsole, comptime fmt: []const u8, args: anytype) void {
        var buffer: [256]u8 = undefined;

        const text = std.fmt.bufPrintZ(&buffer, fmt, args) catch return;

        self.ui_ctx.draw_text(self.cursor_x, self.cursor_y, text, self.color) catch return;

        self.cursor_y += self.line_height;
    }
};
