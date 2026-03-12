const ui = @import("ui");
const std = @import("std");

const panic = std.debug.panic;

pub const App = struct {
    window: ui.Window,

    pub fn init() App {
        const window = ui.Window.init() catch panic("Failed to init window\n", .{});
        return .{ .window = window };
    }

    pub fn deinit(self: *App) void {
        self.window.deinit();
    }
};
