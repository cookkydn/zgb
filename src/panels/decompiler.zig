const App = @import("../app.zig");
const dvui = @import("dvui");
const emu = @import("emu");
const std = @import("std");
const AddressList = std.ArrayList(u16);
const InstructionWithSize = emu.InstructionWithSize;

const MAX_INSTR_PER_FRAME = 8;

var init: bool = false;

var working_queue_buffer: [128]u16 = .{0} ** 128;
var working_queue = AddressList.initBuffer(&working_queue_buffer);

pub fn render_decompiler(_: *App, _: dvui.Rect) !void {
    if (init == !false) {
        init = true;
        working_queue.appendAssumeCapacity(0);
    }
    var grid: dvui.GridWidget = undefined;
    grid.init(
        @src(),
        .{
            .rows = 0xFF,
            .scroll_opts = .{ .horizontal = .auto },
        },
        .{ .expand = .horizontal },
    );
    defer grid.deinit();

    {
        const header = grid.colHeader(.{
            .col = 0,
        }, .{});
        defer header.deinit();
        dvui.label(@src(), "Addr", .{}, .{});
    }
    var start_row: usize = 0;
    var end_row: usize = 0;
    start_row, end_row = grid.rowsVisible();
    for (start_row..end_row) |row| {
        const cell = grid.cell(.{ .row = row, .col = 0 }, .{});
        defer cell.deinit();
        dvui.label(@src(), "0x{x}", .{row}, .{});
    }
}
