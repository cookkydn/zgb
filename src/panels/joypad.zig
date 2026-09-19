const App = @import("../app.zig");
const dvui = @import("dvui");
pub fn render_joypad(app: *App, _: dvui.Rect) !void {
    var wrapper = dvui.box(@src(), .{ .dir = .vertical }, .{
        .expand = .both,
    });
    defer wrapper.deinit();

    var main_row = dvui.box(@src(), .{ .dir = .horizontal }, .{
        .gravity_x = 0.5,
        .gravity_y = 0.5,
    });
    defer main_row.deinit();
    var joyp = &app.emu.joypad.press_state;

    // D-PAD
    {
        var dpad_col = dvui.box(@src(), .{ .dir = .vertical }, .{});
        defer dpad_col.deinit();

        const btn_size: f32 = 35.0;

        // UP
        {
            var row = dvui.box(@src(), .{ .dir = .horizontal }, .{});
            defer row.deinit();
            _ = dvui.spacer(@src(), .{ .min_size_content = .{ .w = btn_size, .h = btn_size } });
            joyp.up = try draw_btn("U", app.emu.joypad.press_state.up, btn_size, btn_size, 5, 1);
            _ = dvui.spacer(@src(), .{ .min_size_content = .{ .w = btn_size, .h = btn_size } });
        }
        // LEFT & RIGHT
        {
            var row = dvui.box(@src(), .{
                .dir = .horizontal,
            }, .{ .min_size_content = .{ .w = btn_size * 4 }, .gravity_x = 0.5 });
            defer row.deinit();
            joyp.left = try draw_btn("L", app.emu.joypad.press_state.left, btn_size, btn_size, 5, 2);
            // Le centre de la croix
            var center = dvui.box(@src(), .{ .dir = .vertical }, .{
                .min_size_content = .{ .w = btn_size, .h = btn_size },
                .background = true,
                .color_fill = .{ .color = .gray },
            });
            center.deinit();
            joyp.right = try draw_btn("R", app.emu.joypad.press_state.right, btn_size, btn_size, 5, 3);
        }
        // DOWN
        {
            var row = dvui.box(@src(), .{ .dir = .horizontal }, .{});
            defer row.deinit();
            _ = dvui.spacer(@src(), .{ .min_size_content = .{ .w = btn_size, .h = btn_size } });
            joyp.down = try draw_btn("D", app.emu.joypad.press_state.down, btn_size, btn_size, 5, 4);
            _ = dvui.spacer(@src(), .{ .min_size_content = .{ .w = btn_size, .h = btn_size } });
        }
    }

    //  SELECT & START
    {
        var center_col = dvui.box(@src(), .{ .dir = .vertical }, .{ .gravity_y = 1.0 });
        defer center_col.deinit();

        var row = dvui.box(@src(), .{ .dir = .horizontal }, .{});
        defer row.deinit();

        joyp.select = try draw_btn("Select", app.emu.joypad.press_state.select, 45, 20, 10, 5);
        _ = dvui.spacer(@src(), .{ .min_size_content = .{ .w = 10 } });
        joyp.start = try draw_btn("Start", app.emu.joypad.press_state.start, 45, 20, 10, 6);
    }

    // A & B
    {
        var action_row = dvui.box(@src(), .{ .dir = .horizontal }, .{});
        defer action_row.deinit();

        const btn_size: f32 = 45.0;
        joyp.b = try draw_btn("B", app.emu.joypad.press_state.b, btn_size, btn_size, btn_size / 2.0, 7);
        _ = dvui.spacer(@src(), .{ .min_size_content = .{ .w = 10 } });
        joyp.a = try draw_btn("A", app.emu.joypad.press_state.a, btn_size, btn_size, btn_size / 2.0, 8);
    }
}

fn draw_btn(label: []const u8, active: bool, width: f32, height: f32, radius: f32, id: usize) !bool {
    var color: dvui.ColorOrGradient = .{ .color = .gray };
    if (active) {
        color = dvui.ColorOrGradient.green;
    }

    var b = dvui.box(@src(), .{ .dir = .vertical }, .{
        .min_size_content = .{ .w = width, .h = height },
        .background = true,
        .color_fill = color,
        .corners = .round(radius),
        .id_extra = id,
    });

    var pressed = active;

    for (dvui.events()) |*e| {
        if (b.matchEvent(e)) {
            if (e.evt == .mouse and e.evt.mouse.action == .press) {
                pressed = true;
                e.handled = true;
            }
        }

        if (e.evt == .mouse and e.evt.mouse.action == .release) {
            pressed = false;
        }
    }
    // ---------------------------------------

    defer b.deinit();

    dvui.label(@src(), "{s}", .{label}, .{
        .color_text = .{ .color = .black },
        .gravity_x = 0.5,
        .gravity_y = 0.5,
    });
    return pressed;
}
