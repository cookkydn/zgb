const App = @import("../app.zig");
const dvui = @import("dvui");
const emu = @import("emu");

pub fn render_vis(app: *App, _: dvui.Rect) !void {
    render_channel("Channel 1 (Square)", app, &app.emu.apu.ch1_buffer, 1);
    render_channel("Channel 2 (Square)", app, &app.emu.apu.ch2_buffer, 2);
    render_channel("Channel 3 (Wave)", app, &app.emu.apu.ch3_buffer, 3);
    render_channel("Channel 4 (Noise)", app, &app.emu.apu.ch4_buffer, 4);
}

fn render_channel(name: []const u8, app: *App, buffer: *[2048]f32, id: usize) void {
    var vbox = dvui.box(@src(), .{}, .{ .min_size_content = .{ .w = 300, .h = 100 }, .expand = .ratio, .id_extra = id });
    defer vbox.deinit();

    var xaxis: dvui.PlotWidget.Axis = .{
        .min = 0,
        .max = 100,
    };
    var yaxis: dvui.PlotWidget.Axis = .{
        .min = -1,
        .max = 1,
    };

    const points = app.emu.apu.buffer_index;

    if (points > 1) {
        xaxis.min = 0;
        xaxis.max = 100;
    }
    var plot = dvui.plot(@src(), .{
        .title = name,
        .x_axis = &xaxis,
        .y_axis = &yaxis,
        .border_thick = 1,
        .mouse_hover = true,
    }, .{
        .expand = .both,
        .id_extra = id,
    });
    defer plot.deinit();
    var s1 = plot.line();
    defer s1.deinit();
    for (0..points) |i| {
        s1.point(@floatFromInt(i), buffer[i]);
    }
    s1.stroke(1, dvui.themeGet().focus);
}
