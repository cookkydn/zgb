const App = @import("../app.zig");
const dvui = @import("dvui");
pub fn render_viewport(app: App, rect: dvui.Rect) !void {
    const scale_x = if (app.pixel_perfect_scaling) @floor(rect.w / 160) else rect.w / 160;
    const scale_y = if (app.pixel_perfect_scaling) @floor(rect.h / 144) else rect.h / 144;

    const scale = @max(1.0, @min(scale_x, scale_y));

    const final_w = 160.0 * scale;
    const final_h = 144.0 * scale;

    var box = dvui.box(@src(), .{
        .dir = .vertical,
    }, .{
        .expand = .both,
    });
    defer box.deinit();

    var screen_box = dvui.box(@src(), .{}, .{
        .min_size_content = .{ .w = final_w, .h = final_h },
        .gravity_x = 0.5,
        .gravity_y = 0.5,
    });

    const tex_rect = dvui.parentGet().data().rectScale();
    screen_box.deinit();

    try dvui.renderTexture(
        app.screen_tex,
        tex_rect,
        .{},
    );
}
