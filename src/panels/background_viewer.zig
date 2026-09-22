const App = @import("../app.zig");
const dvui = @import("dvui");
const emu = @import("emu");

var bg_tex: ?dvui.Texture = undefined;
var bg_buffer: [256 * 256]dvui.Color = [1]dvui.Color{.{}} ** (256 * 256);

pub fn render_bg_viewer(app: *App, bounds: dvui.Rect) !void {
    const ppu = &app.emu.ppu;
    for (0..32) |tile_x| {
        for (0..32) |tile_y| {
            const tile_addr = emu.Ppu.background.getTileAddrAt(
                ppu,
                @truncate(tile_x),
                @truncate(tile_y),
            );
            for (0..8) |x| {
                for (0..8) |y| {
                    const pixel_data = emu.Ppu.tile.getPixelAt(
                        ppu,
                        tile_addr,
                        @truncate(x),
                        @truncate(y),
                    );
                    const color_id = ppu.getColorByBgPalette(pixel_data);
                    bg_buffer[tile_x * 8 + x + (tile_y * 8 + y) * 256] = switch (color_id) {
                        0 => .{ .r = 0x9C, .g = 0xBC, .b = 0x0F },
                        1 => .{ .r = 0x8B, .g = 0xAC, .b = 0x0F },
                        2 => .{ .r = 0x30, .g = 0x62, .b = 0x30 },
                        3 => .{ .r = 0x10, .g = 0x38, .b = 0x0F },
                    };
                }
            }
        }
    }

    const scx = @as(u16, @intCast(ppu.scx));
    const scy = @as(u16, @intCast(ppu.scy));

    const bottom_y = (scy + 143) % 256;
    const right_x = (scx + 159) % 256;

    for (0..160) |i| {
        const x = (scx + @as(u16, @intCast(i))) % 256;

        bg_buffer[x + scy * 256] = .{ .r = 0xFF, .g = 0, .b = 0 };
        bg_buffer[x + bottom_y * 256] = .{ .r = 0xFF, .g = 0, .b = 0 };
    }

    for (0..144) |i| {
        const y = (scy + @as(u16, @intCast(i))) % 256;

        bg_buffer[scx + y * 256] = .{ .r = 0xFF, .g = 0, .b = 0 };
        bg_buffer[right_x + y * 256] = .{ .r = 0xFF, .g = 0, .b = 0 };
    }

    if (bg_tex) |*text| {
        try dvui.textureUpdate(text, @ptrCast(&bg_buffer));

        const scale_x = if (app.pixel_perfect_scaling) @floor(bounds.w / 160) else bounds.w / 160;
        const scale_y = if (app.pixel_perfect_scaling) @floor(bounds.h / 144) else bounds.h / 144;

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
            text.*,
            tex_rect,
            .{},
        );
    } else {
        bg_tex = try dvui.textureCreate(
            @ptrCast(&bg_buffer),
            .{ .height = 256, .width = 256 },
        );
    }
}
