const std = @import("std");
const sdl = @import("zsdl2");
const Window = @import("window.zig").Window;
const Color = @import("color.zig").Color;

pub const Console = struct {
    window: Window,
    cursor_x: u32 = 0,
    cursor_y: u32 = 0,
    font_size: u16 = 16,
    color: Color = Color.WHITE,
    texture: *sdl.Texture,
    frame: sdl.Rect = .{ .x = 0, .y = 0, .h = 100, .w = 100 },

    lines: [64][128]u8 = undefined, // 64 lignes maximum, 128 caractères par ligne
    line_lengths: [64]usize = [_]usize{0} ** 64,
    current_line: usize = 0,
    last_frame_line_count: usize = 0,

    // Le secret de la performance : on ne redessine que si ce drapeau est 'true'
    is_dirty: bool = true,

    pub fn init(window: Window, width: u32, height: u32) Console {
        const texture = sdl.createTexture(
            window.renderer,
            sdl.PixelFormatEnum.argb8888,
            sdl.TextureAccess.target,
            @intCast(width),
            @intCast(height),
        ) catch @panic("Failed to init console texture");
        return .{ .window = window, .texture = texture };
    }

    pub fn beginFrame(self: *Console) void {
        self.current_line = 0;
    }

    pub fn print(self: *Console, comptime fmt: []const u8, args: anytype) !void {
        if (self.current_line >= 64) return;

        var temp_buf: [128]u8 = undefined;
        const text = std.fmt.bufPrint(&temp_buf, fmt, args) catch return;

        const idx = self.current_line;
        const prev_len = self.line_lengths[idx];

        if (prev_len != text.len or !std.mem.eql(u8, text, self.lines[idx][0..prev_len])) {
            self.is_dirty = true;
            @memcpy(self.lines[idx][0..text.len], text);
            self.lines[idx][text.len] = 0;
            self.line_lengths[idx] = text.len;
        }

        self.current_line += 1;
    }

    pub fn render(self: *Console) !void {
        if (self.current_line != self.last_frame_line_count) {
            self.is_dirty = true;
            self.last_frame_line_count = self.current_line;
        }

        if (self.is_dirty) {
            self.window.renderer.setTarget(self.texture) catch return;

            try self.window.renderer.setDrawColorRGBA(0, 0, 0, 0);
            try self.window.renderer.clear();

            const font = try self.window.font.get_font(self.font_size);
            var draw_y: i32 = 20;

            for (0..self.current_line) |i| {
                const len = self.line_lengths[i];
                if (len > 0) {
                    const textZ: [:0]const u8 = self.lines[i][0..len :0];

                    const surface = try font.renderTextBlended(textZ, self.color.IntoSDLColor());
                    const tex = try self.window.renderer.createTextureFromSurface(surface);

                    try sdl.renderCopy(
                        self.window.renderer,
                        tex,
                        null,
                        &.{
                            .x = 20,
                            .y = draw_y,
                            .w = surface.w,
                            .h = surface.h,
                        },
                    );

                    tex.destroy();
                    surface.free();
                }
                draw_y += @intFromFloat(@as(f32, @floatFromInt(self.font_size)) * 1.2);
            }

            self.window.renderer.setTarget(null) catch return;
            self.is_dirty = false;
        }

        try self.window.drawFrameBorder(self.frame, 4);
        try sdl.renderCopy(
            self.window.renderer,
            self.texture,
            null,
            &self.frame,
        );
    }
};
