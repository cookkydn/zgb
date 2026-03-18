const Window = @import("window.zig").Window;
const sdl = @import("zsdl2");

extern fn SDL_SetTextureScaleMode(texture: *sdl.Texture, scaleMode: sdl.ScaleMode) c_int;

pub const Canvas = struct {
    window: Window,
    texture: *sdl.Texture,
    width: u32,
    height: u32,
    frame: sdl.Rect = .{ .x = 0, .y = 0, .h = 100, .w = 100 },

    pub fn init(window: Window, width: u32, height: u32) !Canvas {
        const texture = try sdl.createTexture(
            window.renderer,
            sdl.PixelFormatEnum.argb8888,
            sdl.TextureAccess.streaming,
            @intCast(width),
            @intCast(height),
        );

        _ = SDL_SetTextureScaleMode(texture, sdl.ScaleMode.nearest);

        return .{
            .window = window,
            .texture = texture,
            .width = width,
            .height = height,
        };
    }

    pub fn deinit(self: *Canvas) void {
        self.texture.destroy();
    }

    pub fn draw(self: *Canvas, pixel_buffer: []const u32) !void {
        try self.window.drawFrameBorder(self.frame, 4);
        const pitch: c_int = @intCast(self.width * @sizeOf(u32));
        try sdl.updateTexture(
            self.texture,
            null,
            @constCast(pixel_buffer.ptr),
            pitch,
        );
        try sdl.renderCopy(
            self.window.renderer,
            self.texture,
            null,
            &self.frame,
        );
    }
};
