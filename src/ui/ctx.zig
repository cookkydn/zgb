const sdl = @import("zsdl2");
const ttf = @import("zsdl2_ttf");

const FontAtlas = @import("font.zig").FontAtlas;

extern fn SDL_SetWindowSize(window: *sdl.Window, w: c_int, h: c_int) void;
extern fn SDL_SetWindowResizable(window: *sdl.Window, resizable: bool) void;
extern fn SDL_AddEventWatch(filter: *const fn (userdata: ?*anyopaque, event: *sdl.Event) callconv(.c) c_int, userdata: ?*anyopaque) void;

fn resizeCallback(view: ?*anyopaque, event: *sdl.Event) callconv(.c) c_int {
    if (event.type == .windowevent) {
        switch (event.window.event) {
            .resized, .size_changed, .exposed => {
                if (view) |ptr| {
                    const ctx: *UiContext = @ptrCast(@alignCast(ptr));
                    ctx.begin_frame() catch @panic("failed to render\n");
                    ctx.end_frame() catch @panic("failed to render\n");
                }
            },
            else => {},
        }
    }
    return 0;
}

pub const Color = struct {
    r: u8,
    g: u8,
    b: u8,
    a: u8 = 255,

    pub const WHITE = Color{ .r = 255, .g = 255, .b = 255, .a = 255 };
    pub const BLACK = Color{ .r = 0, .g = 0, .b = 0, .a = 255 };
    pub const RED = Color{ .r = 255, .g = 0, .b = 0, .a = 255 };
    pub const GREEN = Color{ .r = 0, .g = 255, .b = 0, .a = 255 };
    pub const BLUE = Color{ .r = 0, .g = 0, .b = 255, .a = 255 };

    pub fn to_sdl_color(self: Color) sdl.Color {
        return .{
            .r = self.r,
            .g = self.g,
            .b = self.b,
            .a = self.a,
        };
    }
};

pub const UiContext = struct {
    window: *sdl.Window,
    renderer: *sdl.Renderer,
    bg_color: Color = Color.BLACK,
    font_atlas: FontAtlas,

    pub const Config = struct {
        title: ?[*:0]const u8 = "window",
        w: u32 = 100,
        h: u32 = 100,
    };

    pub fn init(config: Config) sdl.Error!UiContext {
        try sdl.init(.{ .video = true });
        try sdl.setHint("SDL_HINT_RENDER_SCALE_QUALITY", "1");

        const window = try sdl.createWindow(
            config.title orelse "window",
            sdl.Window.pos_centered,
            sdl.Window.pos_centered,
            @intCast(config.w),
            @intCast(config.h),
            .{
                .allow_highdpi = true,
            },
        );

        const renderer = try sdl.createRenderer(
            window,
            null,
            .{ .accelerated = true, .present_vsync = true },
        );

        const ctx: UiContext = .{
            .window = window,
            .renderer = renderer,
            .font_atlas = try FontAtlas.init("../../fonts/Hack-Regular.ttf"),
        };

        return ctx;
    }

    pub fn resize(self: *UiContext, w: u32, h: u32) void {
        SDL_SetWindowSize(self.window, @intCast(w), @intCast(h));
    }

    pub fn set_resizable(self: *UiContext, resizable: bool) void {
        SDL_SetWindowResizable(self.window, resizable);
        if (resizable) {
            SDL_AddEventWatch(resizeCallback, self);
        }
    }

    pub fn begin_frame(self: *UiContext) sdl.Error!void {
        try self.renderer.setDrawColor(self.bg_color.to_sdl_color());
        try self.renderer.clear();
    }

    pub fn end_frame(self: *UiContext) sdl.Error!void {
        sdl.renderPresent(self.renderer);
    }

    pub fn draw_rect(self: *UiContext, x: u32, y: u32, w: u32, h: u32, color: Color) sdl.Error!void {
        try self.renderer.setDrawColor(color.to_sdl_color());
        try self.renderer.drawRect(.{
            .x = @intCast(x),
            .y = @intCast(y),
            .w = @intCast(w),
            .h = @intCast(h),
        });
    }

    pub fn draw_filled_rect(self: *UiContext, x: u32, y: u32, w: u32, h: u32, color: Color) sdl.Error!void {
        try self.renderer.setDrawColor(color.to_sdl_color());
        try self.renderer.fillRect(.{
            .x = @intCast(x),
            .y = @intCast(y),
            .w = @intCast(w),
            .h = @intCast(h),
        });
    }

    pub fn draw_text(self: *UiContext, x: u32, y: u32, text: [:0]const u8, color: Color) !void {
        var scale = (try UiContext.get_scale(self.window, self.renderer));
        const font_size: u16 = @intFromFloat(16 * scale.scale_x);
        const font = try self.font_atlas.get_font(font_size);
        const surface = try font.renderTextBlended(text, color.to_sdl_color());
        const texture = try self.renderer.createTextureFromSurface(surface);
        defer texture.destroy();
        defer surface.free();
        if (scale.scale_x < 1) scale.scale_x = 1;
        if (scale.scale_y < 1) scale.scale_y = 1;

        const dest_w: c_int = @intFromFloat(@as(f32, @floatFromInt(surface.w)) / scale.scale_x);
        const dest_h: c_int = @intFromFloat(@as(f32, @floatFromInt(surface.h)) / scale.scale_y);

        try sdl.renderCopy(
            self.renderer,
            texture,
            null,
            &.{
                .x = @intCast(x),
                .y = @intCast(y),
                .w = dest_w,
                .h = dest_h,
            },
        );
    }

    pub fn get_scale(window: *sdl.Window, renderer: *sdl.Renderer) !struct { scale_x: f32, scale_y: f32 } {
        var window_w: c_int = 0;
        var window_h: c_int = 0;
        window.getSize(&window_w, &window_h);

        var renderer_w: c_int = 0;
        var renderer_h: c_int = 0;
        try renderer.getOutputSize(&renderer_w, &renderer_h);

        const scale_x = @as(f32, @floatFromInt(renderer_w)) / @as(f32, @floatFromInt(window_w));
        const scale_y = @as(f32, @floatFromInt(renderer_h)) / @as(f32, @floatFromInt(window_h));

        return .{
            .scale_x = scale_x,
            .scale_y = scale_y,
        };
    }

    pub fn set_dpi_scale(self: *UiContext) !void {
        const scales = try UiContext.get_scale(self.window, self.renderer);
        try self.renderer.setScale(scales.scale_x, scales.scale_y);
    }
};
