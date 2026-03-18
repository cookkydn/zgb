const std = @import("std");
const sdl = @import("zsdl2");

pub const App = struct {
    window: *sdl.Window,
    renderer: *sdl.Renderer,

    width: i32,
    height: i32,

    is_running: bool = true,

    pub fn init(title: [:0]const u8, width: i32, height: i32) !App {
        try sdl.init(.{ .video = true });
        errdefer sdl.quit();

        const window = try sdl.createWindow(
            title,
            sdl.Window.pos_centered,
            sdl.Window.pos_centered,
            @intCast(width),
            @intCast(height),
            .{ .resizable = true, .allow_highdpi = true },
        );
        errdefer window.destroy();

        const renderer = try sdl.createRenderer(
            window,
            null,
            .{ .accelerated = true, .present_vsync = true },
        );
        errdefer renderer.destroy();

        return .{
            .window = window,
            .renderer = renderer,
            .width = width,
            .height = height,
            .is_running = true,
        };
    }

    pub fn deinit(self: *App) void {
        self.renderer.destroy();
        self.window.destroy();
        sdl.quit();
    }

    pub fn beginFrame(self: *App) !void {
        try self.renderer.setDrawColorRGBA(25, 25, 25, 255);
        try self.renderer.clear();
    }

    pub fn endFrame(self: *App) void {
        self.renderer.present();
    }

    pub fn createGameBoyScreenTexture(self: *App) !*sdl.Texture {
        return try sdl.createTexture(
            self.renderer,
            .argb8888,
            .streaming,
            160,
            144,
        );
    }
};
