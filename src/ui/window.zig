const sdl = @import("zsdl2");
pub const Window = struct {
    window: *sdl.Window,
    renderer: *sdl.Renderer,

    pub fn init() !Window {
        try sdl.init(.{ .video = true });

        const window = try sdl.createWindow(
            "window",
            sdl.Window.pos_centered,
            sdl.Window.pos_centered,
            1024,
            768,
            .{
                .allow_highdpi = true,
            },
        );

        const renderer = try sdl.createRenderer(
            window,
            null,
            .{ .accelerated = true, .present_vsync = true },
        );

        return .{
            .renderer = renderer,
            .window = window,
        };
    }

    pub fn deinit(self: *Window) void {
        self.renderer.destroy();
        self.window.destroy();
    }

    pub fn present(self: *Window) void {
        sdl.renderPresent(self.renderer);
    }
};
