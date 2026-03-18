const sdl = @import("zsdl2");

const FontAltas = @import("font.zig").FontAtlas;
const Frame = @import("frame.zig").Frame;

pub const Window = struct {
    window: *sdl.Window,
    renderer: *sdl.Renderer,
    font: FontAltas,

    width: i32,
    height: i32,

    is_running: bool = true,

    pub fn init(title: [:0]const u8, width: i32, height: i32) !Window {
        try sdl.init(.{ .video = true });

        const window = try sdl.createWindow(
            title,
            sdl.Window.pos_centered,
            sdl.Window.pos_centered,
            @intCast(width),
            @intCast(height),
            .{
                .allow_highdpi = true,
                .resizable = true,
            },
        );
        errdefer window.destroy();

        const renderer = try sdl.createRenderer(
            window,
            null,
            .{ .accelerated = true, .present_vsync = true },
        );
        errdefer renderer.destroy();

        const font = try FontAltas.init("./fonts/Hack-Regular.ttf");

        return .{
            .renderer = renderer,
            .window = window,
            .font = font,
            .width = width,
            .height = height,
        };
    }

    pub fn deinit(self: *Window) void {
        self.renderer.destroy();
        self.window.destroy();
        sdl.quit();
    }

    pub fn beginFrame(self: *Window) !void {
        try self.renderer.setDrawColorRGBA(25, 25, 25, 255);
        try self.renderer.clear();
    }

    pub fn endFrame(self: *Window) void {
        sdl.renderPresent(self.renderer);
    }

    pub fn pollEvents(self: *Window, event: *sdl.Event) bool {
        if (sdl.pollEvent(event)) {
            switch (event.type) {
                .quit => {
                    self.is_running = false;
                },

                .windowevent => {
                    if (event.window.event == sdl.WindowEventId.resized) {
                        self.width = event.window.data1;
                        self.height = event.window.data2;
                    }
                },

                .keydown => {
                    if (event.key.keysym.sym == sdl.Keycode.escape) {
                        self.is_running = false;
                    }
                },
                else => {},
            }
            return true;
        }
        return false;
    }
};
