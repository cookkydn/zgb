const sdl = @import("zsdl2");

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

    pub fn IntoSDLColor(self: Color) sdl.Color {
        return .{
            .r = self.r,
            .g = self.g,
            .b = self.b,
            .a = self.a,
        };
    }
};
