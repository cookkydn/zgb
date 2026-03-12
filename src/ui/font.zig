const ttf = @import("zsdl2_ttf");

pub const FontAtlas = struct {
    file: [:0]const u8,
    fonts: [192]?*ttf.Font = [_]?*ttf.Font{null} ** 192,

    pub fn init(file: [:0]const u8) !FontAtlas {
        try ttf.init();
        return .{ .file = file };
    }

    pub fn get_font(self: *FontAtlas, pt_size: u16) !*ttf.Font {
        if (self.fonts[pt_size]) |font| {
            return font;
        }
        const font = try ttf.Font.open(self.file, pt_size);
        self.fonts[pt_size] = font;
        return font;
    }
};
