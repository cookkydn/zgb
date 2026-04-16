const std = @import("std");

pub const Sprite = packed struct(u32) {
    y_pos: u8,
    x_pos: u8,
    tile_index: u8,
    flags: packed struct {
        cgb_palette: u3,
        cgb_bank: u1,
        dmg_palette: u1,
        x_flip: bool,
        y_flip: bool,
        priority: bool,
    },

    pub fn fromOAM(oam: *const [4]u8) Sprite {
        const raw = std.mem.readInt(u32, oam, .little);
        return @bitCast(raw);
    }
};
