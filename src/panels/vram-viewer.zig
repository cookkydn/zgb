const AppState = @import("../app.zig").AppState;
const LayoutManager = @import("../ui/layout.zig").LayoutManager;
const ig = @import("cimgui");
const Texture = @import("../ui/texture.zig").Texture;
const Tile = @import("../emu/ppu/tile.zig").Tile;

pub const VramViewer = struct {
    visible: bool = false,
    tiles_tex: Texture = undefined,

    pub fn draw(self: *VramViewer, app: *AppState) void {
        if (!self.visible) return;
        defer ig.igEnd();
        if (!ig.igBegin(LayoutManager.Panels.vram, &self.visible, ig.ImGuiWindowFlags_None)) return;

        const gb = &app.emu.gb;

        var vram_buffer: [24576]u32 = .{0xFFFFFFFF} ** 24576;
        for (0..24) |tile_x| {
            const t_x: u16 = @truncate(tile_x);
            for (0..16) |tile_y| {
                const t_y: u16 = @truncate(tile_y);
                const tile = Tile.fromAddr(0x8000 + t_x * 2 * 8 + 48 * 8 * t_y, &gb.ppu.mem);
                for (0..8) |i| {
                    for (0..8) |j| {
                        const color = tile.getPixelAt(@truncate(i), @truncate(j));
                        const color_argb: u32 = blk: switch (color) {
                            0 => break :blk 0xFF9CBC0F,
                            1 => break :blk 0xFF8BAC0F,
                            2 => break :blk 0xFF306230,
                            3 => break :blk 0xFF10380F,
                        };
                        vram_buffer[(t_y) * 24 * 8 * 8 + j * 24 * 8 + t_x * 8 + i] = color_argb;
                    }
                }
            }
        }
        self.tiles_tex.update(&vram_buffer);
        ig.igImage(.{
            ._TexID = self.tiles_tex.imTextureId(),
        }, .{
            .x = 192,
            .y = 128,
        });
    }
};
