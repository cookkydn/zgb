const Ppu = @import("./ppu.zig").Ppu;
const PpuMem = @import("./ppu_mem.zig").PpuMem;
const Tile = @import("./tile.zig").Tile;

pub const Background = struct {
    fn getPpu(self: *Background) *Ppu {
        return @alignCast(@fieldParentPtr("bg", self));
    }

    fn getMem(self: *Background) *PpuMem {
        return &self.getPpu().mem;
    }

    fn getBgTileMapArea(self: *Background) u16 {
        return if (self.getMem().lcdc & 8 > 0) 0x9C00 else 0x9800;
    }

    /// Get the tile at position (x,y)
    pub fn getTileAt(self: *Background, tile_x: u16, tile_y: u16) Tile {
        const tile_index_addr = self.getBgTileMapArea() + (tile_y * 32) + tile_x;
        const tile_index = self.getMem().read_vram(tile_index_addr);
        const addressing = Ppu.AddressingMode.getAddressingMode(self.getMem().lcdc);

        // Some dark magic for signed mode which offset 0x9000
        // by a signed address
        const tile_addr: u16 = switch (addressing) {
            .UNSIGNED => 0x8000 + (@as(u16, tile_index) << 4),
            .SIGNED => 0x8800 + (@as(u16, tile_index ^ 0x80) << 4),
        };

        return Tile.fromAddr(tile_addr, self.getMem());
    }
};
