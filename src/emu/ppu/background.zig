const Ppu = @import("./ppu.zig").Ppu;

fn getBgTileMapArea(lcdc: u8) u16 {
    return if (lcdc & 8 > 0) 0x9C00 else 0x9800;
}

/// Get the tile address at position (x,y)
pub fn getTileAddrAt(ppu: *Ppu, tile_x: u16, tile_y: u16) u16 {
    const lcdc = ppu.lcdc;
    const tile_index_addr = getBgTileMapArea(lcdc) + (tile_y * 32) + tile_x;
    const tile_index = ppu.read_vram(tile_index_addr);
    const addressing = Ppu.AddressingMode.getAddressingMode(lcdc);

    // Some dark magic for signed mode which offset 0x9000
    // by a signed address
    const tile_addr: u16 = switch (addressing) {
        .UNSIGNED => 0x8000 + (@as(u16, tile_index) << 4),
        .SIGNED => 0x8800 + (@as(u16, tile_index ^ 0x80) << 4),
    };

    return tile_addr;
}
