const Ppu = @import("./ppu.zig");
const std = @import("std");
const assert = std.debug.assert;

pub fn getPixelAt(ppu: *Ppu, tile_addr: u16, x: u16, y: u16) u2 {
    assert(x < 8);
    assert(y < 8);
    const data = ppu.read_vram_slice(tile_addr, 16)[0..16];
    const lsb_line = data[y * 2];
    const msb_line = data[(y * 2) + 1];

    const bit_index: u3 = @truncate(7 - x);
    const lsb_bit: u2 = @truncate((lsb_line >> bit_index) & 1);
    const msb_bit: u2 = @truncate((msb_line >> bit_index) & 1);

    return (msb_bit << 1) | lsb_bit;
}
