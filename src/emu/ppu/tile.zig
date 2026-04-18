const std = @import("std");
const PpuMem = @import("./ppu_mem.zig").PpuMem;
const assert = std.debug.assert;

pub const Tile = struct {
    data: *[16]u8,

    pub fn fromAddr(addr: u16, mem: *PpuMem) Tile {
        return .{
            .data = mem.read_vram_slice(addr, 16)[0..16],
        };
    }

    pub fn getPixelAt(self: Tile, x: u16, y: u16) u2 {
        assert(x < 8);
        assert(y < 8);
        const lsb_line = self.data[y * 2];
        const msb_line = self.data[(y * 2) + 1];

        const bit_index: u3 = @truncate(7 - x);
        const lsb_bit: u2 = @truncate((lsb_line >> bit_index) & 1);
        const msb_bit: u2 = @truncate((msb_line >> bit_index) & 1);

        return (msb_bit << 1) | lsb_bit;
    }
};
