// Pixel processing unit
const std = @import("std");
const constants = @import("../const.zig");

const GbModel = @import("../hardware.zig").GbModel;

const SCREEN_HEIGHT = constants.SCREEN_HEIGHT;
const SCREEN_WIDTH = constants.SCREEN_WIDTH;

pub const PPU = struct {
    vram: []u8,
    ppu_registers: [0xC]u8,
    frame_buffer: [SCREEN_HEIGHT * SCREEN_WIDTH]u32 = .{0} ** (SCREEN_HEIGHT * SCREEN_WIDTH),

    pub fn init(model: GbModel, allocator: std.mem.Allocator) PPU {
        const vram_size = switch (model) {
            .DMG0 => 0x2000,
        };

        const vram = allocator.alloc(u8, vram_size) catch @panic("Unable to allocate vram: OutOfMemory error\n");

        return .{
            .vram = vram,
            .ppu_registers = .{0x00} ** 0xC,
        };
    }

    pub fn deinit(self: *PPU, allocator: std.mem.Allocator) void {
        allocator.free(self.vram);
    }

    fn putPixel(self: *PPU, x: usize, y: usize, color_id: u2) void {
        if (x >= SCREEN_WIDTH or y >= SCREEN_HEIGHT) return;

        const index = (y * SCREEN_WIDTH) + x;

        const color_argb: u32 = switch (color_id) {
            0 => constants.ARGB_COLOR_PALETTE.WHITE,
            1 => constants.ARGB_COLOR_PALETTE.LIGHT_GRAY,
            2 => constants.ARGB_COLOR_PALETTE.DARK_GRAY,
            3 => constants.ARGB_COLOR_PALETTE.BLACK,
        };

        self.frame_buffer[index] = color_argb;
    }
};
