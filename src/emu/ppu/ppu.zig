// Pixel processing unit
const std = @import("std");
const constants = @import("../const.zig");
const alu = @import("../cpu/arithmetics.zig");

const GbModel = @import("../hardware.zig").GbModel;
const CPU = @import("../cpu/cpu.zig").CPU;
const Bus = @import("../memory/bus.zig").Bus;
const Sprite = @import("sprite.zig").Sprite;

const SCREEN_HEIGHT = constants.SCREEN_HEIGHT;
const SCREEN_WIDTH = constants.SCREEN_WIDTH;

pub const PPU = struct {
    /// 0x8000 to 0x9FFF
    vram: []u8,
    oam: [0xA0]u8 = .{0} ** 0xA0,

    // Registers
    lcdc: u8 = 0,
    stat: u8 = 0,
    scy: u8 = 0,
    scx: u8 = 0,
    ly: u8 = 0,
    window_y: u8 = 0,
    window_x: u8 = 0,

    bg_palette_data: u8 = 0,
    obp0: u8 = 0,
    obp1: u8 = 0,

    frame_buffer: [SCREEN_HEIGHT * SCREEN_WIDTH]u32 = .{0} ** (SCREEN_HEIGHT * SCREEN_WIDTH),
    dots: u16 = 0,

    fn getCpu(self: *PPU) *CPU {
        const bus: *Bus = @alignCast(@fieldParentPtr("ppu", self));
        return @alignCast(@fieldParentPtr("bus", bus));
    }

    pub fn setMode(self: *PPU, mode: Mode) void {
        self.stat = (self.stat & 0xFC) | @intFromEnum(mode);
    }

    pub fn getMode(self: *PPU) Mode {
        return @enumFromInt(self.stat & 0b11);
    }

    pub fn init(model: GbModel, allocator: std.mem.Allocator) PPU {
        const vram_size = switch (model) {
            .DMG0 => 0x2000,
        };

        const vram = allocator.alloc(u8, vram_size) catch @panic("Unable to allocate vram: OutOfMemory error\n");

        return .{
            .vram = vram,
        };
    }

    pub fn deinit(self: *PPU, allocator: std.mem.Allocator) void {
        allocator.free(self.vram);
    }

    pub fn tick(self: *PPU, cycles: u16) void {
        if (self.lcdc & 0x80 == 0) return self.turn_off();

        self.dots += cycles;

        switch (self.getMode()) {
            .oam_scan => {
                if (self.dots < 80) return;
                self.dots -= 80;
                self.setMode(.drawing);
            },
            .drawing => {
                if (self.dots < 172) return;
                self.dots -= 172;
                self.setMode(.h_blank);
                self.renderScanLine();
            },
            .h_blank => {
                if (self.dots < 204) return;
                self.dots -= 204;
                self.ly += 1;
                if (self.ly == 144) {
                    self.setMode(.v_blank);
                    self.getCpu().interrupts.request_vblank();
                } else {
                    self.setMode(.oam_scan);
                }
            },
            .v_blank => {
                if (self.dots < 456) return;
                self.dots -= 456;
                self.ly += 1;

                if (self.ly <= 153) return;
                self.ly = 0;
                self.setMode(.oam_scan);
            },
        }
    }

    fn renderScanLine(self: *PPU) void {
        const absolute_y: u16 = (@as(u16, self.ly) + @as(u16, self.scy)) % 256;
        const tile_y: u16 = absolute_y / 8;
        const offset_y: u16 = absolute_y % 8;
        for (0..SCREEN_WIDTH) |x| {
            const absolute_x: u16 = (@as(u16, @truncate(x)) + @as(u16, self.scx)) % 256;
            const tile_x: u16 = absolute_x / 8;
            const offset_x: u16 = absolute_x % 8;

            const tile_addr = self.getBGTileMapArea() + (tile_y * 32) + tile_x;
            const tile_index = self.vram[tile_addr - 0x8000];
            const pixel_data = self.getBackgroundPixelData(tile_index, offset_x, offset_y);
            const pixel_color = self.getColorByBgPalette(pixel_data);
            self.putPixel(x, self.ly, pixel_color);
        }
        if (self.lcdc & 0x02 > 0) {
            for (0..40) |i| {
                const sprite = Sprite.from_oam(self.oam[i * 4 .. (i * 4) + 4][0..4]);
                if (sprite.y_pos > self.ly + 8 and sprite.y_pos <= self.ly + 16) {
                    const lsb = self.vram[@as(u16, sprite.tile_index) * 16 + (if (sprite.flags.y_flip) 8 - (self.ly - (sprite.y_pos - 16)) else self.ly - (sprite.y_pos - 16)) * 2];
                    const msb = self.vram[@as(u16, sprite.tile_index) * 16 + (if (sprite.flags.y_flip) 8 - (self.ly - (sprite.y_pos - 16)) else self.ly - (sprite.y_pos - 16)) * 2 + 1];
                    for (0..8) |j| {
                        const bit_index: u3 = if (sprite.flags.x_flip) @truncate(j) else @truncate(7 - j);
                        const lsb_bit = (lsb >> bit_index) & 1;
                        const msb_bit = (msb >> bit_index) & 1;
                        const color_index = @as(u2, @truncate((msb_bit << 1) | lsb_bit));
                        const color = self.getColorByObjPalette(color_index, sprite.flags.dmg_palette);
                        if (color_index != 0) {
                            self.putPixel(sprite.x_pos - 8 + j, self.ly, color);
                        }
                    }
                }
            }
        }
    }

    fn getBackgroundPixelData(self: *PPU, tile_index: u8, x: u16, y: u16) u2 {
        std.debug.assert(x < 8 and y < 8);
        const addressing = self.getAddressingMode();

        const tile_addr: u16 = addr: switch (addressing) {
            .SIGNED => {
                const tile_id: i8 = @bitCast(tile_index);
                const offset: i16 = @as(i16, tile_id) * 16;
                break :addr 0x9000 +% @as(u16, @bitCast(offset));
            },
            .UNSIGNED => 0x8000 + (@as(u16, tile_index) * 16),
        };

        const vram_offset = tile_addr - 0x8000;

        const lsb = self.vram[vram_offset + (y * 2)];
        const msb = self.vram[vram_offset + (y * 2) + 1];

        const bit_index: u3 = @truncate(7 - x);
        const lsb_bit = (lsb >> bit_index) & 1;
        const msb_bit = (msb >> bit_index) & 1;

        return @as(u2, @truncate((msb_bit << 1) | lsb_bit));
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

    inline fn getBGTileMapArea(self: *PPU) u16 {
        return if (self.lcdc & 8 > 0) 0x9C00 else 0x9800;
    }

    inline fn getAddressingMode(self: *PPU) AddressingMode {
        return if (self.lcdc & 16 > 0) .UNSIGNED else .SIGNED;
    }

    inline fn getColorByBgPalette(self: *PPU, color_id: u2) u2 {
        return switch (color_id) {
            0 => @truncate((self.bg_palette_data & 0x03)),
            1 => @truncate((self.bg_palette_data & 0x0C) >> 2),
            2 => @truncate((self.bg_palette_data & 0x30) >> 4),
            3 => @truncate((self.bg_palette_data & 0xC0) >> 6),
        };
    }

    inline fn getColorByObjPalette(self: *PPU, color_id: u2, obp: u1) u2 {
        return switch (obp) {
            0 => switch (color_id) {
                0 => @truncate((self.obp0 & 0x03)),
                1 => @truncate((self.obp0 & 0x0C) >> 2),
                2 => @truncate((self.obp0 & 0x30) >> 4),
                3 => @truncate((self.obp0 & 0xC0) >> 6),
            },
            1 => switch (color_id) {
                0 => @truncate((self.obp1 & 0x03)),
                1 => @truncate((self.obp1 & 0x0C) >> 2),
                2 => @truncate((self.obp1 & 0x30) >> 4),
                3 => @truncate((self.obp1 & 0xC0) >> 6),
            },
        };
    }

    pub fn turn_off(self: *PPU) void {
        if (self.frame_buffer[0] == constants.ARGB_COLOR_PALETTE.WHITE_OFF) return;
        self.setMode(Mode.h_blank);
        self.ly = 0;

        for (0..SCREEN_HEIGHT * SCREEN_WIDTH) |i| {
            self.frame_buffer[i] = constants.ARGB_COLOR_PALETTE.WHITE_OFF;
        }
    }

    pub const Mode = enum(u2) {
        h_blank = 0,
        v_blank = 1,
        oam_scan = 2,
        drawing = 3,
    };

    const AddressingMode = enum { SIGNED, UNSIGNED };
};
