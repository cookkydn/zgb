// Pixel processing unit
const std = @import("std");
const constants = @import("../const.zig");
const alu = @import("../cpu/arithmetics.zig");

const GbModel = @import("../hardware.zig").GbModel;
const CPU = @import("../cpu/cpu.zig").CPU;
const Bus = @import("../memory/bus.zig").Bus;

const SCREEN_HEIGHT = constants.SCREEN_HEIGHT;
const SCREEN_WIDTH = constants.SCREEN_WIDTH;

pub const PPU = struct {
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
    obj_palette_0: u8 = 0,
    obj_palette_1: u8 = 0,

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
        if (self.lcdc & 0x80 == 0) return;

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
        for (0..SCREEN_WIDTH) |x| {
            const tile_index = self.getBGTileMapArea() + @as(u16, self.ly / 8) * 32 + @as(u16, @truncate(x / 8));
            const tile_number = self.vram[tile_index - 0x8000];
            const pixel_data = self.getPixelData(tile_number, @as(u16, @truncate(x % 8)), @as(u16, self.ly % 8));
            const pixel_color = self.getColorByPalette(pixel_data);
            self.putPixel(x, self.ly, pixel_color);
        }
    }

    fn getPixelData(self: *PPU, tile_index: u8, x: u16, y: u16) u2 {
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

    inline fn getColorByPalette(self: *PPU, color_id: u2) u2 {
        return switch (color_id) {
            0 => @truncate((self.bg_palette_data & 0x03)),
            1 => @truncate((self.bg_palette_data & 0x0C) >> 2),
            2 => @truncate((self.bg_palette_data & 0x30) >> 4),
            3 => @truncate((self.bg_palette_data & 0xC0) >> 6),
        };
    }

    pub const Mode = enum(u2) {
        h_blank = 0,
        v_blank = 1,
        oam_scan = 2,
        drawing = 3,
    };

    const AddressingMode = enum { SIGNED, UNSIGNED };
};
