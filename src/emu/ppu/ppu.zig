// Pixel processing unit
const constants = @import("../const.zig");
const std = @import("std");

const GbModel = @import("../hardware.zig").GbModel;
const Cpu = @import("../cpu/cpu.zig").Cpu;
const Bus = @import("../memory/bus.zig").Bus;
const Sprite = @import("sprite.zig").Sprite;
const PpuMem = @import("ppu_mem.zig").PpuMem;
const Gameboy = @import("../root.zig").Gameboy;
const Background = @import("background.zig").Background;

const SCREEN_HEIGHT = constants.screen_height;
const SCREEN_WIDTH = constants.screen_width;

pub const Ppu = struct {
    mem: PpuMem,
    allocator: std.mem.Allocator,
    bg: Background = .{},

    frame_buffer: [SCREEN_HEIGHT * SCREEN_WIDTH]u32 = .{0} ** (SCREEN_HEIGHT * SCREEN_WIDTH),
    dots: u16 = 0,

    pub fn init(model: GbModel, allocator: std.mem.Allocator) Ppu {
        return .{
            .mem = PpuMem.init(model, allocator) catch @panic("Failed to initialize PPU mem"),
            .allocator = allocator,
        };
    }

    fn getCpu(self: *Ppu) *Cpu {
        const bus: *Bus = @alignCast(@fieldParentPtr("ppu", self));
        return @alignCast(@fieldParentPtr("bus", bus));
    }

    fn setMode(self: *@This(), mode: Mode) void {
        self.mem.stat = (self.mem.stat & 0xFC) | @intFromEnum(mode);
    }

    pub fn getMode(self: *Ppu) Mode {
        return @enumFromInt(self.mem.stat & 0b11);
    }

    pub fn deinit(self: *Ppu) void {
        self.mem.deinit();
    }

    pub fn tick(self: *Ppu, cycles: u16) void {
        if (self.mem.lcdc & 0x80 == 0) return self.turn_off();
        const gb = Gameboy.getGB("ppu", self);

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
                self.mem.ly += 1;
                if (self.mem.ly == 144) {
                    self.setMode(.v_blank);
                    gb.cpu.int.request_vblank();
                } else {
                    self.setMode(.oam_scan);
                }
            },
            .v_blank => {
                if (self.dots < 456) return;
                self.dots -= 456;
                self.mem.ly += 1;

                if (self.mem.ly <= 153) return;
                self.mem.ly = 0;
                self.setMode(.oam_scan);
            },
        }
    }

    fn renderScanLine(self: *Ppu) void {
        const ly = @as(u16, self.mem.ly);
        const absolute_y: u16 = (ly + @as(u16, self.mem.scy)) % 256;
        const tile_y: u16 = absolute_y / 8;
        const offset_y: u16 = absolute_y % 8;
        for (0..SCREEN_WIDTH) |x| {
            const absolute_x: u16 = (@as(u16, @truncate(x)) + @as(u16, self.mem.scx)) % 256;
            const tile_x: u16 = absolute_x / 8;
            const offset_x: u16 = absolute_x % 8;

            const tile = self.bg.getTileAt(tile_x, tile_y);
            const pixel_data = tile.getPixelAt(offset_x, offset_y);
            const pixel_color = self.getColorByBgPalette(pixel_data);
            self.putPixel(x, self.mem.ly, pixel_color);
        }
        if (self.mem.lcdc & 0x02 > 0) {
            for (0..40) |i| {
                const sprite = Sprite.fromOam(self.mem.oam[i * 4 .. (i * 4) + 4][0..4]);
                if (sprite.y_pos > self.mem.ly + 8 and sprite.y_pos <= self.mem.ly + 16) {
                    const lsb = self.mem.vram[@as(u16, sprite.tile_index) * 16 + (if (sprite.flags.y_flip) 8 - (self.mem.ly - (sprite.y_pos - 16)) else self.mem.ly - (sprite.y_pos - 16)) * 2];
                    const msb = self.mem.vram[@as(u16, sprite.tile_index) * 16 + (if (sprite.flags.y_flip) 8 - (self.mem.ly - (sprite.y_pos - 16)) else self.mem.ly - (sprite.y_pos - 16)) * 2 + 1];
                    for (0..8) |j| {
                        const bit_index: u3 = if (sprite.flags.x_flip) @truncate(j) else @truncate(7 - j);
                        const lsb_bit = (lsb >> bit_index) & 1;
                        const msb_bit = (msb >> bit_index) & 1;
                        const color_index = @as(u2, @truncate((msb_bit << 1) | lsb_bit));
                        const color = self.getColorByObjPalette(color_index, sprite.flags.dmg_palette);
                        if (color_index != 0) {
                            self.putPixel(sprite.x_pos - 8 + j, self.mem.ly, color);
                        }
                    }
                }
            }
        }
    }

    fn putPixel(self: *Ppu, x: usize, y: usize, color_id: u2) void {
        if (x >= SCREEN_WIDTH or y >= SCREEN_HEIGHT) return;

        const index = (y * SCREEN_WIDTH) + x;

        const color_argb: u32 = switch (color_id) {
            0 => constants.argb_color_palette.white,
            1 => constants.argb_color_palette.light_gray,
            2 => constants.argb_color_palette.dark_gray,
            3 => constants.argb_color_palette.black,
        };

        self.frame_buffer[index] = color_argb;
    }

    inline fn getColorByBgPalette(self: *Ppu, color_id: u2) u2 {
        return switch (color_id) {
            0 => @truncate((self.mem.bgp & 0x03)),
            1 => @truncate((self.mem.bgp & 0x0C) >> 2),
            2 => @truncate((self.mem.bgp & 0x30) >> 4),
            3 => @truncate((self.mem.bgp & 0xC0) >> 6),
        };
    }

    inline fn getColorByObjPalette(self: *Ppu, color_id: u2, obp: u1) u2 {
        return switch (obp) {
            0 => switch (color_id) {
                0 => @truncate((self.mem.obp0 & 0x03)),
                1 => @truncate((self.mem.obp0 & 0x0C) >> 2),
                2 => @truncate((self.mem.obp0 & 0x30) >> 4),
                3 => @truncate((self.mem.obp0 & 0xC0) >> 6),
            },
            1 => switch (color_id) {
                0 => @truncate((self.mem.obp1 & 0x03)),
                1 => @truncate((self.mem.obp1 & 0x0C) >> 2),
                2 => @truncate((self.mem.obp1 & 0x30) >> 4),
                3 => @truncate((self.mem.obp1 & 0xC0) >> 6),
            },
        };
    }

    pub fn turn_off(self: *Ppu) void {
        if (self.frame_buffer[0] == constants.argb_color_palette.white_off) return;
        self.setMode(Mode.h_blank);
        self.mem.ly = 0;

        for (0..SCREEN_HEIGHT * SCREEN_WIDTH) |i| {
            self.frame_buffer[i] = constants.argb_color_palette.white_off;
        }
    }

    pub const Mode = enum(u2) {
        drawing = 3,
        h_blank = 0,
        oam_scan = 2,
        v_blank = 1,
    };

    pub const AddressingMode = enum {
        SIGNED,
        UNSIGNED,

        pub fn getAddressingMode(lcdc: u8) AddressingMode {
            return if (lcdc & 0x10 > 0) .UNSIGNED else .SIGNED;
        }
    };
};
