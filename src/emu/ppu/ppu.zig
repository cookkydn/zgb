//! Pixel processing unit
pub const Ppu = @This();

const std = @import("std");
const GbModel = @import("../hardware.zig").GbModel;
const Cpu = @import("../cpu/cpu.zig").Cpu;
const Bus = @import("../memory/bus.zig").Bus;
const Sprite = @import("sprite.zig").Sprite;
const Gameboy = @import("../root.zig").Gameboy;
const background = @import("background.zig");
const tile = @import("tile.zig");
const Window = @import("./window.zig").Window;
const Allocator = std.mem.Allocator;
const alu = @import("../cpu/arithmetics.zig");

const SCREEN_HEIGHT = 144;
const SCREEN_WIDTH = 160;

/// Vram `0x8000` to `0x9FFF`
///
/// Switchable bank in CGB (*todo*)
vram: []u8,

/// OAM `0xFE00` to `0xFE9F`
oam: []u8,

/// LCD Control `0xFF40` (*R/W*)
lcdc: u8 = 0,

/// LCD Status `0xFF41` (*Mixed*)
stat: u8 = 0,

/// SCY `0xFF42` (*R/W*)
scy: u8 = 0,

/// SCX `0xFF43` (*R/W*)
scx: u8 = 0,

/// LY `0xFF44`(*R*)
ly: u8 = 0,

/// LYC `0xFF45` (*R/W*)
lyc: u8 = 0,

/// DMA src address & start `0xFF46`(*R/W*)
dma: u8 = 0,

/// BG palette data `0xFF47` (*R/W*)
///
/// **DMG only**
bgp: u8 = 0,

/// Object palette 0 data `0xFF48` (*R/W*)
///
/// **DMG only**
obp0: u8 = 0,

/// Object palette 1 data `0xFF49` (*R/W*)
///
/// **DMG only**
obp1: u8 = 0,

/// Window y position `0xFF4A` (*R/W*)
wy: u8 = 0,

/// Window x position `0xFF4B` (*R/W*)
///
/// *note: add 7 to this value*
wx: u8 = 0,
window: Window = .{},

debug_was_bg_and_window_display_set: bool = false,

frame_buffer: [SCREEN_HEIGHT * SCREEN_WIDTH]u32 = .{0} ** (SCREEN_HEIGHT * SCREEN_WIDTH),
dots: u16 = 0,

pub fn init(model: GbModel, allocator: Allocator) !Ppu {
    const vram = try allocator.alloc(u8, model.vramSize());
    const oam = try allocator.alloc(u8, 0xA0);
    return .{
        .vram = vram,
        .oam = oam,
    };
}

fn setMode(self: *@This(), mode: Mode) void {
    self.stat = (self.stat & 0xFC) | @intFromEnum(mode);
}

pub fn getMode(self: *Ppu) Mode {
    return @enumFromInt(self.stat & 0b11);
}

pub fn deinit(self: *Ppu, all: Allocator) void {
    all.free(self.vram);
    all.free(self.oam);
}

pub fn tick(self: *Ppu, cycles: u16) void {
    if (self.lcdc & 0x80 == 0) return self.turn_off();
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
            self.ly += 1;
            if (self.ly == 144) {
                self.setMode(.v_blank);
                gb.cpu.int.requestVblank();
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

fn renderScanLine(self: *Ppu) void {
    if (self.isBgAndWindowEnabled() and !self.debug_was_bg_and_window_display_set) {
        std.log.debug("BG & Window: enabled", .{});
        self.debug_was_bg_and_window_display_set = true;
    } else if (!self.isBgAndWindowEnabled() and self.debug_was_bg_and_window_display_set) {
        std.log.debug("BG & Window: disabled", .{});
        self.debug_was_bg_and_window_display_set = false;
    }
    const ly = @as(u16, self.ly);
    const absolute_y: u16 = (ly + @as(u16, self.scy)) % 256;
    const tile_y: u16 = absolute_y / 8;
    const offset_y: u16 = absolute_y % 8;
    const obj_size_flag: bool = self.lcdc & 0x4 == 0x4;
    self.window.newLine();
    for (0..SCREEN_WIDTH) |x| {
        if (!self.isBgAndWindowEnabled()) {
            self.putPixel(x, self.ly, 0);
        } else {
            const absolute_x: u16 = (@as(u16, @truncate(x)) + @as(u16, self.scx)) % 256;
            const tile_x: u16 = absolute_x / 8;
            const offset_x: u16 = absolute_x % 8;
            const tile_addr = background.getTileAddrAt(self, tile_x, tile_y);
            const pixel_data = tile.getPixelAt(self, tile_addr, offset_x, offset_y);
            var pixel_color = self.getColorByBgPalette(pixel_data);
            if (self.window.getPixelColorAt(absolute_x, absolute_y)) |pixel| {
                pixel_color = self.getColorByBgPalette(pixel);
            }
            self.putPixel(x, self.ly, pixel_color);
        }
    }
    if (self.lcdc & 0x02 > 0) {
        if (obj_size_flag) {
            for (0..20) |i| {
                const sprite = Sprite.fromOam(self.oam[i * 4 .. (i * 4) + 4][0..4]);
                if (sprite.y_pos > self.ly and sprite.y_pos <= self.ly + 16) {
                    const lsb = self.vram[@as(u16, sprite.tile_index) *% 16 +% (if (sprite.flags.y_flip) 8 -% (self.ly -% (sprite.y_pos -% 16)) else self.ly -% (sprite.y_pos -% 16)) *% 2];
                    const msb = self.vram[@as(u16, sprite.tile_index) *% 16 +% (if (sprite.flags.y_flip) 8 -% (self.ly -% (sprite.y_pos -% 16)) else self.ly -% (sprite.y_pos -% 16)) *% 2 +% 1];
                    for (0..8) |j| {
                        const bit_index: u3 = if (sprite.flags.x_flip) @truncate(j) else @truncate(7 - j);
                        const lsb_bit = (lsb >> bit_index) & 1;
                        const msb_bit = (msb >> bit_index) & 1;
                        const color_index = @as(u2, @truncate((msb_bit << 1) | lsb_bit));
                        const color = self.getColorByObjPalette(color_index, sprite.flags.dmg_palette);
                        if (color_index != 0) {
                            self.putPixel(sprite.x_pos -| 8 + j, self.ly, color);
                        }
                    }
                }
            }
        } else {
            for (0..40) |i| {
                const sprite = Sprite.fromOam(self.oam[i * 4 .. (i * 4) + 4][0..4]);
                if (sprite.y_pos > self.ly + 8 and sprite.y_pos <= self.ly + 16) {
                    const lsb = self.vram[@as(u16, sprite.tile_index) * 16 +% (if (sprite.flags.y_flip) 8 -% (self.ly -% (sprite.y_pos -% 16)) else self.ly -% (sprite.y_pos -% 16)) * 2];
                    const msb = self.vram[@as(u16, sprite.tile_index) * 16 +% (if (sprite.flags.y_flip) 8 -% (self.ly -% (sprite.y_pos - 16)) else self.ly -% (sprite.y_pos -% 16)) * 2 + 1];
                    for (0..8) |j| {
                        const bit_index: u3 = if (sprite.flags.x_flip) @truncate(j) else @truncate(7 - j);
                        const lsb_bit = (lsb >> bit_index) & 1;
                        const msb_bit = (msb >> bit_index) & 1;
                        const color_index = @as(u2, @truncate((msb_bit << 1) | lsb_bit));
                        const color = self.getColorByObjPalette(color_index, sprite.flags.dmg_palette);
                        if (color_index != 0) {
                            self.putPixel(sprite.x_pos -| 8 + j, self.ly, color);
                        }
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
        0 => argb_color_palette.white,
        1 => argb_color_palette.light_gray,
        2 => argb_color_palette.dark_gray,
        3 => argb_color_palette.black,
    };

    self.frame_buffer[index] = color_argb;
}

inline fn getColorByBgPalette(self: *Ppu, color_id: u2) u2 {
    return switch (color_id) {
        0 => @truncate((self.bgp & 0x03)),
        1 => @truncate((self.bgp & 0x0C) >> 2),
        2 => @truncate((self.bgp & 0x30) >> 4),
        3 => @truncate((self.bgp & 0xC0) >> 6),
    };
}

inline fn getColorByObjPalette(self: *Ppu, color_id: u2, obp: u1) u2 {
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

pub fn turn_off(self: *Ppu) void {
    if (self.frame_buffer[0] == argb_color_palette.white_off) return;
    self.setMode(Mode.h_blank);
    self.ly = 0;

    for (0..SCREEN_HEIGHT * SCREEN_WIDTH) |i| {
        self.frame_buffer[i] = argb_color_palette.white_off;
    }
}

pub fn isBgAndWindowEnabled(self: *Ppu) bool {
    return self.lcdc & 1 == 1;
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

const argb_color_palette = struct {
    pub const white_off = 0xFF9CBC0F;
    pub const white = 0xFF9CBC0F;
    pub const light_gray = 0xFF8BAC0F;
    pub const dark_gray = 0xFF306230;
    pub const black = 0xFF10380F;
};

// -- Memory --
pub fn write_vram(self: *Ppu, addr: u16, value: u8) void {
    // TODO CGB VRAM Banking
    self.vram[addr - 0x8000] = value;
}

pub fn read_vram(self: *Ppu, addr: u16) u8 {
    return self.read_vram_slice(addr, 1)[0];
}

pub fn read_vram_slice(self: *Ppu, addr: u16, length: u16) []u8 {
    // TODO CGB VRAM Banking
    const addr_offset = addr - 0x8000;
    return self.vram[addr_offset..(addr_offset + length)];
}

/// Handle writes for the memory region `0xFF40` to `0xFF4B`
pub fn write_registers(self: *Ppu, addr: u16, value: u8) void {
    switch (addr) {
        0xFF40 => self.lcdc = value,
        0xFF41 => self.stat = alu.maskedWrite(self.stat, 0x78, value),
        0xFF42 => self.scy = value,
        0xFF43 => self.scx = value,
        0xFF44 => {}, // readonly
        0xFF45 => self.lyc = value,
        0xFF46 => {
            self.dma = value;
            self.execute_dma();
        },
        0xFF47 => self.bgp = value,
        0xFF48 => self.obp0 = value,
        0xFF49 => self.obp1 = value,
        0xFF4A => self.wy = value,
        0xFF4B => self.wx = value,
        else => unreachable,
    }
}

/// Handle reads for the memory region `0xFF40` to `0xFF4B`
pub fn read_registers(
    self: *Ppu,
    addr: u16,
) u8 {
    switch (addr) {
        0xFF40 => return self.lcdc,
        0xFF41 => return self.stat,
        0xFF42 => return self.scy,
        0xFF43 => return self.scx,
        0xFF44 => return self.ly,
        0xFF45 => return self.lyc,
        0xFF46 => return self.dma,
        0xFF47 => return self.bgp,
        0xFF48 => return self.obp0,
        0xFF49 => return self.obp1,
        0xFF4A => return self.wy,
        0xFF4B => return self.wx,
        else => unreachable,
    }
}

/// Execute DMA transfers
///
/// Copy from `[DMA*256]` to `[DMA*256] + 160` into OAM
fn execute_dma(self: *Ppu) void {
    const gb = Gameboy.getGB("ppu", self);
    const transfer_src: u16 = @as(u16, self.dma) << 8;
    for (0..0xA0) |i| {
        self.oam[i] = gb.bus.read_at(transfer_src + @as(u16, @truncate(i)));
    }
    gb.timer.tick(640);
}
