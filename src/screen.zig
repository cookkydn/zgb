const sdl = @import("zsdl2");
const std = @import("std");
const CPU = @import("cpu.zig").CPU;
const Memory = @import("mem.zig").Memory;

const LCD_STATUS = 0xFF41;
const LCDC = 0xFF40;

const OAM_CYCLE_COUNT = 80;
const SENDING_CYCLE_COUNT = 172;
const HBLANK_CYCLE_COUNT = 204;
const SCANLINE_CYCLE_COUNT = 456;
pub const Screen = struct {
    window: *sdl.Window,
    renderer: *sdl.Renderer,
    x_pos: u16,
    cycle_count: u32,
    tiles: [384]Tile,
    pub fn get_cpu(self: *Screen) *CPU {
        return @fieldParentPtr("screen", self);
    }

    pub fn init() !Screen {
        try sdl.init(.{ .audio = true, .video = true });

        const window = try sdl.Window.create(
            "zgb",
            sdl.Window.pos_undefined,
            sdl.Window.pos_undefined,
            256 * 2,
            256 * 2,
            .{ .opengl = true, .allow_highdpi = false },
        );
        const renderer = try sdl.createRenderer(window, null, .{ .accelerated = true, .present_vsync = true, .software = false, .target_texture = false });

        return .{
            .renderer = renderer,
            .window = window,
            .x_pos = 0,
            .cycle_count = 0,
            .tiles = .{Tile.empty()} ** 384,
        };
    }

    pub fn lcd_tick(self: *Screen, cycle: u32) void {
        // std.debug.print("lcd: {}, screen: {:0>3}, mode: {s:>11}, ly: {}, lcdc: 0b{b:0>8}", .{ self.is_lcd_enabled(), self.cycle_count, @tagName(self.get_ppu_mode()), self.get_ly(), self.get_lcdc() });
        if (!self.is_lcd_enabled() and self.get_ppu_mode() != .h_blank) {
            self.reset_ly();
            self.set_ppu_mode(.h_blank);
            return;
        } else if (!self.is_lcd_enabled()) return;
        self.cycle_count += cycle;
        switch (self.get_ppu_mode()) {
            .oam_search => {
                if (self.cycle_count >= OAM_CYCLE_COUNT) {
                    self.cycle_count %= OAM_CYCLE_COUNT;
                    self.set_ppu_mode(.lcd_operate);
                }
            },
            .lcd_operate => {
                if (self.cycle_count >= SENDING_CYCLE_COUNT) {
                    self.cycle_count %= SENDING_CYCLE_COUNT;
                    self.set_ppu_mode(.h_blank);
                    //TODO see interrupts
                }
            },
            .h_blank => {
                if (self.cycle_count >= HBLANK_CYCLE_COUNT) {
                    self.cycle_count %= HBLANK_CYCLE_COUNT;
                    self.inc_ly();
                    if (self.get_ly() >= 144) {
                        self.set_ppu_mode(.v_blank);
                        self.get_cpu().int.request_vblank();
                    } else {
                        self.set_ppu_mode(.oam_search);
                    }
                }
            },
            .v_blank => {
                if (self.cycle_count >= SCANLINE_CYCLE_COUNT) {
                    self.cycle_count %= SCANLINE_CYCLE_COUNT;
                    self.inc_ly();
                    if (self.get_ly() == 0) {
                        self.set_ppu_mode(.oam_search);
                        self.render() catch unreachable;
                        std.debug.print("== RENDER ==", .{});
                    }
                }
            },
        }
    }

    pub fn render(self: *Screen) !void {
        const mem = self.get_cpu().mem.data;
        if (self.is_lcd_enabled()) {
            try self.renderer.setDrawColor(.{ .r = 0, .g = 0, .b = 0, .a = 0 });
            try self.renderer.clear();
            for (0..32) |y| {
                for (0..32) |x| {
                    switch (self.get_tile_select_mode()) {
                        .UNSIGNED => {
                            const tile_id = self.get_cpu().mem.data[0x9800 + x + (y * 32)];
                            const tile_addr = 0x8000 + @as(u16, tile_id) * 16;
                            var tile = Tile.from_mem(mem[tile_addr..(tile_addr + 16)], tile_id, mem[0xFF47]);
                            try tile.draw_tile(self.renderer, @truncate(x), @truncate(y));
                        },
                        .SIGNED => {
                            const tile_id = @as(i8, @bitCast(self.get_cpu().mem.data[0x9800 + x + (y * 32)]));
                            const tile_addr: u16 = @intCast(@as(i32, 0x9000) + @as(i16, tile_id) * 16);
                            var tile = Tile.from_mem(mem[tile_addr..(tile_addr + 16)], @bitCast(tile_id), mem[0xFF47]);
                            try tile.draw_tile(self.renderer, @truncate(x), @truncate(y));
                        },
                    }
                }
            }

            // for (35..67) |x| {
            //     for (0..12) |y| {
            //         const tile_addr = 0x8000 + (@as(u16, @truncate(x - 35)) + @as(u16, @truncate(y)) * 32) * 16;
            //         var tile = Tile.from_mem(mem[tile_addr..(tile_addr + 16)], 0, mem[0xFF47]);
            //         try tile.draw_tile(self.renderer, @truncate(x), @truncate(y));
            //     }
            // }
            try self.draw_bg_area();
            sdl.renderPresent(self.renderer);
        } else {
            try self.renderer.setDrawColor(.{ .r = 255, .g = 255, .b = 255, .a = 255 });
            try self.renderer.clear();
            sdl.renderPresent(self.renderer);
        }
    }

    fn draw_bg_area(self: *Screen) !void {
        const scy = self.get_cpu().mem.read_at(0xFF42);
        const scx = self.get_cpu().mem.read_at(0xFF43);
        try self.renderer.setDrawColorRGB(255, 0, 0);
        try self.renderer.drawRect(.{ .x = @intCast(scx), .y = @intCast(scy), .w = 160 * 2, .h = 144 * 2 });
    }

    pub fn set_ppu_mode(self: *Screen, mode: PPU_MODE) void {
        const curr = self.get_cpu().mem.data[LCD_STATUS];
        self.get_cpu().mem.data[LCD_STATUS] = (curr & 0b11111100) | @intFromEnum(mode);

        if (mode == .lcd_operate) {
            self.x_pos = 0;
        }
    }

    pub fn get_ppu_mode(self: *Screen) PPU_MODE {
        const val = @as(u2, @truncate(self.get_cpu().mem.data[LCD_STATUS] & 0b11));
        switch (val) {
            0 => return .h_blank,
            1 => return .v_blank,
            2 => return .oam_search,
            3 => return .lcd_operate,
        }
    }

    pub fn deinit(self: *Screen) void {
        self.window.destroy();
        self.renderer.destroy();
        sdl.quit();
    }

    fn is_lcd_enabled(self: *Screen) bool {
        const lcd_contr = self.get_cpu().mem.read_at(0xFF40);
        return lcd_contr & 0b10000000 != 0;
    }

    fn get_ly(self: *Screen) u8 {
        return self.get_cpu().mem.data[0xFF44];
    }

    fn reset_ly(self: *Screen) void {
        self.get_cpu().mem.data[0xFF44] = 0;
    }

    fn inc_ly(self: *Screen) void {
        if (self.get_ly() >= 154) {
            self.get_cpu().mem.data[0xFF44] = 0;
        } else {
            self.get_cpu().mem.data[0xFF44] += 1;
        }
    }

    fn get_lcdc(self: *Screen) u8 {
        return self.get_cpu().mem.read_at(LCDC);
    }

    fn get_tile_select_mode(self: *Screen) TILE_SELECT_MODE {
        const lcdc = self.get_lcdc();
        const bit: u1 = @truncate((lcdc >> 4) & 0b1);

        return @enumFromInt(bit);
    }
};

const TILE_SELECT_MODE = enum(u1) {
    UNSIGNED = 1,
    SIGNED = 0,
};

pub const PPU_MODE = enum(u2) {
    /// Searching for overlapping objects
    oam_search = 2,
    /// Sending pixels
    lcd_operate = 3,
    /// Waiting for next line (hblank)
    h_blank = 0,
    /// Waiting for next frame (vblank)
    v_blank = 1,
};

const Pixel = struct {
    color: u2,
    bg_priority: u1,
    palette: u8,
};

const Tile = struct {
    colors: [64]PixelColor,
    id: u8,

    pub fn empty() Tile {
        return .{ .colors = [_]PixelColor{.black} ** 64, .id = 0 };
    }

    pub fn from_mem(data: []u8, index: u8, palette: u8) Tile {
        var colors = [_]PixelColor{.black} ** 64;
        for (0..8) |i| {
            const low_byte = data[i * 2];
            const high_byte = data[(i * 2) + 1];
            for (0..8) |j| {
                const bit_index: u3 = @truncate(7 - j);
                const low_bit = (low_byte >> bit_index) & 0b1;
                const high_bit = (high_byte >> bit_index) & 0b1;
                const color_id: u2 = @truncate((high_bit << 1) | low_bit);
                const color = PixelColor.from_palette(color_id, palette);
                colors[j + (i * 8)] = color;
            }
        }

        return .{ .colors = colors, .id = index };
    }

    pub fn draw_tile(self: *Tile, renderer: *sdl.Renderer, x: u16, y: u16) !void {
        for (0..8) |i| {
            for (0..8) |j| {
                switch (self.colors[i + j * 8]) {
                    .black => try renderer.setDrawColorRGB(0, 0, 0),
                    .dark_grey => try renderer.setDrawColorRGB(64, 64, 64),
                    .light_grey => try renderer.setDrawColorRGB(128, 128, 128),
                    .white => try renderer.setDrawColorRGB(255, 255, 255),
                }
                try renderer.drawRectF(.{ .x = @floatFromInt((i + x * 8) * 2), .y = @floatFromInt((j + y * 8) * 2), .h = 2, .w = 2 });
                // try renderer.drawPoint(@intCast((i + x * 8) * 4), @intCast((j + y * 8) * 4));
            }
        }
    }
};

const PixelColor = enum(u2) {
    black = 0b11,
    dark_grey = 0b10,
    light_grey = 0b01,
    white = 0b00,

    pub fn from_palette(value: u2, palette: u8) PixelColor {
        switch (value) {
            0 => {
                const color: u2 = @truncate(palette & 0b11);
                return @enumFromInt(color);
            },
            1 => {
                const color: u2 = @truncate((palette & 0b1100) >> 2);
                return @enumFromInt(color);
            },
            2 => {
                const color: u2 = @truncate((palette & 0b110000) >> 4);
                return @enumFromInt(color);
            },
            3 => {
                const color: u2 = @truncate((palette & 0b11000000) >> 6);
                return @enumFromInt(color);
            },
        }
    }
};
