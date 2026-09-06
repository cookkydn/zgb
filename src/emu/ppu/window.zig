const Ppu = @import("./ppu.zig").Ppu;
const tile = @import("./tile.zig");
const std = @import("std");

pub const Window = @This();

y_cond: bool = false,
counter: u8 = 7,
debug_was_window_enabled: bool = false,

fn getPpu(self: *Window) *Ppu {
    return @alignCast(@fieldParentPtr("window", self));
}

pub fn isWindowEnabled(self: *Window) bool {
    return self.getPpu().lcdc & 0x10 > 0;
}

fn getWindowTileMapArea(self: *Window) u16 {
    return if (self.getPpu().lcdc & 0x40 > 0) 0x9C00 else 0x9800;
}

pub fn newLine(self: *Window) void {
    if (self.isWindowEnabled() and !self.debug_was_window_enabled) {
        std.log.debug("Window: enabled", .{});
        self.debug_was_window_enabled = true;
    } else if (!self.isWindowEnabled() and self.debug_was_window_enabled) {
        std.log.debug("Window: disabled", .{});
        self.debug_was_window_enabled = false;
    }
    if (!self.isWindowEnabled()) return;
    const mem = self.getPpu();
    if (mem.wy == mem.ly) {
        self.y_cond = true;
    }
    self.counter = 7;
}

pub fn getPixelColorAt(self: *Window, screen_x: u16, screen_y: u16) ?u2 {
    if (!self.isWindowEnabled()) return null;
    defer self.counter += 1;
    // if (wx == self.counter and self.y_cond) {
    //     todo reset background rendering ??
    // }

    if (self.y_cond) {
        const tile_y: u16 = screen_y / 8;
        const offset_y: u16 = screen_y % 8;

        const tile_x: u16 = screen_x / 8;
        const offset_x: u16 = screen_x % 8;

        // TILE

        const tile_index_addr = self.getWindowTileMapArea() + (tile_y * 32) + tile_x;
        const tile_index = self.getPpu().read_vram(tile_index_addr);
        const addressing = Ppu.AddressingMode.getAddressingMode(self.getPpu().lcdc);

        const tile_addr: u16 = switch (addressing) {
            .UNSIGNED => 0x8000 + (@as(u16, tile_index) << 4),
            .SIGNED => 0x8800 + (@as(u16, tile_index ^ 0x80) << 4),
        };

        const pixel_data = tile.getPixelAt(self.getPpu(), tile_addr, offset_x, offset_y);
        return pixel_data;
    }
    return null;
}
