const std = @import("std");

pub const GbModel = enum {
    DMG0,

    pub fn bios_size(self: GbModel) usize {
        return switch (self) {
            .DMG0 => 0x100,
        };
    }

    pub fn vram_size(self: @This()) usize {
        return switch (self) {
            .DMG0 => 0x2000,
        };
    }
};

pub const MBCType = enum {
    NOMBC,
    MBC1,
    Other,

    pub fn from_byte(byte: u8) MBCType {
        return switch (byte) {
            0x00 => .NOMBC,
            0x01 => .MBC1,
            else => {
                std.debug.print("Warning: Unsuported cartridge type: 0x{x}\n", .{byte});
                return .Other;
            },
        };
    }
};
