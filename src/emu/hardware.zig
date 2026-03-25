const std = @import("std");

pub const GbModel = enum {
    DMG0,

    pub fn getBiosSize(self: GbModel) usize {
        return switch (self) {
            .DMG0 => 0x100,
        };
    }
};

pub const MBCType = enum {
    None,
    NoMbc,

    pub fn from_byte(byte: u8) MBCType {
        return switch (byte) {
            0x00 => .NoMbc,
            else => {
                std.debug.print("Warning: Unsuported cartridge type: 0x{x}\n", .{byte});
                return .NoMbc;
            },
        };
    }

    pub fn getRamSize(self: MBCType) usize {
        return switch (self) {
            .NoMbc => 0x2000,
            else => 0,
        };
    }
};
