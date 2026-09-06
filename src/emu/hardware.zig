const std = @import("std");

pub const GbModel = enum {
    dmg_0,

    pub fn biosSize(self: GbModel) usize {
        return switch (self) {
            .dmg_0 => 0x100,
        };
    }

    pub fn vramSize(self: @This()) usize {
        return switch (self) {
            .dmg_0 => 0x2000,
        };
    }
};

pub const MBCType = enum {
    no_mbc,
    mbc_1,
    mbc_1_with_ram,
    mbc_1_with_ram_and_battery,
    other,

    pub fn fromByte(byte: u8) MBCType {
        return switch (byte) {
            0x00 => .no_mbc,
            0x01 => .mbc_1,
            0x02 => .mbc_1_with_ram,
            0x03 => .mbc_1_with_ram_and_battery,
            else => {
                std.log.warn("Warning: Unsuported cartridge type: 0x{x}\n", .{byte});
                return .other;
            },
        };
    }
};
