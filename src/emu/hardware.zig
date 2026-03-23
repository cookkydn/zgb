pub const GbModel = enum {
    DMG0,

    pub fn getBiosSize(self: GbModel) usize {
        return switch (self) {
            .DMG0 => 0x100,
        };
    }
};

pub const MBCType = enum {
    NoMbc,

    pub fn from_byte(byte: u8) MBCType {
        return switch (byte) {
            0x00 => .NoMbc,
            else => {
                return .NoMbc;
                // @panic("Unhandled cartridge type")
            },
        };
    }

    pub fn getRomSize(self: MBCType) usize {
        return switch (self) {
            .NoMbc => 0x8000,
        };
    }

    pub fn getRamSize(self: MBCType) usize {
        return switch (self) {
            .NoMbc => 0x2000,
        };
    }
};
