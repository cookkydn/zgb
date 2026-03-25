const std = @import("std");
const MBCType = @import("../hardware.zig").MBCType;

pub const Cartridge = struct {
    allocator: std.mem.Allocator,
    mbc_type: MBCType = .None,
    has_ram: bool = false,
    rom: ?[]u8,
    ram: ?[]u8,

    pub fn init(all: std.mem.Allocator) Cartridge {
        return .{
            .allocator = all,
            .rom = null,
            .ram = null,
        };
    }

    pub fn deinit(self: *Cartridge) void {
        if (self.rom) |rom| self.allocator.free(rom);
        if (self.ram) |ram| self.allocator.free(ram);
    }
};
