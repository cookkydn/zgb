const std = @import("std");

pub const Cartridge = struct {
    rom: ?[]u8,
    ram: ?[]u8,

    pub fn init() Cartridge {
        return .{
            .rom = null,
            .ram = null,
        };
    }

    pub fn deinit(self: *Cartridge, allocator: std.mem.Allocator) void {
        if (self.rom) |rom| allocator.free(rom);
        if (self.ram) |ram| allocator.free(ram);
    }
};
