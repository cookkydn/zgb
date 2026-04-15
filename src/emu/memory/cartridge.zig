const MBCType = @import("../hardware.zig").MBCType;
const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Cartridge = struct {
    title: *const [15]u8,
    mbc_type: MBCType,
    allocator: Allocator,
    filename: []const u8,
    rom: []u8,
    rom_bank: u5 = 1,
    ram: []u8,
    ram_bank: u5 = 1,
    mode: u1 = 0,

    pub fn from_file(filename: []const u8, allocator: Allocator) error{ CartridgeNotFound, CartridgeTooBig }!Cartridge {
        const file = std.fs.cwd().openFile(filename, .{
            .mode = .read_only,
        }) catch return error.CartridgeNotFound;
        defer file.close();
        const content = std.fs.cwd().readFileAlloc(allocator, filename, 0x400000) catch |err| {
            if (err == error.FileTooBig) return error.CartridgeTooBig;
            return error.CartridgeNotFound;
        };
        // Assert that the rom has the minimal size
        std.debug.assert(content.len >= 0x8000);

        const title = content[0x0134..0x0143];
        const mbc_type = MBCType.from_byte(content[0x147]);
        // unused
        // const rom_size: usize = 32768 * (1 << content[0x148]);
        const ram_size: usize = switch (content[0x149]) {
            0x00 => 0,
            0x02 => 0x2000,
            0x03 => 0x8000,
            0x04 => 0x20000,
            0x05 => 0x10000,
            else => blk: {
                std.debug.print("Warning: unknown ram size header {}", .{content[0x149]});
                break :blk 0;
            },
        };
        const rom: []u8 = content;
        const ram: []u8 = allocator.alloc(u8, ram_size) catch @panic("Out of memory");
        @memset(ram, 0);

        return Cartridge{
            .title = title,
            .mbc_type = mbc_type,
            .allocator = allocator,
            .filename = filename,
            .rom = rom,
            .ram = ram,
        };
    }

    pub fn deinit(self: Cartridge) void {
        self.allocator.free(self.rom);
        self.allocator.free(self.ram);
    }
};
