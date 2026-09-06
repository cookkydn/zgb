pub const Cartridge = @This();

const MBCType = @import("../hardware.zig").MBCType;
const std = @import("std");
const Allocator = std.mem.Allocator;

title: *const [15]u8,
mbc_type: MBCType,
allocator: Allocator,
filename: [:0]const u8,
rom: []u8,
rom_bank: u5 = 1,
ram: []u8,
ram_bank: u5 = 1,
mode: u1 = 0,

pub fn fromFile(filename: []const u8, allocator: Allocator, io: std.Io) error{ CartridgeNotFound, CartridgeTooBig }!Cartridge {
    std.log.info("Opening \"{s}\"", .{filename});
    const file = std.Io.Dir.cwd().openFile(io, filename, .{
        .mode = .read_only,
    }) catch return error.CartridgeNotFound;
    defer file.close(io);
    const content = std.Io.Dir.cwd().readFileAlloc(io, filename, allocator, .limited(0x400000)) catch |err| {
        if (err == error.FileTooBig) return error.CartridgeTooBig;
        return error.CartridgeNotFound;
    };
    // Assert that the rom has the minimal size
    std.debug.assert(content.len >= 0x8000);

    const title = content[0x0134..0x0143];
    const CGB_flag = content[0x0143];
    const mbc_type = MBCType.fromByte(content[0x147]);
    // unused
    const rom_size: usize = 32768 * ((@as(usize, 1)) << @as(u6, @truncate(content[0x148])));
    const ram_size: usize = switch (content[0x149]) {
        0x00 => 0,
        0x02 => 0x2000,
        0x03 => 0x8000,
        0x04 => 0x20000,
        0x05 => 0x10000,
        else => blk: {
            std.log.warn("Warning: unknown ram size header {}", .{content[0x149]});
            break :blk 0;
        },
    };
    const rom: []u8 = content;
    const ram: []u8 = allocator.alloc(u8, ram_size) catch @panic("Out of memory");
    const filename_copy = allocator.dupeZ(u8, filename) catch unreachable;
    @memset(ram, 0);
    std.log.info("Cartridge info\n\ttitle: {s}\n\tmbc_type: {s}\n\tCGB_flag 0x{x}\n\trom_size: {d} ({d} banks)\n\tram_size: {d}", .{
        title,
        @tagName(mbc_type),
        CGB_flag,
        rom_size,
        rom_size / 32768,
        ram_size,
    });
    return Cartridge{
        .title = title,
        .mbc_type = mbc_type,
        .allocator = allocator,
        .filename = filename_copy,
        .rom = rom,
        .ram = ram,
    };
}

pub fn deinit(self: Cartridge) void {
    self.allocator.free(self.rom);
    self.allocator.free(self.ram);
    self.allocator.free(self.filename);
}
