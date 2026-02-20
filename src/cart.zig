const Memory = @import("mem.zig").Memory;
const std = @import("std");
pub fn load_cart(comptime path: []const u8, mem: *Memory) void {
    const file = std.fs.cwd().openFile(path, .{ .mode = .read_only }) catch @panic("File error !");
    defer file.close();
    var reader = std.fs.File.reader(file, &.{});
    const result = reader.interface.readSliceAll(mem.data);
    if (result == error.ReadFailed) {
        @panic("Failed to read message");
    }
}

pub fn set_initial_hard_reg(mem: *Memory) void {
    mem.data[0xFF00] = 0xCF;
    mem.data[0xFF01] = 0x00;
    mem.data[0xFF02] = 0x7E;
    mem.data[0xFF04] = 0x18;
    mem.data[0xFF05] = 0x00;
    mem.data[0xFF06] = 0x00;
    mem.data[0xFF07] = 0xF8;
    mem.data[0xFF0F] = 0xE1;
    // TODO Sound registers
    mem.data[0xFF40] = 0x91;
    mem.data[0xFF41] = 0x81;
    mem.data[0xFF42] = 0x00;
    mem.data[0xFF43] = 0x00;
    mem.data[0xFF44] = 0x91;
    mem.data[0xFF45] = 0x00;
    mem.data[0xFF46] = 0xFF;
    mem.data[0xFF47] = 0xFC;
    mem.data[0xFFFF] = 0x00;
}
