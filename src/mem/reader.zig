const Memory = @import("../mem.zig").Memory;

pub const Reader = struct {
    seek: *u16,
    mem: *Memory,

    pub fn init(mem: *Memory) Reader {
        return .{ .seek = undefined, .mem = mem };
    }

    pub fn read_u8(self: *Reader) u8 {
        const byte = self.mem.read_at(self.seek.*);
        self.seek.* +%= 1;
        return byte;
    }

    pub fn read_i8(self: *Reader) i8 {
        return @bitCast(self.read_u8());
    }

    pub fn read_u16(self: *Reader) u16 {
        const lsb = @as(u16, self.read_u8());
        const msb = @as(u16, self.read_u8());
        const imm16 = (msb << 8) | lsb;
        return imm16;
    }
};
