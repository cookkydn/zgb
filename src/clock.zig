const CPU = @import("cpu.zig").CPU;
const Memory = @import("mem.zig").Memory;

pub const Clock = struct {
    i: u32,
    j: u32,

    pub fn init() Clock {
        return Clock{
            .i = 0x18,
            .j = 0x18,
        };
    }
    pub fn get_cpu(self: *Clock) *CPU {
        return @alignCast(@fieldParentPtr("clock", self));
    }

    pub fn get_mem(self: *Clock) *Memory {
        return self.get_cpu().mem;
    }

    pub fn reset(self: *Clock) void {
        self.i = 0;
    }

    pub fn get_div(self: *Clock) u8 {
        return self.get_cpu().mem.data[0xFF04];
    }

    pub fn set_div(self: *Clock, value: u8) void {
        self.get_cpu().mem.data[0xFF04] = value;
    }

    pub fn tick_emu(self: *Clock, i: comptime_int) void {
        self.i += i;
        self.j += i;
    }

    pub fn tick_clock(self: *Clock) void {
        if (self.j < 64) return;
        self.set_div(self.get_div() +% @as(u8, @truncate(self.j / 64)));
        self.j %= 64;
    }
};
