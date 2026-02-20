const CPU = @import("cpu.zig").CPU;
const Memory = @import("mem.zig").Memory;

pub const Clock = struct {
    i: u32,

    pub fn init() Clock {
        return Clock{
            .i = 0,
        };
    }
    pub fn get_cpu(self: *Clock) *CPU {
        return @fieldParentPtr("clock", self);
    }

    pub fn get_mem(self: *Clock) *Memory {
        return self.get_cpu().mem;
    }

    pub fn reset(self: *Clock) void {
        self.i = 0;
    }

    pub fn tick(self: *Clock, i: comptime_int) void {
        self.i += i;
    }
};
