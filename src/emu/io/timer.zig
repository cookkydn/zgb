const CPU = @import("../cpu/cpu.zig").CPU;
const Bus = @import("../memory/bus.zig").Bus;

pub const Timer = struct {
    div: u8 = 0,
    tima: u8 = 0,
    tma: u8 = 0,
    tac: u8 = 0,

    div_counter: u16 = 0,
    tima_counter: u32 = 0,

    pub fn init() Timer {
        return .{};
    }

    fn get_cpu(self: *Timer) *CPU {
        const bus: *Bus = @alignCast(@fieldParentPtr("timer", self));
        return @alignCast(@fieldParentPtr("bus", bus));
    }

    /// cycles are in T-cycles
    pub fn tick(self: *Timer, cycles: u16) void {
        self.div_counter += cycles;
        while (self.div_counter >= 256) {
            self.div_counter -= 256;
            self.div +%= 1;
        }

        if (self.tac & 0b100 != 0) {
            self.tima_counter += cycles;
            const freq = self.getFrequency();

            while (self.tima_counter >= freq) {
                self.tima_counter -= freq;

                if (self.tima == 255) {
                    self.tima = self.tma;
                    self.get_cpu().interrupts.request_timer();
                } else {
                    self.tima += 1;
                }
            }
        }
    }

    fn getFrequency(self: *Timer) u16 {
        return switch (self.tac & 0b11) {
            0b00 => 1024, // 4096 Hz
            0b01 => 16, // 262144 Hz
            0b10 => 64, // 65536 Hz
            0b11 => 256, // 16384 Hz
            else => unreachable,
        };
    }
};
