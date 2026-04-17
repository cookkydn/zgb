const Gameboy = @import("../root.zig").Gameboy;

pub const Timer = struct {
    /// Divider (`0xFF04`)
    ///
    /// Incremented at a rate of 16384Hz, writing to this registers resets it to 0.
    /// It is also reset and paused on `stop` instruction
    div: u8 = 0,

    /// Timer counter (`0xFF05`)
    ///
    /// Incremented at the speed defined by `tac` (`0xFF07`).
    /// When overflowing it reset to the value specified by `tma` (`0xFF06`),
    /// and it triggers the timer interrupt
    tima: u8 = 0,

    /// Timer modulo (`0xFF06`)
    ///
    /// When tima overflow it is reset to the value specified by this registers
    tma: u8 = 0,

    /// Timer Control (`0xFF07`)
    ///
    /// This registers controls the timer module, enabling or disabling it and selecting the timer frequency
    ///
    /// ```text
    /// +------+-------+--------------+----------------------+
    /// | Bits |Access | Nom          | Desc.                |
    /// +------+-------+--------------+----------------------+
    /// | 7..3 |   R   | Unused       | Always 1             |
    /// | 2    |  R/W  | Timer Enable | 0 = Off, 1 = On      |
    /// | 1..0 |  R/W  | Input Clock  | Select the frequency |
    /// +------+-------+--------------+----------------------+
    /// ```
    ///
    /// **Input Clock frequencies:**
    /// - `00` : CPU Clock / 1024 (4096 Hz)
    /// - `01` : CPU Clock / 16   (262144 Hz)
    /// - `10` : CPU Clock / 64   (65536 Hz)
    /// - `11` : CPU Clock / 256  (16384 Hz)
    tac: u8 = 0,

    /// Internal counter, have double the precision of the div counter
    div_counter: u16 = 0,
    /// Internal counter, have four times the precision of the tima counter
    tima_counter: u32 = 0,

    /// Updates the internal state of the console's Timer.
    ///
    /// **Parameters:**
    /// - `cycles`: The number of CPU cycles (T-cycles) consumed by the last instruction.
    ///
    /// This function should be called after each CPU instruction execution.
    /// It updates the Game Boy's two internal clocks:
    ///
    /// 1. **The DIV (Divider) register:**
    ///     It continuously increments at 16384 Hz (every 256 T-cycles),
    ///     independently of the `tac` register's state.
    ///
    /// 2. **The TIMA (Timer Counter) register:**
    ///     If the Timer is enabled, it increments at the frequency configured by `tac`.
    ///     When it overflow, two things happen:
    ///     - `tima` is reset with the modulo value stored in `tma`.
    ///     - A Timer interrupt is requested.
    pub fn tick(self: *Timer, cycles: u16) void {
        const int = &Gameboy.getGB("timer", self).int;
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
                    int.request_timer();
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
