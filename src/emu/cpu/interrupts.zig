const CPU = @import("cpu.zig").Cpu;

const v_blank_src = 0x40;
const stat_src = 0x48;
const timer_src = 0x50;
const serial_src = 0x58;
const joypad_src = 0x60;

const v_blank_mask: u8 = 0b00000001;
const stat_mask: u8 = 0b00000010;
const timer_mask: u8 = 0b00000100;
const serial_mask: u8 = 0b00001000;
const joypad_mask: u8 = 0b00010000;

pub const Interrupts = struct {
    /// Interrupt flag (`0xFF0F`)
    ///
    /// When an interrupt request handling it set the corresponding bit in this register
    /// ```text
    /// +------+-------+-------------------+
    /// | Bits | Access| Interrup          |
    /// +------+-------+-------------------+
    /// | 7..5 |   R   | Unused (always 1) |
    /// | 4    |  R/W  | Joypad            |
    /// | 3    |  R/W  | Serial            |
    /// | 2    |  R/W  | Timer             |
    /// | 1    |  R/W  | LCD               |
    /// | 0    |  R/W  | Vblank            |
    /// +------+-------+-------------------+
    /// ```
    if_reg: u8 = 0xE0,

    /// Interrupt enable (`0xFFFF`)
    ///
    /// Controls whether an interrupt can be called by the cpu
    /// ```text
    /// +------+-------+-------------------+
    /// | Bits | Access| Interrup          |
    /// +------+-------+-------------------+
    /// | 7..5 |   R   | Unused (always 1) |
    /// | 4    |  R/W  | Joypad            |
    /// | 3    |  R/W  | Serial            |
    /// | 2    |  R/W  | Timer             |
    /// | 1    |  R/W  | LCD               |
    /// | 0    |  R/W  | Vblank            |
    /// +------+-------+-------------------+
    /// ```
    ie_reg: u8 = 0xE0,

    pub fn getCpu(self: *Interrupts) *CPU {
        return @alignCast(@fieldParentPtr("int", self));
    }

    pub fn handleInterrupts(self: *Interrupts) u16 {
        var cpu = self.getCpu();
        const ime = &cpu.state.ime;

        const int_mask = self.ie_reg & self.if_reg & 0x1F;
        if (int_mask != 0) cpu.state.halted = false;

        if (ime.* == .DISABLED) return 0;
        if (ime.* == .ENABLED_NEXT) {
            ime.* = .ENABLED;
            return 0;
        }

        if (int_mask == 0) return 0;
        var cycles: u16 = 8;
        ime.* = .DISABLED;

        if (int_mask & v_blank_mask != 0) {
            self.if_reg &= ~v_blank_mask;
            cycles += cpu.execute_instruction(.{ .call_imm16 = .{ .imm16 = v_blank_src } });
        } else if (int_mask & stat_mask != 0) {
            self.if_reg &= ~stat_mask;
            cycles += cpu.execute_instruction(.{ .call_imm16 = .{ .imm16 = stat_src } });
        } else if (int_mask & timer_mask != 0) {
            self.if_reg &= ~timer_mask;
            cycles += cpu.execute_instruction(.{ .call_imm16 = .{ .imm16 = timer_src } });
        } else if (int_mask & serial_mask != 0) {
            self.if_reg &= ~serial_mask;
            cycles += cpu.execute_instruction(.{ .call_imm16 = .{ .imm16 = serial_src } });
        } else if (int_mask & joypad_mask != 0) {
            self.if_reg &= ~joypad_mask;
            cycles += cpu.execute_instruction(.{ .call_imm16 = .{ .imm16 = joypad_src } });
        }
        return cycles;
    }

    pub fn request_vblank(self: *Interrupts) void {
        self.if_reg |= v_blank_mask;
    }

    pub fn request_stat(self: *Interrupts) void {
        self.if_reg |= stat_mask;
    }

    pub fn request_timer(self: *Interrupts) void {
        self.if_reg |= timer_mask;
    }

    pub fn request_serial(self: *Interrupts) void {
        self.if_reg |= serial_mask;
    }

    pub fn request_joypad(self: *Interrupts) void {
        self.if_reg |= joypad_mask;
    }
};
