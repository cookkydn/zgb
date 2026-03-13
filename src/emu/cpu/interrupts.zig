const CPU = @import("cpu.zig").CPU;
const constants = @import("../const.zig");

const V_BLANK_MASK: u8 = 0b00000001;
const STAT_MASK: u8 = 0b00000010;
const TIMER_MASK: u8 = 0b00000100;
const SERIAL_MASK: u8 = 0b00001000;
const JOYPAD_MASK: u8 = 0b00010000;

pub const Interrupts = struct {
    IF: u8 = 0,
    IE: u8 = 0,
    pub fn init() Interrupts {
        return .{};
    }

    pub fn get_cpu(self: *Interrupts) *CPU {
        return @alignCast(@fieldParentPtr("interrupts", self));
    }

    pub fn handle_interrupts(self: *Interrupts) void {
        if (self.IE & self.IF != 0) {
            self.get_cpu().state.halted = false;
        }

        if (self.get_cpu().state.ime == .DISABLED) return;
        if (self.get_cpu().state.ime == .ENABLED_NEXT) {
            self.get_cpu().state.ime = .ENABLED;
            return;
        }
        var cycles: u16 = 0;
        if (self.IF & self.IE & V_BLANK_MASK != 0) {
            self.get_cpu().state.ime = .DISABLED;
            self.IF &= ~V_BLANK_MASK;
            cycles = self.get_cpu().execute_instruction(.{ .call_imm16 = .{ .imm16 = constants.V_BLANK_SRC } });
            cycles += 8;
        } else if (self.IF & self.IE & STAT_MASK != 0) {
            self.get_cpu().state.ime = .DISABLED;
            self.IF &= ~STAT_MASK;
            cycles = self.get_cpu().execute_instruction(.{ .call_imm16 = .{ .imm16 = constants.STAT_SRC } });
            cycles += 8;
        } else if (self.IF & self.IE & TIMER_MASK != 0) {
            self.get_cpu().state.ime = .DISABLED;
            self.IF &= ~TIMER_MASK;
            cycles = self.get_cpu().execute_instruction(.{ .call_imm16 = .{ .imm16 = constants.TIMER_SRC } });
            cycles += 8;
        } else if (self.IF & self.IE & SERIAL_MASK != 0) {
            self.get_cpu().state.ime = .DISABLED;
            self.IF &= ~SERIAL_MASK;
            cycles = self.get_cpu().execute_instruction(.{ .call_imm16 = .{ .imm16 = constants.SERIAL_SRC } });
            cycles += 8;
        } else if (self.IF & self.IE & JOYPAD_MASK != 0) {
            self.get_cpu().state.ime = .DISABLED;
            self.IF &= ~JOYPAD_MASK;
            cycles = self.get_cpu().execute_instruction(.{ .call_imm16 = .{ .imm16 = constants.JOYPAD_SRC } });
            cycles += 8;
        }
        self.get_cpu().bus.timer.tick(cycles);
    }

    pub fn request_vblank(self: *Interrupts) void {
        self.IF |= 0b00000001;
    }

    pub fn request_stat(self: *Interrupts) void {
        self.IF |= 0b00000010;
    }

    pub fn request_timer(self: *Interrupts) void {
        self.IF |= 0b00000100;
    }

    pub fn request_serial(self: *Interrupts) void {
        self.IF |= 0b00001000;
    }

    pub fn request_joypad(self: *Interrupts) void {
        self.IF |= 0b00010000;
    }
};
