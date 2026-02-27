const std = @import("std");
const CPU = @import("../cpu.zig").CPU;

const V_BLANK_MASK: u8 = 0b00000001;
const STAT_MASK: u8 = 0b00000010;
const TIMER_MASK: u8 = 0b00000100;
const SERIAL_MASK: u8 = 0b00001000;
const JOYPAD_MASK: u8 = 0b00010000;

const V_BLANK_SRC = 0x40;
const STAT_SRC = 0x48;
const TIMER_SRC = 0x50;
const SERIAL_SRC = 0x58;
const JOYPAD_SRC = 0x60;

const IE = 0xFFFF;
const IF = 0xFF0F;

/// The Interupt master enable flag
const IME_STATE = enum {
    ENABLED,
    DISABLED,
    /// Meaning will be enabled next iteration
    ENABLED_NEXT,
};

pub const Interrupts = struct {
    ime: IME_STATE,
    halted: bool,
    pub fn init() Interrupts {
        return .{
            .ime = .DISABLED,
            .halted = false,
        };
    }

    pub fn get_cpu(self: *Interrupts) *CPU {
        return @alignCast(@fieldParentPtr("int", self));
    }

    pub fn get_ie(self: *Interrupts) u8 {
        return self.get_cpu().mem.read_at(IE);
    }

    pub fn get_if(self: *Interrupts) u8 {
        return self.get_cpu().mem.read_at(IF);
    }

    pub fn handle_interrupts(self: *Interrupts) void {
        if (self.get_ie() & self.get_if() != 0) {
            self.halted = false;
        }

        if (self.ime == .DISABLED) return;
        if (self.ime == .ENABLED_NEXT) {
            self.ime = .ENABLED;
            return;
        }
        if (self.get_if() & self.get_ie() & V_BLANK_MASK != 0) {
            std.debug.print("\n===VBLANK===\n", .{});
            self.ime = .DISABLED;
            self.get_cpu().mem.data[IF] &= ~V_BLANK_MASK;
            self.get_cpu().clock.tick_emu(2);
            self.get_cpu().execute_instruction(.{ .call_imm16 = .{ .imm16 = V_BLANK_SRC } });
        } else if (self.get_if() & self.get_ie() & STAT_MASK != 0) {
            std.debug.print("\n===STAT===\n", .{});
            self.ime = .DISABLED;
            self.get_cpu().mem.data[IF] &= ~STAT_MASK;
            self.get_cpu().clock.tick_emu(2);
            self.get_cpu().execute_instruction(.{ .call_imm16 = .{ .imm16 = STAT_SRC } });
        } else if (self.get_if() & self.get_ie() & TIMER_MASK != 0) {
            std.debug.print("\n===TIMER===\n", .{});
            self.ime = .DISABLED;
            self.get_cpu().mem.data[IF] &= ~TIMER_MASK;
            self.get_cpu().clock.tick_emu(2);
            self.get_cpu().execute_instruction(.{ .call_imm16 = .{ .imm16 = TIMER_SRC } });
        } else if (self.get_if() & self.get_ie() & SERIAL_MASK != 0) {
            std.debug.print("\n===SERIAL===\n", .{});
            self.ime = .DISABLED;
            self.get_cpu().mem.data[IF] &= ~SERIAL_MASK;
            self.get_cpu().clock.tick_emu(2);
            self.get_cpu().execute_instruction(.{ .call_imm16 = .{ .imm16 = SERIAL_SRC } });
        } else if (self.get_if() & self.get_ie() & JOYPAD_MASK != 0) {
            std.debug.print("\n===JOYPAD===\n", .{});
            self.ime = .DISABLED;
            self.get_cpu().mem.data[IF] &= ~JOYPAD_MASK;
            self.get_cpu().clock.tick_emu(2);
            self.get_cpu().execute_instruction(.{ .call_imm16 = .{ .imm16 = JOYPAD_SRC } });
        }
        return;
    }

    pub fn request_vblank(self: *Interrupts) void {
        self.get_cpu().mem.data[IF] |= 0b00000001;
    }

    pub fn request_stat(self: *Interrupts) void {
        self.get_cpu().mem.data[IF] |= 0b00000010;
    }

    pub fn request_timer(self: *Interrupts) void {
        self.get_cpu().mem.data[IF] |= 0b00000100;
    }

    pub fn request_serial(self: *Interrupts) void {
        self.get_cpu().mem.data[IF] |= 0b00001000;
    }

    pub fn request_joypad(self: *Interrupts) void {
        self.get_cpu().mem.data[IF] |= 0b00010000;
    }
};
