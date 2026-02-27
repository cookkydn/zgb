const std = @import("std");
const instr_mod = @import("./cpu/instructions.zig");
const Instruction = instr_mod.Instruction;
const R8 = instr_mod.R8;
const R16 = instr_mod.R16;
const R16Stk = instr_mod.R16Stk;
const R16Mem = instr_mod.R16Mem;
const Cond = instr_mod.Cond;
const Memory = @import("mem.zig").Memory;
const Allocator = std.mem.Allocator;
const Clock = @import("clock.zig").Clock;
const Screen = @import("screen.zig").Screen;
const alu = @import("arithmetics.zig");
const Joypad = @import("joypad.zig").Joypad;
const Interrupts = @import("cpu/interrupts.zig").Interrupts;

pub const CPU = struct {
    registers: Registers,
    mem: Memory,
    clock: Clock,
    screen: Screen,
    int: Interrupts,
    quit: bool,
    pause: bool,
    joypad: Joypad,

    pub fn init(all: Allocator) !CPU {
        return .{
            .registers = Registers.init(),
            .mem = Memory.init(all),
            .clock = Clock.init(),
            .screen = try Screen.init(),
            .int = Interrupts.init(),
            .quit = false,
            .pause = false,
            .joypad = Joypad.init(),
        };
    }

    pub fn deinit(self: *CPU, all: Allocator) void {
        self.mem.deinit(all);
        self.screen.deinit();
    }

    pub fn execute_instruction(self: *CPU, instruction: Instruction) void {
        const reg = &self.*.registers;
        const mem = &self.*.mem;
        self.clock.tick_emu(1);
        switch (instruction) {
            .nop, .breakpoint => return,
            .ld_r16_imm16 => |arg| {
                reg.set_r16(.{ .R16 = arg.r16 }, arg.imm16);
                self.clock.tick_emu(2);
            },
            .ld_r16mem_a => |arg| {
                const r16mem = reg.load_r16(.{ .R16Mem = arg.r16mem });
                mem.write(r16mem, reg.a);
                self.clock.tick_emu(1);
            },
            .ld_a_r16mem => |arg| {
                const r16mem = reg.load_r16(.{ .R16Mem = arg.r16mem });
                reg.a = mem.read_at(r16mem);
                self.clock.tick_emu(1);
            },
            .ld_imm16_sp => |arg| {
                mem.write(arg.imm16, @truncate(reg.sp & 0xFF));
                mem.write(arg.imm16 + 1, @truncate(reg.sp >> 8));
                self.clock.tick_emu(4);
            },
            .inc_r16 => |arg| {
                const r16 = reg.load_r16(.{ .R16 = arg.r16 });
                reg.set_r16(.{ .R16 = arg.r16 }, r16 +% 1);
                self.clock.tick_emu(1);
            },
            .dec_r16 => |arg| {
                const r16 = reg.load_r16(.{ .R16 = arg.r16 });
                reg.set_r16(.{ .R16 = arg.r16 }, r16 -% 1);
                self.clock.tick_emu(1);
            },
            .add_hl_r16 => |arg| {
                const hl = reg.get_hl();
                const r16 = reg.load_r16(.{ .R16 = arg.r16 });
                reg.f.c = is_sum_overflowing(hl, r16);
                reg.f.h = ((hl & 0x0FFF) + (r16 & 0x0FFF)) > 0x0FFF;
                reg.f.n = false;
                reg.set_hl(hl +% r16);
                self.clock.tick_emu(1);
            },
            .inc_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                const res = r8 +% 1;
                reg.set_r8(arg.r8, res);
                reg.f.z = res == 0;
                reg.f.n = false;
                reg.f.h = (((r8 & 0xF) +% 1) & 0x10) == 0x10;
            },
            .dec_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                const res = r8 -% 1;
                reg.set_r8(arg.r8, res);
                reg.f.z = res == 0;
                reg.f.n = true;
                reg.f.h = (((r8 & 0xF) -% 1) & 0x10) == 0x10;
            },
            .ld_r8_imm8 => |arg| {
                reg.set_r8(arg.r8, arg.imm8);
                self.clock.tick_emu(1);
            },
            .rlca => {
                reg.f.c = reg.a & 0x80 == 0x80;
                reg.a <<= 1;
                if (reg.f.c) reg.a |= 1;
                reg.f.z = false;
                reg.f.h = false;
                reg.f.n = false;
            },
            .rrca => {
                reg.f.c = reg.a & 1 == 1;
                reg.a >>= 1;
                if (reg.f.c) reg.a |= 0x80;
                reg.f.z = false;
                reg.f.h = false;
                reg.f.n = false;
            },
            .rra => {
                const tmp = reg.f.c;
                reg.f.c = reg.a & 1 == 1;
                reg.a = reg.a >> 1;
                if (tmp) reg.a |= 0x80;
                reg.f.z = false;
                reg.f.h = false;
                reg.f.n = false;
            },
            .rla => {
                const tmp = reg.f.c;
                reg.f.c = reg.a & 0x80 == 0x80;
                reg.a = reg.a << 1;
                if (tmp) reg.a |= 1;
                reg.f.z = false;
                reg.f.h = false;
                reg.f.n = false;
            },
            .daa => {
                var adj: u8 = 0;
                if (reg.f.n) {
                    if (reg.f.h) adj += 6;
                    if (reg.f.c) adj += 0x60;
                    reg.a -= adj;
                } else {
                    if (reg.f.h or (reg.a & 0xF > 0x9)) adj += 6;

                    if (reg.f.c or reg.a > 0x99) {
                        adj += 0x60;
                        reg.f.c = true;
                    }
                    reg.a += adj;
                }
                reg.f.h = false;
                reg.f.z = reg.a == 0;
            },
            .cpl => {
                reg.a = ~reg.a;
                reg.f.n = true;
                reg.f.h = true;
            },
            .scf => {
                reg.f.n = false;
                reg.f.h = false;
                reg.f.c = true;
            },
            .ccf => {
                reg.f.n = false;
                reg.f.h = false;
                reg.f.c = !reg.f.c;
            },
            .jr_imm8 => |arg| {
                reg.pc +%= @as(u16, @bitCast(@as(i16, arg.offset)));
                self.clock.tick_emu(2);
            },
            .jr_cond_imm8 => |arg| {
                const cc = reg.f.check_cc(arg.cond);
                if (cc) {
                    reg.pc +%= @as(u16, @bitCast(@as(i16, arg.offset)));
                    self.clock.tick_emu(1);
                }
                self.clock.tick_emu(1);
            },
            .stop => {
                //TODO follow chart
                self.quit = true;
            },
            .ld_r8_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8_src);
                reg.set_r8(arg.r8_dst, r8);
            },
            .halt => {
                self.int.halted = true;
                //TODO Halt bug
            },
            .add_a_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                reg.f.h = alu.u8_half_add_carry(reg.a, r8);
                reg.f.c = alu.u8_add_carry(reg.a, r8);
                reg.a +%= r8;
                reg.f.z = reg.a == 0;
                reg.f.n = false;
            },
            .adc_a_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                const carry: u8 = if (reg.f.c) 1 else 0;

                const old_a = reg.a;
                const full_sum = @as(u16, old_a) + @as(u16, r8) + @as(u16, carry);
                reg.a = @truncate(full_sum);

                reg.f.z = reg.a == 0;
                reg.f.n = false;
                reg.f.h = ((old_a & 0xF) + (r8 & 0xF) + carry) > 0xF;
                reg.f.c = full_sum > 0xFF;
            },
            .sub_a_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                reg.f.n = true;
                reg.f.c = alu.u8_sub_carry(reg.a, r8);
                reg.f.h = alu.u8_half_sub_carry(reg.a, r8);
                reg.a -%= r8;
                reg.f.z = reg.a == 0;
            },
            .sbc_a_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                const carry: u8 = if (reg.f.c) 1 else 0;
                const sub = reg.a -% r8;
                reg.f.z = reg.a == 0;
                reg.f.n = true;
                reg.f.c = alu.u8_sub_carry(reg.a, r8) or alu.u8_sub_carry(sub, carry);
                reg.f.h = alu.u8_half_sub_carry(reg.a, r8) or alu.u8_half_sub_carry(sub, carry);
                reg.a -%= r8 +% carry;
            },
            .and_a_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                reg.a &= r8;
                reg.f.z = reg.a == 0;
                reg.f.c = false;
                reg.f.h = true;
                reg.f.n = false;
            },
            .xor_a_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                reg.a ^= r8;
                reg.f.z = reg.a == 0;
                reg.f.c = false;
                reg.f.h = false;
                reg.f.n = false;
            },
            .or_a_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                reg.a |= r8;
                reg.f.z = reg.a == 0;
                reg.f.c = false;
                reg.f.h = false;
                reg.f.n = false;
            },
            .cp_a_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                reg.f.z = reg.a -% r8 == 0;
                reg.f.n = true;
                reg.f.h = (((reg.a & 0xF) -% (r8 & 0xF)) & 0x10) == 0x10;
                reg.f.c = r8 > reg.a;
            },
            .add_a_imm8 => |arg| {
                reg.f.n = false;
                reg.f.h = alu.u8_half_add_carry(reg.a, arg.imm8);
                reg.f.c = alu.u8_add_carry(reg.a, arg.imm8);
                reg.f.z = reg.a == 0;
                reg.a = reg.a +% arg.imm8;
                self.clock.tick_emu(1);
            },
            .adc_a_imm8 => |arg| {
                const carry: u8 = if (reg.f.c) 1 else 0;

                const old_a = reg.a;
                const full_sum = @as(u16, old_a) + @as(u16, arg.imm8) + @as(u16, carry);
                reg.a = @truncate(full_sum);

                reg.f.z = reg.a == 0;
                reg.f.n = false;
                reg.f.h = ((old_a & 0xF) + (arg.imm8 & 0xF) + carry) > 0xF;
                reg.f.c = full_sum > 0xFF;
                self.clock.tick_emu(1);
            },
            .sub_a_imm8 => |arg| {
                reg.f.n = true;
                reg.f.c = alu.u8_sub_carry(reg.a, arg.imm8);
                reg.f.h = alu.u8_half_sub_carry(reg.a, arg.imm8);
                reg.a -%= arg.imm8;
                reg.f.z = reg.a == 0;
                self.clock.tick_emu(1);
            },
            .sbc_a_imm8 => |arg| {
                const carry: u8 = if (reg.f.c) 1 else 0;
                const sub = reg.a -% arg.imm8;
                reg.f.z = reg.a == 0;
                reg.f.n = true;
                reg.f.c = alu.u8_sub_carry(reg.a, arg.imm8) or alu.u8_sub_carry(sub, carry);
                reg.f.h = alu.u8_half_sub_carry(reg.a, arg.imm8) or alu.u8_half_sub_carry(sub, carry);
                reg.a -%= arg.imm8 +% carry;
            },
            .and_a_imm8 => |arg| {
                reg.a &= arg.imm8;
                reg.f.z = reg.a == 0;
                reg.f.c = false;
                reg.f.h = true;
                reg.f.n = false;
            },
            .xor_a_imm8 => |arg| {
                reg.a ^= arg.imm8;
                reg.f.z = reg.a == 0;
                reg.f.c = false;
                reg.f.h = false;
                reg.f.n = false;
                self.clock.tick_emu(1);
            },
            .or_a_imm8 => |arg| {
                reg.a |= arg.imm8;
                reg.f.z = reg.a == 0;
                reg.f.c = false;
                reg.f.h = false;
                reg.f.n = false;
                self.clock.tick_emu(1);
            },
            .cp_a_imm8 => |arg| {
                const result: struct { u8, u1 } = @subWithOverflow(reg.a, arg.imm8);
                reg.f.z = result[0] == 0;
                reg.f.n = true;
                reg.f.h = (reg.a & 0x0F) < (arg.imm8 & 0x0F);
                reg.f.c = arg.imm8 > reg.a;
                self.clock.tick_emu(1);
            },
            .ret_cond => |arg| {
                const cc = reg.f.check_cc(arg.cond);
                if (cc) {
                    const pc = mem.pop();
                    reg.pc = pc;
                }
            },
            .ret => {
                const pc = mem.pop();
                reg.pc = pc;
            },
            .reti => {
                self.int.ime = .ENABLED;
                const pc = mem.pop();
                reg.pc = pc;
            },
            .jp_cond_imm16 => |arg| {
                const cc = reg.f.check_cc(arg.cond);
                if (cc) {
                    reg.pc = arg.imm16;
                    self.clock.tick_emu(3);
                    return;
                }
                self.clock.tick_emu(2);
            },
            .jp_imm16 => |arg| {
                //std.debug.print("Jumping to 0x{x:0>4}\n", .{arg.imm16});
                reg.pc = arg.imm16;
            },
            .jp_hl => {
                reg.pc = reg.get_hl();
            },
            .ldh_imm8_a => |arg| {
                mem.write(0xFF00 | @as(u16, arg.imm8), reg.a);
            },
            .ld_imm16_a => |arg| {
                mem.write(arg.imm16, reg.a);
                self.clock.tick_emu(3);
            },
            .call_cond_imm16 => |arg| {
                const cc = reg.f.check_cc(arg.cond);
                if (cc) {
                    mem.push(reg.pc);
                    reg.pc = arg.imm16;
                    self.clock.tick_emu(5);
                    return;
                }
                self.clock.tick_emu(2);
            },
            .call_imm16 => |arg| {
                mem.push(reg.pc);
                reg.pc = arg.imm16;
            },
            .rst_tgt3 => |arg| {
                mem.push(reg.pc);
                reg.pc = arg.target_addr;
            },
            .pop_r16stk => |arg| {
                const val = mem.pop();
                reg.set_r16(.{ .R16Stk = arg.r16stk }, val);
            },
            .push_r16stk => |arg| {
                const r16 = self.registers.load_r16(.{ .R16Stk = arg.r16stk });
                mem.push(r16);
            },
            .ldh_c_a => {
                mem.write(0xFF00 | @as(u16, reg.c), reg.a);
            },
            .ldh_a_imm8 => |arg| {
                reg.a = mem.read_at(0xFF00 | @as(u16, arg.imm8));
            },
            .ld_a_imm16 => |arg| {
                reg.a = mem.read_at(arg.imm16);
            },
            .add_sp_imm8 => |arg| {
                reg.sp = alu.offset_by(reg.sp, arg.offset);
                reg.f.z = false;
                reg.f.n = false;
                reg.f.c = alu.u8_add_carry(@truncate(reg.sp), @bitCast(arg.offset));
                reg.f.h = alu.u8_half_add_carry(@truncate(reg.sp), @bitCast(arg.offset));
                self.clock.tick_emu(3);
            },
            .ld_hl_sp_plus_imm8 => |arg| {
                reg.set_hl(alu.offset_by(reg.sp, arg.offset));
                reg.f.z = false;
                reg.f.n = false;
                reg.f.c = alu.u8_add_carry(@truncate(reg.sp), @bitCast(arg.offset));
                reg.f.h = alu.u8_half_add_carry(@truncate(reg.sp), @bitCast(arg.offset));
                self.clock.tick_emu(2);
            },
            .ld_sp_hl => {
                reg.sp = reg.get_hl();
                self.clock.tick_emu(1);
            },
            .di => {
                self.int.ime = .DISABLED;
            },
            .ei => {
                self.int.ime = .ENABLED_NEXT;
            },
            .rr_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                reg.f.c = r8 & 0b1 == 1;
                const res = (r8 >> 1) | ((r8 & 0b1) << 7);
                reg.set_r8(arg.r8, res);
                reg.f.z = res == 0;
                reg.f.h = false;
                reg.f.n = false;
                self.clock.tick_emu(1);
            },
            .sla_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                const result = @shlWithOverflow(r8, 1);
                reg.set_r8(arg.r8, result[0]);
                reg.f.z = result[0] == 0;
                reg.f.n = false;
                reg.f.h = false;
                reg.f.c = result[1] == 1;
                self.clock.tick_emu(1);
            },
            .swap_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                const result = ((r8 & 0b1111) << 4) | ((r8 & 0b11110000) >> 4);
                reg.set_r8(arg.r8, result);
                reg.f.z = result == 0;
                reg.f.n = false;
                reg.f.c = false;
                reg.f.h = false;
                self.clock.tick_emu(1);
            },
            .srl_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                reg.f.c = r8 & 0b1 == 1;
                const res = r8 >> 1;
                reg.set_r8(arg.r8, res);
                reg.f.z = res == 0;
                reg.f.h = false;
                reg.f.n = false;
                self.clock.tick_emu(1);
            },
            .bit_b3_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                reg.f.z = (r8 >> arg.bit_index) & 1 == 0;
                reg.f.n = false;
                reg.f.h = true;
            },
            .res_b3_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                reg.set_r8(arg.r8, r8 & ~(@as(u8, 1) << arg.bit_index));
            },
            .set_b3_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                reg.set_r8(arg.r8, r8 | (@as(u8, 1) << arg.bit_index));
            },
            .invalid => {
                unreachable;
            },
            else => {
                std.debug.print("Unhandled instruction (yet)\n", .{});
                unreachable;
            },
        }
    }
};

const Registers = struct {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: Flags,
    h: u8,
    l: u8,

    sp: u16,
    pc: u16,

    fn get_cpu(self: *Registers) *CPU {
        return @alignCast(@fieldParentPtr("registers", self));
    }

    pub fn set_af(self: *Registers, value: u16) void {
        self.a = @truncate((value & 0xFF00) >> 8);
        self.f.z = value & 0b10000000 != 0;
        self.f.n = value & 0b01000000 != 0;
        self.f.h = value & 0b00100000 != 0;
        self.f.c = value & 0b00010000 != 0;
    }

    pub fn get_af(self: Registers) u16 {
        return @as(u16, self.a) << 8 | self.f.get_f();
    }

    pub fn set_bc(self: *Registers, value: u16) void {
        self.b = @truncate(value >> 8);
        self.c = @truncate(value & 0xFF);
    }

    pub fn get_bc(self: Registers) u16 {
        return @as(u16, self.b) << 8 | @as(u16, self.c);
    }

    pub fn set_de(self: *Registers, value: u16) void {
        self.d = @truncate(value >> 8);
        self.e = @truncate(value & 0xFF);
    }

    pub fn get_de(self: Registers) u16 {
        return @as(u16, self.d) << 8 | @as(u16, self.e);
    }

    pub fn set_hl(self: *Registers, value: u16) void {
        self.h = @truncate(value >> 8);
        self.l = @truncate(value & 0xFF);
    }

    pub fn get_hl(self: Registers) u16 {
        return @as(u16, self.h) << 8 | @as(u16, self.l);
    }

    pub fn clear_flags(self: *Registers) void {
        self.f.c = false;
        self.f.h = false;
        self.f.n = false;
        self.f.z = false;
    }

    pub fn load_r8(self: *Registers, r8: R8) u8 {
        const mem = self.get_cpu().mem;
        switch (r8) {
            .b => {
                return self.b;
            },
            .c => {
                return self.c;
            },
            .d => {
                return self.d;
            },
            .e => {
                return self.e;
            },
            .h => {
                return self.h;
            },
            .l => {
                return self.l;
            },
            .hl => {
                return mem.data[self.get_hl()];
            },
            .a => {
                return self.a;
            },
        }
    }

    pub fn load_r16(self: *Registers, r16: union(enum) { R16: R16, R16Mem: R16Mem, R16Stk: R16Stk }) u16 {
        switch (r16) {
            .R16 => |r16_| {
                switch (r16_) {
                    .bc => return self.get_bc(),
                    .de => return self.get_de(),
                    .hl => return self.get_hl(),
                    .sp => return self.sp,
                }
            },
            .R16Mem => |r16mem| {
                switch (r16mem) {
                    .bc => return self.get_bc(),
                    .de => return self.get_de(),
                    .hli => {
                        self.set_hl(self.get_hl() +% 1);
                        return self.get_hl() -% 1;
                    },
                    .hld => {
                        self.set_hl(self.get_hl() -% 1);
                        return self.get_hl() +% 1;
                    },
                }
            },
            .R16Stk => |r16stk| {
                switch (r16stk) {
                    .bc => return self.get_bc(),
                    .de => return self.get_de(),
                    .hl => return self.get_hl(),
                    .af => return self.get_af(),
                }
            },
        }
    }

    pub fn set_r8(self: *Registers, r8: R8, value: u8) void {
        const mem = &self.get_cpu().*.mem;
        switch (r8) {
            .b => self.b = value,
            .c => self.c = value,
            .d => self.d = value,
            .e => self.e = value,
            .h => self.h = value,
            .l => self.l = value,
            .hl => mem.write(self.get_hl(), value),
            .a => self.a = value,
        }
    }

    pub fn set_r16(self: *Registers, r16: union(enum) { R16: R16, R16Mem: R16Mem, R16Stk: R16Stk }, value: u16) void {
        switch (r16) {
            .R16 => |r16_| {
                switch (r16_) {
                    .bc => self.set_bc(value),
                    .de => self.set_de(value),
                    .hl => self.set_hl(value),
                    .sp => self.sp = value,
                }
            },
            .R16Mem => |r16mem| {
                switch (r16mem) {
                    .bc => self.set_bc(value),
                    .de => self.set_de(value),
                    .hli => self.set_hl(value + 1),
                    .hld => self.set_hl(value - 1),
                }
            },
            .R16Stk => |r16stk| {
                switch (r16stk) {
                    .bc => self.set_bc(value),
                    .de => self.set_de(value),
                    .hl => self.set_hl(value),
                    .af => self.set_af(value),
                }
            },
        }
    }

    pub fn init() Registers {
        return Registers{
            .a = 0x01,
            .b = 0x00,
            .c = 0x13,
            .d = 0x00,
            .e = 0xD8,
            .h = 0x01,
            .l = 0x4D,
            .f = .{ .z = true, .c = true, .h = true, .n = true },
            .sp = 0xFFFE,
            .pc = 0x0100,
        };
    }

    pub fn dump(self: *Registers) void {
        const print = std.debug.print;
        print("af: 0x{x:0>4} (0b{b:0>4})\n", .{ self.get_af(), self.f.get_f() >> 4 });
        print("bc: 0x{x:0>4}\n", .{self.get_bc()});
        print("de: 0x{x:0>4}\n", .{self.get_de()});
        print("hl: 0x{x:0>4}\n", .{self.get_hl()});
        print("sp: 0x{x:0>4}\n", .{self.sp});
        print("pc: 0x{x:0>4}\n", .{self.pc});
    }
};

const Flags = struct {
    z: bool,
    n: bool,
    h: bool,
    c: bool,
    pub fn get_f(self: Flags) u16 {
        const z = @as(u8, @intFromBool(self.z)) << 7;
        const n = @as(u8, @intFromBool(self.n)) << 6;
        const h = @as(u8, @intFromBool(self.h)) << 5;
        const c = @as(u8, @intFromBool(self.c)) << 4;
        return @as(u16, z | n | h | c);
    }

    pub fn check_cc(self: Flags, cc: Cond) bool {
        switch (cc) {
            .nz => return !self.z,
            .z => return self.z,
            .nc => return !self.c,
            .c => return self.c,
        }
    }
};

pub fn is_sum_overflowing(a: anytype, b: anytype) bool {
    return @addWithOverflow(a, b)[1] == 1;
}

pub fn is_sub_underflowing(a: anytype, b: anytype) bool {
    return @subWithOverflow(a, b)[1] == 1;
}

pub fn add_u8_hc(a: u8, b: u8) bool {
    return (((a & 0xF) + (b & 0xF)) & 0x10) == 0x10;
}
