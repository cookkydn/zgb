const hardware = @import("../hardware.zig");
const alu = @import("./arithmetics.zig");
const std = @import("std");

const Bus = @import("../memory/bus.zig").Bus;
const Registers = @import("registers.zig").Registers;
const Instruction = @import("./instructions.zig").Instruction;
const CpuState = @import("state.zig").CpuState;
const Interrupts = @import("interrupts.zig").Interrupts;

pub const CPU = struct {
    model: hardware.GbModel,
    bus: Bus,
    registers: Registers = Registers.init(),
    state: CpuState = CpuState.init(),
    interrupts: Interrupts = Interrupts.init(),

    pub fn init(model: hardware.GbModel) CPU {
        var bus = Bus.init();
        bus.loadBios() catch |err| {
            std.debug.print("Failed to load bios: {s}\n", .{@errorName(err)});
            @panic("");
        };
        return .{
            .model = model,
            .bus = bus,
        };
    }

    pub fn deinit(self: *CPU) void {
        self.bus.deinit();
    }

    pub fn execute_instruction(self: *CPU, instruction: Instruction) u16 {
        const reg = &self.*.registers;
        const mem = &self.bus;
        const cycles = 4;
        switch (instruction) {
            .nop, .breakpoint => return cycles,
            .ld_r16_imm16 => |arg| {
                reg.set_r16(.{ .R16 = arg.r16 }, arg.imm16);
                // self.clock.tick_emu(2);
            },
            .ld_r16mem_a => |arg| {
                const r16mem = reg.load_r16(.{ .R16Mem = arg.r16mem });
                mem.write_at(r16mem, reg.a);
                // self.clock.tick_emu(1);
            },
            .ld_a_r16mem => |arg| {
                const r16mem = reg.load_r16(.{ .R16Mem = arg.r16mem });
                reg.a = mem.read_at(r16mem);
                // self.clock.tick_emu(1);
            },
            .ld_imm16_sp => |arg| {
                mem.write_at(arg.imm16, @truncate(reg.sp & 0xFF));
                mem.write_at(arg.imm16 + 1, @truncate(reg.sp >> 8));
                // self.clock.tick_emu(4);
            },
            .inc_r16 => |arg| {
                const r16 = reg.load_r16(.{ .R16 = arg.r16 });
                reg.set_r16(.{ .R16 = arg.r16 }, r16 +% 1);
                // self.clock.tick_emu(1);
            },
            .dec_r16 => |arg| {
                const r16 = reg.load_r16(.{ .R16 = arg.r16 });
                reg.set_r16(.{ .R16 = arg.r16 }, r16 -% 1);
                // self.clock.tick_emu(1);
            },
            .add_hl_r16 => |arg| {
                const hl = reg.get_hl();
                const r16 = reg.load_r16(.{ .R16 = arg.r16 });
                reg.f.c = alu.u16_add_carry(hl, r16);
                reg.f.h = ((hl & 0x0FFF) + (r16 & 0x0FFF)) > 0x0FFF;
                reg.f.n = false;
                reg.set_hl(hl +% r16);
                // self.clock.tick_emu(1);
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
                // self.clock.tick_emu(1);
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
                // self.clock.tick_emu(2);
            },
            .jr_cond_imm8 => |arg| {
                const cc = reg.f.check_cc(arg.cond);
                if (cc) {
                    reg.pc +%= @as(u16, @bitCast(@as(i16, arg.offset)));
                    // self.clock.tick_emu(1);
                }
                // self.clock.tick_emu(1);
            },
            .stop => {
                //TODO follow chart
                self.state.stopped = true;
            },
            .ld_r8_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8_src);
                reg.set_r8(arg.r8_dst, r8);
            },
            .halt => {
                self.state.halted = true;
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
                // self.clock.tick_emu(1);
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
                // self.clock.tick_emu(1);
            },
            .sub_a_imm8 => |arg| {
                reg.f.n = true;
                reg.f.c = alu.u8_sub_carry(reg.a, arg.imm8);
                reg.f.h = alu.u8_half_sub_carry(reg.a, arg.imm8);
                reg.a -%= arg.imm8;
                reg.f.z = reg.a == 0;
                // self.clock.tick_emu(1);
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
                // self.clock.tick_emu(1);
            },
            .or_a_imm8 => |arg| {
                reg.a |= arg.imm8;
                reg.f.z = reg.a == 0;
                reg.f.c = false;
                reg.f.h = false;
                reg.f.n = false;
                // self.clock.tick_emu(1);
            },
            .cp_a_imm8 => |arg| {
                const result: struct { u8, u1 } = @subWithOverflow(reg.a, arg.imm8);
                reg.f.z = result[0] == 0;
                reg.f.n = true;
                reg.f.h = (reg.a & 0x0F) < (arg.imm8 & 0x0F);
                reg.f.c = arg.imm8 > reg.a;
                // self.clock.tick_emu(1);
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
                self.state.ime = .ENABLED;
                const pc = mem.pop();
                reg.pc = pc;
            },
            .jp_cond_imm16 => |arg| {
                const cc = reg.f.check_cc(arg.cond);
                if (cc) {
                    reg.pc = arg.imm16;
                    // self.clock.tick_emu(3);
                    return cycles;
                }
                // self.clock.tick_emu(2);
            },
            .jp_imm16 => |arg| {
                //std.debug.print("Jumping to 0x{x:0>4}\n", .{arg.imm16});
                reg.pc = arg.imm16;
            },
            .jp_hl => {
                reg.pc = reg.get_hl();
            },
            .ldh_imm8_a => |arg| {
                mem.write_at(0xFF00 | @as(u16, arg.imm8), reg.a);
            },
            .ld_imm16_a => |arg| {
                mem.write_at(arg.imm16, reg.a);
                // self.clock.tick_emu(3);
            },
            .call_cond_imm16 => |arg| {
                const cc = reg.f.check_cc(arg.cond);
                if (cc) {
                    mem.push(reg.pc);
                    reg.pc = arg.imm16;
                    // self.clock.tick_emu(5);
                    return cycles;
                }
                // self.clock.tick_emu(2);
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
                mem.write_at(0xFF00 | @as(u16, reg.c), reg.a);
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
                // self.clock.tick_emu(3);
            },
            .ld_hl_sp_plus_imm8 => |arg| {
                reg.set_hl(alu.offset_by(reg.sp, arg.offset));
                reg.f.z = false;
                reg.f.n = false;
                reg.f.c = alu.u8_add_carry(@truncate(reg.sp), @bitCast(arg.offset));
                reg.f.h = alu.u8_half_add_carry(@truncate(reg.sp), @bitCast(arg.offset));
                // self.clock.tick_emu(2);
            },
            .ld_sp_hl => {
                reg.sp = reg.get_hl();
                // self.clock.tick_emu(1);
            },
            .di => {
                self.state.ime = .DISABLED;
            },
            .ei => {
                self.state.ime = .ENABLED_NEXT;
            },
            .rl_r8 => |arg| {
                const carry: u8 = if (reg.f.c) 1 else 0;
                var r8 = reg.load_r8(arg.r8);
                const result = @shlWithOverflow(r8, 1);
                r8 = result[0] | carry;
                reg.f.c = result[1] == 1;
                reg.f.z = r8 == 0;
                reg.f.h = false;
                reg.f.n = false;
                reg.set_r8(arg.r8, r8);
            },
            .rr_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                reg.f.c = r8 & 0b1 == 1;
                const res = (r8 >> 1) | ((r8 & 0b1) << 7);
                reg.set_r8(arg.r8, res);
                reg.f.z = res == 0;
                reg.f.h = false;
                reg.f.n = false;
                // self.clock.tick_emu(1);
            },
            .sla_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                const result = @shlWithOverflow(r8, 1);
                reg.set_r8(arg.r8, result[0]);
                reg.f.z = result[0] == 0;
                reg.f.n = false;
                reg.f.h = false;
                reg.f.c = result[1] == 1;
                // self.clock.tick_emu(1);
            },
            .swap_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                const result = ((r8 & 0b1111) << 4) | ((r8 & 0b11110000) >> 4);
                reg.set_r8(arg.r8, result);
                reg.f.z = result == 0;
                reg.f.n = false;
                reg.f.c = false;
                reg.f.h = false;
                // self.clock.tick_emu(1);
            },
            .srl_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                reg.f.c = r8 & 0b1 == 1;
                const res = r8 >> 1;
                reg.set_r8(arg.r8, res);
                reg.f.z = res == 0;
                reg.f.h = false;
                reg.f.n = false;
                // self.clock.tick_emu(1);
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
        return cycles;
    }
};
