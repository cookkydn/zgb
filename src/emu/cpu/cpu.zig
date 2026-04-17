const alu = @import("./arithmetics.zig");
const hardware = @import("../hardware.zig");

const Bus = @import("../memory/bus.zig").Bus;
const Registers = @import("registers.zig").Registers;
const Instruction = @import("./instructions.zig").Instruction;
const CpuState = @import("state.zig").CpuState;
const Interrupts = @import("interrupts.zig").Interrupts;
const Gameboy = @import("../root.zig").Gameboy;

pub const Cpu = struct {
    model: hardware.GbModel,
    reg: Registers = Registers{},
    state: CpuState = CpuState{},
    int: Interrupts = Interrupts{},

    pub fn init(model: hardware.GbModel) Cpu {
        return .{
            .model = model,
        };
    }

    pub fn execute_instruction(self: *Cpu, instruction: Instruction) u16 {
        var reg = &self.reg;
        var mem = &Gameboy.getGB("cpu", self).bus;

        var cycles: u16 = 4;
        switch (instruction) {
            .nop, .breakpoint => return cycles,
            .ld_r16_imm16 => |arg| {
                reg.setR16(.{ .r16 = arg.r16 }, arg.imm16);
                cycles += 8;
            },
            .ld_r16mem_a => |arg| {
                const r16mem = reg.loadR16(.{ .r16_mem = arg.r16mem });
                mem.write_at(r16mem, reg.a);
                cycles += 4;
            },
            .ld_a_r16mem => |arg| {
                const r16mem = reg.loadR16(.{ .r16_mem = arg.r16mem });
                reg.a = mem.read_at(r16mem);
                cycles += 4;
            },
            .ld_imm16_sp => |arg| {
                mem.write_at(arg.imm16, @truncate(reg.sp & 0xFF));
                mem.write_at(arg.imm16 + 1, @truncate(reg.sp >> 8));
                cycles += 16;
            },
            .inc_r16 => |arg| {
                const r16 = reg.loadR16(.{ .r16 = arg.r16 });
                reg.setR16(.{ .r16 = arg.r16 }, r16 +% 1);
                cycles += 4;
            },
            .dec_r16 => |arg| {
                const r16 = reg.loadR16(.{ .r16 = arg.r16 });
                reg.setR16(.{ .r16 = arg.r16 }, r16 -% 1);
                cycles += 4;
            },
            .add_hl_r16 => |arg| {
                const hl = reg.getHL();
                const r16 = reg.loadR16(.{ .r16 = arg.r16 });
                reg.f.c = alu.u16AddCarry(hl, r16);
                reg.f.h = ((hl & 0x0FFF) + (r16 & 0x0FFF)) > 0x0FFF;
                reg.f.n = false;
                reg.setHL(hl +% r16);
                cycles += 4;
            },
            .inc_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                const res = r8 +% 1;
                reg.setR8(arg.r8, res);
                reg.f.z = res == 0;
                reg.f.n = false;
                reg.f.h = (((r8 & 0xF) +% 1) & 0x10) == 0x10;
            },
            .dec_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                const res = r8 -% 1;
                reg.setR8(arg.r8, res);
                reg.f.z = res == 0;
                reg.f.n = true;
                reg.f.h = (((r8 & 0xF) -% 1) & 0x10) == 0x10;
            },
            .ld_r8_imm8 => |arg| {
                reg.setR8(arg.r8, arg.imm8);
                cycles += 4;
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
                    if (reg.f.h) adj += 0x6;
                    if (reg.f.c) adj += 0x60;
                    reg.a -%= adj;
                } else {
                    if (reg.f.h or (reg.a & 0xF > 0x9)) adj += 6;

                    if (reg.f.c or reg.a > 0x99) {
                        adj += 0x60;
                        reg.f.c = true;
                    }
                    reg.a +%= adj;
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
                cycles += 8;
            },
            .jr_cond_imm8 => |arg| {
                const cc = reg.f.checkCond(arg.cond);
                if (cc) {
                    reg.pc +%= @as(u16, @bitCast(@as(i16, arg.offset)));
                    cycles += 8;
                }
                cycles += 4;
            },
            .stop => {
                //TODO follow chart
                self.state.stopped = true;
            },
            .ld_r8_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8_src);
                reg.setR8(arg.r8_dst, r8);
            },
            .halt => {
                self.state.halted = true;
                //TODO Halt bug
            },
            .add_a_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                reg.f.h = alu.u8HalfAddCarry(reg.a, r8);
                reg.f.c = alu.u8AddCarry(reg.a, r8);
                reg.a +%= r8;
                reg.f.z = reg.a == 0;
                reg.f.n = false;
                if (arg.r8 == .hl) {
                    cycles += 4;
                }
            },
            .adc_a_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                const carry: u8 = if (reg.f.c) 1 else 0;

                const old_a = reg.a;
                const full_sum = @as(u16, old_a) + @as(u16, r8) + @as(u16, carry);
                reg.a = @truncate(full_sum);

                reg.f.z = reg.a == 0;
                reg.f.n = false;
                reg.f.h = ((old_a & 0xF) + (r8 & 0xF) + carry) > 0xF;
                reg.f.c = full_sum > 0xFF;
                if (arg.r8 == .hl) {
                    cycles += 4;
                }
            },
            .sub_a_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                reg.f.n = true;
                reg.f.c = alu.u8SubCarry(reg.a, r8);
                reg.f.h = alu.u8HalfSubCarry(reg.a, r8);
                reg.a -%= r8;
                reg.f.z = reg.a == 0;
                if (arg.r8 == .hl) {
                    cycles += 4;
                }
            },
            .sbc_a_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                const carry: u8 = if (reg.f.c) 1 else 0;
                const sub = reg.a -% r8;
                reg.f.n = true;
                reg.f.c = alu.u8SubCarry(reg.a, r8) or alu.u8SubCarry(sub, carry);
                reg.f.h = alu.u8HalfSubCarry(reg.a, r8) or alu.u8HalfSubCarry(sub, carry);
                reg.a = reg.a -% r8 -% carry;
                reg.f.z = reg.a == 0;
                if (arg.r8 == .hl) {
                    cycles += 4;
                }
            },
            .and_a_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                reg.a &= r8;
                reg.f.z = reg.a == 0;
                reg.f.c = false;
                reg.f.h = true;
                reg.f.n = false;
                if (arg.r8 == .hl) {
                    cycles += 4;
                }
            },
            .xor_a_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                reg.a ^= r8;
                reg.f.z = reg.a == 0;
                reg.f.c = false;
                reg.f.h = false;
                reg.f.n = false;
                if (arg.r8 == .hl) {
                    cycles += 4;
                }
            },
            .or_a_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                reg.a |= r8;
                reg.f.z = reg.a == 0;
                reg.f.c = false;
                reg.f.h = false;
                reg.f.n = false;
                if (arg.r8 == .hl) {
                    cycles += 4;
                }
            },
            .cp_a_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                reg.f.z = reg.a -% r8 == 0;
                reg.f.n = true;
                reg.f.h = (((reg.a & 0xF) -% (r8 & 0xF)) & 0x10) == 0x10;
                reg.f.c = r8 > reg.a;
                if (arg.r8 == .hl) {
                    cycles += 4;
                }
            },
            .add_a_imm8 => |arg| {
                reg.f.n = false;
                reg.f.h = alu.u8HalfAddCarry(reg.a, arg.imm8);
                reg.f.c = alu.u8AddCarry(reg.a, arg.imm8);
                reg.a = reg.a +% arg.imm8;
                reg.f.z = reg.a == 0;
                cycles += 4;
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
                cycles += 4;
            },
            .sub_a_imm8 => |arg| {
                reg.f.n = true;
                reg.f.c = alu.u8SubCarry(reg.a, arg.imm8);
                reg.f.h = alu.u8HalfSubCarry(reg.a, arg.imm8);
                reg.a -%= arg.imm8;
                reg.f.z = reg.a == 0;
                cycles += 4;
            },
            .sbc_a_imm8 => |arg| {
                const carry: u8 = if (reg.f.c) 1 else 0;
                const sub = reg.a -% arg.imm8;
                reg.f.n = true;
                reg.f.c = alu.u8SubCarry(reg.a, arg.imm8) or alu.u8SubCarry(sub, carry);
                reg.f.h = alu.u8HalfSubCarry(reg.a, arg.imm8) or alu.u8HalfSubCarry(sub, carry);
                reg.a = reg.a -% arg.imm8 -% carry;
                reg.f.z = reg.a == 0;
                cycles += 4;
            },
            .and_a_imm8 => |arg| {
                reg.a &= arg.imm8;
                reg.f.z = reg.a == 0;
                reg.f.c = false;
                reg.f.h = true;
                reg.f.n = false;
                cycles += 4;
            },
            .xor_a_imm8 => |arg| {
                reg.a ^= arg.imm8;
                reg.f.z = reg.a == 0;
                reg.f.c = false;
                reg.f.h = false;
                reg.f.n = false;
                cycles += 4;
            },
            .or_a_imm8 => |arg| {
                reg.a |= arg.imm8;
                reg.f.z = reg.a == 0;
                reg.f.c = false;
                reg.f.h = false;
                reg.f.n = false;
                cycles += 4;
            },
            .cp_a_imm8 => |arg| {
                const result: struct { u8, u1 } = @subWithOverflow(reg.a, arg.imm8);
                reg.f.z = result[0] == 0;
                reg.f.n = true;
                reg.f.h = (reg.a & 0x0F) < (arg.imm8 & 0x0F);
                reg.f.c = arg.imm8 > reg.a;
                cycles += 4;
            },
            .ret_cond => |arg| {
                const cc = reg.f.checkCond(arg.cond);
                if (cc) {
                    const pc = mem.pop();
                    reg.pc = pc;
                    cycles += 16;
                }
                cycles += 4;
            },
            .ret => {
                const pc = mem.pop();
                reg.pc = pc;
                cycles += 12;
            },
            .reti => {
                self.state.ime = .ENABLED;
                const pc = mem.pop();
                reg.pc = pc;
                cycles += 12;
            },
            .jp_cond_imm16 => |arg| {
                const cc = reg.f.checkCond(arg.cond);
                if (cc) {
                    reg.pc = arg.imm16;
                    cycles += 12;
                    return cycles;
                }
                cycles += 8;
            },
            .jp_imm16 => |arg| {
                reg.pc = arg.imm16;
                cycles += 12;
            },
            .jp_hl => {
                reg.pc = reg.getHL();
            },
            .ldh_imm8_a => |arg| {
                mem.write_at(0xFF00 | @as(u16, arg.imm8), reg.a);
                cycles += 4;
            },
            .ld_imm16_a => |arg| {
                mem.write_at(arg.imm16, reg.a);
                cycles += 8;
            },
            .call_cond_imm16 => |arg| {
                const cc = reg.f.checkCond(arg.cond);
                if (cc) {
                    mem.push(reg.pc);
                    reg.pc = arg.imm16;
                    cycles += 20;
                    return cycles;
                }
                cycles += 8;
            },
            .call_imm16 => |arg| {
                mem.push(reg.pc);
                reg.pc = arg.imm16;
                cycles += 20;
            },
            .rst_tgt3 => |arg| {
                mem.push(reg.pc);
                reg.pc = arg.target_addr;
                cycles += 12;
            },
            .pop_r16stk => |arg| {
                const val = mem.pop();
                reg.setR16(.{ .r16_stk = arg.r16stk }, val);
            },
            .push_r16stk => |arg| {
                const r16 = self.reg.loadR16(.{ .r16_stk = arg.r16stk });
                mem.push(r16);
                cycles += 8;
                if (arg.r16stk == .af) {
                    cycles += 4;
                }
            },
            .ldh_c_a => {
                mem.write_at(0xFF00 | @as(u16, reg.c), reg.a);
                cycles += 4;
            },
            .ldh_a_c => {
                reg.a = mem.read_at(0xFF00 | @as(u16, reg.c));
                cycles += 4;
            },
            .ldh_a_imm8 => |arg| {
                reg.a = mem.read_at(0xFF00 | @as(u16, arg.imm8));
                cycles += 8;
            },
            .ld_a_imm16 => |arg| {
                reg.a = mem.read_at(arg.imm16);
                cycles += 12;
            },
            .add_sp_imm8 => |arg| {
                reg.f.c = alu.u8AddCarry(@truncate(reg.sp), @bitCast(arg.offset));
                reg.f.h = alu.u8HalfAddCarry(@truncate(reg.sp), @bitCast(arg.offset));
                reg.sp = alu.offsetBy(reg.sp, arg.offset);
                reg.f.z = false;
                reg.f.n = false;
                cycles += 12;
            },
            .ld_hl_sp_plus_imm8 => |arg| {
                reg.f.c = alu.u8AddCarry(@truncate(reg.sp), @bitCast(arg.offset));
                reg.f.h = alu.u8HalfAddCarry(@truncate(reg.sp), @bitCast(arg.offset));
                reg.setHL(alu.offsetBy(reg.sp, arg.offset));
                reg.f.z = false;
                reg.f.n = false;
                cycles += 8;
            },
            .ld_sp_hl => {
                reg.sp = reg.getHL();
                cycles += 4;
            },
            .di => {
                self.state.ime = .DISABLED;
            },
            .ei => {
                self.state.ime = .ENABLED_NEXT;
            },
            .rlc_r8 => |arg| {
                var r8 = reg.loadR8(arg.r8);
                const result = @shlWithOverflow(r8, 1);

                const bit7: u8 = if (result[1] == 1) 1 else 0;
                r8 = result[0] | bit7;

                reg.f.c = result[1] == 1;
                reg.f.z = r8 == 0;
                reg.f.h = false;
                reg.f.n = false;

                reg.setR8(arg.r8, r8);

                cycles += 4;
                if (arg.r8 == .hl) {
                    cycles += 8;
                }
            },
            .rrc_r8 => |arg| {
                var val = reg.loadR8(arg.r8);

                const carry = val & 1;

                val = (val >> 1) | (carry << 7);

                reg.f.c = carry == 1;
                reg.f.z = val == 0;
                reg.f.h = false;
                reg.f.n = false;

                reg.setR8(arg.r8, val);

                cycles += 4;
                if (arg.r8 == .hl) {
                    cycles += 8;
                }
            },
            .rl_r8 => |arg| {
                const carry: u8 = if (reg.f.c) 1 else 0;
                var r8 = reg.loadR8(arg.r8);
                const result = @shlWithOverflow(r8, 1);
                r8 = result[0] | carry;
                reg.f.c = result[1] == 1;
                reg.f.z = r8 == 0;
                reg.f.h = false;
                reg.f.n = false;
                reg.setR8(arg.r8, r8);
                cycles += 4;
                if (arg.r8 == .hl) {
                    cycles += 8;
                }
            },
            .rr_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                const old_carry: u8 = if (reg.f.c) 1 else 0;
                reg.f.c = r8 & 0b1 == 1;
                const res = (r8 >> 1) | (old_carry << 7);
                reg.setR8(arg.r8, res);
                reg.f.z = res == 0;
                reg.f.h = false;
                reg.f.n = false;
                cycles += 4;
                if (arg.r8 == .hl) {
                    cycles += 8;
                }
            },
            .sla_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                const result = @shlWithOverflow(r8, 1);
                reg.setR8(arg.r8, result[0]);
                reg.f.z = result[0] == 0;
                reg.f.n = false;
                reg.f.h = false;
                reg.f.c = result[1] == 1;
                cycles += 4;
                if (arg.r8 == .hl) {
                    cycles += 8;
                }
            },
            .sra_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                const bit7 = r8 & 0x80;
                const carry = r8 & 1;
                const result = r8 >> 1 | bit7;
                reg.setR8(arg.r8, result);
                reg.f.z = result == 0;
                reg.f.n = false;
                reg.f.h = false;
                reg.f.c = carry == 1;
                cycles += 4;
                if (arg.r8 == .hl) {
                    cycles += 8;
                }
            },
            .swap_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                const result = ((r8 & 0b1111) << 4) | ((r8 & 0b11110000) >> 4);
                reg.setR8(arg.r8, result);
                reg.f.z = result == 0;
                reg.f.n = false;
                reg.f.c = false;
                reg.f.h = false;
                cycles += 4;
                if (arg.r8 == .hl) {
                    cycles += 8;
                }
            },
            .srl_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                reg.f.c = r8 & 0b1 == 1;
                const res = r8 >> 1;
                reg.setR8(arg.r8, res);
                reg.f.z = res == 0;
                reg.f.h = false;
                reg.f.n = false;
                cycles += 4;
                if (arg.r8 == .hl) {
                    cycles += 8;
                }
            },
            .bit_b3_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                reg.f.z = (r8 >> arg.bit_index) & 1 == 0;
                reg.f.n = false;
                reg.f.h = true;
                cycles += 4;
                if (arg.r8 == .hl) {
                    cycles += 4;
                }
            },
            .res_b3_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                reg.setR8(arg.r8, r8 & ~(@as(u8, 1) << arg.bit_index));
                cycles += 4;
                if (arg.r8 == .hl) {
                    cycles += 8;
                }
            },
            .set_b3_r8 => |arg| {
                const r8 = reg.loadR8(arg.r8);
                reg.setR8(arg.r8, r8 | (@as(u8, 1) << arg.bit_index));
                cycles += 4;
                if (arg.r8 == .hl) {
                    cycles += 8;
                }
            },
            .invalid => {
                unreachable;
            },
        }
        return cycles;
    }
};
