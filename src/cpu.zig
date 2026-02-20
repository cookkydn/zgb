const std = @import("std");
const instr_mod = @import("instructions.zig");
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
const Joypad = @import("joypad.zig").Joypad;

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
        const mem = &self.mem;
        self.clock.tick(1);
        switch (instruction) {
            .nop, .breakpoint => return,
            .ld_r16_imm16 => |arg| {
                reg.set_r16(.{ .R16 = arg.r16 }, arg.imm16);
                self.clock.tick(2);
            },
            .ld_r16mem_a => |arg| {
                const r16mem = reg.load_r16(.{ .R16Mem = arg.r16mem });
                mem.write(r16mem, reg.a);
                self.clock.tick(1);
            },
            .ld_a_r16mem => |arg| {
                const r16mem = reg.load_r16(.{ .R16Mem = arg.r16mem });
                reg.a = mem.read_at(r16mem);
                self.clock.tick(1);
            },
            .ld_imm16_sp => |arg| {
                mem.write(arg.imm16, @truncate(reg.sp & 0xFF));
                mem.write(arg.imm16 + 1, @truncate(reg.sp >> 8));
                self.clock.tick(4);
            },
            .inc_r16 => |arg| {
                const r16 = reg.load_r16(.{ .R16 = arg.r16 });
                reg.set_r16(.{ .R16 = arg.r16 }, r16 +% 1);
                self.clock.tick(1);
            },
            .dec_r16 => |arg| {
                const r16 = reg.load_r16(.{ .R16 = arg.r16 });
                reg.set_r16(.{ .R16 = arg.r16 }, r16 -% 1);
                self.clock.tick(1);
            },
            .add_hl_r16 => |arg| {
                const hl = reg.get_hl();
                const r16 = reg.load_r16(.{ .R16 = arg.r16 });
                reg.f.c = is_sum_overflowing(hl, r16);
                reg.f.h = ((hl & 0x0FFF) + (r16 & 0x0FFF)) > 0x0FFF;
                reg.f.n = false;
                reg.set_hl(hl +% r16);
                self.clock.tick(1);
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
                self.clock.tick(1);
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
                self.clock.tick(2);
            },
            .jr_cond_imm8 => |arg| {
                const cc = reg.f.check_cc(arg.cond);
                if (cc) {
                    reg.pc +%= @as(u16, @bitCast(@as(i16, arg.offset)));
                    self.clock.tick(1);
                }
                self.clock.tick(1);
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
                reg.a +%= r8;
                reg.f.z = reg.a == 0;
                reg.f.c = is_sum_overflowing(reg.a, r8);
                reg.f.h = (((reg.a & 0xF) + (r8 & 0xF)) & 0x10) == 0x10;
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
                reg.a -%= r8;
                reg.f.z = reg.a == 0;
                reg.f.c = is_sub_underflowing(reg.a, r8);
                reg.f.h = (((reg.a & 0xF) -% (r8 & 0xF)) & 0x10) == 0x10;
                reg.f.n = true;
            },
            .sbc_a_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                const carry: u8 = if (reg.f.c) 1 else 0;

                reg.a -%= r8 +% carry;
                reg.f.z = reg.a == 0;
                reg.f.c = is_sub_underflowing(reg.a, r8 +% carry);
                reg.f.h = (((reg.a & 0xF) -% (r8 +% carry & 0xF)) & 0x10) == 0x10;
                reg.f.n = true;
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
                reg.a = reg.a +% arg.imm8;
                reg.f.z = reg.a == 0;
                reg.f.h = true;
                reg.f.c = false;
                reg.f.n = false;
                self.clock.tick(1);
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
                self.clock.tick(1);
            },
            .sub_a_imm8 => |arg| {
                reg.f.c = reg.a < arg.imm8;
                reg.a = reg.a -% arg.imm8;
                reg.f.z = reg.a == 0;
                reg.f.h = (reg.a & 0x0F) < (arg.imm8 & 0x0F);
                reg.f.n = true;
                self.clock.tick(1);
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
                self.clock.tick(1);
            },
            .or_a_imm8 => |arg| {
                reg.a |= arg.imm8;
                reg.f.z = reg.a == 0;
                reg.f.c = false;
                reg.f.h = false;
                reg.f.n = false;
                self.clock.tick(1);
            },
            .cp_a_imm8 => |arg| {
                const result: struct { u8, u1 } = @subWithOverflow(reg.a, arg.imm8);
                reg.f.z = result[0] == 0;
                reg.f.n = true;
                reg.f.h = (reg.a & 0x0F) < (arg.imm8 & 0x0F);
                reg.f.c = arg.imm8 > reg.a;
                self.clock.tick(1);
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
                self.int.ime = true;
                const pc = mem.pop();
                reg.pc = pc;
            },
            .jp_cond_imm16 => |arg| {
                const cc = reg.f.check_cc(arg.cond);
                if (cc) {
                    reg.pc = arg.imm16;
                    self.clock.tick(3);
                    return;
                }
                self.clock.tick(2);
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
                reg.a = mem.read_at(arg.imm16);
            },
            .call_cond_imm16 => |arg| {
                const cc = reg.f.check_cc(arg.cond);
                if (cc) {
                    mem.push(reg.pc);
                    reg.pc = arg.imm16;
                    self.clock.tick(5);
                    return;
                }
                self.clock.tick(2);
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
                const off = @as(u16, @bitCast(@as(i16, arg.offset)));
                reg.sp +%= off;
                self.clock.tick(3);
                reg.f.z = false;
                reg.f.n = false;
                reg.f.c = is_sum_overflowing(reg.sp, off);
                reg.f.h = ((reg.sp & 0x0FFF) + (off & 0x0FFF)) > 0x0FFF;
            },
            .ld_hl_sp_plus_imm8 => |arg| {
                const off = @as(u16, @bitCast(@as(i16, arg.offset)));
                reg.set_hl(reg.sp +% off);
                self.clock.tick(2);
                reg.f.z = false;
                reg.f.n = false;
                reg.f.c = is_sum_overflowing(reg.sp, off);
                reg.f.h = ((reg.sp & 0x0FFF) + (off & 0x0FFF)) > 0x0FFF;
            },
            .ld_sp_hl => {
                reg.sp = reg.get_hl();
                self.clock.tick(1);
            },
            .di => {
                self.int.ime = false;
            },
            .ei => {
                self.int.ime_to_set = true;
            },
            .rr_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                reg.f.c = r8 & 0b1 == 1;
                const res = (r8 >> 1) | (r8 & 0b1) << 7;
                reg.set_r8(arg.r8, res);
                reg.f.z = res == 0;
                reg.f.h = false;
                reg.f.n = false;
                self.clock.tick(1);
            },
            .sla_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                const result = @shlWithOverflow(r8, 1);
                reg.set_r8(arg.r8, result[0]);
                reg.f.z = result[0] == 0;
                reg.f.n = false;
                reg.f.h = false;
                reg.f.c = result[1] == 1;
                self.clock.tick(1);
            },
            .swap_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                const result = ((r8 & 0b1111) << 4) | ((r8 & 0b11110000) >> 4);
                reg.set_r8(arg.r8, result);
                reg.f.z = result == 0;
                reg.f.n = false;
                reg.f.c = false;
                reg.f.h = false;
                self.clock.tick(1);
            },
            .srl_r8 => |arg| {
                const r8 = reg.load_r8(arg.r8);
                reg.f.c = r8 & 0b1 == 1;
                const res = r8 >> 1;
                reg.set_r8(arg.r8, res);
                reg.f.z = res == 0;
                reg.f.h = false;
                reg.f.n = false;
                self.clock.tick(1);
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

    /// Same as read instruction but does not increment PC
    pub fn take_instruction(self: *CPU) Instruction {
        const pc = self.registers.pc;
        const instr = self.read_instruction();
        self.registers.pc = pc;
        return instr;
    }

    pub fn read_instruction(self: *CPU) Instruction {
        const byte = self.mem.read();
        switch (byte) {
            0x00 => return .nop,
            0b00000001...0b00111111 => {
                // Block 0
                const byte_part = @as(u3, @truncate(byte & 0b00000111));
                switch (byte_part) {
                    0b100 => {
                        const r8 = R8.read_r8(@truncate((byte & 0b00111000) >> 3));
                        return .{ .inc_r8 = .{ .r8 = r8 } };
                    },
                    0b101 => {
                        const r8 = R8.read_r8(@truncate((byte & 0b00111000) >> 3));
                        return .{ .dec_r8 = .{ .r8 = r8 } };
                    },
                    0b110 => {
                        const imm8 = self.mem.read();
                        const r8 = R8.read_r8(@truncate((byte & 0b00111000) >> 3));
                        return .{ .ld_r8_imm8 = .{ .imm8 = imm8, .r8 = r8 } };
                    },
                    0b111 => {
                        if (byte == 0b00000111) {
                            return .rlca;
                        } else if (byte == 0b00001111) {
                            return .rrca;
                        } else if (byte == 0b00010111) {
                            return .rla;
                        } else if (byte == 0b00011111) {
                            return .rra;
                        } else if (byte == 0b00100111) {
                            return .daa;
                        } else if (byte == 0b00101111) {
                            return .cpl;
                        } else if (byte == 0b00110111) {
                            return .scf;
                        } else if (byte == 0b00111111) {
                            return .ccf;
                        }
                        std.debug.print("Impossible instruction 0b{b:0>8} (0x{x})\n", .{ byte, byte });
                        unreachable;
                    },
                    0b000 => {
                        if (byte == 0b00011000) {
                            const imm8 = @as(i8, @bitCast(self.mem.read()));
                            return .{ .jr_imm8 = .{ .offset = imm8 } };
                        } else if (byte & 0b11100111 == 0b00100000) {
                            const cond = Cond.read_cond(@truncate((byte & 0b00011000) >> 3));
                            const imm8 = @as(i8, @bitCast(self.mem.read()));
                            return .{ .jr_cond_imm8 = .{ .cond = cond, .offset = imm8 } };
                        } else if (byte == 0b00010000) {
                            return .stop;
                        }
                        std.debug.print("Impossible instruction 0b{b:0>8} (0x{x})\n", .{ byte, byte });
                        unreachable;
                    },
                    else => {
                        const nibble = @as(u4, @truncate(byte & 0b00001111));
                        switch (nibble) {
                            0b0001 => {
                                const r16 = R16.read_r16(@truncate((byte & 0b00110000) >> 4));
                                const imm16 = self.mem.read_imm16();
                                return .{ .ld_r16_imm16 = .{ .imm16 = imm16, .r16 = r16 } };
                            },
                            0b0010 => {
                                const r16mem = R16Mem.read_r16mem(@truncate((byte & 0b00110000) >> 4));
                                return .{ .ld_r16mem_a = .{ .r16mem = r16mem } };
                            },
                            0b1000 => {
                                const imm16 = self.mem.read_imm16();
                                return .{ .ld_imm16_sp = .{ .imm16 = imm16 } };
                            },
                            0b0011 => {
                                const r16 = R16.read_r16(@truncate((byte & 0b00110000) >> 4));
                                return .{ .inc_r16 = .{ .r16 = r16 } };
                            },
                            0b1010 => {
                                const r16mem = R16Mem.read_r16mem(@truncate((byte & 0b00110000) >> 4));
                                return .{ .ld_a_r16mem = .{ .r16mem = r16mem } };
                            },
                            0b1011 => {
                                const r16 = R16.read_r16(@truncate((byte & 0b00110000) >> 4));
                                return .{ .dec_r16 = .{ .r16 = r16 } };
                            },
                            0b1001 => {
                                const r16 = R16.read_r16(@truncate((byte & 0b00110000) >> 4));
                                return .{ .add_hl_r16 = .{ .r16 = r16 } };
                            },
                            else => {
                                std.debug.print("Impossible instruction 0b{b:0>8} (0x{x})\n", .{ byte, byte });
                                unreachable;
                            },
                        }
                    },
                }
            },
            0b01000000...0b01111111 => {
                const r8_dst = R8.read_r8(@truncate((byte & 0b00111000) >> 3));
                const r8_src = R8.read_r8(@truncate(byte & 0b00000111));
                if (r8_src == .b and r8_dst == .b) {
                    return .breakpoint;
                }
                if (r8_dst == .hl and r8_src == .hl) {
                    return .halt;
                }
                return .{ .ld_r8_r8 = .{ .r8_dst = r8_dst, .r8_src = r8_src } };
            },
            0b10000000...0b10000111 => {
                const r8 = R8.read_r8(@truncate(byte & 0b00000111));
                return .{ .add_a_r8 = .{ .r8 = r8 } };
            },
            0b10001000...0b10001111 => {
                const r8 = R8.read_r8(@truncate(byte & 0b00000111));
                return .{ .adc_a_r8 = .{ .r8 = r8 } };
            },
            0b10010000...0b10010111 => {
                const r8 = R8.read_r8(@truncate(byte & 0b00000111));
                return .{ .sub_a_r8 = .{ .r8 = r8 } };
            },
            0b10011000...0b10011111 => {
                const r8 = R8.read_r8(@truncate(byte & 0b00000111));
                return .{ .sbc_a_r8 = .{ .r8 = r8 } };
            },
            0b10100000...0b10100111 => {
                const r8 = R8.read_r8(@truncate(byte & 0b00000111));
                return .{ .and_a_r8 = .{ .r8 = r8 } };
            },
            0b10101000...0b10101111 => {
                const r8 = R8.read_r8(@truncate(byte & 0b00000111));
                return .{ .xor_a_r8 = .{ .r8 = r8 } };
            },
            0b10110000...0b10110111 => {
                const r8 = R8.read_r8(@truncate(byte & 0b00000111));
                return .{ .or_a_r8 = .{ .r8 = r8 } };
            },
            0b10111000...0b10111111 => {
                const r8 = R8.read_r8(@truncate(byte & 0b00000111));
                return .{ .cp_a_r8 = .{ .r8 = r8 } };
            },
            0b11000000...0b11111111 => {
                // Block 3
                // immediate arithmetic
                switch (byte) {
                    0b11000110 => {
                        const imm8 = self.mem.read();
                        return .{ .add_a_imm8 = .{ .imm8 = imm8 } };
                    },
                    0b11001110 => {
                        const imm8 = self.mem.read();
                        return .{ .adc_a_imm8 = .{ .imm8 = imm8 } };
                    },
                    0b11010110 => {
                        const imm8 = self.mem.read();
                        return .{ .sub_a_imm8 = .{ .imm8 = imm8 } };
                    },
                    0b11100110 => {
                        const imm8 = self.mem.read();
                        return .{ .and_a_imm8 = .{ .imm8 = imm8 } };
                    },
                    0b11101110 => {
                        const imm8 = self.mem.read();
                        return .{ .xor_a_imm8 = .{ .imm8 = imm8 } };
                    },
                    0b11110110 => {
                        const imm8 = self.mem.read();
                        return .{ .or_a_imm8 = .{ .imm8 = imm8 } };
                    },
                    0b11111110 => {
                        const imm8 = self.mem.read();
                        return .{ .cp_a_imm8 = .{ .imm8 = imm8 } };
                    },
                    else => {},
                }

                // Jumps, routines, stack, and prefixed
                if (byte & 0b11100111 == 0b11000000) {
                    const cc = Cond.read_cond(@truncate((byte & 0b00110000) >> 4));
                    return .{ .ret_cond = .{ .cond = cc } };
                } else if (byte == 0b11001001) {
                    return .ret;
                } else if (byte == 0b11011001) {
                    return .reti;
                } else if (byte & 0b11100111 == 0b11000010) {
                    const cc = Cond.read_cond(@truncate((byte & 0b00011000) >> 3));
                    const imm16 = self.mem.read_imm16();
                    return .{ .jp_cond_imm16 = .{ .cond = cc, .imm16 = imm16 } };
                } else if (byte == 0b11000011) {
                    const imm16 = self.mem.read_imm16();
                    return .{ .jp_imm16 = .{ .imm16 = imm16 } };
                } else if (byte == 0b11101001) {
                    return .jp_hl;
                } else if (byte & 0b11100111 == 0b11000100) {
                    const cc = Cond.read_cond(@truncate(byte >> 3 & 0b11));
                    const imm16 = self.mem.read_imm16();
                    return .{ .call_cond_imm16 = .{ .cond = cc, .imm16 = imm16 } };
                } else if (byte == 0b11001101) {
                    const imm16 = self.mem.read_imm16();
                    return .{ .call_imm16 = .{ .imm16 = imm16 } };
                } else if (byte & 0b11000111 == 0b11000111) {
                    const target = ((byte & 0b00111000) >> 3) * 8;
                    return .{ .rst_tgt3 = .{ .target_addr = target } };
                } else if (byte & 0b11001111 == 0b11000001) {
                    const r16stk = R16Stk.read_r16(@truncate((byte & 0b00110000) >> 4));
                    return .{ .pop_r16stk = .{ .r16stk = r16stk } };
                } else if (byte & 0b11001111 == 0b11000101) {
                    const r16stk = R16Stk.read_r16(@truncate((byte & 0b00110000) >> 4));
                    return .{ .push_r16stk = .{ .r16stk = r16stk } };
                } else if (byte == 0b11001011) {
                    // Prefix
                    const instr = self.mem.read();
                    if (instr & 0b11000000 == 0) {
                        switch (@as(u5, @truncate((instr & 0b11111000) >> 3))) {
                            0b00000 => {
                                const r8 = R8.read_r8(@truncate(instr & 0b111));
                                return .{ .rlc_r8 = .{ .r8 = r8 } };
                            },
                            0b00001 => {
                                const r8 = R8.read_r8(@truncate(instr & 0b111));
                                return .{ .rrc_r8 = .{ .r8 = r8 } };
                            },
                            0b00010 => {
                                const r8 = R8.read_r8(@truncate(instr & 0b111));
                                return .{ .rl_r8 = .{ .r8 = r8 } };
                            },
                            0b00011 => {
                                const r8 = R8.read_r8(@truncate(instr & 0b111));
                                return .{ .rr_r8 = .{ .r8 = r8 } };
                            },
                            0b00100 => {
                                const r8 = R8.read_r8(@truncate(instr & 0b111));
                                return .{ .sla_r8 = .{ .r8 = r8 } };
                            },
                            0b00101 => {
                                const r8 = R8.read_r8(@truncate(instr & 0b111));
                                return .{ .sra_r8 = .{ .r8 = r8 } };
                            },
                            0b00110 => {
                                const r8 = R8.read_r8(@truncate(instr & 0b111));
                                return .{ .swap_r8 = .{ .r8 = r8 } };
                            },
                            0b00111 => {
                                const r8 = R8.read_r8(@truncate(instr & 0b111));
                                return .{ .srl_r8 = .{ .r8 = r8 } };
                            },
                            else => {
                                std.debug.print("Unkown inner instruction: 0b{b:0>8} (0x{x:0>2})\n", .{ instr, instr });
                                unreachable;
                            },
                        }
                    } else {
                        if (instr & 0b11000000 == 0b01000000) {
                            const b3 = @as(u3, @truncate((instr & 0b00111000) >> 3));
                            const r8 = R8.read_r8(@truncate(instr & 0b00000111));
                            return .{ .bit_b3_r8 = .{ .bit_index = b3, .r8 = r8 } };
                        } else if (instr & 0b11000000 == 0b10000000) {
                            const b3 = @as(u3, @truncate((instr & 0b00111000) >> 3));
                            const r8 = R8.read_r8(@truncate(instr & 0b00000111));
                            return .{ .res_b3_r8 = .{ .bit_index = b3, .r8 = r8 } };
                        } else if (instr & 0b11000000 == 0b11000000) {
                            const b3 = @as(u3, @truncate((instr & 0b00111000) >> 3));
                            const r8 = R8.read_r8(@truncate(instr & 0b00000111));
                            return .{ .set_b3_r8 = .{ .bit_index = b3, .r8 = r8 } };
                        }
                        std.debug.print("Unkown inner instruction: 0b{b:0>8} (0x{x:0>2})\n", .{ instr, instr });
                        unreachable;
                    }
                }

                // Loads and some others
                switch (byte) {
                    0b11100010 => {
                        return .ldh_c_a;
                    },
                    0b11100000 => {
                        const imm8 = self.mem.read();
                        return .{ .ldh_imm8_a = .{ .imm8 = imm8 } };
                    },
                    0b11101010 => {
                        const imm16 = self.mem.read_imm16();
                        return .{ .ld_imm16_a = .{ .imm16 = imm16 } };
                    },
                    0b11110000 => {
                        const imm8 = self.mem.read();
                        return .{ .ldh_a_imm8 = .{ .imm8 = imm8 } };
                    },
                    0b11111010 => {
                        const imm16 = self.mem.read_imm16();
                        return .{ .ld_a_imm16 = .{ .imm16 = imm16 } };
                    },
                    0b11101000 => {
                        const imm8: i8 = @bitCast(self.mem.read());
                        return .{ .add_sp_imm8 = .{ .offset = imm8 } };
                    },
                    0b11111000 => {
                        const imm8: i8 = @bitCast(self.mem.read());
                        return .{ .ld_hl_sp_plus_imm8 = .{ .offset = imm8 } };
                    },
                    0b11111001 => {
                        return .ld_sp_hl;
                    },
                    0b11110011 => {
                        return .di;
                    },
                    0b11111011 => {
                        return .ei;
                    },
                    else => {
                        std.debug.print("Unkown instruction: 0b{b:0>8} (0x{x:0>2})\n", .{ byte, byte });
                        return .invalid;
                    },
                }
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
        switch (r8) {
            .b => {
                self.b = value;
            },
            .c => {
                self.c = value;
            },
            .d => {
                self.d = value;
            },
            .e => {
                self.e = value;
            },
            .h => {
                self.h = value;
            },
            .l => {
                self.l = value;
            },
            .hl => {
                self.get_cpu().mem.write(self.get_hl(), value);
            },
            .a => {
                self.a = value;
            },
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
        print("af: 0x{X:0>4} (0b{b:0>4})\n", .{ self.get_af(), self.f.get_f() >> 4 });
        print("bc: 0x{X:0>4}\n", .{self.get_bc()});
        print("de: 0x{X:0>4}\n", .{self.get_de()});
        print("hl: 0x{X:0>4}\n", .{self.get_hl()});
        print("sp: 0x{X:0>4}\n", .{self.sp});
        print("pc: 0x{X:0>4}\n", .{self.pc});
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

const V_BLANK_MASK = 0b00000001;
const IE = 0xFFFF;
const IF = 0xFF0F;

pub const Interrupts = struct {
    ime: bool,
    ime_to_set: bool,
    halted: bool,
    pub fn init() Interrupts {
        return .{
            .ime = false,
            .ime_to_set = false,
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
        if (!self.ime) return;
        // std.debug.print("Handling interrupts\n", .{});
        if (self.get_if() & V_BLANK_MASK != 0 and self.get_ie() & V_BLANK_MASK != 0) {
            std.debug.print("===VBLANK===\n", .{});
            self.ime = true;
            self.get_cpu().mem.data[IF] &= 0b11111110;
            self.get_cpu().clock.tick(2);
            self.get_cpu().execute_instruction(.{ .call_imm16 = .{ .imm16 = 0x40 } });
        }
        return;
    }

    pub fn request_vblank(self: *Interrupts) void {
        self.get_cpu().mem.data[IF] |= 0b00000001;
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
