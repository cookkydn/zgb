const std = @import("std");

pub const Instruction = union(enum) {
    /// ✅ No-operation
    nop,
    /// ✅ Copy imm16 into register r16
    /// `r16 <- imm16`
    ld_r16_imm16: ld_r16_imm16,
    /// ✅ Copy the value in register A into the byte pointed to by r16.
    /// `[r16] <- A`
    ld_r16mem_a: ld_r16mem_a,
    /// ✅ Copy the byte pointed to by r16 into register A.
    /// `A <- [r16]`
    ld_a_r16mem: ld_a_r16mem,
    /// ✅ Copy SP & $FF at address imm16 and SP >> 8 at address imm16 + 1.
    /// `[imm16] <- SP & 0xFF`
    /// `[imm16+1] <- SP >> 8`
    ld_imm16_sp: ld_imm16_sp,
    /// ✅ Increment the value in register r16 by 1.
    /// `r16 <- r16 + 1`
    inc_r16: inc_r16,
    /// ✅ Decrement the value in register r16 by 1.
    /// `r16 <- r16 - 1`
    dec_r16: dec_r16,
    /// ✅ Add the value in r16 to HL.
    /// `HL <- HL + r16`
    add_hl_r16: add_hl_r16,
    /// ✅ Increment the value in register r8 by 1.
    /// `r8 <- r8 + 1`
    inc_r8: inc_r8,
    /// ✅ Decrement the value in register r8 by 1.
    /// `r8 <- r8 - 1`
    dec_r8: dec_r8,
    /// ✅ Copy the value imm8 into register r8.
    /// `r8 <- imm8`
    ld_r8_imm8: ld_r8_imm8,
    /// ✅
    rlca,
    /// ✅
    rrca,
    /// ✅
    rla,
    /// ✅Rotate register A right, through the carry flag.
    rra,
    /// ✅ Decimal Adjust Accumulator.
    ///
    /// Designed to be used after performing an arithmetic instruction (ADD, ADC, SUB, SBC) whose inputs were in Binary-Coded Decimal (BCD), adjusting the result to likewise be in BCD.
    daa,
    /// ✅ Complement accumulator (A = ~A); also called bitwise NOT.
    cpl,
    /// ✅ Set Carry Flag.
    scf,
    /// ✅
    ccf,
    /// ✅ Relative Jump to address n16.
    jr_imm8: jr_imm8,
    /// ✅ Relative Jump to address n16 if the condition is met.
    jr_cond_imm8: jr_cond_imm8,
    /// ...
    stop,
    /// ✅ Copy the value in register on the right into the register on the left.
    ld_r8_r8: ld_r8_r8,
    /// ... Enter CPU low-power consumption mode until an interrupt occurs.
    halt,
    /// ✅ Add the value in r8 to A.
    add_a_r8: add_a_r8,
    /// ✅ Add the value in r8 plus the carry flag to A.
    adc_a_r8: adc_a_r8,
    /// ✅ Subtract the value in r8 from A.
    sub_a_r8: sub_a_r8,
    /// ✅ Subtract the value in r8 and the carry flag from A.
    sbc_a_r8: sbc_a_r8,
    /// Set A to the bitwise AND between the value in r8 and A.
    and_a_r8: and_a_r8,
    /// Set A to the bitwise XOR between the value in r8 and A.
    xor_a_r8: xor_a_r8,
    /// Set A to the bitwise OR between the value in r8 and A.
    or_a_r8: or_a_r8,
    /// ComPare the value in A with the value in r8.
    ///
    /// This subtracts the value in r8 from A and sets flags accordingly, but discards the result.
    cp_a_r8: cp_a_r8,
    /// Add the value imm8 to A.
    /// `A <- A + imm8`
    add_a_imm8: add_a_imm8,
    /// ✅ Add the value n8 plus the carry flag to A
    adc_a_imm8: adc_a_imm8,
    sub_a_imm8: sub_a_imm8,
    sbc_a_imm8: sbc_a_imm8,
    /// Set A to the bitwise AND between the value imm8 and A.
    and_a_imm8: and_a_imm8,
    /// Set A to the bitwise XOR between the value imm8 and A.
    xor_a_imm8: xor_a_imm8,
    /// Set A to the bitwise OR between the value imm8 and A.
    or_a_imm8: or_a_imm8,
    /// Compare the value in A with the value imm8.
    ///
    /// This subtracts the value imm8 from A and sets flags accordingly, but discards the result.
    cp_a_imm8: cp_a_imm8,
    /// Return from subroutine if condition cc is met.
    ret_cond: ret_cond,
    /// Return from subroutine. This is basically a POP PC (if such an instruction existed). See POP r16 for an explanation of how POP works.
    ret,
    /// Return from subroutine and enable interrupts. This is basically equivalent to executing EI then RET, meaning that IME is set right after this instruction.
    reti,
    /// Jump to address imm16 if condition cc is met.
    jp_cond_imm16: jp_cond_imm16,
    /// Jump to address imm16; effectively, copy imm16 into PC.
    /// `PC <- imm16`
    jp_imm16: jp_imm16,
    /// ✅ Jump to address in HL; effectively, copy the value in register HL into PC.
    jp_hl,
    /// Call address n16 if condition cc is met.
    call_cond_imm16: call_cond_imm16,
    /// Call address imm16.
    ///
    /// This pushes the address of the instruction after the CALL on the stack, such that RET can pop it later; then, it executes an implicit JP imm16.
    call_imm16: call_imm16,
    rst_tgt3: rst_tgt3,
    /// Pop register r16 from the stack.
    pop_r16stk: pop_r16stk,
    /// Push register r16 into the stack.
    push_r16stk: push_r16stk,
    /// Copy the value in register A into the byte at address $FF00+C.
    /// `[0xFF00+C] <- A`
    ldh_c_a,
    /// Copy the value in register A into the byte at address n16.
    ///
    /// The destination address n16 is encoded as its 8-bit low byte and assumes a high byte of $FF, so it must be between $FF00 and $FFFF.
    ldh_imm8_a: ldh_imm8_a,
    /// Copy the value in register A into the byte at address imm16.
    /// `A <- [Imm16]`
    ld_imm16_a: ld_imm16_a,
    ldh_a_c,
    /// Copy the byte at address n16 into register A.
    ///
    /// The source address imm16 is encoded as its 8-bit low byte and assumes a high byte of `0xFF`, so it must be between `0xFF00` and `0xFFFF`.
    ldh_a_imm8: ldh_a_imm8,
    /// Copy the byte at address imm16 into register A.
    /// `A <- [imm16]`
    ld_a_imm16: ld_a_imm16,
    /// ✅ Add the signed value e8 to SP.
    add_sp_imm8: add_sp_imm8,
    /// ✅ Add the signed value e8 to SP and copy the result in HL.
    ld_hl_sp_plus_imm8: ld_hl_sp_plus_imm8,
    /// Copy register HL into register SP.
    ld_sp_hl,
    /// Disable Interrupts by clearing the IME flag.
    di,
    /// Enable Interrupts by setting the IME flag.
    ///
    /// The flag is only set after the instruction following EI.
    ei,
    rlc_r8: rlc_r8,
    rrc_r8: rrc_r8,
    rl_r8: rl_r8,
    /// Rotate register r8 right, through the carry flag.
    rr_r8: rr_r8,
    /// Shift Left Arithmetically register r8.
    sla_r8: sla_r8,
    sra_r8: sra_r8,
    /// Swap the upper 4 bits in register r8 and the lower 4 ones.
    swap_r8: swap_r8,
    /// Shift Right Logically register r8.
    srl_r8: srl_r8,
    /// Test bit u3 in register r8, set the zero flag if bit not set.
    bit_b3_r8: bit_b3_r8,
    /// Set bit u3 in register r8 to 0.
    res_b3_r8: res_b3_r8,
    /// Set bit u3 in register r8 to 1.
    set_b3_r8: set_b3_r8,
    invalid,
    breakpoint,

    pub fn format(
        self: Instruction,
        writer: anytype,
    ) !void {
        try writer.print("{s}", .{@tagName(self)});
        switch (self) {
            inline else => |instr_value| {
                const T = @TypeOf(instr_value);
                if (T == void) {
                    return;
                } else {
                    try writer.print(" {{", .{});
                    const fields = std.meta.fields(T);
                    inline for (fields, 0..) |f, i| {
                        const comma = if (i > 0) ", " else "";
                        const val = @field(instr_value, f.name);
                        const val_type = @FieldType(T, f.name);
                        switch (@typeInfo(val_type)) {
                            .int => {
                                if (@bitSizeOf(@TypeOf(val)) == 8) {
                                    try writer.print("{s}.{s} = 0x{x:0>2}", .{ comma, f.name, val });
                                } else {
                                    try writer.print("{s}.{s} = 0x{x:0>4}", .{ comma, f.name, val });
                                }
                            },
                            .@"enum" => {
                                try writer.print("{s}.{s} = {s}", .{ comma, f.name, @tagName(val) });
                            },
                            else => {
                                try writer.print("{s}.{s} = {any}", .{ comma, f.name, val });
                            },
                        }
                    }
                }
                try writer.writeAll(" }");
            },
        }
    }
};

pub const R8 = enum {
    b,
    c,
    d,
    e,
    h,
    l,
    hl,
    a,
    pub fn read_r8(value: u3) R8 {
        switch (value) {
            0 => return .b,
            1 => return .c,
            2 => return .d,
            3 => return .e,
            4 => return .h,
            5 => return .l,
            6 => return .hl,
            7 => return .a,
        }
    }
};

pub const R16 = enum {
    bc,
    de,
    hl,
    sp,
    pub fn read_r16(value: u2) R16 {
        switch (value) {
            0 => return .bc,
            1 => return .de,
            2 => return .hl,
            3 => return .sp,
        }
    }
};

pub const R16Stk = enum {
    bc,
    de,
    hl,
    af,
    pub fn read_r16(value: u2) R16Stk {
        switch (value) {
            0 => return .bc,
            1 => return .de,
            2 => return .hl,
            3 => return .af,
        }
    }
};

pub const R16Mem = enum {
    bc,
    de,
    hli,
    hld,
    pub fn read_r16mem(value: u2) R16Mem {
        switch (value) {
            0 => return .bc,
            1 => return .de,
            2 => return .hli,
            3 => return .hld,
        }
    }
};

pub const Cond = enum {
    nz,
    z,
    nc,
    c,
    pub fn read_cond(value: u2) Cond {
        switch (value) {
            0 => return .nz,
            1 => return .z,
            2 => return .nc,
            3 => return .c,
        }
    }
};

const generic_imm_u8 = struct {
    imm8: u8,
};

const generic_imm_i8 = struct {
    offset: i8,
};

const generic_imm_u16 = struct {
    imm16: u16,
};

const generic_r8 = struct {
    r8: R8,
};

const generic_r16 = struct {
    r16: R16,
};

const generic_r16stk = struct {
    r16stk: R16Stk,
};

const generic_r16mem = struct {
    r16mem: R16Mem,
};

const generic_b3_r8 = struct {
    bit_index: u3,
    r8: R8,
};

pub const generic_cond_imm16 = struct {
    cond: Cond,
    imm16: u16,
};

pub const ld_r16_imm16 = struct {
    r16: R16,
    imm16: u16,
};

pub const ld_r16mem_a = generic_r16mem;
pub const ld_a_r16mem = generic_r16mem;

pub const ld_imm16_sp = generic_imm_u16;

pub const inc_r16 = generic_r16;
pub const dec_r16 = generic_r16;
pub const add_hl_r16 = generic_r16;

pub const inc_r8 = generic_r8;
pub const dec_r8 = generic_r8;

pub const ld_r8_imm8 = struct {
    r8: R8,
    imm8: u8,
};

pub const jr_imm8 = generic_imm_i8;

pub const jr_cond_imm8 = struct {
    cond: Cond,
    offset: i8,
};

pub const ld_r8_r8 = struct {
    r8_dst: R8,
    r8_src: R8,
};

pub const add_a_r8 = generic_r8;
pub const adc_a_r8 = generic_r8;
pub const sub_a_r8 = generic_r8;
pub const sbc_a_r8 = generic_r8;
pub const and_a_r8 = generic_r8;
pub const xor_a_r8 = generic_r8;
pub const or_a_r8 = generic_r8;
pub const cp_a_r8 = generic_r8;

pub const add_a_imm8 = generic_imm_u8;
pub const adc_a_imm8 = generic_imm_u8;
pub const sub_a_imm8 = generic_imm_u8;
pub const sbc_a_imm8 = generic_imm_u8;
pub const and_a_imm8 = generic_imm_u8;
pub const xor_a_imm8 = generic_imm_u8;
pub const or_a_imm8 = generic_imm_u8;
pub const cp_a_imm8 = generic_imm_u8;

pub const ret_cond = struct {
    cond: Cond,
};

pub const jp_cond_imm16 = generic_cond_imm16;

pub const jp_imm16 = generic_imm_u16;

pub const call_cond_imm16 = generic_cond_imm16;

pub const call_imm16 = generic_imm_u16;

pub const rst_tgt3 = struct {
    target_addr: u8,
};

pub const pop_r16stk = generic_r16stk;
pub const push_r16stk = generic_r16stk;

pub const ldh_imm8_a = generic_imm_u8;
pub const ld_imm16_a = generic_imm_u16;
pub const ldh_a_imm8 = generic_imm_u8;
pub const ld_a_imm16 = generic_imm_u16;

pub const add_sp_imm8 = generic_imm_i8;
pub const ld_hl_sp_plus_imm8 = generic_imm_i8;

pub const rlc_r8 = generic_r8;
pub const rrc_r8 = generic_r8;
pub const rl_r8 = generic_r8;
pub const rr_r8 = generic_r8;
pub const sla_r8 = generic_r8;
pub const sra_r8 = generic_r8;
pub const swap_r8 = generic_r8;
pub const srl_r8 = generic_r8;

pub const bit_b3_r8 = generic_b3_r8;
pub const res_b3_r8 = generic_b3_r8;
pub const set_b3_r8 = generic_b3_r8;
