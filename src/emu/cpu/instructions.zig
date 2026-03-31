const std = @import("std");
const Bus = @import("../memory/bus.zig").Bus;

pub const Instruction = union(enum) {
    /// No-operation
    nop,
    /// Copy imm16 into register r16
    /// `r16 <- imm16`
    ld_r16_imm16: ld_r16_imm16,
    /// Copy the value in register A into the byte pointed to by r16.
    /// `[r16] <- A`
    ld_r16mem_a: ld_r16mem_a,
    /// Copy the byte pointed to by r16 into register A.
    /// `A <- [r16]`
    ld_a_r16mem: ld_a_r16mem,
    /// Copy SP & $FF at address imm16 and SP >> 8 at address imm16 + 1.
    /// `[imm16] <- SP & 0xFF`
    /// `[imm16+1] <- SP >> 8`
    ld_imm16_sp: ld_imm16_sp,
    /// Increment the value in register r16 by 1.
    /// `r16 <- r16 + 1`
    inc_r16: inc_r16,
    /// Decrement the value in register r16 by 1.
    /// `r16 <- r16 - 1`
    dec_r16: dec_r16,
    /// Add the value in r16 to HL.
    /// `HL <- HL + r16`
    add_hl_r16: add_hl_r16,
    /// Increment the value in register r8 by 1.
    /// `r8 <- r8 + 1`
    inc_r8: inc_r8,
    /// Decrement the value in register r8 by 1.
    /// `r8 <- r8 - 1`
    dec_r8: dec_r8,
    /// Copy the value imm8 into register r8.
    /// `r8 <- imm8`
    ld_r8_imm8: ld_r8_imm8,
    ///
    rlca,
    ///
    rrca,
    ///
    rla,
    /// Rotate register A right, through the carry flag.
    rra,
    /// Decimal Adjust Accumulator.
    ///
    /// Designed to be used after performing an arithmetic instruction (ADD, ADC, SUB, SBC) whose inputs were in Binary-Coded Decimal (BCD), adjusting the result to likewise be in BCD.
    daa,
    /// Complement accumulator (A = ~A); also called bitwise NOT.
    cpl,
    /// Set Carry Flag.
    scf,
    ///
    ccf,
    /// Relative Jump to address n16.
    jr_imm8: jr_imm8,
    /// Relative Jump to address n16 if the condition is met.
    jr_cond_imm8: jr_cond_imm8,
    /// ...
    stop,
    /// Copy the value in register on the right into the register on the left.
    ld_r8_r8: ld_r8_r8,
    /// ... Enter CPU low-power consumption mode until an interrupt occurs.
    halt,
    /// Add the value in r8 to A.
    add_a_r8: add_a_r8,
    /// Add the value in r8 plus the carry flag to A.
    adc_a_r8: adc_a_r8,
    /// Subtract the value in r8 from A.
    sub_a_r8: sub_a_r8,
    /// Subtract the value in r8 and the carry flag from A.
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
    /// Add the value n8 plus the carry flag to A
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
    /// Jump to address in HL; effectively, copy the value in register HL into PC.
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
    /// Add the signed value e8 to SP.
    add_sp_imm8: add_sp_imm8,
    /// Add the signed value e8 to SP and copy the result in HL.
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
                            .int => |int_info| {
                                if (int_info.signedness == .signed) {
                                    try writer.print("{s}.{s} = {}", .{ comma, f.name, val });
                                } else {
                                    if (@bitSizeOf(@TypeOf(val)) == 8) {
                                        try writer.print("{s}.{s} = 0x{x:0>2}", .{ comma, f.name, val });
                                    } else {
                                        try writer.print("{s}.{s} = 0x{x:0>4}", .{ comma, f.name, val });
                                    }
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

    pub fn from_bus(bus: *Bus) Instruction {
        switch (bus.read_u8()) {
            // --- 0x00 to 0x0F ---
            0x00 => return .nop,
            0x01 => return .{ .ld_r16_imm16 = .{ .r16 = .bc, .imm16 = bus.read_u16() } },
            0x02 => return .{ .ld_r16mem_a = .{ .r16mem = .bc } },
            0x03 => return .{ .inc_r16 = .{ .r16 = .bc } },
            0x04 => return .{ .inc_r8 = .{ .r8 = .b } },
            0x05 => return .{ .dec_r8 = .{ .r8 = .b } },
            0x06 => return .{ .ld_r8_imm8 = .{ .r8 = .b, .imm8 = bus.read_u8() } },
            0x07 => return .rlca,
            0x08 => return .{ .ld_imm16_sp = .{ .imm16 = bus.read_u16() } },
            0x09 => return .{ .add_hl_r16 = .{ .r16 = .bc } },
            0x0A => return .{ .ld_a_r16mem = .{ .r16mem = .bc } },
            0x0B => return .{ .dec_r16 = .{ .r16 = .bc } },
            0x0C => return .{ .inc_r8 = .{ .r8 = .c } },
            0x0D => return .{ .dec_r8 = .{ .r8 = .c } },
            0x0E => return .{ .ld_r8_imm8 = .{ .r8 = .c, .imm8 = bus.read_u8() } },
            0x0F => return .rrca,
            // --- 0x10 to 0x1F ---
            0x10 => {
                _ = bus.read_u8();
                return .stop;
            },
            0x11 => return .{ .ld_r16_imm16 = .{ .r16 = .de, .imm16 = bus.read_u16() } },
            0x12 => return .{ .ld_r16mem_a = .{ .r16mem = .de } },
            0x13 => return .{ .inc_r16 = .{ .r16 = .de } },
            0x14 => return .{ .inc_r8 = .{ .r8 = .d } },
            0x15 => return .{ .dec_r8 = .{ .r8 = .d } },
            0x16 => return .{ .ld_r8_imm8 = .{ .r8 = .d, .imm8 = bus.read_u8() } },
            0x17 => return .rla,
            0x18 => return .{ .jr_imm8 = .{ .offset = bus.read_i8() } },
            0x19 => return .{ .add_hl_r16 = .{ .r16 = .de } },
            0x1A => return .{ .ld_a_r16mem = .{ .r16mem = .de } },
            0x1B => return .{ .dec_r16 = .{ .r16 = .de } },
            0x1C => return .{ .inc_r8 = .{ .r8 = .e } },
            0x1D => return .{ .dec_r8 = .{ .r8 = .e } },
            0x1E => return .{ .ld_r8_imm8 = .{ .r8 = .e, .imm8 = bus.read_u8() } },
            0x1F => return .rra,
            // --- 0x20 to 0x2F ---
            0x20 => return .{ .jr_cond_imm8 = .{ .cond = .nz, .offset = bus.read_i8() } },
            0x21 => return .{ .ld_r16_imm16 = .{ .r16 = .hl, .imm16 = bus.read_u16() } },
            0x22 => return .{ .ld_r16mem_a = .{ .r16mem = .hli } },
            0x23 => return .{ .inc_r16 = .{ .r16 = .hl } },
            0x24 => return .{ .inc_r8 = .{ .r8 = .h } },
            0x25 => return .{ .dec_r8 = .{ .r8 = .h } },
            0x26 => return .{ .ld_r8_imm8 = .{ .r8 = .h, .imm8 = bus.read_u8() } },
            0x27 => return .daa,
            0x28 => return .{ .jr_cond_imm8 = .{ .cond = .z, .offset = bus.read_i8() } },
            0x29 => return .{ .add_hl_r16 = .{ .r16 = .hl } },
            0x2A => return .{ .ld_a_r16mem = .{ .r16mem = .hli } },
            0x2B => return .{ .dec_r16 = .{ .r16 = .hl } },
            0x2C => return .{ .inc_r8 = .{ .r8 = .l } },
            0x2D => return .{ .dec_r8 = .{ .r8 = .l } },
            0x2E => return .{ .ld_r8_imm8 = .{ .r8 = .l, .imm8 = bus.read_u8() } },
            0x2F => return .cpl,
            // --- 0x30 to 0x3F ---
            0x30 => return .{ .jr_cond_imm8 = .{ .cond = .nc, .offset = bus.read_i8() } },
            0x31 => return .{ .ld_r16_imm16 = .{ .r16 = .sp, .imm16 = bus.read_u16() } },
            0x32 => return .{ .ld_r16mem_a = .{ .r16mem = .hld } },
            0x33 => return .{ .inc_r16 = .{ .r16 = .sp } },
            0x34 => return .{ .inc_r8 = .{ .r8 = .hl } },
            0x35 => return .{ .dec_r8 = .{ .r8 = .hl } },
            0x36 => return .{ .ld_r8_imm8 = .{ .r8 = .hl, .imm8 = bus.read_u8() } },
            0x37 => return .scf,
            0x38 => return .{ .jr_cond_imm8 = .{ .cond = .c, .offset = bus.read_i8() } },
            0x39 => return .{ .add_hl_r16 = .{ .r16 = .sp } },
            0x3A => return .{ .ld_a_r16mem = .{ .r16mem = .hld } },
            0x3B => return .{ .dec_r16 = .{ .r16 = .sp } },
            0x3C => return .{ .inc_r8 = .{ .r8 = .a } },
            0x3D => return .{ .dec_r8 = .{ .r8 = .a } },
            0x3E => return .{ .ld_r8_imm8 = .{ .r8 = .a, .imm8 = bus.read_u8() } },
            0x3F => return .ccf,
            // --- 0x40 to 0x4F ---
            0x40 => return .{ .ld_r8_r8 = .{ .r8_dst = .b, .r8_src = .b } },
            0x41 => return .{ .ld_r8_r8 = .{ .r8_dst = .b, .r8_src = .c } },
            0x42 => return .{ .ld_r8_r8 = .{ .r8_dst = .b, .r8_src = .d } },
            0x43 => return .{ .ld_r8_r8 = .{ .r8_dst = .b, .r8_src = .e } },
            0x44 => return .{ .ld_r8_r8 = .{ .r8_dst = .b, .r8_src = .h } },
            0x45 => return .{ .ld_r8_r8 = .{ .r8_dst = .b, .r8_src = .l } },
            0x46 => return .{ .ld_r8_r8 = .{ .r8_dst = .b, .r8_src = .hl } },
            0x47 => return .{ .ld_r8_r8 = .{ .r8_dst = .b, .r8_src = .a } },
            0x48 => return .{ .ld_r8_r8 = .{ .r8_dst = .c, .r8_src = .b } },
            0x49 => return .{ .ld_r8_r8 = .{ .r8_dst = .c, .r8_src = .c } },
            0x4A => return .{ .ld_r8_r8 = .{ .r8_dst = .c, .r8_src = .d } },
            0x4B => return .{ .ld_r8_r8 = .{ .r8_dst = .c, .r8_src = .e } },
            0x4C => return .{ .ld_r8_r8 = .{ .r8_dst = .c, .r8_src = .h } },
            0x4D => return .{ .ld_r8_r8 = .{ .r8_dst = .c, .r8_src = .l } },
            0x4E => return .{ .ld_r8_r8 = .{ .r8_dst = .c, .r8_src = .hl } },
            0x4F => return .{ .ld_r8_r8 = .{ .r8_dst = .c, .r8_src = .a } },
            // --- 0x50 to 0x5F ---
            0x50 => return .{ .ld_r8_r8 = .{ .r8_dst = .d, .r8_src = .b } },
            0x51 => return .{ .ld_r8_r8 = .{ .r8_dst = .d, .r8_src = .c } },
            0x52 => return .{ .ld_r8_r8 = .{ .r8_dst = .d, .r8_src = .d } },
            0x53 => return .{ .ld_r8_r8 = .{ .r8_dst = .d, .r8_src = .e } },
            0x54 => return .{ .ld_r8_r8 = .{ .r8_dst = .d, .r8_src = .h } },
            0x55 => return .{ .ld_r8_r8 = .{ .r8_dst = .d, .r8_src = .l } },
            0x56 => return .{ .ld_r8_r8 = .{ .r8_dst = .d, .r8_src = .hl } },
            0x57 => return .{ .ld_r8_r8 = .{ .r8_dst = .d, .r8_src = .a } },
            0x58 => return .{ .ld_r8_r8 = .{ .r8_dst = .e, .r8_src = .b } },
            0x59 => return .{ .ld_r8_r8 = .{ .r8_dst = .e, .r8_src = .c } },
            0x5A => return .{ .ld_r8_r8 = .{ .r8_dst = .e, .r8_src = .d } },
            0x5B => return .{ .ld_r8_r8 = .{ .r8_dst = .e, .r8_src = .e } },
            0x5C => return .{ .ld_r8_r8 = .{ .r8_dst = .e, .r8_src = .h } },
            0x5D => return .{ .ld_r8_r8 = .{ .r8_dst = .e, .r8_src = .l } },
            0x5E => return .{ .ld_r8_r8 = .{ .r8_dst = .e, .r8_src = .hl } },
            0x5F => return .{ .ld_r8_r8 = .{ .r8_dst = .e, .r8_src = .a } },
            // --- 0x60 to 0x6F ---
            0x60 => return .{ .ld_r8_r8 = .{ .r8_dst = .h, .r8_src = .b } },
            0x61 => return .{ .ld_r8_r8 = .{ .r8_dst = .h, .r8_src = .c } },
            0x62 => return .{ .ld_r8_r8 = .{ .r8_dst = .h, .r8_src = .d } },
            0x63 => return .{ .ld_r8_r8 = .{ .r8_dst = .h, .r8_src = .e } },
            0x64 => return .{ .ld_r8_r8 = .{ .r8_dst = .h, .r8_src = .h } },
            0x65 => return .{ .ld_r8_r8 = .{ .r8_dst = .h, .r8_src = .l } },
            0x66 => return .{ .ld_r8_r8 = .{ .r8_dst = .h, .r8_src = .hl } },
            0x67 => return .{ .ld_r8_r8 = .{ .r8_dst = .h, .r8_src = .a } },
            0x68 => return .{ .ld_r8_r8 = .{ .r8_dst = .l, .r8_src = .b } },
            0x69 => return .{ .ld_r8_r8 = .{ .r8_dst = .l, .r8_src = .c } },
            0x6A => return .{ .ld_r8_r8 = .{ .r8_dst = .l, .r8_src = .d } },
            0x6B => return .{ .ld_r8_r8 = .{ .r8_dst = .l, .r8_src = .e } },
            0x6C => return .{ .ld_r8_r8 = .{ .r8_dst = .l, .r8_src = .h } },
            0x6D => return .{ .ld_r8_r8 = .{ .r8_dst = .l, .r8_src = .l } },
            0x6E => return .{ .ld_r8_r8 = .{ .r8_dst = .l, .r8_src = .hl } },
            0x6F => return .{ .ld_r8_r8 = .{ .r8_dst = .l, .r8_src = .a } },
            // --- 0x70 to 0x7F ---
            0x70 => return .{ .ld_r8_r8 = .{ .r8_dst = .hl, .r8_src = .b } },
            0x71 => return .{ .ld_r8_r8 = .{ .r8_dst = .hl, .r8_src = .c } },
            0x72 => return .{ .ld_r8_r8 = .{ .r8_dst = .hl, .r8_src = .d } },
            0x73 => return .{ .ld_r8_r8 = .{ .r8_dst = .hl, .r8_src = .e } },
            0x74 => return .{ .ld_r8_r8 = .{ .r8_dst = .hl, .r8_src = .h } },
            0x75 => return .{ .ld_r8_r8 = .{ .r8_dst = .hl, .r8_src = .l } },
            0x76 => return .halt,
            0x77 => return .{ .ld_r8_r8 = .{ .r8_dst = .hl, .r8_src = .a } },
            0x78 => return .{ .ld_r8_r8 = .{ .r8_dst = .a, .r8_src = .b } },
            0x79 => return .{ .ld_r8_r8 = .{ .r8_dst = .a, .r8_src = .c } },
            0x7A => return .{ .ld_r8_r8 = .{ .r8_dst = .a, .r8_src = .d } },
            0x7B => return .{ .ld_r8_r8 = .{ .r8_dst = .a, .r8_src = .e } },
            0x7C => return .{ .ld_r8_r8 = .{ .r8_dst = .a, .r8_src = .h } },
            0x7D => return .{ .ld_r8_r8 = .{ .r8_dst = .a, .r8_src = .l } },
            0x7E => return .{ .ld_r8_r8 = .{ .r8_dst = .a, .r8_src = .hl } },
            0x7F => return .{ .ld_r8_r8 = .{ .r8_dst = .a, .r8_src = .a } },
            // --- 0x80 to 0x8F ---
            0x80 => return .{ .add_a_r8 = .{ .r8 = .b } },
            0x81 => return .{ .add_a_r8 = .{ .r8 = .c } },
            0x82 => return .{ .add_a_r8 = .{ .r8 = .d } },
            0x83 => return .{ .add_a_r8 = .{ .r8 = .e } },
            0x84 => return .{ .add_a_r8 = .{ .r8 = .h } },
            0x85 => return .{ .add_a_r8 = .{ .r8 = .l } },
            0x86 => return .{ .add_a_r8 = .{ .r8 = .hl } },
            0x87 => return .{ .add_a_r8 = .{ .r8 = .a } },
            0x88 => return .{ .adc_a_r8 = .{ .r8 = .b } },
            0x89 => return .{ .adc_a_r8 = .{ .r8 = .c } },
            0x8A => return .{ .adc_a_r8 = .{ .r8 = .d } },
            0x8B => return .{ .adc_a_r8 = .{ .r8 = .e } },
            0x8C => return .{ .adc_a_r8 = .{ .r8 = .h } },
            0x8D => return .{ .adc_a_r8 = .{ .r8 = .l } },
            0x8E => return .{ .adc_a_r8 = .{ .r8 = .hl } },
            0x8F => return .{ .adc_a_r8 = .{ .r8 = .a } },
            // --- 0x90 to 0x9F ---
            0x90 => return .{ .sub_a_r8 = .{ .r8 = .b } },
            0x91 => return .{ .sub_a_r8 = .{ .r8 = .c } },
            0x92 => return .{ .sub_a_r8 = .{ .r8 = .d } },
            0x93 => return .{ .sub_a_r8 = .{ .r8 = .e } },
            0x94 => return .{ .sub_a_r8 = .{ .r8 = .h } },
            0x95 => return .{ .sub_a_r8 = .{ .r8 = .l } },
            0x96 => return .{ .sub_a_r8 = .{ .r8 = .hl } },
            0x97 => return .{ .sub_a_r8 = .{ .r8 = .a } },
            0x98 => return .{ .sbc_a_r8 = .{ .r8 = .b } },
            0x99 => return .{ .sbc_a_r8 = .{ .r8 = .c } },
            0x9A => return .{ .sbc_a_r8 = .{ .r8 = .d } },
            0x9B => return .{ .sbc_a_r8 = .{ .r8 = .e } },
            0x9C => return .{ .sbc_a_r8 = .{ .r8 = .h } },
            0x9D => return .{ .sbc_a_r8 = .{ .r8 = .l } },
            0x9E => return .{ .sbc_a_r8 = .{ .r8 = .hl } },
            0x9F => return .{ .sbc_a_r8 = .{ .r8 = .a } },
            // --- 0xA0 to 0xAF ---
            0xA0 => return .{ .and_a_r8 = .{ .r8 = .b } },
            0xA1 => return .{ .and_a_r8 = .{ .r8 = .c } },
            0xA2 => return .{ .and_a_r8 = .{ .r8 = .d } },
            0xA3 => return .{ .and_a_r8 = .{ .r8 = .e } },
            0xA4 => return .{ .and_a_r8 = .{ .r8 = .h } },
            0xA5 => return .{ .and_a_r8 = .{ .r8 = .l } },
            0xA6 => return .{ .and_a_r8 = .{ .r8 = .hl } },
            0xA7 => return .{ .and_a_r8 = .{ .r8 = .a } },
            0xA8 => return .{ .xor_a_r8 = .{ .r8 = .b } },
            0xA9 => return .{ .xor_a_r8 = .{ .r8 = .c } },
            0xAA => return .{ .xor_a_r8 = .{ .r8 = .d } },
            0xAB => return .{ .xor_a_r8 = .{ .r8 = .e } },
            0xAC => return .{ .xor_a_r8 = .{ .r8 = .h } },
            0xAD => return .{ .xor_a_r8 = .{ .r8 = .l } },
            0xAE => return .{ .xor_a_r8 = .{ .r8 = .hl } },
            0xAF => return .{ .xor_a_r8 = .{ .r8 = .a } },
            // --- 0xB0 to 0xBF ---
            0xB0 => return .{ .or_a_r8 = .{ .r8 = .b } },
            0xB1 => return .{ .or_a_r8 = .{ .r8 = .c } },
            0xB2 => return .{ .or_a_r8 = .{ .r8 = .d } },
            0xB3 => return .{ .or_a_r8 = .{ .r8 = .e } },
            0xB4 => return .{ .or_a_r8 = .{ .r8 = .h } },
            0xB5 => return .{ .or_a_r8 = .{ .r8 = .l } },
            0xB6 => return .{ .or_a_r8 = .{ .r8 = .hl } },
            0xB7 => return .{ .or_a_r8 = .{ .r8 = .a } },
            0xB8 => return .{ .cp_a_r8 = .{ .r8 = .b } },
            0xB9 => return .{ .cp_a_r8 = .{ .r8 = .c } },
            0xBA => return .{ .cp_a_r8 = .{ .r8 = .d } },
            0xBB => return .{ .cp_a_r8 = .{ .r8 = .e } },
            0xBC => return .{ .cp_a_r8 = .{ .r8 = .h } },
            0xBD => return .{ .cp_a_r8 = .{ .r8 = .l } },
            0xBE => return .{ .cp_a_r8 = .{ .r8 = .hl } },
            0xBF => return .{ .cp_a_r8 = .{ .r8 = .a } },
            // --- 0xC0 to 0xCF ---
            0xC0 => return .{ .ret_cond = .{ .cond = .nz } },
            0xC1 => return .{ .pop_r16stk = .{ .r16stk = .bc } },
            0xC2 => return .{ .jp_cond_imm16 = .{ .cond = .nz, .imm16 = bus.read_u16() } },
            0xC3 => return .{ .jp_imm16 = .{ .imm16 = bus.read_u16() } },
            0xC4 => return .{ .call_cond_imm16 = .{ .cond = .nz, .imm16 = bus.read_u16() } },
            0xC5 => return .{ .push_r16stk = .{ .r16stk = .bc } },
            0xC6 => return .{ .add_a_imm8 = .{ .imm8 = bus.read_u8() } },
            0xC7 => return .{ .rst_tgt3 = .{ .target_addr = 0x00 } },
            0xC8 => return .{ .ret_cond = .{ .cond = .z } },
            0xC9 => return .ret,
            0xCA => return .{ .jp_cond_imm16 = .{ .cond = .z, .imm16 = bus.read_u16() } },
            0xCB => return Instruction.prefix_from_bus(bus),
            0xCC => return .{ .call_cond_imm16 = .{ .cond = .z, .imm16 = bus.read_u16() } },
            0xCD => return .{ .call_imm16 = .{ .imm16 = bus.read_u16() } },
            0xCE => return .{ .adc_a_imm8 = .{ .imm8 = bus.read_u8() } },
            0xCF => return .{ .rst_tgt3 = .{ .target_addr = 0x08 } },
            // --- 0xD0 to 0xDF ---
            0xD0 => return .{ .ret_cond = .{ .cond = .nc } },
            0xD1 => return .{ .pop_r16stk = .{ .r16stk = .de } },
            0xD2 => return .{ .jp_cond_imm16 = .{ .cond = .nc, .imm16 = bus.read_u16() } },
            0xD3 => return .invalid,
            0xD4 => return .{ .call_cond_imm16 = .{ .cond = .nc, .imm16 = bus.read_u16() } },
            0xD5 => return .{ .push_r16stk = .{ .r16stk = .de } },
            0xD6 => return .{ .sub_a_imm8 = .{ .imm8 = bus.read_u8() } },
            0xD7 => return .{ .rst_tgt3 = .{ .target_addr = 0x10 } },
            0xD8 => return .{ .ret_cond = .{ .cond = .c } },
            0xD9 => return .reti,
            0xDA => return .{ .jp_cond_imm16 = .{ .cond = .c, .imm16 = bus.read_u16() } },
            0xDB => return .invalid,
            0xDC => return .{ .call_cond_imm16 = .{ .cond = .c, .imm16 = bus.read_u16() } },
            0xDD => return .invalid,
            0xDE => return .{ .sbc_a_imm8 = .{ .imm8 = bus.read_u8() } },
            0xDF => return .{ .rst_tgt3 = .{ .target_addr = 0x18 } },
            // --- 0xE0 to 0xEF ---
            0xE0 => return .{ .ldh_imm8_a = .{ .imm8 = bus.read_u8() } },
            0xE1 => return .{ .pop_r16stk = .{ .r16stk = .hl } },
            0xE2 => return .ldh_c_a,
            0xE3 => return .invalid,
            0xE4 => return .invalid,
            0xE5 => return .{ .push_r16stk = .{ .r16stk = .hl } },
            0xE6 => return .{ .and_a_imm8 = .{ .imm8 = bus.read_u8() } },
            0xE7 => return .{ .rst_tgt3 = .{ .target_addr = 0x20 } },
            0xE8 => return .{ .add_sp_imm8 = .{ .offset = bus.read_i8() } },
            0xE9 => return .jp_hl,
            0xEA => return .{ .ld_imm16_a = .{ .imm16 = bus.read_u16() } },
            0xEB => return .invalid,
            0xEC => return .invalid,
            0xED => return .invalid,
            0xEE => return .{ .xor_a_imm8 = .{ .imm8 = bus.read_u8() } },
            0xEF => return .{ .rst_tgt3 = .{ .target_addr = 0x28 } },
            // --- 0xF0 to 0xFF ---
            0xF0 => return .{ .ldh_a_imm8 = .{ .imm8 = bus.read_u8() } },
            0xF1 => return .{ .pop_r16stk = .{ .r16stk = .af } },
            0xF2 => return .ldh_a_c,
            0xF3 => return .di,
            0xF4 => return .invalid,
            0xF5 => return .{ .push_r16stk = .{ .r16stk = .af } },
            0xF6 => return .{ .or_a_imm8 = .{ .imm8 = bus.read_u8() } },
            0xF7 => return .{ .rst_tgt3 = .{ .target_addr = 0x30 } },
            0xF8 => return .{ .ld_hl_sp_plus_imm8 = .{ .offset = bus.read_i8() } },
            0xF9 => return .ld_sp_hl,
            0xFA => return .{ .ld_a_imm16 = .{ .imm16 = bus.read_u16() } },
            0xFB => return .ei,
            0xFC => return .invalid,
            0xFD => return .invalid,
            0xFE => return .{ .cp_a_imm8 = .{ .imm8 = bus.read_u8() } },
            0xFF => return .{ .rst_tgt3 = .{ .target_addr = 0x38 } },
        }
    }

    pub fn prefix_from_bus(bus: *Bus) Instruction {
        switch (bus.read_u8()) {
            // --- 0x00 to 0x0F ---
            0x00 => return .{ .rlc_r8 = .{ .r8 = .b } },
            0x01 => return .{ .rlc_r8 = .{ .r8 = .c } },
            0x02 => return .{ .rlc_r8 = .{ .r8 = .d } },
            0x03 => return .{ .rlc_r8 = .{ .r8 = .e } },
            0x04 => return .{ .rlc_r8 = .{ .r8 = .h } },
            0x05 => return .{ .rlc_r8 = .{ .r8 = .l } },
            0x06 => return .{ .rlc_r8 = .{ .r8 = .hl } },
            0x07 => return .{ .rlc_r8 = .{ .r8 = .a } },
            0x08 => return .{ .rrc_r8 = .{ .r8 = .b } },
            0x09 => return .{ .rrc_r8 = .{ .r8 = .c } },
            0x0A => return .{ .rrc_r8 = .{ .r8 = .d } },
            0x0B => return .{ .rrc_r8 = .{ .r8 = .e } },
            0x0C => return .{ .rrc_r8 = .{ .r8 = .h } },
            0x0D => return .{ .rrc_r8 = .{ .r8 = .l } },
            0x0E => return .{ .rrc_r8 = .{ .r8 = .hl } },
            0x0F => return .{ .rrc_r8 = .{ .r8 = .a } },
            // --- 0x10 to 0x1F ---
            0x10 => return .{ .rl_r8 = .{ .r8 = .b } },
            0x11 => return .{ .rl_r8 = .{ .r8 = .c } },
            0x12 => return .{ .rl_r8 = .{ .r8 = .d } },
            0x13 => return .{ .rl_r8 = .{ .r8 = .e } },
            0x14 => return .{ .rl_r8 = .{ .r8 = .h } },
            0x15 => return .{ .rl_r8 = .{ .r8 = .l } },
            0x16 => return .{ .rl_r8 = .{ .r8 = .hl } },
            0x17 => return .{ .rl_r8 = .{ .r8 = .a } },
            0x18 => return .{ .rr_r8 = .{ .r8 = .b } },
            0x19 => return .{ .rr_r8 = .{ .r8 = .c } },
            0x1A => return .{ .rr_r8 = .{ .r8 = .d } },
            0x1B => return .{ .rr_r8 = .{ .r8 = .e } },
            0x1C => return .{ .rr_r8 = .{ .r8 = .h } },
            0x1D => return .{ .rr_r8 = .{ .r8 = .l } },
            0x1E => return .{ .rr_r8 = .{ .r8 = .hl } },
            0x1F => return .{ .rr_r8 = .{ .r8 = .a } },
            // --- 0x20 to 0x2F ---
            0x20 => return .{ .sla_r8 = .{ .r8 = .b } },
            0x21 => return .{ .sla_r8 = .{ .r8 = .c } },
            0x22 => return .{ .sla_r8 = .{ .r8 = .d } },
            0x23 => return .{ .sla_r8 = .{ .r8 = .e } },
            0x24 => return .{ .sla_r8 = .{ .r8 = .h } },
            0x25 => return .{ .sla_r8 = .{ .r8 = .l } },
            0x26 => return .{ .sla_r8 = .{ .r8 = .hl } },
            0x27 => return .{ .sla_r8 = .{ .r8 = .a } },
            0x28 => return .{ .sra_r8 = .{ .r8 = .b } },
            0x29 => return .{ .sra_r8 = .{ .r8 = .c } },
            0x2A => return .{ .sra_r8 = .{ .r8 = .d } },
            0x2B => return .{ .sra_r8 = .{ .r8 = .e } },
            0x2C => return .{ .sra_r8 = .{ .r8 = .h } },
            0x2D => return .{ .sra_r8 = .{ .r8 = .l } },
            0x2E => return .{ .sra_r8 = .{ .r8 = .hl } },
            0x2F => return .{ .sra_r8 = .{ .r8 = .a } },
            // --- 0x30 to 0x3F ---
            0x30 => return .{ .swap_r8 = .{ .r8 = .b } },
            0x31 => return .{ .swap_r8 = .{ .r8 = .c } },
            0x32 => return .{ .swap_r8 = .{ .r8 = .d } },
            0x33 => return .{ .swap_r8 = .{ .r8 = .e } },
            0x34 => return .{ .swap_r8 = .{ .r8 = .h } },
            0x35 => return .{ .swap_r8 = .{ .r8 = .l } },
            0x36 => return .{ .swap_r8 = .{ .r8 = .hl } },
            0x37 => return .{ .swap_r8 = .{ .r8 = .a } },
            0x38 => return .{ .srl_r8 = .{ .r8 = .b } },
            0x39 => return .{ .srl_r8 = .{ .r8 = .c } },
            0x3A => return .{ .srl_r8 = .{ .r8 = .d } },
            0x3B => return .{ .srl_r8 = .{ .r8 = .e } },
            0x3C => return .{ .srl_r8 = .{ .r8 = .h } },
            0x3D => return .{ .srl_r8 = .{ .r8 = .l } },
            0x3E => return .{ .srl_r8 = .{ .r8 = .hl } },
            0x3F => return .{ .srl_r8 = .{ .r8 = .a } },
            // --- 0x40 to 0x4F ---
            0x40 => return .{ .bit_b3_r8 = .{ .bit_index = 0, .r8 = .b } },
            0x41 => return .{ .bit_b3_r8 = .{ .bit_index = 0, .r8 = .c } },
            0x42 => return .{ .bit_b3_r8 = .{ .bit_index = 0, .r8 = .d } },
            0x43 => return .{ .bit_b3_r8 = .{ .bit_index = 0, .r8 = .e } },
            0x44 => return .{ .bit_b3_r8 = .{ .bit_index = 0, .r8 = .h } },
            0x45 => return .{ .bit_b3_r8 = .{ .bit_index = 0, .r8 = .l } },
            0x46 => return .{ .bit_b3_r8 = .{ .bit_index = 0, .r8 = .hl } },
            0x47 => return .{ .bit_b3_r8 = .{ .bit_index = 0, .r8 = .a } },
            0x48 => return .{ .bit_b3_r8 = .{ .bit_index = 1, .r8 = .b } },
            0x49 => return .{ .bit_b3_r8 = .{ .bit_index = 1, .r8 = .c } },
            0x4A => return .{ .bit_b3_r8 = .{ .bit_index = 1, .r8 = .d } },
            0x4B => return .{ .bit_b3_r8 = .{ .bit_index = 1, .r8 = .e } },
            0x4C => return .{ .bit_b3_r8 = .{ .bit_index = 1, .r8 = .h } },
            0x4D => return .{ .bit_b3_r8 = .{ .bit_index = 1, .r8 = .l } },
            0x4E => return .{ .bit_b3_r8 = .{ .bit_index = 1, .r8 = .hl } },
            0x4F => return .{ .bit_b3_r8 = .{ .bit_index = 1, .r8 = .a } },
            // --- 0x50 to 0x5F ---
            0x50 => return .{ .bit_b3_r8 = .{ .bit_index = 2, .r8 = .b } },
            0x51 => return .{ .bit_b3_r8 = .{ .bit_index = 2, .r8 = .c } },
            0x52 => return .{ .bit_b3_r8 = .{ .bit_index = 2, .r8 = .d } },
            0x53 => return .{ .bit_b3_r8 = .{ .bit_index = 2, .r8 = .e } },
            0x54 => return .{ .bit_b3_r8 = .{ .bit_index = 2, .r8 = .h } },
            0x55 => return .{ .bit_b3_r8 = .{ .bit_index = 2, .r8 = .l } },
            0x56 => return .{ .bit_b3_r8 = .{ .bit_index = 2, .r8 = .hl } },
            0x57 => return .{ .bit_b3_r8 = .{ .bit_index = 2, .r8 = .a } },
            0x58 => return .{ .bit_b3_r8 = .{ .bit_index = 3, .r8 = .b } },
            0x59 => return .{ .bit_b3_r8 = .{ .bit_index = 3, .r8 = .c } },
            0x5A => return .{ .bit_b3_r8 = .{ .bit_index = 3, .r8 = .d } },
            0x5B => return .{ .bit_b3_r8 = .{ .bit_index = 3, .r8 = .e } },
            0x5C => return .{ .bit_b3_r8 = .{ .bit_index = 3, .r8 = .h } },
            0x5D => return .{ .bit_b3_r8 = .{ .bit_index = 3, .r8 = .l } },
            0x5E => return .{ .bit_b3_r8 = .{ .bit_index = 3, .r8 = .hl } },
            0x5F => return .{ .bit_b3_r8 = .{ .bit_index = 3, .r8 = .a } },
            // --- 0x60 to 0x6F ---
            0x60 => return .{ .bit_b3_r8 = .{ .bit_index = 4, .r8 = .b } },
            0x61 => return .{ .bit_b3_r8 = .{ .bit_index = 4, .r8 = .c } },
            0x62 => return .{ .bit_b3_r8 = .{ .bit_index = 4, .r8 = .d } },
            0x63 => return .{ .bit_b3_r8 = .{ .bit_index = 4, .r8 = .e } },
            0x64 => return .{ .bit_b3_r8 = .{ .bit_index = 4, .r8 = .h } },
            0x65 => return .{ .bit_b3_r8 = .{ .bit_index = 4, .r8 = .l } },
            0x66 => return .{ .bit_b3_r8 = .{ .bit_index = 4, .r8 = .hl } },
            0x67 => return .{ .bit_b3_r8 = .{ .bit_index = 4, .r8 = .a } },
            0x68 => return .{ .bit_b3_r8 = .{ .bit_index = 5, .r8 = .b } },
            0x69 => return .{ .bit_b3_r8 = .{ .bit_index = 5, .r8 = .c } },
            0x6A => return .{ .bit_b3_r8 = .{ .bit_index = 5, .r8 = .d } },
            0x6B => return .{ .bit_b3_r8 = .{ .bit_index = 5, .r8 = .e } },
            0x6C => return .{ .bit_b3_r8 = .{ .bit_index = 5, .r8 = .h } },
            0x6D => return .{ .bit_b3_r8 = .{ .bit_index = 5, .r8 = .l } },
            0x6E => return .{ .bit_b3_r8 = .{ .bit_index = 5, .r8 = .hl } },
            0x6F => return .{ .bit_b3_r8 = .{ .bit_index = 5, .r8 = .a } },
            // --- 0x70 to 0x7F ---
            0x70 => return .{ .bit_b3_r8 = .{ .bit_index = 6, .r8 = .b } },
            0x71 => return .{ .bit_b3_r8 = .{ .bit_index = 6, .r8 = .c } },
            0x72 => return .{ .bit_b3_r8 = .{ .bit_index = 6, .r8 = .d } },
            0x73 => return .{ .bit_b3_r8 = .{ .bit_index = 6, .r8 = .e } },
            0x74 => return .{ .bit_b3_r8 = .{ .bit_index = 6, .r8 = .h } },
            0x75 => return .{ .bit_b3_r8 = .{ .bit_index = 6, .r8 = .l } },
            0x76 => return .{ .bit_b3_r8 = .{ .bit_index = 6, .r8 = .hl } },
            0x77 => return .{ .bit_b3_r8 = .{ .bit_index = 6, .r8 = .a } },
            0x78 => return .{ .bit_b3_r8 = .{ .bit_index = 7, .r8 = .b } },
            0x79 => return .{ .bit_b3_r8 = .{ .bit_index = 7, .r8 = .c } },
            0x7A => return .{ .bit_b3_r8 = .{ .bit_index = 7, .r8 = .d } },
            0x7B => return .{ .bit_b3_r8 = .{ .bit_index = 7, .r8 = .e } },
            0x7C => return .{ .bit_b3_r8 = .{ .bit_index = 7, .r8 = .h } },
            0x7D => return .{ .bit_b3_r8 = .{ .bit_index = 7, .r8 = .l } },
            0x7E => return .{ .bit_b3_r8 = .{ .bit_index = 7, .r8 = .hl } },
            0x7F => return .{ .bit_b3_r8 = .{ .bit_index = 7, .r8 = .a } },
            // --- 0x80 to 0x8F ---
            0x80 => return .{ .res_b3_r8 = .{ .bit_index = 0, .r8 = .b } },
            0x81 => return .{ .res_b3_r8 = .{ .bit_index = 0, .r8 = .c } },
            0x82 => return .{ .res_b3_r8 = .{ .bit_index = 0, .r8 = .d } },
            0x83 => return .{ .res_b3_r8 = .{ .bit_index = 0, .r8 = .e } },
            0x84 => return .{ .res_b3_r8 = .{ .bit_index = 0, .r8 = .h } },
            0x85 => return .{ .res_b3_r8 = .{ .bit_index = 0, .r8 = .l } },
            0x86 => return .{ .res_b3_r8 = .{ .bit_index = 0, .r8 = .hl } },
            0x87 => return .{ .res_b3_r8 = .{ .bit_index = 0, .r8 = .a } },
            0x88 => return .{ .res_b3_r8 = .{ .bit_index = 1, .r8 = .b } },
            0x89 => return .{ .res_b3_r8 = .{ .bit_index = 1, .r8 = .c } },
            0x8A => return .{ .res_b3_r8 = .{ .bit_index = 1, .r8 = .d } },
            0x8B => return .{ .res_b3_r8 = .{ .bit_index = 1, .r8 = .e } },
            0x8C => return .{ .res_b3_r8 = .{ .bit_index = 1, .r8 = .h } },
            0x8D => return .{ .res_b3_r8 = .{ .bit_index = 1, .r8 = .l } },
            0x8E => return .{ .res_b3_r8 = .{ .bit_index = 1, .r8 = .hl } },
            0x8F => return .{ .res_b3_r8 = .{ .bit_index = 1, .r8 = .a } },
            // --- 0x90 to 0x9F ---
            0x90 => return .{ .res_b3_r8 = .{ .bit_index = 2, .r8 = .b } },
            0x91 => return .{ .res_b3_r8 = .{ .bit_index = 2, .r8 = .c } },
            0x92 => return .{ .res_b3_r8 = .{ .bit_index = 2, .r8 = .d } },
            0x93 => return .{ .res_b3_r8 = .{ .bit_index = 2, .r8 = .e } },
            0x94 => return .{ .res_b3_r8 = .{ .bit_index = 2, .r8 = .h } },
            0x95 => return .{ .res_b3_r8 = .{ .bit_index = 2, .r8 = .l } },
            0x96 => return .{ .res_b3_r8 = .{ .bit_index = 2, .r8 = .hl } },
            0x97 => return .{ .res_b3_r8 = .{ .bit_index = 2, .r8 = .a } },
            0x98 => return .{ .res_b3_r8 = .{ .bit_index = 3, .r8 = .b } },
            0x99 => return .{ .res_b3_r8 = .{ .bit_index = 3, .r8 = .c } },
            0x9A => return .{ .res_b3_r8 = .{ .bit_index = 3, .r8 = .d } },
            0x9B => return .{ .res_b3_r8 = .{ .bit_index = 3, .r8 = .e } },
            0x9C => return .{ .res_b3_r8 = .{ .bit_index = 3, .r8 = .h } },
            0x9D => return .{ .res_b3_r8 = .{ .bit_index = 3, .r8 = .l } },
            0x9E => return .{ .res_b3_r8 = .{ .bit_index = 3, .r8 = .hl } },
            0x9F => return .{ .res_b3_r8 = .{ .bit_index = 3, .r8 = .a } },
            // --- 0xA0 to 0xAF ---
            0xA0 => return .{ .res_b3_r8 = .{ .bit_index = 4, .r8 = .b } },
            0xA1 => return .{ .res_b3_r8 = .{ .bit_index = 4, .r8 = .c } },
            0xA2 => return .{ .res_b3_r8 = .{ .bit_index = 4, .r8 = .d } },
            0xA3 => return .{ .res_b3_r8 = .{ .bit_index = 4, .r8 = .e } },
            0xA4 => return .{ .res_b3_r8 = .{ .bit_index = 4, .r8 = .h } },
            0xA5 => return .{ .res_b3_r8 = .{ .bit_index = 4, .r8 = .l } },
            0xA6 => return .{ .res_b3_r8 = .{ .bit_index = 4, .r8 = .hl } },
            0xA7 => return .{ .res_b3_r8 = .{ .bit_index = 4, .r8 = .a } },
            0xA8 => return .{ .res_b3_r8 = .{ .bit_index = 5, .r8 = .b } },
            0xA9 => return .{ .res_b3_r8 = .{ .bit_index = 5, .r8 = .c } },
            0xAA => return .{ .res_b3_r8 = .{ .bit_index = 5, .r8 = .d } },
            0xAB => return .{ .res_b3_r8 = .{ .bit_index = 5, .r8 = .e } },
            0xAC => return .{ .res_b3_r8 = .{ .bit_index = 5, .r8 = .h } },
            0xAD => return .{ .res_b3_r8 = .{ .bit_index = 5, .r8 = .l } },
            0xAE => return .{ .res_b3_r8 = .{ .bit_index = 5, .r8 = .hl } },
            0xAF => return .{ .res_b3_r8 = .{ .bit_index = 5, .r8 = .a } },
            // --- 0xB0 to 0xBF ---
            0xB0 => return .{ .res_b3_r8 = .{ .bit_index = 6, .r8 = .b } },
            0xB1 => return .{ .res_b3_r8 = .{ .bit_index = 6, .r8 = .c } },
            0xB2 => return .{ .res_b3_r8 = .{ .bit_index = 6, .r8 = .d } },
            0xB3 => return .{ .res_b3_r8 = .{ .bit_index = 6, .r8 = .e } },
            0xB4 => return .{ .res_b3_r8 = .{ .bit_index = 6, .r8 = .h } },
            0xB5 => return .{ .res_b3_r8 = .{ .bit_index = 6, .r8 = .l } },
            0xB6 => return .{ .res_b3_r8 = .{ .bit_index = 6, .r8 = .hl } },
            0xB7 => return .{ .res_b3_r8 = .{ .bit_index = 6, .r8 = .a } },
            0xB8 => return .{ .res_b3_r8 = .{ .bit_index = 7, .r8 = .b } },
            0xB9 => return .{ .res_b3_r8 = .{ .bit_index = 7, .r8 = .c } },
            0xBA => return .{ .res_b3_r8 = .{ .bit_index = 7, .r8 = .d } },
            0xBB => return .{ .res_b3_r8 = .{ .bit_index = 7, .r8 = .e } },
            0xBC => return .{ .res_b3_r8 = .{ .bit_index = 7, .r8 = .h } },
            0xBD => return .{ .res_b3_r8 = .{ .bit_index = 7, .r8 = .l } },
            0xBE => return .{ .res_b3_r8 = .{ .bit_index = 7, .r8 = .hl } },
            0xBF => return .{ .res_b3_r8 = .{ .bit_index = 7, .r8 = .a } },
            // --- 0xC0 to 0xCF ---
            0xC0 => return .{ .set_b3_r8 = .{ .bit_index = 0, .r8 = .b } },
            0xC1 => return .{ .set_b3_r8 = .{ .bit_index = 0, .r8 = .c } },
            0xC2 => return .{ .set_b3_r8 = .{ .bit_index = 0, .r8 = .d } },
            0xC3 => return .{ .set_b3_r8 = .{ .bit_index = 0, .r8 = .e } },
            0xC4 => return .{ .set_b3_r8 = .{ .bit_index = 0, .r8 = .h } },
            0xC5 => return .{ .set_b3_r8 = .{ .bit_index = 0, .r8 = .l } },
            0xC6 => return .{ .set_b3_r8 = .{ .bit_index = 0, .r8 = .hl } },
            0xC7 => return .{ .set_b3_r8 = .{ .bit_index = 0, .r8 = .a } },
            0xC8 => return .{ .set_b3_r8 = .{ .bit_index = 1, .r8 = .b } },
            0xC9 => return .{ .set_b3_r8 = .{ .bit_index = 1, .r8 = .c } },
            0xCA => return .{ .set_b3_r8 = .{ .bit_index = 1, .r8 = .d } },
            0xCB => return .{ .set_b3_r8 = .{ .bit_index = 1, .r8 = .e } },
            0xCC => return .{ .set_b3_r8 = .{ .bit_index = 1, .r8 = .h } },
            0xCD => return .{ .set_b3_r8 = .{ .bit_index = 1, .r8 = .l } },
            0xCE => return .{ .set_b3_r8 = .{ .bit_index = 1, .r8 = .hl } },
            0xCF => return .{ .set_b3_r8 = .{ .bit_index = 1, .r8 = .a } },
            // --- 0xD0 to 0xDF ---
            0xD0 => return .{ .set_b3_r8 = .{ .bit_index = 2, .r8 = .b } },
            0xD1 => return .{ .set_b3_r8 = .{ .bit_index = 2, .r8 = .c } },
            0xD2 => return .{ .set_b3_r8 = .{ .bit_index = 2, .r8 = .d } },
            0xD3 => return .{ .set_b3_r8 = .{ .bit_index = 2, .r8 = .e } },
            0xD4 => return .{ .set_b3_r8 = .{ .bit_index = 2, .r8 = .h } },
            0xD5 => return .{ .set_b3_r8 = .{ .bit_index = 2, .r8 = .l } },
            0xD6 => return .{ .set_b3_r8 = .{ .bit_index = 2, .r8 = .hl } },
            0xD7 => return .{ .set_b3_r8 = .{ .bit_index = 2, .r8 = .a } },
            0xD8 => return .{ .set_b3_r8 = .{ .bit_index = 3, .r8 = .b } },
            0xD9 => return .{ .set_b3_r8 = .{ .bit_index = 3, .r8 = .c } },
            0xDA => return .{ .set_b3_r8 = .{ .bit_index = 3, .r8 = .d } },
            0xDB => return .{ .set_b3_r8 = .{ .bit_index = 3, .r8 = .e } },
            0xDC => return .{ .set_b3_r8 = .{ .bit_index = 3, .r8 = .h } },
            0xDD => return .{ .set_b3_r8 = .{ .bit_index = 3, .r8 = .l } },
            0xDE => return .{ .set_b3_r8 = .{ .bit_index = 3, .r8 = .hl } },
            0xDF => return .{ .set_b3_r8 = .{ .bit_index = 3, .r8 = .a } },
            // --- 0xE0 to 0xEF ---
            0xE0 => return .{ .set_b3_r8 = .{ .bit_index = 4, .r8 = .b } },
            0xE1 => return .{ .set_b3_r8 = .{ .bit_index = 4, .r8 = .c } },
            0xE2 => return .{ .set_b3_r8 = .{ .bit_index = 4, .r8 = .d } },
            0xE3 => return .{ .set_b3_r8 = .{ .bit_index = 4, .r8 = .e } },
            0xE4 => return .{ .set_b3_r8 = .{ .bit_index = 4, .r8 = .h } },
            0xE5 => return .{ .set_b3_r8 = .{ .bit_index = 4, .r8 = .l } },
            0xE6 => return .{ .set_b3_r8 = .{ .bit_index = 4, .r8 = .hl } },
            0xE7 => return .{ .set_b3_r8 = .{ .bit_index = 4, .r8 = .a } },
            0xE8 => return .{ .set_b3_r8 = .{ .bit_index = 5, .r8 = .b } },
            0xE9 => return .{ .set_b3_r8 = .{ .bit_index = 5, .r8 = .c } },
            0xEA => return .{ .set_b3_r8 = .{ .bit_index = 5, .r8 = .d } },
            0xEB => return .{ .set_b3_r8 = .{ .bit_index = 5, .r8 = .e } },
            0xEC => return .{ .set_b3_r8 = .{ .bit_index = 5, .r8 = .h } },
            0xED => return .{ .set_b3_r8 = .{ .bit_index = 5, .r8 = .l } },
            0xEE => return .{ .set_b3_r8 = .{ .bit_index = 5, .r8 = .hl } },
            0xEF => return .{ .set_b3_r8 = .{ .bit_index = 5, .r8 = .a } },
            // --- 0xF0 to 0xFF ---
            0xF0 => return .{ .set_b3_r8 = .{ .bit_index = 6, .r8 = .b } },
            0xF1 => return .{ .set_b3_r8 = .{ .bit_index = 6, .r8 = .c } },
            0xF2 => return .{ .set_b3_r8 = .{ .bit_index = 6, .r8 = .d } },
            0xF3 => return .{ .set_b3_r8 = .{ .bit_index = 6, .r8 = .e } },
            0xF4 => return .{ .set_b3_r8 = .{ .bit_index = 6, .r8 = .h } },
            0xF5 => return .{ .set_b3_r8 = .{ .bit_index = 6, .r8 = .l } },
            0xF6 => return .{ .set_b3_r8 = .{ .bit_index = 6, .r8 = .hl } },
            0xF7 => return .{ .set_b3_r8 = .{ .bit_index = 6, .r8 = .a } },
            0xF8 => return .{ .set_b3_r8 = .{ .bit_index = 7, .r8 = .b } },
            0xF9 => return .{ .set_b3_r8 = .{ .bit_index = 7, .r8 = .c } },
            0xFA => return .{ .set_b3_r8 = .{ .bit_index = 7, .r8 = .d } },
            0xFB => return .{ .set_b3_r8 = .{ .bit_index = 7, .r8 = .e } },
            0xFC => return .{ .set_b3_r8 = .{ .bit_index = 7, .r8 = .h } },
            0xFD => return .{ .set_b3_r8 = .{ .bit_index = 7, .r8 = .l } },
            0xFE => return .{ .set_b3_r8 = .{ .bit_index = 7, .r8 = .hl } },
            0xFF => return .{ .set_b3_r8 = .{ .bit_index = 7, .r8 = .a } },
        }
    }

    pub fn take_at(bus: *Bus, addr: u16) InstructionEntry {
        const pc = bus.getCpu().registers.pc;
        bus.getCpu().registers.pc = addr;
        const instr = Instruction.from_bus(bus);
        const size = bus.getCpu().registers.pc - addr;
        bus.getCpu().registers.pc = pc;
        return .{ .instruction = instr, .size = size };
    }
};

pub const InstructionEntry = struct { instruction: Instruction, size: u16 };

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
