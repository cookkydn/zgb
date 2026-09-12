/// **Interrupt controller**
///
/// Handle interrupt flags and acknowledgement
pub const Intc = @This();

const v_blank_src = 0x40;
const stat_src = 0x48;
const timer_src = 0x50;
const serial_src = 0x58;
const joypad_src = 0x60;

const v_blank_mask: u8 = 0b00001;
const stat_mask: u8 = 0b00010;
const timer_mask: u8 = 0b00100;
const serial_mask: u8 = 0b01000;
const joypad_mask: u8 = 0b10000;

/// Interrupt flag (`0xFF0F`)
///
/// When an interrupt request handling it set the corresponding bit in this register
/// ```text
/// +------+-------+-------------------+
/// | Bits | Access| Interrupt         |
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
/// | Bits | Access| Interrupt         |
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

/// Return a 5 bit number, each 1 represent an interrupt which is requested and enabled
fn getIntMask(self: Intc) u5 {
    return @truncate(self.ie_reg & self.if_reg & 0x1F);
}

/// Check if an interrupt is requested and enabled.
/// Return true if any of the interrupts match that condition.
pub fn hasPending(self: Intc) bool {
    return self.getIntMask() != 0;
}

/// Remove the flag of the pending interrupt and return the procedure address
///
/// If more than one interrupt is pending, it acknowledge only the most important one
pub fn acknowledge(self: *Intc) ?u16 {
    const int_mask = self.getIntMask();

    if (int_mask & v_blank_mask != 0) {
        self.if_reg &= ~v_blank_mask;
        return v_blank_src;
    } else if (int_mask & stat_mask != 0) {
        self.if_reg &= ~stat_mask;
        return stat_src;
    } else if (int_mask & timer_mask != 0) {
        self.if_reg &= ~timer_mask;
        return timer_src;
    } else if (int_mask & serial_mask != 0) {
        self.if_reg &= ~serial_mask;
        return serial_src;
    } else if (int_mask & joypad_mask != 0) {
        self.if_reg &= ~joypad_mask;
        return joypad_src;
    }

    return null;
}

pub fn requestVblank(self: *Intc) void {
    self.if_reg |= v_blank_mask;
}

pub fn requestStat(self: *Intc) void {
    self.if_reg |= stat_mask;
}

pub fn requestTimer(self: *Intc) void {
    self.if_reg |= timer_mask;
}

pub fn requestSerial(self: *Intc) void {
    self.if_reg |= serial_mask;
}

pub fn requestJoypad(self: *Intc) void {
    self.if_reg |= joypad_mask;
}

const std = @import("std");
const expect = std.testing.expect;

test "Intc - initial state" {
    const intc = Intc{};

    try expect(intc.ie_reg == 0xE0);
    try expect(intc.if_reg == 0xE0);
}

test "Intc - hasPending" {
    var intc = Intc{};

    intc.ie_reg |= stat_mask;
    intc.requestStat();
    try expect(intc.hasPending());
}

test "Intc - request interrupt" {
    var intc = Intc{};

    intc.requestVblank();
    try expect(intc.if_reg == 0xE1);
    intc.requestStat();
    try expect(intc.if_reg == 0xE3);
    intc.requestTimer();
    try expect(intc.if_reg == 0xE7);
    intc.requestSerial();
    try expect(intc.if_reg == 0xEF);
    intc.requestJoypad();
    try expect(intc.if_reg == 0xFF);
}

test "Intc - acknowledge" {
    var intc = Intc{};

    // Do not acknowledge an interrupt not enabled
    intc.requestJoypad();
    try expect(intc.acknowledge() == null);

    // Return the correct address and remove the flag
    intc.ie_reg |= stat_mask;
    intc.requestStat();
    try expect(intc.acknowledge() == stat_src);
    try expect(intc.if_reg == 0xF0);

    // Acknowledge only one interrupt at a time
    intc.ie_reg |= v_blank_mask;
    intc.requestVblank();
    intc.requestStat();
    try expect(intc.acknowledge() == v_blank_src);
    try expect(intc.if_reg == 0xF2);
}
