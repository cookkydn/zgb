pub fn offset_by(base: u16, offset: i8) u16 {
    const off = @as(u16, @bitCast(@as(i16, offset)));
    return base +% off;
}

pub fn u8_add_carry(a: u8, b: u8) bool {
    return @as(u16, a) + @as(u16, b) > 0xFF;
}

pub fn u8_sub_carry(a: u8, b: u8) bool {
    return b > a;
}

pub fn u8_half_add_carry(a: u8, b: u8) bool {
    return (a & 0xF) +% (b & 0xF) > 0xF;
}

pub fn u8_half_sub_carry(a: u8, b: u8) bool {
    return ((a & 0xF) -% (b & 0xF)) > 0xF;
}

test "offset_by" {
    const expect = @import("std").testing.expectEqual;
    try expect(offset_by(0, 1), 1);
    try expect(offset_by(5, 1), 6);
    try expect(offset_by(5, -1), 4);
    try expect(offset_by(65535, 1), 0);
    try expect(offset_by(0, -1), 65535);
    try expect(offset_by(5, 127), 132);
    try expect(offset_by(666, -127), 539);
}

test "u8_add_carry" {
    const expect = @import("std").testing.expect;
    try expect(u8_add_carry(0xFF, 0xFF));
    try expect(u8_add_carry(0xFF, 1));
    try expect(u8_add_carry(1, 0xFF));
}
