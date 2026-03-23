const CPU = @import("cpu.zig").CPU;
const instr = @import("instructions.zig");

pub const Registers = struct {
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

    pub fn load_r8(self: *Registers, r8: instr.R8) u8 {
        var mem = &self.get_cpu().bus;
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
                return mem.read_at(self.get_hl());
            },
            .a => {
                return self.a;
            },
        }
    }

    pub fn load_r16(self: *Registers, r16: union(enum) {
        R16: instr.R16,
        R16Mem: instr.R16Mem,
        R16Stk: instr.R16Stk,
    }) u16 {
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

    pub fn set_r8(self: *Registers, r8: instr.R8, value: u8) void {
        var mem = &self.get_cpu().bus;
        switch (r8) {
            .b => self.b = value,
            .c => self.c = value,
            .d => self.d = value,
            .e => self.e = value,
            .h => self.h = value,
            .l => self.l = value,
            .hl => mem.write_at(self.get_hl(), value),
            .a => self.a = value,
        }
    }

    pub fn set_r16(self: *Registers, r16: union(enum) {
        R16: instr.R16,
        R16Mem: instr.R16Mem,
        R16Stk: instr.R16Stk,
    }, value: u16) void {
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
            .a = 0xFF,
            .b = 0xFF,
            .c = 0xFF,
            .d = 0xFF,
            .e = 0xFF,
            .h = 0xFF,
            .l = 0xFF,
            .f = .{ .z = true, .c = true, .h = true, .n = true },
            .sp = 0xFFFF,
            .pc = 0x0000,
        };
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

    pub fn check_cc(self: Flags, cc: instr.Cond) bool {
        switch (cc) {
            .nz => return !self.z,
            .z => return self.z,
            .nc => return !self.c,
            .c => return self.c,
        }
    }
};
