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

    fn getCPU(self: *Registers) *CPU {
        return @alignCast(@fieldParentPtr("registers", self));
    }

    pub fn setAF(self: *Registers, value: u16) void {
        self.a = @truncate((value & 0xFF00) >> 8);
        self.f.z = value & 0b10000000 != 0;
        self.f.n = value & 0b01000000 != 0;
        self.f.h = value & 0b00100000 != 0;
        self.f.c = value & 0b00010000 != 0;
    }

    pub fn getAF(self: Registers) u16 {
        return @as(u16, self.a) << 8 | self.f.getF();
    }

    pub fn setBC(self: *Registers, value: u16) void {
        self.b = @truncate(value >> 8);
        self.c = @truncate(value & 0xFF);
    }

    pub fn getBC(self: Registers) u16 {
        return @as(u16, self.b) << 8 | @as(u16, self.c);
    }

    pub fn setDE(self: *Registers, value: u16) void {
        self.d = @truncate(value >> 8);
        self.e = @truncate(value & 0xFF);
    }

    pub fn getDE(self: Registers) u16 {
        return @as(u16, self.d) << 8 | @as(u16, self.e);
    }

    pub fn setHL(self: *Registers, value: u16) void {
        self.h = @truncate(value >> 8);
        self.l = @truncate(value & 0xFF);
    }

    pub fn getHL(self: Registers) u16 {
        return @as(u16, self.h) << 8 | @as(u16, self.l);
    }

    pub fn clearFlags(self: *Registers) void {
        self.f.c = false;
        self.f.h = false;
        self.f.n = false;
        self.f.z = false;
    }

    pub fn loadR8(self: *Registers, r8: instr.R8) u8 {
        var mem = &self.getCPU().bus;
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
                return mem.read_at(self.getHL());
            },
            .a => {
                return self.a;
            },
        }
    }

    pub fn loadR16(self: *Registers, r16: union(enum) {
        r16: instr.R16,
        r16_mem: instr.R16Mem,
        r16_stk: instr.R16Stk,
    }) u16 {
        switch (r16) {
            .r16 => |r16_| {
                switch (r16_) {
                    .bc => return self.getBC(),
                    .de => return self.getDE(),
                    .hl => return self.getHL(),
                    .sp => return self.sp,
                }
            },
            .r16_mem => |r16mem| {
                switch (r16mem) {
                    .bc => return self.getBC(),
                    .de => return self.getDE(),
                    .hli => {
                        self.setHL(self.getHL() +% 1);
                        return self.getHL() -% 1;
                    },
                    .hld => {
                        self.setHL(self.getHL() -% 1);
                        return self.getHL() +% 1;
                    },
                }
            },
            .r16_stk => |r16stk| {
                switch (r16stk) {
                    .bc => return self.getBC(),
                    .de => return self.getDE(),
                    .hl => return self.getHL(),
                    .af => return self.getAF(),
                }
            },
        }
    }

    pub fn setR8(self: *Registers, r8: instr.R8, value: u8) void {
        var mem = &self.getCPU().bus;
        switch (r8) {
            .b => self.b = value,
            .c => self.c = value,
            .d => self.d = value,
            .e => self.e = value,
            .h => self.h = value,
            .l => self.l = value,
            .hl => mem.write_at(self.getHL(), value),
            .a => self.a = value,
        }
    }

    pub fn setR16(self: *Registers, r16: union(enum) {
        r16: instr.R16,
        r16_mem: instr.R16Mem,
        r16_stk: instr.R16Stk,
    }, value: u16) void {
        switch (r16) {
            .r16 => |r16_| {
                switch (r16_) {
                    .bc => self.setBC(value),
                    .de => self.setDE(value),
                    .hl => self.setHL(value),
                    .sp => self.sp = value,
                }
            },
            .r16_mem => |r16mem| {
                switch (r16mem) {
                    .bc => self.setBC(value),
                    .de => self.setDE(value),
                    .hli => self.setHL(value + 1),
                    .hld => self.setHL(value - 1),
                }
            },
            .r16_stk => |r16stk| {
                switch (r16stk) {
                    .bc => self.setBC(value),
                    .de => self.setDE(value),
                    .hl => self.setHL(value),
                    .af => self.setAF(value),
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
    pub fn getF(self: Flags) u16 {
        const z = @as(u8, @intFromBool(self.z)) << 7;
        const n = @as(u8, @intFromBool(self.n)) << 6;
        const h = @as(u8, @intFromBool(self.h)) << 5;
        const c = @as(u8, @intFromBool(self.c)) << 4;
        return @as(u16, z | n | h | c);
    }

    pub fn checkCond(self: Flags, cc: instr.Cond) bool {
        switch (cc) {
            .nz => return !self.z,
            .z => return self.z,
            .nc => return !self.c,
            .c => return self.c,
        }
    }
};
