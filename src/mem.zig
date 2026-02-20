const std = @import("std");
const Allocator = std.mem.Allocator;
const CPU = @import("cpu.zig").CPU;

pub const Memory = struct {
    data: []u8,

    pub fn init(all: Allocator) Memory {
        const data = all.alloc(u8, 0xFFFF + 1) catch std.debug.panic("No memory...", .{});
        return .{ .data = data };
    }

    pub fn deinit(self: *Memory, all: Allocator) void {
        all.free(self.data);
    }

    pub fn get_cpu(self: *Memory) *CPU {
        return @fieldParentPtr("mem", self);
    }

    pub fn read(self: *Memory) u8 {
        const cpu = self.get_cpu();
        const byte = self.read_at(cpu.registers.pc);
        cpu.registers.pc +%= 1;
        return byte;
    }

    pub fn read_imm16(self: *Memory) u16 {
        const lsb = @as(u16, self.read());
        const msb = @as(u16, self.read());
        const imm16 = lsb | (msb << 8);
        return imm16;
    }

    pub fn read_at(self: *Memory, addr: u16) u8 {
        if (!can_read(addr)) return 0xFF;
        switch (Memory.addr_type(addr)) {
            .RAM, .ROM, .INTERRUPT => {
                return self.data[addr];
            },
            .UNUSED => return 0xFF,
            .ECHO => return self.read_at(addr - 0x2000),
            .REGISTER => {
                switch (addr) {
                    // zig fmt: off
                    0xFF00, 0xFF40, 
                    0xFF41,
                    0xFF44, 0xFF46 
                    // zig fmt: on
                    => {
                        return self.data[addr];
                    },
                    else => {
                        std.debug.print("Attempted to read at addr 0x{X:0>4} which is an unhandled register.\n", .{addr});
                        unreachable;
                    },
                }
            },
            .OAM => {
                std.debug.print("Attempted to read at addr 0x{X:0>4} which is an unhandled OAM space.\n", .{addr});
                unreachable;
                // switch (self.get_cpu().screen.get_ppu_mode()) {
                //     .oam_search, .lcd_operate => return,
                //     .h_blank, .v_blank => self.data[addr] = value,
                // }
            },
            .VRAM => {
                switch (self.get_cpu().screen.get_ppu_mode()) {
                    .lcd_operate => return 0xFF,
                    .h_blank, .v_blank, .oam_search => return self.data[addr],
                }
            },
        }
    }

    pub fn push(self: *Memory, value: u16) void {
        const sp = &self.get_cpu().registers.sp;
        sp.* -= 1;
        self.data[sp.*] = @truncate(value >> 8);
        sp.* -= 1;
        self.data[sp.*] = @truncate(value);
    }

    pub fn pop(self: *Memory) u16 {
        const sp = &self.get_cpu().registers.sp;
        const l = @as(u16, self.data[sp.*]);
        sp.* += 1;
        const h = @as(u16, self.data[sp.*]);
        sp.* += 1;
        return h << 8 | l;
    }

    pub fn write(self: *Memory, addr: u16, value: u8) void {
        switch (Memory.addr_type(addr)) {
            .RAM, .ROM, .INTERRUPT => {
                self.data[addr] = value;
            },
            .UNUSED, .ECHO => return,
            .REGISTER => {
                switch (addr) {
                    // zig fmt: off
                    0xFF01...0xFF14, 
                    0xFF16...0xFF19, 
                    0xFF1A...0xFF1E, 
                    0xFF20...0xFF25,
                    0xFF30...0xFF40, 
                    0xFF42, 0xFF43, 
                    0xFF45,
                    0xFF47...0xFF4B, 
                    0xFF4F...0xFF55, 
                    0xFF68...0xFF70 
                    // zig fmt: on
                    => {
                        self.data[addr] = value;
                    },
                    0xFF00 => {
                        // JOYP: Joypad
                        // Lower nibble is readonly
                        self.write_mask(addr, value, 0xF0);
                    },
                    0xFF26 => {
                        // NR52: Audio master control
                        // Only first bit is writable
                        self.write_mask(addr, value, 0x80);
                    },
                    0xFF41 => {
                        // STAT: LCD status
                        // Last three bits are readonly
                        self.write_mask(addr, value, 0xF8);
                    },
                    0xFF46 => {
                        // DMA: OAM DMA source address & start
                        // Writing to this register starts the DMA
                        self.data[addr] = value;
                        self.execute_dma();
                    },
                    0xFF7F => {
                        // Unused
                        return;
                    },
                    else => {
                        std.debug.print("Attempted to write at addr 0x{X:0>4} which is an unhandled register.\n", .{addr});
                        unreachable;
                    },
                }
            },
            .OAM => {
                switch (self.get_cpu().screen.get_ppu_mode()) {
                    .oam_search, .lcd_operate => return,
                    .h_blank, .v_blank => self.data[addr] = value,
                }
            },
            .VRAM => {
                switch (self.get_cpu().screen.get_ppu_mode()) {
                    .lcd_operate => return,
                    .h_blank, .v_blank, .oam_search => self.data[addr] = value,
                }
            },
        }
    }

    /// Return true if memory region is readable
    /// This switch quite simple and does not represent the actual readability of the GB rom
    fn can_read(addr: u16) bool {
        return switch (addr) {
            0x0000...0xDFFF => true,
            0xE000...0xFDFF => false,
            0xFE00...0xFE9F => true,
            0xFEA0...0xFEFF => false,
            0xFF00...0xFFFF => true,
        };
    }

    fn addr_type(addr: u16) Memory_type {
        return switch (addr) {
            0x0000...0x7FFF => return .ROM,
            0x8000...0x9FFF => return .VRAM,
            0xA000...0xDFFF => return .RAM,
            0xE000...0xFDFF => return .UNUSED,
            0xFE00...0xFE9F => return .OAM,
            0xFEA0...0xFEFF => return .UNUSED,
            0xFF00...0xFF0E => return .REGISTER,
            0xFF0F => return .INTERRUPT,
            0xFF10...0xFF7F => .REGISTER,
            0xFF80...0xFFFE => return .RAM,
            0xFFFF => return .INTERRUPT,
        };
    }

    fn execute_dma(self: *Memory) void {
        const source = @as(u16, self.read_at(0xFF46)) << 8;
        @memcpy(self.data[0xFE00..0xFE9F], self.data[source..(source + 0x9F)]);
        self.get_cpu().clock.tick(160);
    }
    fn write_mask(self: *Memory, addr: u16, value: u8, mask: u8) void {
        self.data[addr] = (self.data[addr] & ~mask) | (value & mask);
    }

    pub fn print_mem(self: *Memory, start: u16, end: u16) void {
        if (start == end) {
            std.debug.print("0x{x:0>2} (0b{b:0>8})\n", .{ self.data[start], self.data[start] });
        } else {
            var i = start;
            while (i < end) : (i +|= 1) {
                std.debug.print("  [0x{x:0>4}]: 0x{x:0>2} (0b{b:0>8})\n", .{ i, self.data[i], self.data[i] });
            }
            std.debug.print("  [0x{x:0>4}]: 0x{x:0>2} (0b{b:0>8})\n", .{ i, self.data[i], self.data[i] });
        }
    }
};

const Memory_type = enum { ROM, RAM, UNUSED, ECHO, REGISTER, VRAM, OAM, INTERRUPT };
