const std = @import("std");
const hardware = @import("../hardware.zig");
const constants = @import("../const.zig");
const alu = @import("../cpu/arithmetics.zig");

const Cartridge = @import("cartridge.zig").Cartridge;
const CPU = @import("../cpu/cpu.zig").CPU;
const PPU = @import("../ppu/ppu.zig").PPU;
const APU = @import("../apu/apu.zig").APU;
const Timer = @import("../io/timer.zig").Timer;
const Joypad = @import("../io/joypad.zig").Joypad;

pub const Bus = struct {
    bios: ?[]u8,
    wram: [0x2000]u8 = [_]u8{0} ** 0x2000,
    hram: [0x7F]u8 = [_]u8{0} ** 0x7F,
    dumb_registers: [0x77]u8 = .{0} ** 0x77,
    oam_src: u8 = 0,
    cartridge: ?Cartridge,
    ppu: PPU,
    apu: APU,
    allocator: std.mem.Allocator,
    timer: Timer = Timer.init(),
    invalidate_cache: bool = false,
    joypad: Joypad = Joypad{},

    is_bios: bool = true,

    pub fn getCpu(self: *Bus) *CPU {
        return @alignCast(@fieldParentPtr("bus", self));
    }

    pub fn init() Bus {
        const allocator = std.heap.page_allocator;
        return .{
            .cartridge = null,
            .bios = null,
            .ppu = PPU.init(.DMG0, allocator),
            .apu = APU.init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Bus) void {
        if (self.cartridge) |cartridge| {
            cartridge.deinit();
        }
        self.ppu.deinit();
        self.apu.deinit();
        if (self.bios) |bios| {
            self.allocator.free(bios);
        }
    }

    pub fn write_at(self: *Bus, addr: u16, value: u8) void {
        switch (addr) {
            // -- ROM --
            0x0000...0x1FFF => return,
            0x2000...0x3FFF => {
                switch (self.cartridge.?.mbc_type) {
                    .NOMBC => return,
                    .MBC1 => self.cartridge.?.rom_bank = @truncate((value & 0x1F)),
                    .Other => return,
                }
            },
            0x4000...0x7FFF => return,
            // -- VRAM --
            0x8000...0x9FFF => self.ppu.mem.write_vram(addr, value),
            // -- Cartridge RAM --
            0xA000...0xBFFF => {
                if (self.cartridge.?.ram.len != 0) {
                    self.cartridge.?.ram[addr - 0xA000] = value;
                }
            },
            // -- Work ram --
            0xC000...0xDFFF => {
                self.wram[addr - 0xC000] = value;
                self.invalidate_cache = true;
            },
            // -- Echo ram --
            0xE000...0xFDFF => self.wram[addr - 0xE000] = value,
            // -- OAM --
            0xFE00...0xFE9F => self.ppu.mem.oam[addr - 0xFE00] = value,
            // -- Not usable --
            0xFEA0...0xFEFF => return,
            // -- Registers --
            0xFF00 => {
                self.joypad.p1_joyp = alu.masked_write(self.joypad.p1_joyp, 0x30, value);
                self.joypad.update_reg();
            },
            0xFF01...0xFF02 => self.dumb_registers[addr - 0xFF00] = value,
            0xFF03 => {}, //unused
            0xFF04 => {
                self.timer.div = 0;
                self.timer.div_counter = 0;
            },
            0xFF05 => self.timer.tima = value,
            0xFF06 => self.timer.tma = value,
            0xFF07 => self.timer.tac = value,
            0xFF08...0xFF0E => {}, // unused
            0xFF0F => self.getCpu().interrupts.IF = value,
            0xFF10 => self.apu.nr10 = value,
            0xFF11 => self.apu.nr11 = value,
            0xFF12 => self.apu.nr12 = value,
            0xFF13 => self.apu.nr13 = value,
            0xFF14 => {
                if (value & 0x80 > 0) self.apu.trigger_ch1();
                self.apu.nr14 = alu.masked_write(self.apu.nr14, 0xC7, value);
            },
            0xFF15 => {}, // unused
            0xFF16 => self.apu.nr21 = value,
            0xFF17 => {
                // Turn of channel 2
                if (value & 0xF8 == 0) {
                    self.apu.nr52 &= 0xFD;
                }
                self.apu.nr22 = value;
            },
            0xFF18 => self.apu.nr23 = value,
            0xFF19 => {
                if (value & 0x80 > 0) self.apu.trigger_ch2();
                self.apu.nr24 = alu.masked_write(self.apu.nr24, 0xC7, value);
            },
            0xFF1A...0xFF25 => self.apu.audio_registers[addr - 0xFF10] = value,
            0xFF26 => {
                if (value & 0x80 == 0) self.apu.turn_off();

                self.apu.nr52 = alu.masked_write(self.apu.nr52, 0x80, value);
            },
            0xFF27...0xFF3F => self.apu.audio_registers[addr - 0xFF10] = value,
            0xFF40...0xFF4B => self.ppu.mem.write_registers(addr, value),
            0xFF4C => {}, // CGB only
            0xFF4D => {}, // CGB only
            0xFF4E => {}, // unused
            0xFF4F => {}, // CGB only
            0xFF50 => {
                self.is_bios = false;
                self.invalidate_cache = true;
            },
            0xFF51...0xFF77 => {}, // CGB only
            0xFF78...0xFF7F => {}, // Unused
            // -- High ram --
            0xFF80...0xFFFE => {
                self.hram[addr - 0xFF80] = value;
                self.invalidate_cache = true;
            },
            // -- IE --
            0xFFFF => self.getCpu().interrupts.IE = value,
        }
    }

    pub fn read_at(self: *Bus, address: u16) u8 {
        return switch (address) {
            0x0000...0x00FF => {
                if (self.is_bios) {
                    return self.bios.?[address];
                }
                return self.cartridge.?.rom[address];
            },
            0x0100...0x3FFF => return self.cartridge.?.rom[address],
            0x4000...0x7FFF => {
                switch (self.cartridge.?.mbc_type) {
                    .NOMBC => return self.cartridge.?.rom[address],
                    .MBC1 => {
                        var bank: usize = self.cartridge.?.rom_bank;
                        if (bank == 0) bank = 1;

                        bank |= (@as(usize, self.cartridge.?.ram_bank) << 5);

                        const offset_in_bank = address - 0x4000;

                        const num_banks = self.cartridge.?.rom.len / 0x4000;
                        const safe_bank = if (num_banks > 0) bank % num_banks else 1;

                        const physical_address = (safe_bank * 0x4000) + offset_in_bank;

                        return self.cartridge.?.rom[physical_address];
                    },

                    .Other => return 0xFF,
                }
            },
            0x8000...0x9FFF => return self.ppu.mem.read_vram(address),
            0xA000...0xBFFF => return 0xFF, // RAM (TODO Implement MBC With RAM)
            0xC000...0xCFFF => return self.wram[address - 0xC000],
            0xD000...0xDFFF => return self.wram[address - 0xC000],
            0xE000...0xFDFF => return self.wram[address - 0xE000], // ECHO RAM
            0xFE00...0xFE9F => return self.ppu.mem.oam[address - 0xFE00], // OAM
            0xFEA0...0xFEFF => return 0xFF, // Prohibited
            0xFF00 => return self.joypad.p1_joyp,
            0xFF01 => 0xFF, //TODO Serial
            0xFF02 => 0xFF, //TODO Serial
            0xFF04 => return self.timer.div,
            0xFF0F => return self.getCpu().interrupts.IF,
            0xFF16 => return self.apu.nr21 | 0x3F,
            0xFF17 => return self.apu.nr22,
            0xFF18 => return 0xFF,
            0xFF19 => return self.apu.nr24 | 0xBF,
            0xFF20 => return 0xFF, // Unused
            0xFF40...0xFF4B => return self.ppu.mem.read_registers(address),
            0xFF4D => return 0xFF, // CGB only
            0xFF57...0xFF67 => return 0xFF, // Unused
            0xFF68 => return 0xFF, // CGB only
            0xFF69 => return 0xFF, // CGB only
            0xFF6A => return 0xFF, // CGB only
            0xFF6B => return 0xFF, // CGB only
            0xFF6C => return 0xFF, // CGB only
            0xFF6D...0xFF6F => return 0xFF, // Unused
            0xFF70 => return 0xFF, // CGB only
            0xFF71...0xFF75 => return 0xFF, //Unused
            0xFF76 => return 0xFF, // TODO SOUND
            0xFF77 => return 0xFF, // TODO SOUND
            0xFF78...0xFF7F => return 0xFF, // Unused
            0xFF80...0xFFFE => return self.hram[address - 0xFF80],
            0xFFFF => return self.getCpu().interrupts.IE,
            else => |addr| {
                std.debug.panic("Error: Unhandled read at memory address: 0x{x:0>4}\n", .{addr});
            },
        };
    }

    pub fn read_u8(self: *Bus) u8 {
        self.getCpu().registers.pc += 1;
        return self.read_at(self.getCpu().registers.pc - 1);
    }

    pub fn read_i8(self: *Bus) i8 {
        return @bitCast(self.read_u8());
    }

    pub fn read_u16(self: *Bus) u16 {
        const lsb = @as(u16, self.read_u8());
        const msb = @as(u16, self.read_u8());
        const imm16 = (msb << 8) | lsb;
        return imm16;
    }

    pub fn pop(self: *Bus) u16 {
        const sp = &self.getCpu().registers.sp;
        const l = @as(u16, self.read_at(sp.*));
        sp.* += 1;
        const h = @as(u16, self.read_at(sp.*));
        sp.* += 1;
        return h << 8 | l;
    }

    pub fn push(self: *Bus, value: u16) void {
        const sp = &self.getCpu().registers.sp;
        sp.* -= 1;
        self.write_at(sp.*, @truncate(value >> 8));
        sp.* -= 1;
        self.write_at(sp.*, @truncate(value));
        self.invalidate_cache = false;
    }

    pub fn loadBios(self: *Bus) error{BiosNotFound}!void {
        var model = self.getCpu().model;
        const path = switch (model) {
            .DMG0 => "./bios/dmg0.rom",
        };

        self.bios = std.fs.cwd().readFileAlloc(self.allocator, path, model.bios_size()) catch return error.BiosNotFound;
    }
};
