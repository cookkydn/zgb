const alu = @import("../cpu/arithmetics.zig");
const std = @import("std");

const Cartridge = @import("cartridge.zig");
const Ppu = @import("../ppu/ppu.zig").Ppu;
const Timer = @import("../io/timer.zig").Timer;
const Joypad = @import("../io/joypad.zig").Joypad;
const Gameboy = @import("../root.zig").Gameboy;
const GbModel = @import("../hardware.zig").GbModel;

pub const Bus = @This();

bios: ?[]u8,
wram: [0x2000]u8 = [_]u8{0} ** 0x2000,
hram: [0x7F]u8 = [_]u8{0} ** 0x7F,
dumb_registers: [0x77]u8 = .{0} ** 0x77,
oam_src: u8 = 0,
cartridge: ?Cartridge,
allocator: std.mem.Allocator,
invalidate_cache: bool = false,

is_bios_loaded: bool = true,

test_is_flat_mem_enabled: bool = false,
flat_mem: ?[]u8 = null,

pub fn init(all: std.mem.Allocator) Bus {
    // std.log.info("Loading bus", .{});
    return .{
        .cartridge = null,
        .bios = null,
        .allocator = all,
    };
}

pub fn init_flat_mem(self: *Bus, all: std.mem.Allocator) !void {
    const mem = try all.alloc(u8, 0x10000);
    self.flat_mem = mem;
    @memset(mem, 0);
    self.test_is_flat_mem_enabled = true;
    // std.log.warn("Bus: Flat memory enabled, others components will not work", .{});
}

pub fn deinit(self: *Bus) void {
    if (self.cartridge) |cartridge| {
        cartridge.deinit();
    }
    if (self.bios) |bios| {
        self.allocator.free(bios);
    }
}

pub fn write_at(self: *Bus, addr: u16, value: u8) void {
    if (self.test_is_flat_mem_enabled) {
        if (self.flat_mem) |mem| {
            mem[addr] = value;
        } else {
            std.log.err("Bus: attempted to write to flat mem without allocating it first", .{});
        }
        return;
    }
    const gb = Gameboy.getGB("bus", self);
    switch (addr) {
        // -- ROM --
        0x0000...0x1FFF => return,
        0x2000...0x3FFF => {
            if (self.cartridge) |cartridge| {
                switch (cartridge.mbc_type) {
                    .no_mbc => return,
                    .mbc_1, .mbc_1_with_ram, .mbc_1_with_ram_and_battery => self.cartridge.?.rom_bank = @truncate((value & 0x1F)),
                    .other => return,
                }
            }
        },
        0x4000...0x7FFF => return,
        // -- VRAM --
        0x8000...0x9FFF => gb.ppu.write_vram(addr, value),
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
        0xFE00...0xFE9F => gb.ppu.oam[addr - 0xFE00] = value,
        // -- Not usable --
        0xFEA0...0xFEFF => return,
        // -- Registers --
        0xFF00 => {
            gb.joypad.p1_joyp = alu.maskedWrite(gb.joypad.p1_joyp, 0x30, value);
            gb.joypad.updateReg();
        },
        0xFF01...0xFF02 => self.dumb_registers[addr - 0xFF00] = value,
        0xFF03 => {}, //unused
        0xFF04 => {
            gb.timer.div = 0;
            gb.timer.div_counter = 0;
        },
        0xFF05 => {
            gb.timer.tima = value;
            gb.timer.tima_counter = 0;
        },
        0xFF06 => gb.timer.tma = value,
        0xFF07 => gb.timer.tac = value,
        0xFF08...0xFF0E => {}, // unused
        0xFF0F => gb.cpu.int.if_reg = value,
        0xFF10 => gb.apu.nr10 = value,
        0xFF11 => gb.apu.nr11 = value,
        0xFF12 => gb.apu.nr12 = value,
        0xFF13 => gb.apu.nr13 = value,
        0xFF14 => {
            if (value & 0x80 > 0) gb.apu.trigger_ch1();
            gb.apu.nr14 = alu.maskedWrite(gb.apu.nr14, 0xC7, value);
        },
        0xFF15 => {}, // unused
        0xFF16 => gb.apu.nr21 = value,
        0xFF17 => {
            // Turn of channel 2
            if (value & 0xF8 == 0) {
                gb.apu.nr52 &= 0xFD;
            }
            gb.apu.nr22 = value;
        },
        0xFF18 => gb.apu.nr23 = value,
        0xFF19 => {
            if (value & 0x80 > 0) gb.apu.trigger_ch2();
            gb.apu.nr24 = alu.maskedWrite(gb.apu.nr24, 0xC7, value);
        },
        0xFF1A => gb.apu.nr30 = value,
        0xFF1B => gb.apu.nr31 = value,
        0xFF1C => gb.apu.nr32 = value,
        0xFF1D => gb.apu.nr33 = value,
        0xFF1E => {
            gb.apu.nr34 = value;
            if (value & 0x80 > 0) gb.apu.trigger_ch3();
        },
        0xFF1F...0xFF25 => gb.apu.audio_registers[addr - 0xFF10] = value,
        0xFF26 => {
            if (value & 0x80 == 0) gb.apu.turn_off();

            gb.apu.nr52 = alu.maskedWrite(gb.apu.nr52, 0x80, value);
        },
        0xFF27...0xFF2F => gb.apu.audio_registers[addr - 0xFF10] = value,
        0xFF30...0xFF3F => gb.apu.wave_pattern[addr - 0xFF30] = value,
        0xFF40...0xFF4B => gb.ppu.write_registers(addr, value),
        0xFF4C => {}, // CGB only
        0xFF4D => {}, // CGB only
        0xFF4E => {}, // unused
        0xFF4F => {}, // CGB only
        0xFF50 => {
            self.is_bios_loaded = false;
            if (self.bios) |bios| {
                self.allocator.free(bios);
                self.bios = null;
            }
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
        0xFFFF => gb.cpu.int.ie_reg = value,
    }
}

pub fn read_at(self: *Bus, address: u16) u8 {
    if (self.test_is_flat_mem_enabled) {
        if (self.flat_mem) |mem| {
            return mem[address];
        } else {
            std.debug.panic("Attempted to read to flat mem without allocating it first", .{});
        }
    }
    const gb = Gameboy.getGB("bus", self);
    return switch (address) {
        0x0000...0x00FF => {
            if (self.is_bios_loaded) {
                return self.bios.?[address];
            }
            return self.cartridge.?.rom[address];
        },
        0x0100...0x3FFF => {
            if (self.cartridge) |cartridge| {
                return cartridge.rom[address];
            } else return 0xFF;
        },
        0x4000...0x7FFF => {
            switch (self.cartridge.?.mbc_type) {
                .no_mbc => return self.cartridge.?.rom[address],
                .mbc_1, .mbc_1_with_ram, .mbc_1_with_ram_and_battery => {
                    var bank: usize = self.cartridge.?.rom_bank;
                    if (bank == 0) bank = 1;

                    bank |= (@as(usize, self.cartridge.?.ram_bank) << 5);

                    const offset_in_bank = address - 0x4000;

                    const num_banks = self.cartridge.?.rom.len / 0x4000;
                    const safe_bank = if (num_banks > 0) bank % num_banks else 1;

                    const physical_address = (safe_bank * 0x4000) + offset_in_bank;

                    return self.cartridge.?.rom[physical_address];
                },

                .other => return 0xFF,
            }
        },
        0x8000...0x9FFF => return gb.ppu.read_vram(address),
        0xA000...0xBFFF => {
            if (self.cartridge.?.ram.len != 0) {
                return self.cartridge.?.ram[address - 0xA000];
            } else return 0xFF;
        },
        0xC000...0xCFFF => return self.wram[address - 0xC000],
        0xD000...0xDFFF => return self.wram[address - 0xC000],
        0xE000...0xFDFF => return self.wram[address - 0xE000], // ECHO RAM
        0xFE00...0xFE9F => return gb.ppu.oam[address - 0xFE00], // OAM
        0xFEA0...0xFEFF => return 0xFF, // Prohibited
        0xFF00 => return gb.joypad.p1_joyp,
        0xFF01 => 0xFF, //TODO Serial
        0xFF02 => 0xFF, //TODO Serial
        0xFF04 => return gb.timer.div,
        0xFF05 => return gb.timer.tima,
        0xFF0F => return gb.cpu.int.if_reg,
        0xFF16 => return gb.apu.nr21 | 0x3F,
        0xFF17 => return gb.apu.nr22,
        0xFF18 => return 0xFF,
        0xFF19 => return gb.apu.nr24 | 0xBF,
        0xFF1A => return gb.apu.nr30,
        0xFF20 => return 0xFF, // Unused
        0xFF24 => return gb.apu.nr50,
        0xFF25 => return gb.apu.nr51,
        0xFF26 => return gb.apu.nr52,
        0xFF40...0xFF4B => return gb.ppu.read_registers(address),
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
        0xFFFF => return gb.cpu.int.ie_reg,
        else => |addr| {
            std.debug.panic("Error: Unhandled read at memory address: 0x{x:0>4}\n", .{addr});
        },
    };
}

pub fn read_u8(self: *Bus) u8 {
    const gb = Gameboy.getGB("bus", self);
    gb.cpu.reg.pc +%= 1;
    return self.read_at(gb.cpu.reg.pc -% 1);
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
    const gb = Gameboy.getGB("bus", self);
    const sp = &gb.cpu.reg.sp;
    const l = @as(u16, self.read_at(sp.*));
    sp.* +%= 1;
    const h = @as(u16, self.read_at(sp.*));
    sp.* +%= 1;
    return h << 8 | l;
}

pub fn push(self: *Bus, value: u16) void {
    const gb = Gameboy.getGB("bus", self);
    const sp = &gb.cpu.reg.sp;
    sp.* -%= 1;
    self.write_at(sp.*, @truncate(value >> 8));
    sp.* -%= 1;
    self.write_at(sp.*, @truncate(value));
    self.invalidate_cache = false;
}

pub fn loadBios(self: *Bus, io: std.Io, model: GbModel) !void {
    const path = switch (model) {
        .dmg_0 => "./bios/dmg0.rom",
    };
    self.bios = try std.Io.Dir.cwd().readFileAlloc(io, path, self.allocator, .limited(model.biosSize() + 1));
}
