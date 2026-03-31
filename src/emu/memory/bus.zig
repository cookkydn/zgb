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
    cartridge: Cartridge,
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
            .cartridge = Cartridge.init(allocator),
            .bios = null,
            .ppu = PPU.init(.DMG0, allocator),
            .apu = APU.init(),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Bus) void {
        self.cartridge.deinit(self.allocator);
        self.ppu.deinit(self.allocator);
        self.apu.deinit();
    }

    pub fn write_at(self: *Bus, address: u16, value: u8) void {
        switch (address) {
            // -- ROM --
            0x0000...0x7FFF => return,
            // -- VRAM --
            0x8000...0x9FFF => self.ppu.vram[address - 0x8000] = value,
            // -- Cartridge RAM --
            0xA000...0xBFFF => self.cartridge.ram.?[address - 0xA000] = value,
            // -- Work ram --
            0xC000...0xDFFF => {
                self.wram[address - 0xC000] = value;
                self.invalidate_cache = true;
            },
            // -- Echo ram --
            0xE000...0xFDFF => self.wram[address - 0xE000] = value,
            // -- OAM --
            0xFE00...0xFE9F => self.ppu.oam[address - 0xFE00] = value,
            // -- Not usable --
            0xFEA0...0xFEFF => return,
            // -- Registers --
            0xFF00 => {
                self.joypad.p1_joyp = alu.masked_write(self.joypad.p1_joyp, 0x30, value);
                self.joypad.update_reg();
            },
            0xFF01...0xFF02 => self.dumb_registers[address - 0xFF00] = value,
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
            0xFF10...0xFF3F => self.apu.audio_registers[address - 0xFF10] = value,
            0xFF40 => self.ppu.lcdc = value,
            0xFF41 => self.ppu.stat = alu.masked_write(self.ppu.stat, 0x78, value),
            0xFF42 => self.ppu.scy = value,
            0xFF43 => self.ppu.scx = value,
            0xFF44 => {}, // Ly (readonly)
            0xFF45 => self.ppu.lyc = value,
            0xFF46 => {
                self.oam_src = value;
                const transfer_src: u16 = @as(u16, value) << 8;
                for (0..0xA0) |i| {
                    self.ppu.oam[i] = self.read_at(transfer_src + @as(u16, @truncate(i)));
                }
                self.timer.tick(640);
                self.invalidate_cache = true;
            },
            0xFF47 => self.ppu.bgp = value,
            0xFF48 => self.ppu.obp0 = value,
            0xFF49 => self.ppu.obp1 = value,
            0xFF4A => self.ppu.window_y = value,
            0xFF4B => self.ppu.window_x = value,
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
                self.hram[address - 0xFF80] = value;
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
                return self.cartridge.rom.?[address];
            },
            0x0100...0x7FFF => return if (self.cartridge.rom) |rom| rom[address] else 0xFF,
            0x8000...0x9FFF => return self.ppu.vram[address - 0x8000],
            0xA000...0xBFFF => return 0xFF, // RAM (TODO Implement MBC With RAM)
            0xC000...0xCFFF => return self.wram[address - 0xC000],
            0xD000...0xDFFF => return self.wram[address - 0xC000],
            0xE000...0xFDFF => return self.wram[address - 0xE000], // ECHO RAM
            0xFE00...0xFE9F => return self.ppu.oam[address - 0xFE00], // OAM
            0xFEA0...0xFEFF => return 0xFF, // Prohibited
            0xFF00 => return self.joypad.p1_joyp,
            0xFF01 => 0xFF, //TODO Serial
            0xFF02 => 0xFF, //TODO Serial
            0xFF04 => return self.timer.div,
            0xFF0F => return self.getCpu().interrupts.IF,
            0xFF40 => return self.ppu.lcdc,
            0xFF41 => return self.ppu.stat,
            0xFF42 => return self.ppu.scy,
            0xFF43 => return self.ppu.scx,
            0xFF44 => return self.ppu.ly,
            0xFF46 => return self.oam_src,
            0xFF47 => return self.ppu.bgp,
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

        const file = std.fs.cwd().openFile(path, .{}) catch return error.BiosNotFound;
        defer file.close();
        const rom = file.readToEndAlloc(self.allocator, model.getBiosSize()) catch return error.BiosNotFound;
        self.bios = rom;
    }

    fn unloadBios(self: *Bus) void {
        self.cartridge.allocator.free(self.cartridge.rom);
    }

    pub fn reloadCartridge(self: *Bus) !void {
        try self.loadCartridge(self.cartridge.rom_path);
    }

    pub fn loadCartridge(
        self: *Bus,
        path: []const u8,
    ) error{CartridgeNotFound}!void {
        const file = std.fs.cwd().openFile(path, .{ .mode = .read_only }) catch return error.CartridgeNotFound;
        defer file.close();
        const stat = file.stat() catch @panic("Cannot get file stat\n");
        var reader = std.fs.File.reader(file, &.{});
        const rom = reader.interface.readAlloc(self.allocator, @intCast(stat.size)) catch return error.CartridgeNotFound;
        self.cartridge.rom = rom;

        var cartridge_type = hardware.MBCType.from_byte(rom[constants.CartridgeType]);

        const ram_size = cartridge_type.getRamSize();
        const ram = self.allocator.alloc(u8, ram_size) catch @panic("Cannot allocate ram: OutOfMemory error\n");
        self.cartridge.ram = ram;
        self.cartridge.rom_path = self.allocator.dupe(u8, path) catch @panic("Failed to copy path");
        self.getCpu().registers.pc = 0;
    }
};
