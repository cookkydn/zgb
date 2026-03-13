const std = @import("std");
const hardware = @import("../hardware.zig");
const constants = @import("../const.zig");
const alu = @import("../cpu/arithmetics.zig");

const Cartridge = @import("cartridge.zig").Cartridge;
const CPU = @import("../cpu/cpu.zig").CPU;
const PPU = @import("../ppu/ppu.zig").PPU;
const APU = @import("../apu/apu.zig").APU;
const Timer = @import("../io/timer.zig").Timer;

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

    is_bios: bool = true,

    fn getCpu(self: *Bus) *CPU {
        return @alignCast(@fieldParentPtr("bus", self));
    }

    pub fn init() Bus {
        const allocator = std.heap.page_allocator;
        return .{
            .cartridge = Cartridge.init(),
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
            0x0000...0x7FFF => return, // ROM
            0x8000...0x9FFF => self.ppu.vram[address - 0x8000] = value,
            0xC000...0xDFFF => self.wram[address - 0xC000] = value,
            0xFE00...0xFE9F => self.ppu.oam[address - 0xFE00] = value,
            0xFEA0...0xFEFF => return, // Not usable
            0xFF00 => return, // TODO
            0xFF01...0xFF02 => self.dumb_registers[address - 0xFF00] = value,
            0xFF04 => {
                self.timer.div = 0;
                self.timer.div_counter = 0;
            },
            0xFF05 => self.timer.tima = value,
            0xFF06 => self.timer.tma = value,
            0xFF07 => self.timer.tac = value,
            0xFF0F => self.getCpu().interrupts.IF = value,
            0xFF10...0xFF26 => self.apu.audio_registers[address - 0xFF10] = value,
            0xFF40 => self.ppu.lcdc = value,
            0xFF41 => self.ppu.stat = alu.masked_write(self.ppu.stat, 0x78, value),
            0xFF42 => self.ppu.scy = value,
            0xFF43 => self.ppu.scx = value,
            0xFF44 => return, // Ly (readonly)
            0xFF46 => {
                self.oam_src = value;
                const transfer_src: u16 = @as(u16, value) << 8;
                for (0..0x9F) |i| {
                    self.ppu.oam[i] = self.read_at(transfer_src + @as(u16, @truncate(i)));
                }
                self.timer.tick(640);
            },
            0xFF47 => self.ppu.bg_palette_data = value,
            0xFF48 => self.ppu.obj_palette_0 = value,
            0xFF49 => self.ppu.obj_palette_1 = value,
            0xFF4A => self.ppu.window_y = value,
            0xFF4B => self.ppu.window_x = value,
            0xFF50 => self.is_bios = false,
            0xFF7F => return, // Unused
            0xFF80...0xFFFE => self.hram[address - 0xFF80] = value,
            0xFFFF => self.getCpu().interrupts.IE = value,
            else => |addr| {
                std.debug.panic("Error: Unhandled write at memory address: 0x{x:0>4}\n", .{addr});
            },
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
            0x0100...0x7FFF => return self.cartridge.rom.?[address],

            0xC000...0xCFFF => return self.wram[address - 0xC000],
            0xD000...0xDFFF => return self.wram[address - 0xC000],
            0xFF00 => return 0x3F, //TODO
            0xFF40 => return self.ppu.lcdc,
            0xFF41 => return self.ppu.stat,
            0xFF42 => return self.ppu.scy,
            0xFF43 => return self.ppu.scx,
            0xFF44 => return self.ppu.ly,
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

    pub fn loadCartridge(
        self: *Bus,
        name: []const u8,
    ) error{CartridgeNotFound}!void {
        const path = std.fs.path.join(self.allocator, &.{ ".", "carts", name }) catch @panic("Cannot load cartridge, outOfMemory error\n");
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
    }
};
