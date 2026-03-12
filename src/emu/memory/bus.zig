const std = @import("std");
const hardware = @import("../hardware.zig");
const constants = @import("../const.zig");

const Cartridge = @import("cartridge.zig").Cartridge;
const CPU = @import("../cpu/cpu.zig").CPU;
const PPU = @import("../ppu/ppu.zig").PPU;
const APU = @import("../apu/apu.zig").APU;

pub const Bus = struct {
    bios: ?[]u8,
    wram: [0x2000]u8 = [_]u8{0} ** 0x2000,
    hram: [0x7F]u8 = [_]u8{0} ** 0x7F,
    cartridge: Cartridge,
    ppu: PPU,
    apu: APU,
    allocator: std.mem.Allocator,

    fn get_cpu(self: *Bus) *CPU {
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
            0xFF10...0xFF26 => self.apu.audio_registers[address - 0xFF10] = value,
            0xFF40...0xFF4B => self.ppu.ppu_registers[address - 0xFF40] = value,
            0xFF80...0xFFFE => self.hram[address - 0xFF80] = value,
            else => |addr| {
                std.debug.panic("Error: Unhandled write at memory address: 0x{x:0>4}\n", .{addr});
            },
        }
    }

    pub fn read_at(self: *Bus, address: u16) u8 {
        return switch (address) {
            0x0000...0x00FF => return self.bios.?[address],
            0x0100...0x7FFF => return self.cartridge.rom.?[address],
            0xFF40...0xFF4B => return self.ppu.ppu_registers[address - 0xFF40],
            0xFF80...0xFFFE => return self.hram[address - 0xFF80],
            else => |addr| {
                std.debug.panic("Error: Unhandled read at memory address: 0x{x:0>4}\n", .{addr});
            },
        };
    }

    pub fn read_u8(self: *Bus) u8 {
        self.get_cpu().registers.pc += 1;
        return self.read_at(self.get_cpu().registers.pc - 1);
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
        const sp = &self.get_cpu().registers.sp;
        const l = @as(u16, self.read_at(sp.*));
        sp.* += 1;
        const h = @as(u16, self.read_at(sp.*));
        sp.* += 1;
        return h << 8 | l;
    }

    pub fn push(self: *Bus, value: u16) void {
        const sp = &self.get_cpu().registers.sp;
        sp.* -= 1;
        self.write_at(sp.*, @truncate(value >> 8));
        sp.* -= 1;
        self.write_at(sp.*, @truncate(value));
    }

    pub fn loadBios(self: *Bus) error{BiosNotFound}!void {
        var model = self.get_cpu().model;
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
