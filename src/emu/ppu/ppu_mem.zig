const std = @import("std");
const Emulator = @import("../root.zig").Emulator;
const alu = @import("../cpu/arithmetics.zig");
const Bus = @import("../memory/bus.zig").Bus;
const PPU = @import("ppu.zig").Ppu;
const GbModel = @import("../hardware.zig").GbModel;
const Gameboy = @import("../root.zig").Gameboy;
const Allocator = std.mem.Allocator;

pub const PpuMem = struct {
    allocator: Allocator,
    // emu: *Emulator,
    // -- Memory Regions --

    /// Vram `0x8000` to `0x9FFF`
    ///
    /// Switchable bank in CGB (*todo*)
    vram: []u8,

    /// OAM `0xFE00` to `0xFE9F`
    oam: []u8,

    // -- Registers --

    /// LCD Control `0xFF40` (*R/W*)
    lcdc: u8 = 0,

    /// LCD Status `0xFF41` (*Mixed*)
    stat: u8 = 0,

    /// SCY `0xFF42` (*R/W*)
    scy: u8 = 0,

    /// SCX `0xFF43` (*R/W*)
    scx: u8 = 0,

    /// LY `0xFF44`(*R*)
    ly: u8 = 0,

    /// LYC `0xFF45` (*R/W*)
    lyc: u8 = 0,

    /// DMA src address & start `0xFF46`(*R/W*)
    dma: u8 = 0,

    /// BG palette data `0xFF47` (*R/W*)
    ///
    /// **DMG only**
    bgp: u8 = 0,

    /// Object palette 0 data `0xFF48` (*R/W*)
    ///
    /// **DMG only**
    obp0: u8 = 0,

    /// Object palette 1 data `0xFF49` (*R/W*)
    ///
    /// **DMG only**
    obp1: u8 = 0,

    /// Window y position `0xFF4A` (*R/W*)
    wy: u8 = 0,

    /// Window x position `0xFF4B` (*R/W*)
    ///
    /// *note: add 7 to this value*
    wx: u8 = 0,

    pub fn init(
        model: GbModel,
        allocator: Allocator,
        // emu: *Emulator
    ) !PpuMem {
        const vram = try allocator.alloc(u8, model.vramSize());
        const oam = try allocator.alloc(u8, 0xA0);
        return .{
            .allocator = allocator,
            // .emu = emu,
            .vram = vram,
            .oam = oam,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.allocator.free(self.vram);
        self.allocator.free(self.oam);
    }

    pub fn write_vram(self: *@This(), addr: u16, value: u8) void {
        // TODO CGB VRAM Banking
        self.vram[addr - 0x8000] = value;
    }

    pub fn read_vram(self: *@This(), addr: u16) u8 {
        return self.read_vram_slice(addr, 1)[0];
    }

    pub fn read_vram_slice(self: *PpuMem, addr: u16, length: u16) []u8 {
        // TODO CGB VRAM Banking
        const addr_offset = addr - 0x8000;
        return self.vram[addr_offset..(addr_offset + length)];
    }

    /// Handle writes for the memory region `0xFF40` to `0xFF4B`
    pub fn write_registers(self: *@This(), addr: u16, value: u8) void {
        switch (addr) {
            0xFF40 => self.lcdc = value,
            0xFF41 => self.stat = alu.maskedWrite(self.stat, 0x78, value),
            0xFF42 => self.scy = value,
            0xFF43 => self.scx = value,
            0xFF44 => {}, // readonly
            0xFF45 => self.lyc = value,
            0xFF46 => {
                self.dma = value;
                self.execute_dma();
            },
            0xFF47 => self.bgp = value,
            0xFF48 => self.obp0 = value,
            0xFF49 => self.obp1 = value,
            0xFF4A => self.wy = value,
            0xFF4B => self.wx = value,
            else => unreachable,
        }
    }

    /// Handle reads for the memory region `0xFF40` to `0xFF4B`
    pub fn read_registers(
        self: *@This(),
        addr: u16,
    ) u8 {
        switch (addr) {
            0xFF40 => return self.lcdc,
            0xFF41 => return self.stat,
            0xFF42 => return self.scy,
            0xFF43 => return self.scx,
            0xFF44 => return self.ly,
            0xFF45 => return self.lyc,
            0xFF46 => return self.dma,
            0xFF47 => return self.bgp,
            0xFF48 => return self.obp0,
            0xFF49 => return self.obp1,
            0xFF4A => return self.wy,
            0xFF4B => return self.wx,
            else => unreachable,
        }
    }

    /// Execute DMA transfers
    ///
    /// Copy from `[DMA*256]` to `[DMA*256] + 160` into OAM
    fn execute_dma(self: *@This()) void {
        const ppu: *PPU = @alignCast(@fieldParentPtr("mem", self));
        const gb = Gameboy.getGB("ppu", ppu);
        const transfer_src: u16 = @as(u16, self.dma) << 8;
        for (0..0xA0) |i| {
            self.oam[i] = gb.bus.read_at(transfer_src + @as(u16, @truncate(i)));
        }
        gb.timer.tick(640);
    }
};
