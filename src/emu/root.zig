pub const arithmetics = @import("cpu/arithmetics.zig");
const instr_mod = @import("cpu/instructions.zig");

pub const CPU = @import("cpu/cpu.zig").Cpu;
pub const Bus = @import("memory/bus.zig").Bus;
pub const Apu = @import("apu/apu.zig").Apu;
pub const Registers = @import("cpu/registers.zig").Registers;
pub const Constants = @import("const.zig");
pub const Interrupts = @import("cpu/interrupts.zig").Interrupts;
pub const Cartridge = @import("memory/cartridge.zig").Cartridge;
pub const Timer = @import("io/timer.zig").Timer;
pub const Joypad = @import("io/joypad.zig").Joypad;
const PPU = @import("ppu/ppu.zig").Ppu;
pub const Instruction = instr_mod.Instruction;
pub const InstructionEntry = instr_mod.InstructionEntry;
pub const R8 = instr_mod.R8;
pub const R16 = instr_mod.R16;
pub const R16Mem = instr_mod.R16Mem;
pub const R16Stk = instr_mod.R16Stk;
pub const Cond = instr_mod.Cond;
const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Gameboy = struct {
    allocator: Allocator,
    cpu: CPU,
    bus: Bus,
    ppu: PPU,
    apu: Apu,
    int: Interrupts,
    timer: Timer,
    joypad: Joypad,

    pub fn init(all: Allocator) Gameboy {
        var bus = Bus.init();
        bus.loadBios() catch |err| {
            std.debug.panic("Failed to load bios: {s}\n", .{@errorName(err)});
        };
        return .{
            .allocator = all,
            .cpu = CPU.init(.dmg_0),
            .ppu = PPU.init(.dmg_0, all),
            .bus = bus,
            .apu = Apu.init(all),
            .int = Interrupts{},
            .timer = Timer{},
            .joypad = Joypad{},
        };
    }

    pub fn deinit(self: *@This()) void {
        self.apu.deinit();
        self.ppu.deinit();
        self.bus.deinit();
    }

    pub inline fn getGB(comptime field_name: []const u8, child_ptr: anytype) *Gameboy {
        return @alignCast(@fieldParentPtr(field_name, child_ptr));
    }
};
