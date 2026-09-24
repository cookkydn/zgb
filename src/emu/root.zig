// Exports
// pub const arithmetics = @import("cpu/arithmetics.zig");
// pub const Registers = @import("cpu/registers.zig").Registers;
// pub const Constants = @import("const.zig");
// pub const R8 = instr_mod.R8;
// pub const R16 = instr_mod.R16;
// pub const R16Mem = instr_mod.R16Mem;
// pub const R16Stk = instr_mod.R16Stk;
// pub const Cond = instr_mod.Cond;
// const GbModel = @import("./hardware.zig").GbModel;

pub const Emulator = @This();
pub const Cartridge = @import("cartridge.zig");
pub const Instruction = instr_mod.Instruction;
pub const JoypadBtn = Joypad.JoypadBtn;
pub const Ppu = @import("ppu/ppu.zig");
pub const InstructionWithSize = instr_mod.InstructionWithSize;

const Apu = @import("apu/apu.zig");
const Bus = @import("bus.zig");
const CPU = @import("alu/cpu.zig");
const instr_mod = @import("alu/instructions.zig");
const Joypad = @import("io/joypad.zig");
const std = @import("std");
const Timer = @import("io/timer.zig");

const Io = std.Io;
const log = std.log.scoped(.zgb);
const Allocator = std.mem.Allocator;

allocator: Allocator,
io: Io,
cpu: CPU,
apu: Apu,
bus: Bus,
ppu: Ppu,
timer: Timer,
joypad: Joypad,

pub fn init(all: Allocator, io: Io) !Emulator {
    log.info("ZGB init", .{});
    return .{
        .allocator = all,
        .io = io,
        .cpu = CPU.init(),
        .bus = Bus.init(all),
        .ppu = try Ppu.init(all),
        .timer = Timer{},
        .apu = Apu.init(all),
        .joypad = Joypad{},
    };
}

pub fn deinit(self: *Emulator) void {
    log.info("ZGB deinit", .{});
    self.bus.deinit();
    self.ppu.deinit();
    self.apu.deinit();
}

pub inline fn getGB(comptime field_name: []const u8, child_ptr: anytype) *Emulator {
    return @alignCast(@fieldParentPtr(field_name, child_ptr));
}

// pub const Gameboy = struct {
//     allocator: Allocator,
//     cpu: CPU,
//     bus: Bus,
//     ppu: PPU,
//     apu: Apu,
//     timer: Timer,
//     joypad: Joypad,

//     pub fn init(all: Allocator, io: std.Io) Gameboy {
//         const model: GbModel = .dmg_0;
//         // std.log.info("Initializing ZGB emu\n\tmodel: {s}", .{@tagName(model)});
//         var bus = Bus.init(all);
//         bus.loadBios(io, model) catch |err| {
//             std.debug.panic("Failed to load bios: {s}\n", .{@errorName(err)});
//         };
//         return .{
//             .allocator = all,
//             .cpu = CPU.init(model),
//             .ppu = PPU.init(model, all) catch @panic("Failed to init PPU"),
//             .bus = bus,
//             .apu = Apu.init(all),
//             .timer = Timer{},
//             .joypad = Joypad{},
//         };
//     }

//     pub fn deinit(self: *@This()) void {
//         // std.log.info("Emulator deinit", .{});
//         self.apu.deinit();
//         self.ppu.deinit(self.allocator);
//         self.bus.deinit();
//     }

//     pub inline fn getGB(comptime field_name: []const u8, child_ptr: anytype) *Gameboy {
//         return @alignCast(@fieldParentPtr(field_name, child_ptr));
//     }
// };
