const instr_mod = @import("cpu/instructions.zig");
pub const arithmetics = @import("cpu/arithmetics.zig");

pub const CPU = @import("cpu/cpu.zig").CPU;
pub const Bus = @import("memory/bus.zig").Bus;
pub const Registers = @import("cpu/registers.zig").Registers;
pub const Constants = @import("const.zig");
pub const Cartridge = @import("memory/cartridge.zig").Cartridge;
const PPU = @import("ppu/ppu.zig").PPU;
pub const Instruction = instr_mod.Instruction;
pub const InstructionEntry = instr_mod.InstructionEntry;
pub const R8 = instr_mod.R8;
pub const R16 = instr_mod.R16;
pub const R16Mem = instr_mod.R16Mem;
pub const R16Stk = instr_mod.R16Stk;
pub const Cond = instr_mod.Cond;
// TODO Switch to Emulator instead of having everything in the Bus
// const std = @import("std");
// const Allocator = std.mem.Allocator;

// pub const Emulator = struct {
//     allocator: Allocator,
//     ppu: PPU,

//     pub fn init(allocator: Allocator) Emulator {
//         return .{
//             .allocator = allocator,
//             .ppu = PPU.init(.DMG0, allocator),
//         };
//     }

//     pub fn deinit(self: *@This()) void {
//         self.ppu.deinit();
//     }

//     pub inline fn get_emu(comptime field_name: []const u8, child_ptr: anytype) *Emulator {
//         return @alignCast(@fieldParentPtr(field_name, child_ptr));
//     }
// };
