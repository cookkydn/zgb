const std = @import("std");
const Allocator = std.mem.Allocator;
const cpu_mod = @import("cpu.zig");
const Cart = @import("cart.zig").Cart;
const CPU = cpu_mod.CPU;
const Instruction = @import("./cpu/instructions.zig").Instruction;
const Registers = cpu_mod.Registers;
const Debugger = @import("debugger.zig").Debugger;
const Memory = @import("mem.zig").Memory;
const Reader = @import("mem/reader.zig").Reader;

pub const Bus = @import("mem/bus.zig").Bus;

pub const Emulator = struct {
    cpu: CPU,
    reader: Reader,
    debugger: Debugger,
    allocator: Allocator,

    pub fn start(self: *Emulator) !void {
        self.reader = Reader.init(&self.cpu.mem);
        self.reader.seek = &self.cpu.registers.pc;
    }

    pub fn init(allocator: Allocator) !Emulator {
        var cpu = try CPU.init(allocator);
        const debugger = Debugger.init(&cpu, allocator);

        return .{
            .allocator = allocator,
            .cpu = cpu,
            .debugger = debugger,
            .reader = undefined,
        };
    }

    pub fn load_cart(self: *Emulator, comptime path: []const u8) !void {
        try Cart.load_cart(path, &self.cpu.mem);
        Cart.set_initial_registers(&self.cpu.mem);
    }

    pub fn deinit(self: *Emulator) void {
        self.cpu.deinit(self.allocator);
        self.debugger.deinit();
    }

    pub fn step(self: *Emulator) void {
        self.cpu.int.handle_interrupts();
        self.cpu.joypad.handle_events();
        if (self.cpu.pause) return;

        self.cpu.clock.reset();
        _ = self.cpu.mem.read_at(self.cpu.registers.pc);
        const instr = Instruction.from_mem(&self.reader);
        // std.debug.print("bc: 0x{x:0>4} hl: 0x{x:0>4} pc: 0x{x:0>4} sp: 0x{x:0>4} ime: {} ", .{ self.cpu.registers.get_bc(), self.cpu.registers.get_hl(), self.cpu.registers.pc, self.cpu.registers.sp, self.cpu.int.ime });
        // std.debug.print("-> (0x{x:0>2}) {f} ", .{ instr_code, instr });
        if (instr == .invalid) {
            self.cpu.quit = true;
        } else {
            self.cpu.execute_instruction(instr);
        }
        self.cpu.clock.tick_clock();
    }
};
