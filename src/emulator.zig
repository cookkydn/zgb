const std = @import("std");
const Allocator = std.mem.Allocator;
const cpu_mod = @import("cpu.zig");
const Cart = @import("cart.zig").Cart;
const CPU = cpu_mod.CPU;
const Instruction = @import("./cpu/instructions.zig").Instruction;
const Registers = cpu_mod.Registers;
const Screen = @import("screen.zig").Screen;
const Debugger = @import("debugger.zig").Debugger;
const Memory = @import("mem.zig").Memory;
const Reader = @import("mem/reader.zig").Reader;

pub const Emulator = struct {
    cpu: CPU,
    reader: Reader,
    debugger: Debugger,
    allocator: Allocator,

    pub fn run(self: *Emulator) !void {
        try self.cpu.screen.render();
        self.reader = Reader.init(&self.cpu.mem);
        self.reader.seek = &self.cpu.registers.pc;
        while (true) {
            if (self.cpu.quit) break;
            std.debug.print("bc: 0x{x:0>4} hl: 0x{x:0>4} pc: 0x{x:0>4} sp: 0x{x:0>4} ime: {} ", .{ self.cpu.registers.get_bc(), self.cpu.registers.get_hl(), self.cpu.registers.pc, self.cpu.registers.sp, self.cpu.int.ime });
            self.step();

            // if (!self.cpu.int.halted) {
            //     const ins = self.cpu.read_instruction();
            //     std.debug.print("Executing: {s:<12} ", .{@tagName(ins)});
            //     // if (ins == .breakpoint) {
            //     //     self.cpu.registers.pc -|= 1;
            //     //     std.debug.print("\n", .{});
            //     //     self.debugger.replace_instruction();
            //     //     const input = try self.debugger.read_input();
            //     //     try self.debugger.handle_input(input);
            //     // }
            //     self.cpu.execute_instruction(ins);
            // } else {
            //     std.debug.print("HALTED IF: 0b{b:0>8} IE: 0b{b:0>8} ", .{ self.cpu.int.get_if(), self.cpu.int.get_ie() });
            //     self.cpu.clock.tick_emu(4);
            // }
            self.cpu.screen.lcd_tick(self.cpu.clock.i);
            self.cpu.clock.tick_clock();
            std.debug.print("\n", .{});
        }
        // }
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

    fn step(self: *Emulator) void {
        self.cpu.int.handle_interrupts();
        self.cpu.joypad.handle_events();
        if (self.cpu.pause) return;

        self.cpu.clock.reset();
        const instr = Instruction.from_mem(&self.reader);
        std.debug.print("-> (0x{x:0>2}) {f} ", .{ self.cpu.mem.read_at(self.cpu.registers.pc), instr });
        if (instr == .invalid) {
            self.cpu.quit = true;
        } else {
            self.cpu.execute_instruction(instr);
        }
    }
};
