const std = @import("std");
const cpu_mod = @import("cpu.zig");
const load_cart = @import("cart.zig").load_cart;
const set_init = @import("cart.zig").set_initial_hard_reg;
const CPU = cpu_mod.CPU;
const Instruction = @import("instructions.zig").Instruction;
const Registers = cpu_mod.Registers;
const Screen = @import("screen.zig").Screen;
const Debugger = @import("debugger.zig").Debugger;
pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var cpu = try CPU.init(allocator);
    var debugger = Debugger.init(&cpu, allocator);
    defer cpu.deinit(allocator);
    load_cart("./carts/tetris.gb", &cpu.mem);
    set_init(&cpu.mem);
    defer debugger.deinit();
    try cpu.screen.render();
    main: while (true) {
        if (try debugger.is_breakpoints()) {
            const input = try debugger.read_input();
            try debugger.handle_input(input);
            debugger.replace_instruction();
        }
        if (cpu.quit) break :main;
        if (debugger.can_execute) {
            cpu.joypad.handle_events();
            if (cpu.pause) continue;
            cpu.int.handle_interrupts();
            if (cpu.int.ime_to_set) {
                cpu.int.ime_to_set = false;
                cpu.int.ime = true;
            }

            cpu.clock.reset();
            std.debug.print("pc: 0x{x:0>4} sp: 0x{x:0>4} ime: {} ", .{ cpu.registers.pc, cpu.registers.sp, cpu.int.ime });
            if (!cpu.int.halted) {
                const ins = cpu.read_instruction();
                std.debug.print("Executing: {s:<12} ", .{@tagName(ins)});
                if (ins == .breakpoint) {
                    cpu.registers.pc -|= 1;
                    std.debug.print("\n", .{});
                    const input = try debugger.read_input();
                    try debugger.handle_input(input);
                    debugger.replace_instruction();
                }
                cpu.execute_instruction(ins);
            } else {
                std.debug.print("HALTED IF: 0b{b:0>8} IE: 0b{b:0>8} ", .{ cpu.int.get_if(), cpu.int.get_ie() });
                cpu.clock.tick(4);
            }
            cpu.screen.lcd_tick(cpu.clock.i);
            std.debug.print("\n", .{});
        }
    }
}

fn print_mem(start: u16, end: u16, cpu: *CPU) void {
    var i = start;
    std.debug.print("{{\n", .{});
    while (i < end) {
        std.debug.print("  [0x{x}]:0x{x},\n", .{ i, cpu.mem.read_at(i) });
        i +|= 1;
    }
    std.debug.print("  [0x{x}]:0x{x}\n}}\n", .{ i, cpu.mem.read_at(i) });
}
