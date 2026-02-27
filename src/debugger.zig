const CPU = @import("cpu.zig").CPU;
const std = @import("std");
const print = std.debug.print;

const Debugger_instruction = enum {
    unknown,
    vblank,
    quit,
    exit,
    reg,
    step,
    c,
    br,
    mem,
    pc,
    instr,
    ime,
};

pub const Debugger = struct {
    cpu: *CPU,
    buffer: [1024]u8,
    can_execute: bool,
    first_exec: bool,
    breakpoint_map: std.AutoHashMap(u16, u8),
    future_breakpoint: u16,
    step_count: i64,

    pub fn init(cpu: *CPU, allocator: std.mem.Allocator) Debugger {
        return .{
            .cpu = cpu,
            .buffer = undefined,
            .can_execute = true,
            .first_exec = true,
            .breakpoint_map = .init(allocator),
            .future_breakpoint = 0,
            .step_count = -1,
        };
    }

    pub fn deinit(self: *Debugger) void {
        self.breakpoint_map.deinit();
    }

    pub fn read_input(self: *Debugger) ![]u8 {
        print("> ", .{});
        var stdin_reader = std.fs.File.stdin().reader(&self.buffer);
        const input = try stdin_reader.interface.takeDelimiterExclusive('\n');
        return input;
    }

    pub fn handle_input(self: *Debugger, input: []u8) !void {
        var split_input = std.mem.splitSequence(u8, input, " ");
        const instr = std.meta.stringToEnum(Debugger_instruction, split_input.next().?) orelse Debugger_instruction.unknown;
        switch (instr) {
            .vblank => {
                try self.set_breakpoint(0x40);
            },
            .quit, .exit => {
                self.cpu.quit = true;
            },
            .reg => {
                self.cpu.registers.dump();
                self.can_execute = false;
            },
            .step => {
                const count = std.fmt.parseInt(u32, split_input.next() orelse &[_]u8{'1'}, 10) catch 1;
                self.step_count = @as(i64, count);
            },
            .c => {
                self.can_execute = true;
            },
            .pc => {
                print("pc: 0x{x:0>4}\n", .{self.cpu.registers.pc});
                self.can_execute = false;
            },
            .ime => {
                print("ime: {}\n", .{self.cpu.int.ime});
            },
            .br => {
                self.can_execute = false;
                const addr_str = split_input.next() orelse {
                    print("Missing argument: address\n", .{});
                    return;
                };
                const addr = std.fmt.parseInt(u16, addr_str, 16) catch {
                    print("Invalid argument: address\n", .{});
                    return;
                };
                try self.set_breakpoint(addr);
            },
            .instr => {
                const curr_instr = self.cpu.take_instruction();
                print("{f}\n", .{curr_instr});

                self.can_execute = false;
            },
            .mem => {
                self.can_execute = false;
                const start_addr_str = split_input.next() orelse {
                    print("Missing argument: start address\n", .{});
                    return;
                };
                const start_addr = std.fmt.parseInt(u16, start_addr_str, 16) catch {
                    print("Invalid argument: start address\n", .{});
                    return;
                };
                const end_addr_str = split_input.next() orelse start_addr_str;
                const end_addr = std.fmt.parseInt(u16, end_addr_str, 16) catch {
                    print("Invalid argument: end address\n", .{});
                    return;
                };
                self.cpu.mem.print_mem(start_addr, end_addr);
            },
            .unknown => {
                print("Unknown command\n", .{});
                self.can_execute = false;
            },
        }
    }

    pub fn is_breakpoints(self: *Debugger) !bool {
        if (self.step_count >= 0) {
            self.step_count -= 1;
        }
        if (self.step_count == 0) {
            self.step_count = -1;
            return true;
        }
        if (self.future_breakpoint != 0 and self.cpu.registers.pc != self.future_breakpoint) {
            try self.set_breakpoint(self.future_breakpoint);
            self.future_breakpoint = 0;
        }
        if (self.first_exec) {
            self.first_exec = false;
            return true;
        } else if (!self.can_execute) {
            self.can_execute = true;
            return true;
        }
        return false;
    }

    pub fn set_breakpoint(self: *Debugger, addr: u16) !void {
        if (addr == self.cpu.registers.pc -| 1) {
            self.future_breakpoint = addr;
            return;
        }
        try self.breakpoint_map.put(addr, self.cpu.mem.data[addr]);
        self.cpu.mem.data[addr] = 0x40;
    }

    pub fn replace_instruction(self: *Debugger) void {
        if (self.breakpoint_map.contains(self.cpu.registers.pc)) {
            const pc = self.cpu.registers.pc;
            self.cpu.mem.data[pc] = self.breakpoint_map.get(pc) orelse 0x10;
            _ = self.breakpoint_map.remove(pc);
        }
    }
};
