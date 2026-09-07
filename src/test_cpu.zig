const std = @import("std");
const Io = std.Io;
const emu = @import("emu/root.zig");

var current_init_values: MemValues = std.mem.zeroes(MemValues);
var current_instr: emu.Instruction = .nop;

pub fn main(init: std.process.Init) !void {
    std.log.info("Testing CPU instructions...", .{});
    const io = init.io;
    const dir = try Io.Dir.cwd().openDir(io, "sm83/v1", .{ .iterate = true });
    defer dir.close(io);
    var it = dir.iterate();
    while (try it.next(io)) |file| {
        if (file.kind == .file) {
            try test_file(io, dir, file.name, init.gpa);
        }
    }
}

const Test = struct { name: []u8, initial: MemValues, final: MemValues };
const MemValues = struct {
    pc: u16,
    sp: u16,
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
    ram: [][]u16,
};

fn test_file(io: Io, dir: Io.Dir, filename: []const u8, all: std.mem.Allocator) !void {
    std.log.info("Loading {s}", .{filename});
    const file_data = try dir.readFileAlloc(io, filename, all, .unlimited);
    defer all.free(file_data);
    const json = try std.json.parseFromSlice([]Test, all, file_data, .{
        .ignore_unknown_fields = true,
    });

    defer json.deinit();
    const ocTests = json.value;
    for (ocTests) |ocTest| {
        std.log.info("Testing: {s}", .{ocTest.name});
        var gb = emu.Gameboy.init(all, io);
        try gb.bus.init_flat_mem(all);
        defer {
            if (gb.bus.flat_mem) |mem| {
                all.free(mem);
            }
        }
        defer gb.deinit();
        const ini = ocTest.initial;
        current_init_values = ini;
        gb.cpu.reg.pc = ini.pc;
        gb.cpu.reg.sp = ini.sp;
        gb.cpu.reg.a = ini.a;
        gb.cpu.reg.b = ini.b;
        gb.cpu.reg.c = ini.c;
        gb.cpu.reg.d = ini.d;
        gb.cpu.reg.e = ini.e;
        gb.cpu.reg.f.setF(ini.f);
        gb.cpu.reg.h = ini.h;
        gb.cpu.reg.l = ini.l;
        for (ini.ram) |ramElem| {
            gb.bus.write_at(ramElem[0], @truncate(ramElem[1]));
        }
        const instr = emu.Instruction.fromBus(&gb.bus);
        current_instr = instr;
        _ = gb.cpu.execute_instruction(instr, &gb.bus);

        checkFinalState(&gb, ocTest.final);
    }
    std.log.info("All test passed", .{});
}

fn checkFinalState(gb: *emu.Gameboy, final: MemValues) void {
    const reg = gb.cpu.reg;
    // assertEq("PC", reg.pc, final.pc);
    assertEq("SP", reg.sp, final.sp);
    assertEq("A", reg.a, final.a);
    assertEq("B", reg.b, final.b);
    assertEq("C", reg.c, final.c);
    assertEq("D", reg.d, final.d);
    assertEq("E", reg.e, final.e);
    assertEq("F", reg.f.getF(), @as(u16, final.f));
    assertEq("H", reg.h, final.h);
    assertEq("L", reg.l, final.l);
    for (final.ram) |ramElem| {
        assertEq("RAM", gb.bus.read_at(ramElem[0]), @as(u8, @truncate(ramElem[1])));
    }
}

inline fn assertEq(name: []const u8, a: anytype, b: anytype) void {
    if (a != b) {
        std.log.err("Init: {any}", .{current_init_values});
        std.log.err("Instr: {any}", .{current_instr});
        std.debug.panic("Assertion failed: {s}: {d}!={d}", .{ name, a, b });
    }
}
