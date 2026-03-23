const use_docking = @import("build_options").docking;
const std = @import("std");
const ig = if (use_docking) @import("cimgui_docking") else @import("cimgui");
const emu = @import("emu/root.zig");
const Instruction = emu.Instruction;
const Bus = emu.Bus;
const sokol = @import("sokol");
const sapp = sokol.app;
const InstructionEntry = emu.InstructionEntry;
const AddressList = std.ArrayList(u16);
const BitSet = std.bit_set.ArrayBitSet(usize, WORKING_SIZE);

const panic = std.debug.panic;
const offset_by = emu.arithmetics.offset_by;

const WORKING_SIZE = 0xFFFF;
const MAX_INSTR_ANALYSIS_PER_FRAME = 20;
const OutOfMemoryMessage = "Out of memory error in decompiler\n";

pub const Decompiler = struct {
    bus: *Bus,
    allocator: std.mem.Allocator,
    show_decompiler: bool = true,
    last_pc: u16 = 0,
    is_code: BitSet = BitSet.initEmpty(),
    instruction_cache: [WORKING_SIZE]?InstructionEntry = .{null} ** WORKING_SIZE,
    display_list: AddressList,
    analysis_queue: AddressList,
    focus: bool = false,

    tabs: struct {
        decompiler: bool = true,
        tasks: bool = true,
    } = .{},

    pub fn init(all: std.mem.Allocator, bus: *Bus) Decompiler {
        var analysis_queue = AddressList.initCapacity(all, 128) catch panic(OutOfMemoryMessage, .{});
        analysis_queue.append(all, 0) catch panic(OutOfMemoryMessage, .{});
        return .{
            .display_list = AddressList.initCapacity(all, WORKING_SIZE) catch panic(OutOfMemoryMessage, .{}),
            .analysis_queue = analysis_queue,
            .allocator = all,
            .bus = bus,
        };
    }

    pub fn init_analysis(self: *Decompiler) void {
        self.is_code = BitSet.initEmpty();
        self.analysis_queue.clearRetainingCapacity();
        self.analysis_queue.append(self.allocator, 0) catch panic(OutOfMemoryMessage, .{});
        self.analysis_queue.append(self.allocator, 0x100) catch panic(OutOfMemoryMessage, .{});
    }

    pub fn analyse_branch(self: *Decompiler) void {
        var addr = self.analysis_queue.pop() orelse return;
        var instr_count: usize = 0;
        while (!self.is_code.isSet(addr)) {
            const entry = Instruction.take_at(self.bus, addr);
            self.is_code.set(addr);
            self.instruction_cache[addr] = entry;
            addr += entry.size;
            switch (entry.instruction) {
                .jr_cond_imm8 => |arg| {
                    const dest_addr = offset_by(addr, arg.offset);
                    self.analysis_queue.append(self.allocator, dest_addr) catch panic(OutOfMemoryMessage, .{});
                    self.analysis_queue.append(self.allocator, addr) catch panic(OutOfMemoryMessage, .{});
                    break;
                },
                .jr_imm8 => |arg| {
                    const dest_addr = offset_by(addr, arg.offset);
                    self.analysis_queue.append(self.allocator, dest_addr) catch panic(OutOfMemoryMessage, .{});
                    self.analysis_queue.append(self.allocator, addr) catch panic(OutOfMemoryMessage, .{});
                    break;
                },
                .call_imm16 => |arg| {
                    self.analysis_queue.append(self.allocator, arg.imm16) catch panic(OutOfMemoryMessage, .{});
                    self.analysis_queue.append(self.allocator, addr) catch panic(OutOfMemoryMessage, .{});
                    break;
                },
                .jp_cond_imm16 => |arg| {
                    self.analysis_queue.append(self.allocator, arg.imm16) catch panic(OutOfMemoryMessage, .{});
                    self.analysis_queue.append(self.allocator, addr) catch panic(OutOfMemoryMessage, .{});
                    break;
                },
                .jp_imm16 => |arg| {
                    self.analysis_queue.append(self.allocator, arg.imm16) catch panic(OutOfMemoryMessage, .{});
                    break;
                },
                .call_cond_imm16 => |arg| {
                    self.analysis_queue.append(self.allocator, arg.imm16) catch panic(OutOfMemoryMessage, .{});
                    self.analysis_queue.append(self.allocator, addr) catch panic(OutOfMemoryMessage, .{});
                    break;
                },
                .ret_cond => {
                    self.analysis_queue.append(self.allocator, addr) catch panic(OutOfMemoryMessage, .{});
                    break;
                },
                .rst_tgt3 => |arg| {
                    self.analysis_queue.append(self.allocator, arg.target_addr) catch panic(OutOfMemoryMessage, .{});
                    break;
                },

                .jp_hl,
                .ret,
                .reti,
                => break,
                else => {},
            }
            instr_count += 1;
            if (instr_count >= MAX_INSTR_ANALYSIS_PER_FRAME) {
                self.analysis_queue.append(self.allocator, addr) catch panic(OutOfMemoryMessage, .{});
                break;
            }
        }
    }

    pub fn update_display_list(self: *Decompiler) void {
        self.display_list.clearRetainingCapacity();
        var addr: u16 = 0;
        var line: i32 = 0;
        while (addr < WORKING_SIZE - 1) : (line += 1) {
            self.display_list.append(self.allocator, addr) catch panic(OutOfMemoryMessage, .{});
            if (self.is_code.isSet(addr)) {
                addr += (self.instruction_cache[addr] orelse InstructionEntry{ .instruction = .invalid, .size = 1 }).size;
            } else {
                addr += 1;
                if (!self.is_code.isSet(addr)) {
                    addr += 1;
                }
            }
        }
    }

    fn render_address(_: *Decompiler, addr: u16, is_pc: bool) void {
        if (is_pc) {
            ig.igTableSetBgColor(ig.ImGuiTableBgTarget_RowBg0, 0xFF333300, -1);
            ig.igTextColored(ig.ImVec4{ .x = 1.0, .y = 1.0, .z = 0.0, .w = 1 }, "-> 0x%04x", addr);
        } else {
            ig.igTextDisabled("   0x%04x", addr);
        }
    }

    fn render_instruction(self: *Decompiler, addr: u16) void {
        if (self.is_code.isSet(addr)) {
            if (self.instruction_cache[addr]) |entry| {
                switch (entry.instruction) {
                    // -- Load instructions --
                    .ld_r8_r8 => |arg| ig.igText("ld %s, %s", @tagName(arg.r8_dst).ptr, @tagName(arg.r8_src).ptr),
                    .ld_r8_imm8 => |arg| ig.igText("ld %s, %02xh", @tagName(arg.r8).ptr, arg.imm8),
                    .ld_r16_imm16 => |arg| ig.igText("ld %s, %04xh", @tagName(arg.r16).ptr, arg.imm16),
                    .ld_r16mem_a => |arg| ig.igText("ld (%s), a", @tagName(arg.r16mem).ptr),
                    .ld_imm16_a => |arg| ig.igText("ld (%04xh), a", arg.imm16),
                    .ldh_imm8_a => |arg| ig.igText("ldh (%02xh), a", arg.imm8),
                    .ldh_c_a => ig.igText("ldh (c), a"),
                    .ld_a_r16mem => |arg| ig.igText("ld a, (%s)", @tagName(arg.r16mem).ptr),
                    .ld_a_imm16 => |arg| ig.igText("ld a, (%04xh)", arg.imm16),
                    .ldh_a_imm8 => |arg| ig.igText("ldh a, (%02xh)", arg.imm8),
                    .ldh_a_c => ig.igText("ldh a, c"),

                    // -- 8 bit arithmetic --
                    .adc_a_r8 => |arg| ig.igText("adc a, %s", @tagName(arg.r8).ptr),
                    .add_a_r8 => |arg| ig.igText("add a, %s", @tagName(arg.r8).ptr),
                    .cp_a_r8 => |arg| ig.igText("cp a, %s", @tagName(arg.r8).ptr),
                    .sub_a_r8 => |arg| ig.igText("sub a, %s", @tagName(arg.r8).ptr),
                    .inc_r8 => |arg| ig.igText("inc %s", @tagName(arg.r8).ptr),
                    .dec_r8 => |arg| ig.igText("dec %s", @tagName(arg.r8).ptr),
                    .add_a_imm8 => |arg| ig.igText("add a, %02xh", arg.imm8),
                    .cp_a_imm8 => |arg| ig.igText("cp a, %02xh", arg.imm8),
                    .adc_a_imm8 => |arg| ig.igText("adc a, %02xh", arg.imm8),
                    .sub_a_imm8 => |arg| ig.igText("sub a, %02xh", arg.imm8),

                    // -- 16 bit arithmetic --
                    .add_hl_r16 => |arg| ig.igText("add hl, %s", @tagName(arg.r16).ptr),
                    .inc_r16 => |arg| ig.igText("inc %s", @tagName(arg.r16).ptr),

                    // -- Bitwise logic --
                    .and_a_r8 => |arg| ig.igText("and a, %s", @tagName(arg.r8).ptr),
                    .xor_a_r8 => |arg| ig.igText("xor a, %s", @tagName(arg.r8).ptr),
                    .and_a_imm8 => |arg| ig.igText("and a, %02xh", arg.imm8),
                    .xor_a_imm8 => |arg| ig.igText("xor a, %02xh", arg.imm8),

                    // -- Stack manipulation --
                    .pop_r16stk => |arg| ig.igText("pop %s", @tagName(arg.r16stk).ptr),
                    .push_r16stk => |arg| ig.igText("push %s", @tagName(arg.r16stk).ptr),

                    // -- Bit flag --
                    .bit_b3_r8 => |arg| ig.igText("bit %i, %s", @as(u8, arg.bit_index), @tagName(arg.r8).ptr),

                    // -- Bit shift --
                    .rl_r8 => |arg| ig.igText("rl %s", @tagName(arg.r8).ptr),
                    .rla => ig.igText("rla"),

                    // -- Jumps and subroutines --
                    .call_imm16 => |arg| ig.igText("call %04x", arg.imm16),
                    .jp_imm16 => |arg| ig.igText("jp %04xh", arg.imm16),
                    .jp_cond_imm16 => |arg| ig.igText("jp %s %04xh", @tagName(arg.cond).ptr, arg.imm16),
                    .jr_imm8 => |arg| ig.igText("jr %04xh", offset_by(@truncate(addr), arg.offset) + entry.size),
                    .jr_cond_imm8 => |arg| ig.igText("jr %s %04xh", @tagName(arg.cond).ptr, offset_by(@truncate(addr), arg.offset) + entry.size),
                    .ret => ig.igText("ret"),
                    .rst_tgt3 => |arg| ig.igText("rst %02xh", arg.target_addr),

                    // -- Miscellaneous --
                    .nop => ig.igText("nop"),

                    else => {
                        ig.igTextColored(ig.ImVec4{ .x = 1, .y = 0.4, .z = 0.4, .w = 1 }, "unknown");
                    },
                }
            } else {
                ig.igTextDisabled("loading...");
            }
        } else {
            if (!self.is_code.isSet(addr + 1)) {
                ig.igText(".dw %02x%02xh", self.bus.read_at(addr + 1), self.bus.read_at(addr));
            } else {
                ig.igText(".db %02xh", self.bus.read_at(addr));
            }
        }
    }

    fn render_hex(self: *Decompiler, addr: u16) void {
        var instruction_size: u16 = 1;
        if (self.is_code.isSet(addr)) {
            if (self.instruction_cache[addr]) |entry| {
                instruction_size = entry.size;
            }
        } else if (!self.is_code.isSet(addr + 1)) {
            instruction_size = 2;
        }
        var hex_buf: [24]u8 = undefined;
        hex_buf[0] = '(';
        var pos: usize = 1;
        for (0..instruction_size) |i| {
            const byte = self.bus.read_at(@truncate(addr + i));
            const formatted = std.fmt.bufPrint(hex_buf[pos..], "{X:0>2}", .{byte}) catch "??";
            pos += formatted.len;

            if (i < instruction_size - 1) {
                hex_buf[pos] = ' ';
                pos += 1;
            }
        }
        hex_buf[pos] = ')';
        pos += 1;
        hex_buf[pos] = 0;
        ig.igTextDisabled("%s", &hex_buf);
    }

    fn render_cell(self: *Decompiler, addr: u16, pc: u16) void {
        ig.igTableNextRow();
        if (ig.igTableNextColumn()) self.render_address(addr, addr == pc);
        if (ig.igTableNextColumn()) self.render_instruction(addr);
        if (ig.igTableNextColumn()) self.render_hex(addr);
    }

    pub fn frame(self: *Decompiler, pc: u16) void {
        if (!self.show_decompiler) return;
        defer self.last_pc = pc;
        if (self.bus.invalidate_cache) {
            self.bus.invalidate_cache = false;
            self.instruction_cache = .{null} ** WORKING_SIZE;
            self.is_code = BitSet.initEmpty();
            self.init_analysis();
        }
        if (!self.is_code.isSet(pc)) {
            self.analysis_queue.append(self.allocator, pc) catch panic(OutOfMemoryMessage, .{});
        }
        if (self.analysis_queue.items.len > 0) {
            self.analyse_branch();
            self.update_display_list();
        }

        ig.igSetNextWindowPos(.{ .x = sapp.widthf() - 270, .y = 10 }, ig.ImGuiCond_Once);
        ig.igSetNextWindowSize(.{ .x = 260, .y = 580 }, ig.ImGuiCond_Once);
        if (ig.igBegin("Decompiler", &self.show_decompiler, ig.ImGuiWindowFlags_None)) {
            if (ig.igBeginTabBar("Decompiler_tab", ig.ImGuiTabBarFlags_None)) {
                if (ig.igBeginTabItem("Instructions", &self.tabs.decompiler, ig.ImGuiTabItemFlags_None)) {
                    var clipper: ig.ImGuiListClipper = .{};
                    const diff = if (pc > self.last_pc) pc -% self.last_pc else self.last_pc -% pc;
                    if (diff > 10 or self.focus) {
                        self.focus = false;
                        const line_index = line_loop: {
                            var index: f32 = 0;
                            for (self.display_list.items) |addr| {
                                if (addr == pc) break;
                                index += 1;
                            }
                            break :line_loop index;
                        };

                        const scroll_y = line_index * ig.igGetTextLineHeightWithSpacing();
                        ig.igSetScrollY(scroll_y - (ig.igGetWindowHeight() / 2));
                    }
                    if (!ig.igBeginTable("decompiler_table", 3, ig.ImGuiTableFlags_BordersInnerH)) return;
                    ig.igTableSetupColumn("addr", ig.ImGuiTableColumnFlags_WidthFixed);
                    ig.igTableSetupColumn("instr", ig.ImGuiTableColumnFlags_WidthStretch);
                    ig.igTableSetupColumn("hex", ig.ImGuiTableColumnFlags_WidthFixed);

                    ig.ImGuiListClipper_Begin(&clipper, @intCast(self.display_list.items.len), 0);
                    while (ig.ImGuiListClipper_Step(&clipper)) {
                        var i: i32 = clipper.DisplayStart;
                        while (i < clipper.DisplayEnd) : (i += 1) {
                            const addr = self.display_list.items[@intCast(i)];
                            self.render_cell(addr, pc);
                        }
                    }
                    ig.ImGuiListClipper_End(&clipper);
                    ig.igEndTable();
                    ig.igEndTabItem();
                }
                if (ig.igBeginTabItem("Tasks", &self.tabs.tasks, ig.ImGuiTabItemFlags_None)) {
                    if (ig.igButton("Reanalyse")) {
                        self.init_analysis();
                    }
                    for (self.analysis_queue.items) |address| {
                        ig.igBulletText("0x%04x", address);
                    }
                    ig.igEndTabItem();
                }
                ig.igEndTabBar();
            }
            ig.igEnd();
        }

        ig.igSetNextWindowPos(.{ .x = sapp.widthf() - 380, .y = 10 }, ig.ImGuiCond_Once);
        ig.igSetNextWindowSize(.{ .x = 100, .y = 60 }, ig.ImGuiCond_Once);
        if (ig.igBegin("Decompiler controls", &self.show_decompiler, ig.ImGuiWindowFlags_None)) {
            if (ig.igButton("Focus")) {
                self.focus = true;
            }
            ig.igEnd();
        }
    }
};
