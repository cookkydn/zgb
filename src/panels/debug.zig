const Gameboy = @import("../emu/root.zig").Gameboy;

const ig = @import("cimgui");
const sokol = @import("sokol");
const sg = sokol.gfx;
const std = @import("std");
const LayoutManager = @import("../ui/layout.zig").LayoutManager;
const AppState = @import("../app.zig").AppState;
const NO_FLAG = ig.ImGuiWindowFlags_None;

pub const DebugPanel = struct {
    visible: bool = false,

    backend_name: [*c]const u8,

    pub fn init() DebugPanel {
        return .{ .backend_name = "Unknown" };
    }

    pub fn setBackendName(self: *DebugPanel) void {
        self.backend_name = switch (sg.queryBackend()) {
            .D3D11 => "Direct3D11",
            .GLCORE => "OpenGL",
            .GLES3 => "OpenGLES3",
            .METAL_IOS => "Metal iOS",
            .METAL_MACOS => "Metal macOS",
            .METAL_SIMULATOR => "Metal Simulator",
            .WGPU => "WebGPU",
            .VULKAN => "Vulkan",
            .DUMMY => "Dummy",
        };
    }

    pub fn draw(self: *DebugPanel, app: *AppState) void {
        const gb = &app.emu.gb;
        const cpu = &gb.cpu;

        if (!self.visible) return;

        defer ig.igEnd();
        if (!ig.igBegin(LayoutManager.Panels.debug, &self.visible, NO_FLAG)) return;

        if (ig.igCollapsingHeader("Informations", ig.ImGuiTreeNodeFlags_DefaultOpen)) {
            ig.igText("Dear ImGui: %s", ig.IMGUI_VERSION);
            ig.igText("Sokol Backend: %s", self.backend_name);
            ig.igText("FPS: %.1f", ig.igGetIO().*.Framerate);
            ig.igText("GB Model: %s", @tagName(cpu.model).ptr);
        }

        if (ig.igCollapsingHeader("CPU Registers", ig.ImGuiTreeNodeFlags_DefaultOpen)) {
            if (ig.igBeginTable("cpu_regs", 2, ig.ImGuiTableFlags_None)) {
                defer ig.igEndTable();

                ig.igTableNextRow();
                _ = ig.igTableNextColumn();
                ig.igText("PC: 0x%04X", cpu.reg.pc);
                _ = ig.igTableNextColumn();
                ig.igText("SP: 0x%04X", cpu.reg.sp);

                ig.igTableNextRow();
                _ = ig.igTableNextColumn();
                ig.igText("AF: 0x%04X", cpu.reg.getAF());
                _ = ig.igTableNextColumn();
                ig.igText("BC: 0x%04X", cpu.reg.getBC());

                ig.igTableNextRow();
                _ = ig.igTableNextColumn();
                ig.igText("DE: 0x%04X", cpu.reg.getDE());
                _ = ig.igTableNextColumn();
                ig.igText("HL: 0x%04X", cpu.reg.getHL());
            }

            ig.igSeparator();
            ig.igText("Flags:  Z:%d  N:%d  H:%d  C:%d", cpu.reg.f.z, cpu.reg.f.n, cpu.reg.f.h, cpu.reg.f.c);

            ig.igText("State: ");
            ig.igSameLine();
            if (cpu.state.halted) {
                ig.igTextColored(.{ .x = 1.0, .y = 0.5, .z = 0.0, .w = 1.0 }, "HALTED");
            } else {
                ig.igTextColored(.{ .x = 0.2, .y = 1.0, .z = 0.2, .w = 1.0 }, "RUNNING");
            }

            ig.igText("IME:   ");
            ig.igSameLine();
            switch (cpu.state.ime) {
                .ENABLED => ig.igTextColored(.{ .x = 0.2, .y = 1.0, .z = 0.2, .w = 1.0 }, "ENABLED"),
                .ENABLED_NEXT => ig.igTextColored(.{ .x = 0.6, .y = 1.0, .z = 0.2, .w = 1.0 }, "ENABLED (next)"),
                .DISABLED => ig.igTextColored(.{ .x = 1.0, .y = 0.2, .z = 0.2, .w = 1.0 }, "DISABLED"),
            }
        }

        if (ig.igCollapsingHeader("Timer", ig.ImGuiTreeNodeFlags_DefaultOpen)) {
            var buf: [32]u8 = undefined;
            if (ig.igBeginTable("timer_regs", 2, ig.ImGuiTableFlags_None)) {
                defer ig.igEndTable();

                ig.igTableNextRow();
                _ = ig.igTableNextColumn();
                ig.igText("DIV:  0x%04X", gb.timer.div);
                _ = ig.igTableNextColumn();
                ig.igText("TIMA: 0x%02X", gb.timer.tima);

                ig.igTableNextRow();
                _ = ig.igTableNextColumn();
                ig.igText("TMA:  0x%02X", gb.timer.tma);
                _ = ig.igTableNextColumn();
                ig.igText("TAC:  %s", (std.fmt.bufPrintZ(&buf, "{b:0>8}", .{gb.timer.tac}) catch unreachable).ptr);
            }
        }

        if (ig.igCollapsingHeader("Hardware (PPU & Int)", ig.ImGuiTreeNodeFlags_DefaultOpen)) {
            var buf: [64]u8 = undefined;

            ig.igText("IE:   %s", (std.fmt.bufPrintZ(&buf, "{b:0>8}", .{cpu.int.ie_reg}) catch unreachable).ptr);
            ig.igText("IF:   %s", (std.fmt.bufPrintZ(&buf, "{b:0>8}", .{cpu.int.if_reg}) catch unreachable).ptr);
            ig.igSeparator();

            ig.igText("LCDC: %s", (std.fmt.bufPrintZ(&buf, "{b:0>8}", .{gb.ppu.mem.lcdc}) catch unreachable).ptr);
            ig.igText("STAT: %s", (std.fmt.bufPrintZ(&buf, "{b:0>8}", .{gb.ppu.mem.stat}) catch unreachable).ptr);

            ig.igSeparator();
            ig.igText("LY:   %03d (0x%02X)", gb.ppu.mem.ly, gb.ppu.mem.ly);
            ig.igText("LYC:  %03d (0x%02X)", gb.ppu.mem.lyc, gb.ppu.mem.lyc);

            ig.igSeparator();
            if (ig.igBeginTable("ppu_table", 2, ig.ImGuiTableFlags_None)) {
                defer ig.igEndTable();

                ig.igTableNextRow();
                _ = ig.igTableNextColumn();
                ig.igText("SCX:  %03d", gb.ppu.mem.scx);
                _ = ig.igTableNextColumn();
                ig.igText("SCY:  %03d", gb.ppu.mem.scy);

                ig.igTableNextRow();
                _ = ig.igTableNextColumn();
                ig.igText("WX:   %03d", gb.ppu.mem.wx);
                _ = ig.igTableNextColumn();
                ig.igText("WY:   %03d", gb.ppu.mem.wy);
            }

            ig.igSeparator();
            ig.igText("BGP:  %s", (std.fmt.bufPrintZ(&buf, "{b:0>8}", .{gb.ppu.mem.bgp}) catch unreachable).ptr);
            ig.igText("OBP0: %s", (std.fmt.bufPrintZ(&buf, "{b:0>8}", .{gb.ppu.mem.obp0}) catch unreachable).ptr);
            ig.igText("OBP1: %s", (std.fmt.bufPrintZ(&buf, "{b:0>8}", .{gb.ppu.mem.obp1}) catch unreachable).ptr);
        }

        if (ig.igCollapsingHeader("Cartridge", ig.ImGuiTreeNodeFlags_DefaultOpen)) {
            if (gb.bus.cartridge) |cart| {
                ig.igText("Loaded: %s", cart.filename.ptr);
                ig.igText("ROM Bank: %d", @as(u8, cart.rom_bank));
                ig.igText("RAM Bank: %d", @as(u8, cart.ram_bank));
            } else {
                ig.igTextColored(.{ .x = 1.0, .y = 0.2, .z = 0.2, .w = 1.0 }, "No cartridge loaded");
            }
        }

        if (ig.igCollapsingHeader("Joypad", ig.ImGuiTreeNodeFlags_DefaultOpen)) {
            var buf: [32]u8 = undefined;
            ig.igText("JOYP: %s", (std.fmt.bufPrintZ(&buf, "{b:0>8}", .{gb.joypad.p1_joyp}) catch unreachable).ptr);
        }
    }
};
