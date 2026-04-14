const Cpu = @import("../emu/root.zig").CPU;

const ig = @import("cimgui");
const sokol = @import("sokol");
const sg = sokol.gfx;
const sapp = sokol.app;
const std = @import("std");

const NO_FLAG = ig.ImGuiWindowFlags_None;

pub const DebugWindow = struct {
    visible: bool = false,
    cpu: *Cpu,

    backendName: [*c]const u8,

    pub fn init(cpu: *Cpu) DebugWindow {
        return .{
            .cpu = cpu,
            .backendName = switch (sg.queryBackend()) {
                .D3D11 => "Direct3D11",
                .GLCORE => "OpenGL",
                .GLES3 => "OpenGLES3",
                .METAL_IOS => "Metal iOS",
                .METAL_MACOS => "Metal macOS",
                .METAL_SIMULATOR => "Metal Simulator",
                .WGPU => "WebGPU",
                .VULKAN => "Vulkan",
                .DUMMY => "Dummy",
            },
        };
    }

    pub fn frame(self: *DebugWindow) void {
        if (!self.visible) return;

        defer ig.igEnd();
        if (!ig.igBegin("Debug info", &self.visible, NO_FLAG)) return;
        if (ig.igBeginTabBar("Debug tab", ig.ImGuiTabBarFlags_None)) {
            if (ig.igBeginTabItem("Debug", null, ig.ImGuiTabBarFlags_None)) {
                ig.igText("Dear ImGui Version: %s", ig.IMGUI_VERSION);
                ig.igText("Sokol Backend: %s", self.backendName);
                ig.igText("FPS: %.f", ig.igGetIO().*.Framerate);
                ig.igText("GB model: %s", @tagName(self.cpu.model).ptr);
                ig.igEndTabItem();
            }
            if (ig.igBeginTabItem("Registers", null, ig.ImGuiTabBarFlags_None)) {
                ig.igText("PC: 0x%04x", self.cpu.registers.pc);
                ig.igText("AF: 0x%04x", self.cpu.registers.get_af());
                ig.igText("BC: 0x%04x", self.cpu.registers.get_bc());
                ig.igText("DE: 0x%04x", self.cpu.registers.get_de());
                ig.igText("HL: 0x%04x", self.cpu.registers.get_hl());
                ig.igText("SP: 0x%04x", self.cpu.registers.sp);
                ig.igText("IME: ");
                ig.igSameLine();
                if (self.cpu.state.ime == .ENABLED) { // En supposant que ce soit un booléen
                    ig.igTextColored(.{ .x = 0.2, .y = 1.0, .z = 0.2, .w = 1.0 }, "ENABLED"); // Vert
                } else if (self.cpu.state.ime == .ENABLED_NEXT) {
                    ig.igTextColored(.{ .x = 0.2, .y = 1.0, .z = 0.2, .w = 1.0 }, "ENABLED (next)"); // Vert
                } else {
                    ig.igTextColored(.{ .x = 1.0, .y = 0.2, .z = 0.2, .w = 1.0 }, "DISABLED"); // Rouge
                }
                ig.igText(
                    "F: z %i n %i h %i c %i",
                    self.cpu.registers.f.z,
                    self.cpu.registers.f.n,
                    self.cpu.registers.f.h,
                    self.cpu.registers.f.c,
                );
                ig.igEndTabItem();
            }
            if (ig.igBeginTabItem("Memory Registers", null, ig.ImGuiTabBarFlags_None)) {
                var buf: [64]u8 = undefined;
                const s_lcdc = std.fmt.bufPrintZ(&buf, "LCDC: {b:0>8}", .{self.cpu.bus.ppu.lcdc}) catch unreachable;
                ig.igText("%s", s_lcdc.ptr);
                const s_ly = std.fmt.bufPrintZ(&buf, "LY:   {b:0>8} (0x{x})", .{ self.cpu.bus.ppu.ly, self.cpu.bus.ppu.ly }) catch unreachable;
                ig.igText("%s", s_ly.ptr);
                const s_ie = std.fmt.bufPrintZ(&buf, "IE:   {b:0>8}", .{self.cpu.interrupts.IE}) catch unreachable;
                ig.igText("%s", s_ie.ptr);
                const s_if = std.fmt.bufPrintZ(&buf, "IF:   {b:0>8}", .{self.cpu.interrupts.IF}) catch unreachable;
                ig.igText("%s", s_if.ptr);
                ig.igText("SCX: %i", self.cpu.bus.ppu.scx);
                ig.igText("SCY: %i", self.cpu.bus.ppu.scy);
                ig.igEndTabItem();
            }
            if (ig.igBeginTabItem("Joypad", null, ig.ImGuiTabBarFlags_None)) {
                var buf: [64]u8 = undefined;
                const s_joyp = std.fmt.bufPrintZ(&buf, "Joy: {b:0>8}", .{self.cpu.bus.joypad.p1_joyp}) catch unreachable;
                ig.igText("%s", s_joyp.ptr);
                ig.igEndTabItem();
            }
        }
        ig.igEndTabBar();
    }
};
