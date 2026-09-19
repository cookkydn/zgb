// const Gameboy = @import("../emu/root.zig").Gameboy;

// const ig = @import("cimgui");
// const sokol = @import("sokol");
// const sg = sokol.gfx;
// const std = @import("std");
// const AppState = @import("../app.zig").AppState;

// const ui = @import("ui");
// const WindowFlag = ui.flags.WindowFlag;
// const print = ui.fmt.print;

const App = @import("../app.zig");
const dvui = @import("dvui");

pub fn render(app: App, _: dvui.Rect) !void {
    const fps = dvui.FPS();
    const reg = app.emu.cpu.reg;
    dvui.label(@src(), "FPS: {d:.1}", .{fps}, .{});
    {
        var row = dvui.box(@src(), .{ .dir = .horizontal }, .{});
        defer row.deinit();

        dvui.label(@src(), "State: ", .{}, .{});

        renderStatusFlag(1, "IME", app.emu.cpu.state.ime == .ENABLED);
        renderStatusFlag(2, "HALT", app.emu.cpu.state.halted);
    }
    _ = dvui.spacer(@src(), .{ .min_size_content = .height(10) });

    // PC & SP
    {
        var row = dvui.box(@src(), .{ .dir = .horizontal }, .{});
        defer row.deinit();

        dvui.label(@src(), "PC: 0x{X:0>4}", .{reg.pc}, .{});
        dvui.label(@src(), "SP: 0x{X:0>4}", .{reg.sp}, .{});
    }

    // AF, BC, DE, HL
    {
        renderRegPair(1, "AF", reg.getAF(), "A", reg.a, "F", @truncate(reg.f.getF()));
        renderRegPair(2, "BC", reg.getBC(), "B", reg.b, "C", reg.c);
        renderRegPair(3, "DE", reg.getDE(), "D", reg.d, "E", reg.e);
        renderRegPair(4, "HL", reg.getHL(), "H", reg.h, "L", reg.l);
    }

    // Flags
    {
        var row = dvui.box(@src(), .{ .dir = .horizontal }, .{});
        defer row.deinit();
        dvui.label(@src(), "Flags:", .{}, .{});
        renderFlag(1, "Z", reg.f.z);
        renderFlag(2, "N", reg.f.n);
        renderFlag(3, "H", reg.f.h);
        renderFlag(4, "C", reg.f.c);
    }
}

fn renderRegPair(id: usize, name16: []const u8, val16: u16, nameHigh: []const u8, valHigh: u8, nameLow: []const u8, valLow: u8) void {
    var row = dvui.box(@src(), .{ .dir = .horizontal }, .{ .id_extra = id });
    defer row.deinit();

    dvui.label(@src(), "{s}: 0x{X:0>4}", .{ name16, val16 }, .{});
    dvui.label(@src(), "({s}: {X:0>2}  {s}: {X:0>2})", .{ nameHigh, valHigh, nameLow, valLow }, .{ .color_text = .{ .color = .gray } });
}

fn renderStatusFlag(id: usize, name: []const u8, active: bool) void {
    var color: dvui.ColorOrGradient = .{ .color = .gray };
    if (active) {
        color = dvui.ColorOrGradient.green;
    }

    const options = dvui.Options{
        .id_extra = id,
        .color_text = color,
    };
    dvui.label(@src(), "[{s}]", .{name}, options);
}

fn renderFlag(id: usize, name: []const u8, active: bool) void {
    var color = dvui.ColorOrGradient.white;
    if (active) {
        color = dvui.ColorOrGradient.green;
    }
    const options = dvui.Options{
        .id_extra = id,
        .color_text = color,
    };
    dvui.label(@src(), "[{s}]", .{if (active) name else "-"}, options);
}

// pub const DebugPanel = struct {
//     visible: bool = false,

//     backend_name: [*c]const u8,

//     pub fn init() DebugPanel {
//         return .{ .backend_name = "Unknown" };
//     }

//     pub fn setBackendName(self: *DebugPanel) void {
//         self.backend_name = switch (sg.queryBackend()) {
//             .D3D11 => "Direct3D11",
//             .GLCORE => "OpenGL",
//             .GLES3 => "OpenGLES3",
//             .METAL_IOS => "Metal iOS",
//             .METAL_MACOS => "Metal macOS",
//             .METAL_SIMULATOR => "Metal Simulator",
//             .WGPU => "WebGPU",
//             .VULKAN => "Vulkan",
//             .DUMMY => "Dummy",
//         };
//     }

//     pub fn draw(self: *DebugPanel, app: *AppState) void {
//         const gb = &app.emu.gb;
//         const cpu = &gb.cpu;

//         if (!self.visible) return;

//         defer ig.igEnd();
//         const flags = [_]WindowFlag{
//             .NoCollapse,
//             .NoTitleBar,
//         };
//         if (!ig.igBegin("Debug", &self.visible, ui.flags.combine(flags))) return;
//         const treeFlags = [_]ui.flags.TreeNodeFlag{
//             .DefaultOpen,
//         };
//         if (ig.igCollapsingHeader("Informations", ui.flags.combine(treeFlags))) {
//             print("Dear ImGui: {s}", .{ig.IMGUI_VERSION});
//             print("Sokol Backend: {s}", .{self.backend_name});
//             print("FPS: {d:.1}", .{ig.igGetIO().*.Framerate});
//             print("GB Model: {s}", .{@tagName(cpu.model).ptr});
//         }

//         if (ig.igCollapsingHeader("CPU Registers", ui.flags.combine(treeFlags))) {
//             if (ig.igBeginTable("cpu_regs", 2, ig.ImGuiTableFlags_None)) {
//                 defer ig.igEndTable();

//                 ig.igTableNextRow();
//                 _ = ig.igTableNextColumn();
//                 ig.igText("PC: 0x%04X", cpu.reg.pc);
//                 _ = ig.igTableNextColumn();
//                 ig.igText("SP: 0x%04X", cpu.reg.sp);

//                 ig.igTableNextRow();
//                 _ = ig.igTableNextColumn();
//                 ig.igText("AF: 0x%04X", cpu.reg.getAF());
//                 _ = ig.igTableNextColumn();
//                 ig.igText("BC: 0x%04X", cpu.reg.getBC());

//                 ig.igTableNextRow();
//                 _ = ig.igTableNextColumn();
//                 ig.igText("DE: 0x%04X", cpu.reg.getDE());
//                 _ = ig.igTableNextColumn();
//                 ig.igText("HL: 0x%04X", cpu.reg.getHL());
//             }

//             ig.igSeparator();
//             ig.igText("Flags:  Z:%d  N:%d  H:%d  C:%d", cpu.reg.f.z, cpu.reg.f.n, cpu.reg.f.h, cpu.reg.f.c);

//             ig.igText("State: ");
//             ig.igSameLine();
//             if (cpu.state.halted) {
//                 ig.igTextColored(.{ .x = 1.0, .y = 0.5, .z = 0.0, .w = 1.0 }, "HALTED");
//             } else {
//                 ig.igTextColored(.{ .x = 0.2, .y = 1.0, .z = 0.2, .w = 1.0 }, "RUNNING");
//             }

//             ig.igText("IME:   ");
//             ig.igSameLine();
//             switch (cpu.state.ime) {
//                 .ENABLED => ig.igTextColored(.{ .x = 0.2, .y = 1.0, .z = 0.2, .w = 1.0 }, "ENABLED"),
//                 .ENABLED_NEXT => ig.igTextColored(.{ .x = 0.6, .y = 1.0, .z = 0.2, .w = 1.0 }, "ENABLED (next)"),
//                 .DISABLED => ig.igTextColored(.{ .x = 1.0, .y = 0.2, .z = 0.2, .w = 1.0 }, "DISABLED"),
//             }
//         }

//         if (ig.igCollapsingHeader("Timer", ui.flags.combine(treeFlags))) {
//             var buf: [32]u8 = undefined;
//             if (ig.igBeginTable("timer_regs", 2, ig.ImGuiTableFlags_None)) {
//                 defer ig.igEndTable();

//                 ig.igTableNextRow();
//                 _ = ig.igTableNextColumn();
//                 ig.igText("DIV:  0x%04X", gb.timer.div);
//                 _ = ig.igTableNextColumn();
//                 ig.igText("TIMA: 0x%02X", gb.timer.tima);

//                 ig.igTableNextRow();
//                 _ = ig.igTableNextColumn();
//                 ig.igText("TMA:  0x%02X", gb.timer.tma);
//                 _ = ig.igTableNextColumn();
//                 ig.igText("TAC:  %s", (std.fmt.bufPrintZ(&buf, "{b:0>8}", .{gb.timer.tac}) catch unreachable).ptr);
//             }
//         }

//         if (ig.igCollapsingHeader("Hardware (PPU & Int)", ui.flags.combine(treeFlags))) {
//             var buf: [64]u8 = undefined;

//             ui.fmt.kvHex8("IE:", cpu.int.ie_reg);
//             ui.fmt.kvHex8("IF:", cpu.int.if_reg);
//             ig.igSeparator();

//             ui.fmt.kvBin8("LCDC:", gb.ppu.lcdc);
//             ui.fmt.kvBin8("STAT:", gb.ppu.stat);

//             ig.igSeparator();
//             ig.igText("LY:   %03d (0x%02X)", gb.ppu.ly, gb.ppu.ly);
//             ig.igText("LYC:  %03d (0x%02X)", gb.ppu.lyc, gb.ppu.lyc);

//             ig.igSeparator();
//             if (ig.igBeginTable("ppu_table", 2, ig.ImGuiTableFlags_None)) {
//                 defer ig.igEndTable();

//                 ig.igTableNextRow();
//                 _ = ig.igTableNextColumn();
//                 ig.igText("SCX:  %03d", gb.ppu.scx);
//                 _ = ig.igTableNextColumn();
//                 ig.igText("SCY:  %03d", gb.ppu.scy);

//                 ig.igTableNextRow();
//                 _ = ig.igTableNextColumn();
//                 ig.igText("WX:   %03d", gb.ppu.wx);
//                 _ = ig.igTableNextColumn();
//                 ig.igText("WY:   %03d", gb.ppu.wy);
//             }

//             ig.igSeparator();
//             ig.igText("BGP:  %s", (std.fmt.bufPrintZ(&buf, "{b:0>8}", .{gb.ppu.bgp}) catch unreachable).ptr);
//             ig.igText("OBP0: %s", (std.fmt.bufPrintZ(&buf, "{b:0>8}", .{gb.ppu.obp0}) catch unreachable).ptr);
//             ig.igText("OBP1: %s", (std.fmt.bufPrintZ(&buf, "{b:0>8}", .{gb.ppu.obp1}) catch unreachable).ptr);
//         }

//         if (ig.igCollapsingHeader("Cartridge", ui.flags.combine(treeFlags))) {
//             if (gb.bus.cartridge) |cart| {
//                 ig.igText("Loaded: %s", cart.filename.ptr);
//                 ig.igText("ROM Bank: %d", @as(u8, cart.rom_bank));
//                 ig.igText("RAM Bank: %d", @as(u8, cart.ram_bank));
//             } else {
//                 ig.igTextColored(.{ .x = 1.0, .y = 0.2, .z = 0.2, .w = 1.0 }, "No cartridge loaded");
//             }
//         }

//         if (ig.igCollapsingHeader("Joypad", ui.flags.combine(treeFlags))) {
//             var buf: [32]u8 = undefined;
//             ig.igText("JOYP: %s", (std.fmt.bufPrintZ(&buf, "{b:0>8}", .{gb.joypad.p1_joyp}) catch unreachable).ptr);
//         }
//     }
// };
