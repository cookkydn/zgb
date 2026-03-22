const use_docking = @import("build_options").docking;
const emu = @import("emu/root.zig");
const ig = if (use_docking) @import("cimgui_docking") else @import("cimgui");
const sokol = @import("sokol");
const slog = sokol.log;
const sg = sokol.gfx;
const sgl = sokol.gl;
const sapp = sokol.app;
const sglue = sokol.glue;
const simgui = sokol.imgui;
const sgimgui = sokol.sgimgui;
const std = @import("std");

const Decompiler = @import("decompiler.zig").Decompiler;

const CPU_FREQ = 4194304.0;

const state = struct {
    var pass_action: sg.PassAction = .{};
    var show_debug: bool = true;
    var show_registers: bool = true;
    var show_controls: bool = true;
    var image: sg.Image = .{};
    var sampler: sg.Sampler = .{};
    var view: sg.View = .{};
    var cpu: ?emu.CPU = null;
    var pause: bool = true;
    var decompiler: Decompiler = undefined;
    var cycle_acc: f64 = 0;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
};

export fn init() void {
    // initialize sokol-gfx
    sg.setup(.{
        .environment = sglue.environment(),
        .logger = .{ .func = slog.func },
    });
    // the debug/tracing ui
    sgimgui.setup(.{});
    // sgl
    sgl.setup(.{});
    // initialize sokol-imgui
    simgui.setup(.{
        .logger = .{ .func = slog.func },
    });
    if (use_docking) {
        ig.igGetIO().*.ConfigFlags |= ig.ImGuiConfigFlags_DockingEnable;
    }

    // initial clear color
    state.pass_action.colors[0] = .{
        .load_action = .CLEAR,
        .clear_value = .{ .r = 0.0, .g = 0.5, .b = 1.0, .a = 1.0 },
    };

    state.cpu = emu.CPU.init(.DMG0);
    state.cpu.?.bus.loadCartridge("tetris.gb") catch @panic("failed to load cartridge");
    state.image = sg.makeImage(.{
        .width = 160,
        .height = 144,
        .pixel_format = .SRGB8A8,
        .usage = .{ .stream_update = true },
    });
    state.sampler = sg.makeSampler(.{
        .min_filter = .NEAREST,
        .mag_filter = .NEAREST,
        .wrap_u = .CLAMP_TO_EDGE,
        .wrap_v = .CLAMP_TO_EDGE,
    });
    state.decompiler = Decompiler.init(state.arena.allocator(), &state.cpu.?.bus);
    state.view = sg.makeView(.{ .texture = .{ .image = state.image } });
}

export fn frame() void {
    // call simgui.newFrame() before any ImGui calls
    simgui.newFrame(.{
        .width = sapp.width(),
        .height = sapp.height(),
        .delta_time = sapp.frameDuration(),
        .dpi_scale = sapp.dpiScale(),
    });

    const backendName: [*c]const u8 = switch (sg.queryBackend()) {
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

    var instr_count: i64 = 0;
    if (!state.pause) {
        state.cycle_acc += sapp.frameDuration() * CPU_FREQ;

        if (state.cycle_acc > CPU_FREQ / 10.0) {
            // Too much to keep up, running at a slower pace to keep up
            state.cycle_acc /= 10;
        }
        while (state.cycle_acc > 0) {
            state.cycle_acc -= @floatFromInt(step_emu());
            instr_count += 1;
        }
    }
    const ips = @as(f64, @floatFromInt(instr_count)) / sapp.frameDuration();

    //=== UI CODE STARTS HERE
    if (state.show_debug) {
        ig.igSetNextWindowPos(.{ .x = 10, .y = 10 }, ig.ImGuiCond_Once);
        ig.igSetNextWindowSize(.{ .x = 200, .y = 130 }, ig.ImGuiCond_Once);
        if (ig.igBegin("Debug info", &state.show_debug, ig.ImGuiWindowFlags_None)) {
            ig.igText("Dear ImGui Version: %s", ig.IMGUI_VERSION);
            ig.igText("Sokol Backend: %s", backendName);
            ig.igText("FPS: %.f", 1 / sapp.frameDuration());
            ig.igText("Instr/f: %i", instr_count);
            ig.igText("Instr/s: %.f", ips);
        }
        ig.igEnd();
    }

    ig.igSetNextWindowPos(.{ .x = 10, .y = 150 }, ig.ImGuiCond_Once);
    ig.igSetNextWindowSize(.{ .x = 150, .y = 150 }, ig.ImGuiCond_Once);
    if (ig.igBegin("Registers", &state.show_registers, ig.ImGuiWindowFlags_None)) {
        ig.igText("PC: 0x%04x", state.cpu.?.registers.pc);
        ig.igText("AF: 0x%04x", state.cpu.?.registers.get_af());
        ig.igText("BC: 0x%04x", state.cpu.?.registers.get_bc());
        ig.igText("DE: 0x%04x", state.cpu.?.registers.get_de());
        ig.igText("HL: 0x%04x", state.cpu.?.registers.get_hl());
        ig.igText("SP: 0x%04x", state.cpu.?.registers.sp);
        ig.igText(
            "F: z %i n %i h %i c %i",
            state.cpu.?.registers.f.z,
            state.cpu.?.registers.f.n,
            state.cpu.?.registers.f.h,
            state.cpu.?.registers.f.c,
        );
    }
    ig.igEnd();

    state.decompiler.frame(state.cpu.?.registers.pc);

    ig.igSetNextWindowPos(.{ .x = 10, .y = 310 }, ig.ImGuiCond_Once);
    ig.igSetNextWindowSize(.{ .x = 200, .y = 100 }, ig.ImGuiCond_Once);
    if (ig.igBegin("Controls", &state.show_controls, ig.ImGuiWindowFlags_None)) {
        if (ig.igButton("Restart")) {
            state.pause = true;
            state.cpu = emu.CPU.init(.DMG0);
            state.cpu.?.bus.loadCartridge("tetris.gb") catch @panic("failed to load cartridge");
        }
        ig.igSameLine();
        if (ig.igButton(if (state.pause) "Play" else "Pause")) {
            state.pause = !state.pause;
        }
        if (state.pause) {
            if (state.cpu.?.bus.is_bios) {
                ig.igSameLine();
                if (ig.igButton("Boot")) {
                    while (state.cpu.?.bus.is_bios) {
                        _ = step_emu();
                    }
                }
            }
            if (ig.igButton("Step")) _ = step_emu();
            ig.igSameLine();
            if (ig.igButton("Step x10")) for (0..10) |_| {
                _ = step_emu();
            };
            ig.igSameLine();
            if (ig.igButton("Step x100")) for (0..100) |_| {
                _ = step_emu();
            };
            if (ig.igButton("Step x1000")) for (0..1_000) |_| {
                _ = step_emu();
            };
            ig.igSameLine();
            if (ig.igButton("Step x10 000")) for (0..10_000) |_| {
                _ = step_emu();
            };
            ig.igSameLine();
            if (ig.igButton("Step x100 000")) for (0..100_000) |_| {
                _ = step_emu();
            };
        }
    }
    ig.igEnd();

    var data = sg.ImageData{};
    data.mip_levels[0] = sg.asRange(&state.cpu.?.bus.ppu.frame_buffer);

    sg.updateImage(state.image, data);

    sgl.defaults();
    sgl.enableTexture();
    sgl.texture(state.view, state.sampler);
    sgl.matrixModeProjection();
    sgl.loadIdentity();
    sgl.ortho(0.0, sapp.widthf(), sapp.heightf(), 0.0, -1.0, 1.0);

    const ratio: f32 = @min(@divFloor(sapp.widthf() - 400, 160), @divFloor(sapp.heightf(), 144));

    const render_w: f32 = 160 * ratio;
    const render_h: f32 = 144 * ratio;
    const x0 = (sapp.widthf() - render_w) / 2.0;
    const y0 = (sapp.heightf() - render_h) / 2.0;

    sgl.beginQuads();
    sgl.v2fT2f(x0, y0, 0.0, 0.0);
    sgl.v2fT2f(x0 + render_w, y0, 1.0, 0.0);
    sgl.v2fT2f(x0 + render_w, y0 + render_h, 1.0, 1.0);
    sgl.v2fT2f(x0, y0 + render_h, 0.0, 1.0);
    sgl.end();

    sgimgui.draw();

    state.pass_action.colors[0].clear_value = .{
        .r = 0.1,
        .g = 0.1,
        .b = 0.1,
        .a = 1,
    };

    sg.beginPass(.{ .action = state.pass_action, .swapchain = sglue.swapchain() });
    sgl.draw();
    simgui.render();
    sg.endPass();
    sg.commit();
}

export fn cleanup() void {
    state.arena.deinit();
    simgui.shutdown();
    sgimgui.shutdown();
    sg.shutdown();
}

export fn event(ev: [*c]const sapp.Event) void {
    // forward input events to sokol-imgui
    _ = simgui.handleEvent(ev.*);
}

pub fn main() void {
    sapp.run(.{
        .init_cb = init,
        .frame_cb = frame,
        .cleanup_cb = cleanup,
        .event_cb = event,
        .window_title = "ZGB",
        .width = 1200,
        .height = 600,
        .icon = .{ .sokol_default = true },
        .logger = .{ .func = slog.func },
    });
}

fn step_emu() u16 {
    const instr = emu.Instruction.from_bus(&state.cpu.?.bus);
    const cycles_taken = state.cpu.?.execute_instruction(instr);
    state.cpu.?.bus.ppu.tick(cycles_taken);
    state.cpu.?.bus.timer.tick(cycles_taken);
    state.cpu.?.interrupts.handle_interrupts();
    return cycles_taken;
}
