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

const state = struct {
    var pass_action: sg.PassAction = .{};
    var show_first_window: bool = true;
    var show_second_window: bool = true;
    var show_registers: bool = true;
    var image: sg.Image = .{};
    var sampler: sg.Sampler = .{};
    var view: sg.View = .{};
    var cpu: ?emu.CPU = null;
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
        .pixel_format = .RGBA8,
        .usage = .{ .stream_update = true },
    });
    state.sampler = sg.makeSampler(.{
        .min_filter = .NEAREST,
        .mag_filter = .NEAREST,
        .wrap_u = .CLAMP_TO_EDGE,
        .wrap_v = .CLAMP_TO_EDGE,
    });
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

    for (0..50000) |_| {
        const instr = emu.Instruction.from_bus(&state.cpu.?.bus);
        const cycles_taken = state.cpu.?.execute_instruction(instr);
        state.cpu.?.bus.ppu.tick(cycles_taken);
        state.cpu.?.bus.timer.tick(cycles_taken);
        state.cpu.?.interrupts.handle_interrupts();
    }

    //=== UI CODE STARTS HERE
    ig.igSetNextWindowPos(.{ .x = 10, .y = 30 }, ig.ImGuiCond_Once);
    ig.igSetNextWindowSize(.{ .x = 400, .y = 100 }, ig.ImGuiCond_Once);
    if (ig.igBegin("Hello Dear ImGui!", &state.show_first_window, ig.ImGuiWindowFlags_None)) {
        _ = ig.igColorEdit3("Background", &state.pass_action.colors[0].clear_value.r, ig.ImGuiColorEditFlags_None);
        _ = ig.igText("Dear ImGui Version: %s", ig.IMGUI_VERSION);
    }
    ig.igEnd();

    ig.igSetNextWindowPos(.{ .x = 50, .y = 150 }, ig.ImGuiCond_Once);
    ig.igSetNextWindowSize(.{ .x = 400, .y = 100 }, ig.ImGuiCond_Once);
    if (ig.igBegin("Another Window", &state.show_second_window, ig.ImGuiWindowFlags_None)) {
        _ = ig.igText("Sokol Backend: %s", backendName);
    }
    ig.igEnd();

    ig.igSetNextWindowPos(.{ .x = 420, .y = 30 }, ig.ImGuiCond_Once);
    ig.igSetNextWindowSize(.{ .x = 200, .y = 100 }, ig.ImGuiCond_Once);
    if (ig.igBegin("Registers", &state.show_second_window, ig.ImGuiWindowFlags_None)) {
        _ = ig.igText("PC: 0x%04x", state.cpu.?.registers.pc);
        _ = ig.igText("AF: 0x%04x", state.cpu.?.registers.get_af());
        _ = ig.igText("BC: 0x%04x", state.cpu.?.registers.get_bc());
        _ = ig.igText("DE: 0x%04x", state.cpu.?.registers.get_de());
        _ = ig.igText("HL: 0x%04x", state.cpu.?.registers.get_hl());
        _ = ig.igText("SP: 0x%04x", state.cpu.?.registers.sp);
        _ = ig.igText(
            "F: z %i n %i h %i c %i",
            state.cpu.?.registers.f.z,
            state.cpu.?.registers.f.n,
            state.cpu.?.registers.f.h,
            state.cpu.?.registers.f.c,
        );
    }
    ig.igEnd();

    var data = sg.ImageData{};
    data.mip_levels[0] = sg.asRange(&state.cpu.?.bus.ppu.frame_buffer);

    sg.updateImage(state.image, data);

    // 2. Dessin avec sokol-gl
    sgl.defaults();
    sgl.enableTexture();
    sgl.texture(state.view, state.sampler);

    sgl.beginQuads();
    sgl.v2fT2f(-1.0, 1.0, 0.0, 0.0); // Haut Gauche
    sgl.v2fT2f(1.0, 1.0, 1.0, 0.0); // Haut Droite
    sgl.v2fT2f(1.0, -1.0, 1.0, 1.0); // Bas Droite
    sgl.v2fT2f(-1.0, -1.0, 0.0, 1.0); // Bas Gauche
    sgl.end();

    // the sokol-gfx-imgui debugging ui
    // if (ig.igBeginMainMenuBar()) {
    //     sgimgui.drawMenu("sokol-gfx");
    //     ig.igEndMainMenuBar();
    // }
    sgimgui.draw();
    //=== UI CODE ENDS HERE

    // call simgui.render() inside a sokol-gfx pass
    sg.beginPass(.{ .action = state.pass_action, .swapchain = sglue.swapchain() });
    sgl.draw();
    simgui.render();
    sg.endPass();
    sg.commit();
}

export fn cleanup() void {
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
        .width = 800,
        .height = 600,
        .icon = .{ .sokol_default = true },
        .logger = .{ .func = slog.func },
    });
}
