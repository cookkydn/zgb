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
const RomList = @import("window/rom-list.zig").RomList;
const DebugWindow = @import("window/debug.zig").DebugWindow;

const CPU_FREQ = 4194304.0;

const state = struct {
    var pass_action: sg.PassAction = .{};
    var show_registers: bool = true;
    var show_controls: bool = true;
    var image: sg.Image = .{};
    var sampler: sg.Sampler = .{};
    var view: sg.View = .{};
    var cpu: emu.CPU = undefined;
    var pause: bool = true;
    var decompiler: Decompiler = undefined;
    var cycle_acc: f64 = 0;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var stop_on_vblank: bool = false;
    var rom_list: RomList = undefined;
    var debug: DebugWindow = undefined;
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
    state.decompiler = Decompiler.init(state.arena.allocator(), &state.cpu.bus);
    state.view = sg.makeView(.{ .texture = .{ .image = state.image } });
    state.rom_list = RomList.init(state.arena.allocator(), &state.cpu);
    state.rom_list.read_rom_folder() catch @panic("Failed to read ROM folder");
    state.debug = DebugWindow.init(&state.cpu);
}

export fn frame() void {
    // call simgui.newFrame() before any ImGui calls
    simgui.newFrame(.{
        .width = sapp.width(),
        .height = sapp.height(),
        .delta_time = sapp.frameDuration(),
        .dpi_scale = sapp.dpiScale(),
    });

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
    // const ips = @as(f64, @floatFromInt(instr_count)) / sapp.frameDuration();

    //=== UI CODE STARTS HERE
    if (ig.igBeginMainMenuBar()) {
        if (ig.igBeginMenu("File")) {
            if (ig.igMenuItem("Open ROM")) {
                state.rom_list.visible = !state.rom_list.visible;
            }
            ig.igEndMenu();
        }

        if (ig.igBeginMenu("Decompiler")) {
            if (ig.igMenuItem("Show decompiler")) {
                state.decompiler.visible = !state.decompiler.visible;
            }
            if (ig.igMenuItem("Focus PC")) {
                state.decompiler.focus = true;
            }
            ig.igEndMenu();
        }

        if (ig.igBeginMenu("Debug")) {
            if (ig.igMenuItem("Show debug")) {
                state.debug.visible = !state.debug.visible;
            }
            ig.igEndMenu();
        }

        ig.igEndMainMenuBar();
    }

    state.decompiler.frame(state.cpu.registers.pc);
    state.rom_list.frame();
    state.debug.frame();
    ig.igSetNextWindowPos(.{ .x = 10, .y = 30 }, ig.ImGuiCond_Once);
    if (ig.igBegin("Controls", &state.show_controls, ig.ImGuiWindowFlags_None)) {
        if (ig.igButton("Restart")) {
            state.pause = true;
            state.cpu = emu.CPU.init(.DMG0);
            state.cpu.bus.reloadCartridge() catch @panic("failed to load cartridge");
            state.decompiler.init_analysis();
        }
        ig.igSameLine();
        if (ig.igButton(if (state.pause) "Play" else "Pause")) {
            state.pause = !state.pause;
        }
        if (state.pause) {
            if (state.cpu.bus.is_bios) {
                ig.igSameLine();
                if (ig.igButton("Boot")) {
                    while (state.cpu.bus.is_bios) {
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
            if (ig.igButton("Step x100 000")) for (0..100_000) |_| {
                _ = step_emu();
            };
            if (ig.igButton("Vblank")) {
                state.stop_on_vblank = true;
                state.pause = false;
            }
        }
    }
    ig.igEnd();
    var data = sg.ImageData{};
    data.mip_levels[0] = sg.asRange(&state.cpu.bus.ppu.frame_buffer);

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
    const y0 = (sapp.heightf() - render_h) / 2.0 + 10;

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
    _ = simgui.handleEvent(ev.*);
    if (ig.igGetIO().*.WantCaptureKeyboard) return;
    state.cpu.bus.joypad.handle_event(ev);
}

pub fn main() void {
    sapp.run(.{
        .init_cb = init,
        .frame_cb = frame,
        .cleanup_cb = cleanup,
        .event_cb = event,
        .window_title = "ZGB",
        .width = 1440,
        .height = 900,
        .icon = .{ .sokol_default = true },
        .logger = .{ .func = slog.func },
    });
}

fn step_emu() u16 {
    var cycles_taken: u16 = 4;
    if (!state.cpu.state.halted) {
        const instr = emu.Instruction.from_bus(&state.cpu.bus);
        cycles_taken = state.cpu.execute_instruction(instr);
    }
    state.cpu.bus.ppu.tick(cycles_taken);
    state.cpu.bus.timer.tick(cycles_taken);
    if (state.stop_on_vblank and state.cpu.registers.pc == 0x40) {
        state.pause = true;
        state.stop_on_vblank = false;
    }
    state.cpu.interrupts.handle_interrupts();
    return cycles_taken;
}
