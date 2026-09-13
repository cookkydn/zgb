const emu = @import("emu/root.zig");
const ig = @import("cimgui");
const ui = @import("ui");
const Decompiler = @import("decompiler.zig").Decompiler;
const Texture = ui.Texture;
// const LayoutManager = @import("ui/layout.zig").LayoutManager;
const RomBrowser = @import("panels/rom-browser.zig").RomBrowser;
const SettingsPanel = @import("panels/settings.zig").SettingsPanel;
const DebugPanel = @import("panels/debug.zig").DebugPanel;
const VramViewer = @import("panels/vram-viewer.zig").VramViewer;
const menu = @import("ui/menu.zig");

const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;

// -- Sokol imports --
const sokol = @import("sokol");
const slog = sokol.log;
const sg = sokol.gfx;
const sgl = sokol.gl;
const sapp = sokol.app;
const sglue = sokol.glue;
const simgui = sokol.imgui;
const sgimgui = sokol.gfximgui;
const sgaudio = sokol.audio;

// -- Global state --
pub const AppState = struct {
    all: Allocator,
    emu: Emulator,
    gfx: GfxState,
    panels: PanelsState,
    io: std.Io,

    pub var alloc_impl = switch (builtin.mode) {
        .Debug, .ReleaseSafe => std.heap.DebugAllocator(.{}).init,
        .ReleaseFast, .ReleaseSmall => std.heap.smp_allocator,
    };

    pub const Event = [*c]const sapp.Event;

    pub fn init(all: Allocator, io: std.Io) AppState {
        return .{
            .all = all,
            .emu = Emulator.init(all, io),
            .gfx = GfxState.init(),
            .panels = PanelsState.init(all, io),
            .io = io,
        };
    }

    pub fn initSokol(self: *AppState) void {
        std.log.info("Loading Sokol backend", .{});
        sg.setup(.{
            .environment = sglue.environment(),
            .logger = .{ .func = slog.func },
        });
        sgimgui.setup(.{});
        sgl.setup(.{});
        simgui.setup(.{
            .logger = .{ .func = slog.func },
        });
        sgaudio.setup(.{
            .sample_rate = 48000,
            .logger = .{ .func = slog.func },
            .num_channels = 2,
        });
        ig.igGetIO().*.ConfigFlags |= ig.ImGuiConfigFlags_DockingEnable;
        const style = ig.igGetStyle();

        // Arrondir les fenêtres et les boutons
        style.*.WindowRounding = 6.0;
        style.*.FrameRounding = 4.0;
        style.*.GrabRounding = 4.0; // Pour les sliders

        // Aérer un peu les éléments
        style.*.ItemSpacing = .{ .x = 8.0, .y = 6.0 };
        style.*.FramePadding = .{ .x = 6.0, .y = 4.0 };

        // Enlever les bordures épaisses
        style.*.WindowBorderSize = 0.0;
        style.*.FrameBorderSize = 0.0;

        self.gfx.screen_tex = Texture.init(160, 144);
        self.panels.vram.tiles_tex = Texture.init(192, 128);
        self.panels.debug.setBackendName();
    }

    pub fn deinit(self: *AppState) void {
        std.log.info("Sokol deinit", .{});
        // -- App deinit --
        self.emu.deinit();
        self.gfx.deinit();
        self.panels.deinit();

        // -- Check for leaks --
        if (builtin.mode == .Debug) {
            if (alloc_impl.detectLeaks() > 0) {
                @panic("Memory leaked");
            }
        } else {
            alloc_impl.deinit();
        }
        // -- Sokol deinit --
        simgui.shutdown();
        sgimgui.shutdown();
        sgaudio.shutdown();
        sg.shutdown();
    }

    pub fn frame(self: *AppState) void {
        simgui.newFrame(.{
            .width = sapp.width(),
            .height = sapp.height(),
            .delta_time = sapp.frameDuration(),
            .dpi_scale = sapp.dpiScale(),
        });
        ui.begin();
        menu.draw_menu(self);
        self.emu.frameEmu();
        self.emu.drawScreen();
        self.panels.drawPanels();
        self.gfx.render();
    }

    pub fn event(self: *AppState, ev: Event) void {
        _ = simgui.handleEvent(ev.*);
        if (ig.igGetIO().*.WantCaptureKeyboard) return;
        self.emu.gb.joypad.handleEvent(ev);
    }
};

pub const GfxState = struct {
    screen_tex: Texture = undefined,
    pass_action: sg.PassAction = .{
        .colors = .{
            sg.ColorAttachmentAction{ .load_action = .DONTCARE },
        } ++ ([_]sg.ColorAttachmentAction{.{}} ** 7),
    },

    pub fn init() GfxState {
        return .{};
    }

    pub fn deinit(self: *GfxState) void {
        self.screen_tex.deinit();
    }

    pub fn render(self: *GfxState) void {
        sg.beginPass(.{
            .action = self.pass_action,
            .swapchain = sglue.swapchain(),
        });
        simgui.render();
        sg.endPass();
        sg.commit();
    }
};

pub const Emulator = struct {
    gb: emu.Gameboy,
    pause: bool = true,
    is_overloaded: bool = false,
    skip_boot: bool = true,
    overload_count: u32 = 0,
    cycle_acc: f64 = 0,
    volume: f32 = 0.1,
    io: std.Io,

    const cpu_freq = 4194304.0;

    pub fn init(all: Allocator, io: std.Io) Emulator {
        return .{ .gb = emu.Gameboy.init(all, io), .io = io };
    }

    pub fn deinit(self: *Emulator) void {
        self.gb.deinit();
    }

    pub fn drawScreen(emu_state: *Emulator) void {
        var app = getApp(emu_state, "emu");
        app.gfx.screen_tex.update(&app.emu.gb.ppu.frame_buffer);
        const flags = [_]ui.flags.WindowFlag{ .EmuScreen, .NoDecoration, .NoInputs };

        if (ig.igBegin("Screen", null, ui.flags.combine(flags))) {
            const avail = ig.igGetContentRegionAvail();
            const scale_w = avail.x / 160.0;
            const scale_h = (avail.y) / 144.0;
            const scale = @min(scale_w, scale_h);
            const display_size = ig.ImVec2{ .x = 160.0 * scale, .y = 144.0 * scale };

            var cursor_x = (avail.x - display_size.x) * 0.5;
            if (cursor_x < 0) cursor_x = 0;
            var cursor_y = (avail.y - display_size.y) * 0.5;
            if (cursor_y < 0) cursor_y = 0;

            ig.igSetCursorPosX(ig.igGetCursorPosX() + cursor_x);
            ig.igSetCursorPosY(ig.igGetCursorPosY() + cursor_y);
            app.gfx.screen_tex.drawSized(display_size.x, display_size.y);
        }
        ig.igEnd();
    }

    pub fn pushSound(self: *Emulator) void {
        const apu = &self.gb.apu;
        if (apu.buffer_index >= apu.buffer.len) {
            apu.buffer_index = 0;
            for (apu.buffer, 0..) |_, i| {
                apu.buffer[i] *= self.volume;
            }
            _ = sgaudio.push(&apu.buffer[0], apu.buffer.len / 2);
        }
    }

    pub fn frameEmu(self: *Emulator) void {
        if (!self.pause) {
            while (self.skip_boot and self.gb.bus.is_bios_loaded) {
                var cycles_taken: u16 = 4;
                if (!self.gb.cpu.state.halted) {
                    const instr = emu.Instruction.fromBus(&self.gb.bus);
                    cycles_taken = self.gb.cpu.execute_instruction(instr, &self.gb.bus);
                }
                cycles_taken += self.gb.cpu.handleInterrupts(&self.gb.bus);
                self.gb.ppu.tick(cycles_taken);
                self.gb.timer.tick(cycles_taken);
                self.gb.apu.tick(cycles_taken);
                self.gb.apu.buffer_index = 0;
            }

            self.cycle_acc += sapp.frameDuration() * cpu_freq;
            if (self.cycle_acc > cpu_freq / 10.0) {
                self.cycle_acc /= 10;
                if (self.is_overloaded == false) {
                    self.overload_count += 1;
                    self.is_overloaded = true;
                }
            } else {
                self.is_overloaded = false;
            }
            while (self.cycle_acc > 0) {
                var cycles_taken: u16 = 4;
                if (!self.gb.cpu.state.halted) {
                    const instr = emu.Instruction.fromBus(&self.gb.bus);
                    cycles_taken = self.gb.cpu.execute_instruction(instr, &self.gb.bus);
                }
                cycles_taken += self.gb.cpu.handleInterrupts(&self.gb.bus);
                self.gb.ppu.tick(cycles_taken);
                self.gb.timer.tick(cycles_taken);
                self.gb.apu.tick(cycles_taken);
                self.pushSound();
                self.cycle_acc -= @floatFromInt(cycles_taken);
            }
        }
    }
};

pub const PanelsState = struct {
    rom_browser: RomBrowser,
    settings: SettingsPanel,
    vram: VramViewer,
    debug: DebugPanel,

    pub fn init(all: Allocator, io: std.Io) PanelsState {
        return .{
            .rom_browser = RomBrowser.init(all, io),
            .settings = SettingsPanel{},
            .debug = DebugPanel.init(),
            .vram = VramViewer{},
        };
    }

    pub fn deinit(self: *PanelsState) void {
        self.rom_browser.deinit();
    }

    pub fn drawPanels(self: *PanelsState) void {
        const app = getApp(self, "panels");
        self.rom_browser.draw(app);
        self.settings.draw(app);
        self.debug.draw(app);
        self.vram.draw(app);
    }
};

inline fn getApp(self: anytype, comptime field_name: []const u8) *AppState {
    return @alignCast(@fieldParentPtr(field_name, self));
}
