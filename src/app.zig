const emu = @import("emu/root.zig");
const ig = @import("cimgui");
const Decompiler = @import("decompiler.zig").Decompiler;
const Texture = @import("ui/texture.zig").Texture;
const LayoutManager = @import("ui/layout.zig").LayoutManager;
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
const sgimgui = sokol.sgimgui;
const sgaudio = sokol.audio;

// -- Global state --
pub const AppState = struct {
    all: Allocator,
    emu: Emulator,
    gfx: GfxState,
    layout: LayoutState,
    panels: PanelsState,

    pub var alloc_impl = switch (builtin.mode) {
        .Debug, .ReleaseSafe => std.heap.DebugAllocator(.{}).init,
        .ReleaseFast, .ReleaseSmall => std.heap.smp_allocator,
    };

    pub const Event = [*c]const sapp.Event;

    pub fn init(all: Allocator) AppState {
        return .{
            .all = all,
            .emu = Emulator.init(all),
            .gfx = GfxState.init(),
            .layout = LayoutState.init(),
            .panels = PanelsState.init(all),
        };
    }

    pub fn initSokol(self: *AppState) void {
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
        self.gfx.screen_tex = Texture.init(160, 144);
        self.panels.vram.tiles_tex = Texture.init(192, 128);
        self.panels.debug.setBackendName();
    }

    pub fn deinit(self: *AppState) void {
        // -- App deinit --
        self.emu.deinit();
        self.gfx.deinit();
        self.panels.deinit();

        // -- Check for leaks --
        if (builtin.mode == .Debug) {
            if (alloc_impl.detectLeaks()) {
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
        menu.draw_menu(self);
        self.emu.frameEmu();
        self.layout.updateLayout();
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

    const cpu_freq = 4194304.0;

    pub fn init(all: Allocator) Emulator {
        return .{ .gb = emu.Gameboy.init(all) };
    }

    pub fn deinit(self: *Emulator) void {
        self.gb.deinit();
    }

    pub fn drawScreen(emu_state: *Emulator) void {
        var app = getApp(emu_state, "emu");
        app.gfx.screen_tex.update(&app.emu.gb.ppu.frame_buffer);

        if (ig.igBegin(LayoutManager.Panels.screen, null, ig.ImGuiWindowFlags_None)) {
            const tex_id = app.gfx.screen_tex.imTextureId();
            const avail = ig.igGetContentRegionAvail();
            const scale_w = avail.x / 160.0;
            const scale_h = avail.y / 144.0;
            const scale = @min(scale_w, scale_h);
            const display_size = ig.ImVec2{ .x = 160.0 * scale, .y = 144.0 * scale };

            var cursor_x = (avail.x - display_size.x) * 0.5;
            if (cursor_x < 0) cursor_x = 0;
            var cursor_y = (avail.y - display_size.y) * 0.5;
            if (cursor_y < 0) cursor_y = 0;

            ig.igSetCursorPosX(ig.igGetCursorPosX() + cursor_x);
            ig.igSetCursorPosY(ig.igGetCursorPosY() + cursor_y);
            ig.igImage(
                .{ ._TexID = tex_id },
                display_size,
            );
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
            while (self.skip_boot and self.gb.bus.is_bios) {
                var cycles_taken: u16 = 4;
                if (!self.gb.cpu.state.halted) {
                    const instr = emu.Instruction.fromBus(&self.gb.bus);
                    cycles_taken = self.gb.cpu.execute_instruction(instr);
                }
                cycles_taken += self.gb.cpu.int.handleInterrupts();
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
                    cycles_taken = self.gb.cpu.execute_instruction(instr);
                }
                cycles_taken += self.gb.cpu.int.handleInterrupts();
                self.gb.ppu.tick(cycles_taken);
                self.gb.timer.tick(cycles_taken);
                self.gb.apu.tick(cycles_taken);
                self.pushSound();
                self.cycle_acc -= @floatFromInt(cycles_taken);
            }
        }
    }

    pub fn printDebugInfo(self: Emulator) void {
        const print = std.debug.print;
        print("====== DEBUG REPORT ======\n", .{});
        if (self.gb.bus.cartridge) |cartridge| {
            print("=> Cartridge type: {s}\n", .{@tagName(cartridge.mbc_type)});
            print("=> Cartridge rom size: 0x{x}\n", .{cartridge.rom.len});
            print("=> Cartridge ram size: 0x{x}\n", .{cartridge.ram.len});
            print("=> Cartridge rom bank: 0x{x}\n", .{cartridge.rom_bank});
            print("=> Cartridge ram bank: 0x{x}\n", .{cartridge.ram_bank});
        } else {
            print("=> No cartridge\n", .{});
        }
        print("=== PERFORMANCE REPORT ===\n", .{});
        if (self.overload_count > 0) {
            print("=> Overload count: {}\n", .{self.overload_count});
        } else {
            print("=> Never overloaded\n", .{});
        }
        print("==========================\n", .{});
    }
};

pub const LayoutState = struct {
    set_layout: LayoutManager.LayoutType,

    pub fn init() LayoutState {
        return .{
            .set_layout = .Default,
        };
    }

    pub fn updateLayout(self: *LayoutState) void {
        const app: *AppState = @alignCast(@fieldParentPtr("layout", self));
        const viewport = ig.igGetMainViewport();
        const dockspace_id = ig.igGetID("dockspace");
        if (self.set_layout != .None) {
            LayoutManager.applyLayout(dockspace_id, self.set_layout, app);
            self.set_layout = .None;
        }
        _ = ig.igDockSpaceOverViewportEx(dockspace_id, viewport, ig.ImGuiDockNodeFlags_None, null);
    }
};

pub const PanelsState = struct {
    rom_browser: RomBrowser,
    settings: SettingsPanel,
    vram: VramViewer,
    debug: DebugPanel,

    pub fn init(all: Allocator) PanelsState {
        return .{
            .rom_browser = RomBrowser.init(all),
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
