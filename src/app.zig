const emu = @import("emu/root.zig");
const ig = @import("cimgui");
const Decompiler = @import("decompiler.zig").Decompiler;
const Texture = @import("ui/texture.zig").Texture;
const LayoutManager = @import("ui/layout.zig").LayoutManager;
const RomBrowser = @import("panels/rom-browser.zig").RomBrowser;
const Menu = @import("ui/menu.zig");

const std = @import("std");
const builtin = @import("builtin");
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
    allocator: Allocator,
    emu: EmuState,
    gfx: GfxState,
    layout: LayoutState,
    panels: PanelsState,

    pub var alloc_impl = switch (builtin.mode) {
        .Debug, .ReleaseSafe => std.heap.DebugAllocator(.{}).init,
        .ReleaseFast, .ReleaseSmall => std.heap.smp_allocator,
    };

    pub const Event = [*c]const sapp.Event;

    pub fn init(allocator: Allocator) AppState {
        return .{
            .allocator = allocator,
            .emu = EmuState.init(),
            .gfx = GfxState.init(),
            .layout = LayoutState.init(),
            .panels = PanelsState.init(allocator),
        };
    }

    pub fn init_sokol(self: *AppState) void {
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
        Menu.draw_menu(self);
        self.emu.frame_emu();
        self.layout.update_layout();
        self.emu.draw_screen();
        self.panels.draw_panels();
        self.gfx.render();
    }

    pub fn event(self: *AppState, ev: Event) void {
        _ = simgui.handleEvent(ev.*);
        if (ig.igGetIO().*.WantCaptureKeyboard) return;
        self.emu.cpu.bus.joypad.handle_event(ev);
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

pub const EmuState = struct {
    cpu: emu.CPU,
    pause: bool = true,
    is_overloaded: bool = false,
    overload_count: u32 = 0,
    cycle_acc: f64 = 0,

    const CPU_FREQ = 4194304.0;

    pub fn init() EmuState {
        return .{ .cpu = emu.CPU.init(.DMG0) };
    }

    pub fn deinit(self: *EmuState) void {
        self.cpu.deinit();
    }

    pub fn draw_screen(emu_state: *EmuState) void {
        var app = get_app(emu_state, "emu");
        app.gfx.screen_tex.update(&app.emu.cpu.bus.ppu.frame_buffer);

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

    pub fn frame_emu(self: *EmuState) void {
        if (!self.pause) {
            self.cycle_acc += sapp.frameDuration() * CPU_FREQ;
            if (self.cycle_acc > CPU_FREQ / 10.0) {
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
                if (!self.cpu.state.halted) {
                    const instr = emu.Instruction.from_bus(&self.cpu.bus);
                    cycles_taken = self.cpu.execute_instruction(instr);
                }
                self.cpu.bus.ppu.tick(cycles_taken);
                self.cpu.bus.timer.tick(cycles_taken);
                self.cpu.bus.apu.tick(cycles_taken);
                self.cpu.interrupts.handle_interrupts();
                self.cycle_acc -= @floatFromInt(cycles_taken);
            }
        }
    }

    pub fn print_performance_report(self: EmuState) void {
        const print = std.debug.print;
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

    pub fn update_layout(self: *LayoutState) void {
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

    pub fn init(all: Allocator) PanelsState {
        return .{ .rom_browser = RomBrowser.init(all) };
    }

    pub fn deinit(self: *PanelsState) void {
        self.rom_browser.deinit();
    }

    pub fn draw_panels(self: *PanelsState) void {
        self.rom_browser.draw(get_app(self, "panels"));
    }
};

inline fn get_app(self: anytype, comptime field_name: []const u8) *AppState {
    return @alignCast(@fieldParentPtr(field_name, self));
}
