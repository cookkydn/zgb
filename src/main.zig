const App = @import("app.zig");
const builtin = @import("builtin");
const dvui = @import("dvui");
const Emulator = @import("emu");
const std = @import("std");
const log = @import("logger.zig").log;

const panel_manager = @import("panel_manager.zig");

const window_icon_png = @embedFile("logo.png");

pub const dvui_app: dvui.App = .{
    .config = .{
        .startFn = getConfig,
    },
    .frameFn = appFrame,
    .initFn = appInit,
    .deinitFn = appDeinit,
};
pub const main = dvui.App.main;
pub const panic = dvui.App.panic;
pub const std_options: std.Options = .{
    .logFn = dvui.App.logFn,
};

var orig_content_scale: f32 = 1.0;
var gpa: std.mem.Allocator = undefined;
var app: App = undefined;
var cycle_acc: i128 = 0;
var last_frame_time: i128 = 0;

pub fn getConfig() dvui.App.StartOptions {
    _ = dvui.backend.c.SDL_SetAppMetadata("ZGB", "0.1.0", "dev.cookky.zgb");
    return .{
        .size = .{ .w = 600.0, .h = 600.0 },
        .min_size = .{ .w = 160, .h = 206 },
        .title = "ZGB",
        .icon = window_icon_png,
        .org = "zgb",

        .window_init_options = .{
            .keybinds_zoom = true,
            .theme = dvui.Theme.builtin.dracula,
        },
    };
}

// Runs before the first frame, after backend and dvui.Window.init()
// - runs between win.begin()/win.end()
pub fn appInit(win: *dvui.Window) !void {
    orig_content_scale = win.content_scale;
    win.snap_to_pixels = true;
    gpa = win.gpa;

    app = try App.init(gpa, dvui.io);

    // Add your own bundled font files...:
    try dvui.addFont("RobotoMono", @embedFile("fonts/RobotoMono-Regular.ttf"), null);

    var theme = dvui.Theme.builtin.dracula;
    theme.font_body = .find(.{ .family = "RobotoMono" });
    theme.font_mono = .find(.{ .family = "RobotoMono" });

    win.themeSet(theme);
}

// Run as app is shutting down before dvui.Window.deinit()
pub fn appDeinit(win: *dvui.Window) void {
    _ = win;

    app.deinit();
    panel_manager.deinit();
}

// Run each frame to do normal UI
pub fn appFrame() !dvui.App.Result {
    // Emu
    {
        try app.screen_tex.update(@ptrCast(&app.emu.ppu.frame_buffer));
    }

    // UI
    {
        var scaler = dvui.scale(@src(), .{ .scale = &dvui.currentWindow().content_scale, .pinch_zoom = .global }, .{ .rect = .cast(dvui.windowRect()) });
        scaler.deinit();
        if (try menu()) |res| return res;
        // try dvui.renderTexture(screen_tex, .{ .r = .rect(0, 0, 160, 144) }, .{});
        var scroll = dvui.scrollArea(@src(), .{}, .{ .expand = .both, .style = .window });
        defer scroll.deinit();
        try panel_manager.render(gpa, app);
    }

    // Emu
    {
        if (app.emu.bus.cartridge) |_| {
            const current_time = dvui.frameTimeNS();

            if (last_frame_time == 0) {
                last_frame_time = current_time;
            }

            const ns_elapsed = @as(i64, @intCast(current_time - last_frame_time));
            last_frame_time = current_time;

            var cycles_this_frame = @divTrunc(ns_elapsed * 4_194_304, 1_000_000_000);

            const MAX_CYCLES_PER_FRAME: i64 = 146_800;
            if (cycles_this_frame > MAX_CYCLES_PER_FRAME) {
                cycles_this_frame = MAX_CYCLES_PER_FRAME;
            }

            cycle_acc += cycles_this_frame;
            while (cycle_acc > 0) {
                var cycles_taken: u16 = 4;
                if (!app.emu.cpu.state.halted) {
                    const instr = Emulator.Instruction.fromBus(&app.emu.bus);
                    cycles_taken = app.emu.cpu.execute_instruction(instr, &app.emu.bus);
                }
                cycles_taken += app.emu.cpu.handleInterrupts(&app.emu.bus);
                app.emu.ppu.tick(cycles_taken);
                app.emu.timer.tick(cycles_taken);
                app.emu.apu.tick(cycles_taken);
                app.emu.apu.buffer_index = 0;
                cycle_acc -= cycles_taken;
            }
        }
    }

    // only shows the demo if dvui.Examples.show_demo_window is true
    // .full -> .lite or comment out to speed up compile times
    dvui.Examples.demo(.full);
    dvui.refresh(null, @src(), null);
    return .ok;
}

pub fn menu() !?dvui.App.Result {
    var hbox = dvui.box(@src(), .{ .dir = .horizontal }, .{ .style = .window, .background = true, .expand = .horizontal });
    defer hbox.deinit();

    var m = dvui.menu(@src(), .horizontal, .{});
    defer m.deinit();

    if (dvui.menuItemLabel(@src(), "File", .{ .submenu = true }, .{ .tag = "first-focusable" })) |r| {
        var fw = dvui.floatingMenu(@src(), .{ .from = r }, .{});
        defer fw.deinit();
        if (dvui.menuItemLabel(@src(), "Open Rom", .{}, .{}) != null) {
            const file_path = try dvui.dialogNativeFileOpen(gpa, .{ .title = "Open Rom file", .filters = &.{"*.gb"}, .filter_description = "Rom files", .path = "$HOME" });
            if (file_path) |path| {
                log.debug("File opened: {s}", .{path});
                const cart = try Emulator.Cartridge.fromFile(path, gpa, dvui.io);
                app.emu.deinit();
                app.emu = try Emulator.init(gpa, dvui.io);
                try app.emu.bus.loadBios(dvui.io);
                app.emu.bus.cartridge = cart;
                gpa.free(path);
            } else {
                log.debug("No file opened", .{});
            }
            m.close();
        }
        if (dvui.menuItemLabel(@src(), "Exit", .{}, .{ .expand = .horizontal }) != null) {
            return .close;
        }
    }
    if (dvui.menuItemLabel(@src(), "Window", .{ .submenu = true }, .{})) |r| {
        var fw = dvui.floatingMenu(@src(), .{ .from = r }, .{});
        defer fw.deinit();
        if (dvui.menuItemLabel(@src(), "Fullscreen", .{}, .{ .expand = .horizontal }) != null) {
            m.close();
            dvui.currentWindow().stateSet(.fullscreen);
        }
        if (dvui.menuItemLabel(@src(), "Maximize", .{}, .{ .expand = .horizontal }) != null) {
            m.close();
            dvui.currentWindow().stateSet(.maximize);
        }
        if (dvui.menuItemLabel(@src(), "Normal", .{}, .{ .expand = .horizontal }) != null) {
            m.close();
            dvui.currentWindow().stateSet(.normal);
        }
    }
    if (dvui.menuItemLabel(@src(), "Layout", .{ .submenu = true }, .{})) |r| {
        var fw = dvui.floatingMenu(@src(), .{ .from = r }, .{});
        defer fw.deinit();
        if (dvui.menuItemLabel(@src(), "Play", .{}, .{ .expand = .horizontal }) != null) {
            m.close();
            try panel_manager.applyPreset(.play, gpa);
        }

        if (dvui.menuItemLabel(@src(), "Full", .{}, .{ .expand = .horizontal }) != null) {
            m.close();
            try panel_manager.applyPreset(.full, gpa);
        }
    }
    if (dvui.menuItemLabel(@src(), "Debug", .{ .submenu = true }, .{})) |r| {
        var fw = dvui.floatingMenu(@src(), .{ .from = r }, .{});
        defer fw.deinit();
        if (builtin.mode == .Debug) {
            if (dvui.menuItemLabel(@src(), "Demo Window", .{}, .{ .expand = .horizontal }) != null) {
                m.close();
                dvui.Examples.show_demo_window = !dvui.Examples.show_demo_window;
            }
        }
    }

    return null;
}
