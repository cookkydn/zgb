const dvui = @import("dvui");
const std = @import("std");
const log = @import("logger.zig").log;
const App = @import("app.zig");

const DockingWidget = dvui.DockingWidget;
const Layout = DockingWidget.Layout;

const render_viewport = @import("panels/viewport.zig").render_viewport;
const render_cpu_debug = @import("panels/cpu_debug.zig").render;
const render_joypad = @import("panels/joypad.zig").render_joypad;
const render_bg = @import("panels/background_viewer.zig").render_bg_viewer;
const render_vis = @import("panels/sound_visualizer.zig").render_vis;
const render_decompiler = @import("panels/decompiler.zig").render_decompiler;

pub const PanelId = enum {
    debugger,
    viewport,
    joypad,
    bg_view,
    sound_visualiser,
    decompiler,

    pub fn toDvuiId(id: PanelId) Layout.PanelId {
        return @tagName(id);
    }

    pub fn fromDvuiId(id: Layout.PanelId) ?PanelId {
        return std.meta.stringToEnum(PanelId, id);
    }
};

pub const PanelState = struct {
    visible: bool,
    rect: dvui.Rect,
};

pub const LayoutPreset = enum { full, play, sound, decompiler };

pub const PanelDesc = struct {
    title: [:0]const u8,
    render_fn: *const fn (app: *App, bounds: dvui.Rect) anyerror!void,
};

var docking_layout: ?Layout.DockLayout = null;
var is_play_mode: bool = true;

fn panelInfo(id: Layout.PanelId) DockingWidget.PanelInfo {
    if (PanelId.fromDvuiId(id)) |pId| {
        return .{ .title = registry.get(pId).title, .closable = true };
    }
    log.err("ID: {s} not found in registry", .{id});
    return .{ .title = id, .closable = true };
}

/// Keeps each leaf's otherwise-identical menu widgets distinct.
fn drawHeaderExtra(_: Layout.PanelId) void {}

const registry = std.EnumArray(PanelId, PanelDesc).init(.{
    .viewport = .{ .title = "Gameboy screen", .render_fn = render_viewport },
    .debugger = .{ .title = "CPU debugger", .render_fn = render_cpu_debug },
    .joypad = .{ .title = "Joypad", .render_fn = render_joypad },
    .bg_view = .{ .title = "Background", .render_fn = render_bg },
    .sound_visualiser = .{ .title = "Sound visualizer", .render_fn = render_vis },
    .decompiler = .{ .title = "Decompiler", .render_fn = render_decompiler },
});

pub fn applyPreset(preset: LayoutPreset, allocator: std.mem.Allocator) !void {
    deinit();
    is_play_mode = preset == .play;

    switch (preset) {
        .play => {},
        .sound => {
            docking_layout = try Layout.DockLayout.initSingleLeaf(allocator, PanelId.toDvuiId(.viewport));
            if (docking_layout) |*lay| {
                try lay.splitLeaf(lay.root, .right, PanelId.toDvuiId(.sound_visualiser));
            }
        },
        .decompiler => {
            docking_layout = try Layout.DockLayout.initSingleLeaf(allocator, PanelId.toDvuiId(.viewport));
            if (docking_layout) |*lay| {
                try lay.splitLeaf(lay.root, .right, PanelId.toDvuiId(.decompiler));
            }
        },
        .full => {
            docking_layout = try Layout.DockLayout.initSingleLeaf(allocator, PanelId.toDvuiId(.viewport));
            if (docking_layout) |*lay| {
                try lay.splitLeaf(lay.root, .right, PanelId.toDvuiId(.debugger));

                if (lay.findPanel(PanelId.toDvuiId(.viewport))) |viewport_leaf| {
                    try lay.splitLeaf(viewport_leaf, .bottom, PanelId.toDvuiId(.joypad));
                }
                if (lay.findPanel(PanelId.toDvuiId(.debugger))) |viewport_leaf| {
                    try lay.splitLeaf(viewport_leaf, .bottom, PanelId.toDvuiId(.bg_view));
                }
            }
        },
    }
}

pub fn render(allocator: std.mem.Allocator, app: *App) !void {
    if (docking_layout == null) {
        try applyPreset(.play, allocator);
    }
    if (is_play_mode) {
        try registry.get(.viewport).render_fn(app, dvui.parentGet().data().contentRect());
        return;
    }
    var dock = dvui.dockspace(@src(), .{
        .layout = &docking_layout.?,
        .panelInfo = panelInfo,
        .drawHeaderExtra = drawHeaderExtra,
        .panel_background = .{
            .background = true,
            .border = dvui.Rect.all(1),
            .corners = dvui.CornerRect.all(5),
            .margin = dvui.Rect.all(1),
        },
        .tab_options = .{
            .expand = .horizontal,
        },
    }, .{
        .expand = .both,
    });
    defer dock.deinit();
    while (dock.panel()) |p| {
        defer p.end();
        if (PanelId.fromDvuiId(p.id)) |pId| {
            try registry.get(pId).render_fn(app, dvui.parentGet().data().contentRect());
        } else {
            dvui.label(@src(), "Unknown panel: {s}", .{p.id}, .{});
        }
    }
}

pub fn deinit() void {
    if (docking_layout) |*lay| {
        lay.deinit();
        docking_layout = null;
    }
}
