pub const flags = @import("flags.zig");
pub const fmt = @import("fmt.zig");
pub const inputs = @import("inputs.zig");
pub const layout = @import("layout.zig");
pub const tables = @import("tables.zig");

pub const Texture = @import("texture.zig");

const ig = @import("cimgui");

var current_layout: layout.LayoutPreset = .Default;
var next_layout: ?layout.LayoutPreset = .Default;

pub fn begin() void {
    const window_flags = [_]flags.WindowFlag{
        .MenuBar,
        .NoDocking,
        .NoTitleBar,
        .NoCollapse,
        .NoResize,
        .NoMove,
        .NoBringToFrontOnFocus,
        .NoNavFocus,
    };

    ig.igPushStyleVarImVec2(ig.ImGuiStyleVar_WindowPadding, .{ .x = 0.0, .y = 0.0 });
    ig.igPushStyleVar(ig.ImGuiStyleVar_WindowRounding, 0.0);

    const vp = ig.igGetMainViewport();
    ig.igSetNextWindowPos(vp.*.Pos, ig.ImGuiCond_Always);
    ig.igSetNextWindowSize(vp.*.Size, ig.ImGuiCond_Always);
    ig.igSetNextWindowViewport(vp.*.ID);

    _ = ig.igBegin("Dockspace", null, flags.combine(window_flags));

    var dockspace_id = ig.igGetID("Dockspace");
    _ = ig.igDockSpaceEx(dockspace_id, vp.*.WorkSize, ig.ImGuiDockNodeFlags_AutoHideTabBar, null);

    if (next_layout) |lay| {
        layout.applyLayout(&dockspace_id, lay);
        next_layout = null;
    }

    ig.igEnd();
    ig.igPopStyleVarEx(2);
}

pub fn setLayout(new_layout: layout.LayoutPreset) void {
    next_layout = new_layout;
    current_layout = new_layout;
}

pub fn getLayout() layout.LayoutPreset {
    return current_layout;
}
