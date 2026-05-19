const ig = @import("cimgui");
const std = @import("std");
const dock_builder = @import("dock-builder.zig");

pub const LayoutPreset = enum {
    Default,
    Debug,
};

pub fn applyLayout(dockspace_id: *ig.ImGuiID, layout: LayoutPreset) void {
    const viewport = ig.igGetMainViewport();
    dock_builder.add_node(dockspace_id.*);
    dock_builder.set_node_size(dockspace_id.*, viewport.*.WorkSize);

    switch (layout) {
        .Default => {
            dock_builder.dock_window("Screen", dockspace_id.*);
        },
        .Debug => {
            const dock_id_left = dock_builder.split_node(
                dockspace_id,
                ig.ImGuiDir_Left,
                0.2,
            );
            const dock_id_down = dock_builder.split_node(
                dockspace_id,
                ig.ImGuiDir_Down,
                0.25,
            );

            dock_builder.dock_window("Debug", dock_id_left);
            dock_builder.dock_window("Vram viewer", dock_id_down);
        },
    }
    dock_builder.finish(dockspace_id.*);
}
