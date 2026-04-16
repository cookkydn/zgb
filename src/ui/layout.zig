const ig = @import("cimgui");
const std = @import("std");
const AppState = @import("../app.zig").AppState;
pub const LayoutManager = struct {
    pub const Panels = struct {
        pub const screen = "Écran";
        pub const disasm = "Désassembleur";
        pub const cpu = "État du CPU (Registres)";
        pub const ppu = "Inspecteur PPU (Vidéo)";
        pub const controls = "Controls";
    };

    pub const LayoutType = enum {
        Default,
        None,
    };

    pub fn applyLayout(dockspace_id: ig.ImGuiID, layout_type: LayoutType, app: *AppState) void {
        _ = app;
        DockBuilder.remove_node(dockspace_id);
        const actual_dock_id = DockBuilder.add_node(dockspace_id);

        if (actual_dock_id == 0) {
            std.debug.panic("Failed to create DockBuilder node\n", .{});
        }

        DockBuilder.set_node_size(actual_dock_id, ig.igGetMainViewport().*.WorkSize);
        DockBuilder.set_node_pos(actual_dock_id, .{ .x = 0, .y = 18 });
        var center = actual_dock_id;

        switch (layout_type) {
            .Default => {
                // Without this the screen is not docking idk why
                _ = DockBuilder.split_node(&center, ig.ImGuiDir_Down, 0);
                DockBuilder.dock_window(Panels.screen, center);
            },
            .None => {},
        }

        DockBuilder.finish(actual_dock_id);
    }
};

const DockBuilder = struct {
    extern "c" fn igDockBuilderAddNode(node_id: ig.ImGuiID, flags: ig.ImGuiDockNodeFlags) ig.ImGuiID;
    extern "c" fn igDockBuilderRemoveNode(node_id: ig.ImGuiID) void;
    extern "c" fn igDockBuilderSetNodeSize(node_id: ig.ImGuiID, size: ig.ImVec2) void;
    extern "c" fn igDockBuilderSplitNode(node_id: ig.ImGuiID, split_dir: ig.ImGuiDir, size_ratio_for_node_at_dir: f32, out_id_at_dir: *ig.ImGuiID, out_id_at_opposite_dir: *ig.ImGuiID) ig.ImGuiID;
    extern "c" fn igDockBuilderDockWindow(window_name: [*c]const u8, node_id: ig.ImGuiID) void;
    extern "c" fn igDockBuilderFinish(node_id: ig.ImGuiID) void;
    extern "c" fn igDockBuilderSetNodePos(node_id: ig.ImGuiID, pos: ig.ImVec2) void;

    pub fn add_node(node_id: ig.ImGuiID) ig.ImGuiID {
        // 1024 is dockspace flag, not available in exposed lib
        return igDockBuilderAddNode(node_id, 1024);
    }
    pub const remove_node = igDockBuilderRemoveNode;
    pub const set_node_size = igDockBuilderSetNodeSize;
    pub fn split_node(main_node: *ig.ImGuiID, dir: ig.ImGuiDir, ratio: f32) ig.ImGuiID {
        var new_node: ig.ImGuiID = 0;
        _ = igDockBuilderSplitNode(main_node.*, dir, ratio, &new_node, main_node);
        return new_node;
    }
    pub const dock_window = igDockBuilderDockWindow;
    pub const finish = igDockBuilderFinish;
    pub const set_node_pos = igDockBuilderSetNodePos;
};
