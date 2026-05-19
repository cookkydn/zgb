const ig = @import("cimgui");

extern "c" fn igDockBuilderAddNode(node_id: ig.ImGuiID, flags: ig.ImGuiDockNodeFlags) ig.ImGuiID;
extern "c" fn igDockBuilderRemoveNode(node_id: ig.ImGuiID) void;
extern "c" fn igDockBuilderSetNodeSize(node_id: ig.ImGuiID, size: ig.ImVec2) void;
extern "c" fn igDockBuilderSplitNode(node_id: ig.ImGuiID, split_dir: ig.ImGuiDir, size_ratio_for_node_at_dir: f32, out_id_at_dir: *ig.ImGuiID, out_id_at_opposite_dir: *ig.ImGuiID) ig.ImGuiID;
extern "c" fn igDockBuilderDockWindow(window_name: [*c]const u8, node_id: ig.ImGuiID) void;
extern "c" fn igDockBuilderFinish(node_id: ig.ImGuiID) void;
extern "c" fn igDockBuilderSetNodePos(node_id: ig.ImGuiID, pos: ig.ImVec2) void;
extern "c" fn igDockBuilderGetNode(node_id: ig.ImGuiID) ig.ImGuiID;

pub fn add_node(node_id: ig.ImGuiID) void {
    // 1024 is dockspace flag, not available in exposed lib
    _ = igDockBuilderAddNode(node_id, 1024);
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
pub const get_node = igDockBuilderGetNode;
