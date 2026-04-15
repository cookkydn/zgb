const AppState = @import("../app.zig").AppState;
const ig = @import("cimgui");

pub const SettingsPanel = struct {
    visible: bool = false,

    pub fn draw(self: *SettingsPanel, app: *AppState) void {
        if (!self.visible) return;
        ig.igSetNextWindowFocus();
        if (!ig.igBegin("Settings", &self.visible, ig.ImGuiWindowFlags_None)) return;
        defer ig.igEnd();

        _ = ig.igSliderFloat("Volume", &app.emu.volume, 0, 1);
    }
};
