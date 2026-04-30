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
        ig.igText("Skip bios");
        ig.igSameLine();
        if (app.emu.skip_boot) {
            if (ig.igButton("true")) {
                app.emu.skip_boot = false;
            }
        } else {
            if (ig.igButton("false")) {
                app.emu.skip_boot = true;
            }
        }
    }
};
