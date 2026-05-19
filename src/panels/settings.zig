const AppState = @import("../app.zig").AppState;
const ig = @import("cimgui");
const ui = @import("ui");

const WindowFlag = ui.flags.WindowFlag;

pub const SettingsPanel = struct {
    visible: bool = false,

    pub fn draw(self: *SettingsPanel, app: *AppState) void {
        if (!self.visible) return;
        ig.igSetNextWindowFocus();
        if (!ig.igBegin("Settings", &self.visible, @intFromEnum(WindowFlag.SettingsPanel))) return;
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
