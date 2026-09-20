const App = @import("app.zig");
const dvui = @import("dvui");
const std = @import("std");
const emu = @import("emu");
const JoypadBtn = emu.JoypadBtn;

const Key = dvui.enums.Key;

var keyMap = std.EnumMap(Key, JoypadBtn).init(.{
    .up = .up,
    .down = .down,
    .left = .left,
    .right = .right,
    .x = .a,
    .c = .b,
    .s = .start,
    .d = .select,
});

pub fn handleEvents(app: *App) void {
    for (dvui.events()) |*ev| {
        if (ev.evt != .key) continue;
        if (keyMap.contains(ev.evt.key.code)) {
            if (ev.evt.key.action == .down) {
                app.emu.joypad.press(keyMap.getAssertContains(ev.evt.key.code));
            } else if (ev.evt.key.action == .up) {
                app.emu.joypad.release(keyMap.getAssertContains(ev.evt.key.code));
            }
            ev.handle(@src(), dvui.currentWindow().data());
        }
    }
}
