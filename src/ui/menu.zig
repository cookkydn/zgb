const AppState = @import("../app.zig").AppState;
const ig = @import("cimgui");

pub fn draw_menu(app: *AppState) void {
    if (!ig.igBeginMainMenuBar()) return;
    defer ig.igEndMainMenuBar();

    draw_file_menu(app);
    draw_controls_menu(app);
    draw_view_menu(app);
}

fn draw_file_menu(app: *AppState) void {
    if (!ig.igBeginMenu("File")) return;
    defer ig.igEndMenu();

    if (ig.igMenuItem("Open ROM")) {
        app.panels.rom_browser.visible = true;
        app.panels.rom_browser.refresh();
    }
}

fn draw_controls_menu(app: *AppState) void {
    if (!ig.igBeginMenu("Controls")) return;
    defer ig.igEndMenu();

    if (app.emu.pause) {
        if (ig.igMenuItem("Play")) {
            app.emu.pause = false;
        }
    } else {
        if (ig.igMenuItem("Pause")) {
            app.emu.pause = true;
        }
    }
}

fn draw_view_menu(app: *AppState) void {
    if (!ig.igBeginMenu("View")) return;
    defer ig.igEndMenu();

    if (ig.igBeginMenu("Layout")) {
        if (ig.igMenuItem("Default")) {
            app.layout.set_layout = .Default;
        }
        ig.igEndMenu();
    }
}
