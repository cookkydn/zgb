const AppState = @import("../app.zig").AppState;
const ig = @import("cimgui");
const ui = @import("ui");

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

    if (ig.igMenuItem("Settings")) {
        app.panels.settings.visible = true;
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
            ui.setLayout(.Default);
            app.panels.debug.visible = false;
            app.panels.vram.visible = false;
        }
        if (ig.igMenuItem("Debug")) {
            ui.setLayout(.Debug);
            app.panels.debug.visible = true;
            app.panels.vram.visible = true;
        }
        ig.igEndMenu();
    }
}
