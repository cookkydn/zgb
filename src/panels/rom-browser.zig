const std = @import("std");
const ig = @import("cimgui");
const AppState = @import("../app.zig").AppState;

pub const RomBrowser = struct {
    visible: bool = false,
    allocator: std.mem.Allocator,
    files: std.ArrayList(BrowserEntry),
    selected_idx: ?usize = null,
    current_path: [:0]const u8,

    pub fn init(allocator: std.mem.Allocator) RomBrowser {
        const current_path: [:0]const u8 = allocator.dupeZ(u8, "./roms") catch @panic("Failed allocate memory");
        return .{
            .allocator = allocator,
            .files = std.ArrayList(BrowserEntry).initCapacity(allocator, 5) catch @panic("Failed allocate memory"),
            .current_path = current_path,
        };
    }

    pub fn deinit(self: *RomBrowser) void {
        self.clearFiles();
        self.files.deinit(self.allocator);
        self.allocator.free(self.current_path);
    }

    fn clearFiles(self: *RomBrowser) void {
        for (self.files.items) |entry| {
            switch (entry) {
                .File => |file| {
                    self.allocator.free(file);
                },
                .Directory => |dir| {
                    self.allocator.free(dir);
                },
            }
        }
        self.files.clearRetainingCapacity();
    }

    pub fn refresh(self: *RomBrowser) void {
        self.clearFiles();
        self.selected_idx = null;
        var dir = std.fs.cwd().openDir(self.current_path, .{ .iterate = true }) catch |err| {
            std.debug.print("Failed to open folder {s} : {}\n", .{ self.current_path, err });
            return;
        };
        defer dir.close();

        var iter = dir.iterate();
        while (iter.next() catch null) |entry| {
            if (entry.kind == .file) {
                const name = entry.name;

                if (std.mem.endsWith(u8, name, ".gb") or std.mem.endsWith(u8, name, ".gbc") or std.mem.endsWith(u8, name, ".rom")) {
                    if (self.allocator.dupeZ(u8, name)) |name_z| {
                        self.files.append(self.allocator, .{ .File = name_z }) catch {};
                    } else |_| {}
                }
            } else if (entry.kind == .directory) {
                const name = entry.name;
                if (self.allocator.dupeZ(u8, name)) |name_z| {
                    self.files.append(self.allocator, .{ .Directory = name_z }) catch {};
                } else |_| {}
            }
        }

        std.mem.sort(BrowserEntry, self.files.items, {}, BrowserEntry.cmpBrowserEntries);
    }

    pub fn draw(self: *RomBrowser, app: *AppState) void {
        if (!self.visible) return;

        ig.igSetNextWindowSize(.{ .x = 400, .y = 300 }, ig.ImGuiCond_FirstUseEver);

        if (ig.igBegin("Sélectionner une ROM", &self.visible, ig.ImGuiWindowFlags_None)) {
            if (ig.igButton("Rafraîchir")) {
                self.refresh();
            }

            ig.igSameLine();
            ig.igText("Dossier : %s", self.current_path.ptr);
            ig.igSeparator();

            if (ig.igBeginListBox("##roms_list", .{ .x = -1.0, .y = -1.0 })) {
                var need_refresh = false;
                for (self.files.items, 0..) |entry, i| {
                    switch (entry) {
                        .File => |file| {
                            const is_selected = (self.selected_idx != null and self.selected_idx.? == i);

                            if (ig.igSelectableEx(file.ptr, is_selected, ig.ImGuiSelectableFlags_AllowDoubleClick, .{ .x = 0, .y = 0 })) {
                                self.selected_idx = i;

                                if (ig.igIsMouseDoubleClicked(ig.ImGuiMouseButton_Left)) {
                                    self.loadRom(app, file);
                                }
                            }
                        },
                        .Directory => |dir| {
                            const is_selected = (self.selected_idx != null and self.selected_idx.? == i);
                            const dir_label = std.fmt.allocPrintSentinel(self.allocator, "[d] {s}", .{dir}, 0) catch @panic("Memory allocation failed");
                            defer self.allocator.free(dir_label);
                            if (ig.igSelectableEx(dir_label, is_selected, ig.ImGuiSelectableFlags_AllowDoubleClick, .{ .x = 0, .y = 0 })) {
                                self.selected_idx = i;

                                if (ig.igIsMouseDoubleClicked(ig.ImGuiMouseButton_Left)) {
                                    const current_path = self.current_path;
                                    self.current_path = std.fmt.allocPrintSentinel(self.allocator, "{s}/{s}", .{ self.current_path, dir }, 0) catch @panic("Failed to allocate memory");
                                    self.allocator.free(current_path);
                                    need_refresh = true;
                                }
                            }
                        },
                    }
                }
                if (need_refresh) {
                    self.refresh();
                }
                ig.igEndListBox();
            }
        }
        ig.igEnd();
    }

    fn loadRom(self: *RomBrowser, app: *AppState, filename: [:0]const u8) void {
        const full_path = std.fmt.allocPrintSentinel(self.allocator, "{s}/{s}", .{ self.current_path, filename }, 0) catch return;
        defer self.allocator.free(full_path);
        app.emu.cpu.bus.loadCartridge(full_path) catch @panic("Failed to load cartridge");
        app.emu.pause = false;
        self.visible = false;
    }
};

const BrowserEntry = union(enum) {
    File: [:0]u8,
    Directory: [:0]u8,

    fn cmpBrowserEntries(context: void, a: BrowserEntry, b: BrowserEntry) bool {
        _ = context;

        const a_is_dir = a == .Directory;
        const b_is_dir = b == .Directory;

        if (a_is_dir and !b_is_dir) return true;
        if (!a_is_dir and b_is_dir) return false;

        const name_a = switch (a) {
            .File => |n| n,
            .Directory => |n| n,
        };
        const name_b = switch (b) {
            .File => |n| n,
            .Directory => |n| n,
        };

        return std.ascii.lessThanIgnoreCase(name_a, name_b);
    }
};
