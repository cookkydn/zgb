const std = @import("std");

const use_docking = @import("build_options").docking;
const ig = if (use_docking) @import("cimgui_docking") else @import("cimgui");
const emu = @import("../emu/root.zig");

pub const RomList = struct {
    visible: bool = false,
    all: std.mem.Allocator,
    cpu: *emu.CPU,
    roms: Directory,

    pub fn init(all: std.mem.Allocator, cpu: *emu.CPU) RomList {
        return .{
            .all = all,
            .roms = .{
                .name = "Roms",
                .content = std.ArrayList(DirEntry).initCapacity(all, 5) catch @panic(""),
            },
            .cpu = cpu,
        };
    }

    pub fn read_rom_folder(self: *RomList) !void {
        const path = try std.fs.path.join(self.all, &.{ ".", "roms" });
        var dir = try std.fs.cwd().openDir(path, .{
            .access_sub_paths = true,
            .iterate = true,
        });
        defer dir.close();
        try self.read_dir(&dir, path, &self.roms);
    }

    pub fn read_dir(self: *RomList, fs_dir: *std.fs.Dir, base_path: []u8, dir: *Directory) !void {
        var iter = fs_dir.iterate();
        while (try iter.next()) |entry| {
            switch (entry.kind) {
                .file => {
                    const rom_name = try self.all.dupeZ(u8, entry.name);
                    const path = try std.fs.path.joinZ(self.all, &.{ base_path, rom_name });

                    const rom: Rom = .{ .name = rom_name, .path = path };
                    try dir.*.content.append(self.all, .{ .Rom = rom });
                },
                .directory => {
                    const dir_name = try self.all.dupeZ(u8, entry.name);
                    const path = try std.fs.path.joinZ(self.all, &.{ base_path, entry.name });
                    var sub_dir_fs = try std.fs.cwd().openDir(path, .{
                        .access_sub_paths = true,
                        .iterate = true,
                    });
                    defer sub_dir_fs.close();
                    var sub_dir: Directory = .{
                        .name = dir_name,
                        .content = std.ArrayList(DirEntry).initCapacity(self.all, 10) catch @panic(""),
                    };
                    try self.read_dir(&sub_dir_fs, path, &sub_dir);
                    try dir.*.content.append(self.all, .{ .Directory = sub_dir });
                },
                else => {},
            }
        }
    }

    pub fn frame_dir(self: *RomList, dir: Directory) void {
        if (ig.igTreeNodeEx(dir.name.ptr, ig.ImGuiTreeNodeFlags_DefaultOpen)) {
            for (dir.content.items) |entry| {
                switch (entry) {
                    .Rom => |rom| {
                        ig.igBulletText("%s", rom.name.ptr);
                        if (ig.igIsItemHovered(ig.ImGuiHoveredFlags_None)) {
                            ig.igSetTooltip("%s", rom.path.ptr);
                        }
                        if (ig.igIsItemClicked()) {
                            self.cpu.bus.loadCartridge(rom.path) catch @panic("");
                            self.visible = false;
                        }
                    },
                    .Directory => |sub_dir| {
                        self.frame_dir(sub_dir);
                    },
                }
            }
            ig.igTreePop();
        }
    }

    pub fn frame(self: *RomList) void {
        if (!self.visible) return;
        defer ig.igEnd();
        if (!ig.igBegin("Open rom", &self.visible, ig.ImGuiWindowFlags_None)) return;
        self.frame_dir(self.roms);
    }
};

pub const Rom = struct {
    name: []const u8,
    path: []const u8,
};

pub const Directory = struct {
    name: []const u8,
    content: std.ArrayList(DirEntry),
};

pub const DirEntry = union(enum) {
    Directory: Directory,
    Rom: Rom,
};
