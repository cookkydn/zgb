const Emulator = @import("emu");
const std = @import("std");
const dvui = @import("dvui");
pub const App = @This();

emu: Emulator,
screen_tex: dvui.Texture,
pixel_perfect_scaling: bool = false,

pub fn init(allocator: std.mem.Allocator, io: std.Io) !App {
    var emu = try Emulator.init(allocator, io);
    const screen_tex = try dvui.textureCreate(
        @ptrCast(&emu.ppu.frame_buffer),
        .{
            .height = 144,
            .width = 160,
            .interpolation = .nearest,
        },
    );
    return .{
        .emu = emu,
        .screen_tex = screen_tex,
    };
}

pub fn deinit(self: *App) void {
    self.emu.deinit();
}
