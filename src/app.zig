const dvui = @import("dvui");
const Emulator = @import("emu");
const std = @import("std");
pub const App = @This();

emu: Emulator,
screen_tex: dvui.Texture,
pixel_perfect_scaling: bool = false,
audio_stream: ?*dvui.backend.c.SDL_AudioStream = null,

pub fn init(allocator: std.mem.Allocator, io: std.Io) !App {
    var emu = try Emulator.init(allocator, io);
    try emu.bus.loadBios(io);
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
    if (self.audio_stream) |stream| {
        dvui.backend.c.SDL_DestroyAudioStream(stream);
    }
}
