const Context = @import("context.zig").Context;
const Frame = @import("frame.zig").Frame;

const std = @import("std");
const sdl = @import("zsdl2");

pub const Window = struct {
    frames: std.AutoArrayHashMap(u8, Frame),
    context: Context,
    width: i32 = 1280,
    height: i32 = 720,
    is_running: bool = true,

    window: *sdl.Window,
    renderer: *sdl.Renderer,
};
