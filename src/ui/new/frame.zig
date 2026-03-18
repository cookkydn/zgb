const Rect = @import("rect.zig").Rect;
const Context = @import("context.zig").Context;
const DrawCommand = @import("draw-command.zig").DrawCommand;

const std = @import("std");

pub const Frame = struct {
    id: u8,
    bounds: Rect,
    context: *Context,
    draw_list: std.ArrayList(DrawCommand),
};
