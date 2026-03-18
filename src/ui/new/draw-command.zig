const Rect = @import("rect.zig").Rect;
const Color = @import("../color.zig").Color;
const Context = @import("context.zig").Context;

pub const DrawCommand = union(enum) { RectFill: Rect, Text: struct {
    x: i32,
    y: i32,
    text: []const u8,
    color: Color,
}, CustomCallback: struct {
    render_fn: *const fn (ctx: *Context) void,
    data: *anyopaque,
} };
