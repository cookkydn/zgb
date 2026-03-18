const Frame = @import("../frame.zig").Frame;
const std = @import("std");

pub const TextArea = struct {
    frame: *Frame,

    height: i32 = 50,
    width: i32 = 50,
    max_height: ?i32 = null,

    curr_line: usize = 0,
    lines: [64][128:0]u8 = undefined,
    line_lengths: [64]usize = [_]usize{0} ** 64,

    pub fn init(frame: *Frame) TextArea {
        return .{ .frame = frame };
    }

    pub fn print(self: *TextArea, comptime fmt: []const u8, args: anytype) !void {
        if (self.curr_line >= 64) return;

        var temp_buf: [128]u8 = undefined;
        const text = std.fmt.bufPrint(&temp_buf, fmt, args) catch return;

        const idx = self.curr_line;

        @memcpy(self.lines[idx][0..text.len], text);
        self.lines[idx][text.len] = 0;
        self.line_lengths[idx] = text.len;
        self.curr_line += 1;

        if (self.width < text.len * 16 + 20) {
            self.width = @intCast(text.len * 16 + 20);
        }

        if (self.height < self.curr_line * 16 + 20) {
            self.height = @intCast(self.curr_line * 16 + 20);
        }
    }

    pub fn render(self: *TextArea) !void {
        self.frame.allocateSpace(self.width, self.height);
        for (0..self.curr_line) |i| {
            try self.frame.text(&self.lines[i], 10, @intCast(i * 26));
        }
    }
};
