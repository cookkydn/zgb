const Window = @import("window.zig").Window;
const Color = @import("color.zig").Color;
const sdl = @import("zsdl2");
const std = @import("std");
const components = @import("components/mod.zig");

pub const Frame = struct {
    window: *Window,
    pos_x: i32 = 0,
    pos_y: i32 = 0,
    width: i32 = 400,
    height: i32 = 400,

    cursor_x: i32 = 0,
    cursor_y: i32 = 0,
    padding: i32 = 10,

    pub fn init(window: *Window) Frame {
        return .{ .window = window };
    }

    pub fn begin(self: *Frame) !bool {
        const renderer = self.window.renderer;

        const bg_rect = sdl.Rect{ .x = self.pos_x, .y = self.pos_y, .w = self.width, .h = self.height };
        try renderer.setDrawColorRGBA(40, 40, 45, 255);
        try renderer.fillRect(bg_rect);

        try renderer.setDrawColorRGBA(80, 80, 90, 255);
        try sdl.renderDrawRect(renderer, bg_rect);

        self.cursor_x = self.padding;
        self.cursor_y = self.padding;

        try sdl.renderSetClipRect(renderer, &bg_rect);

        return true;
    }

    pub fn end(self: *Frame) !void {
        try sdl.renderSetClipRect(self.window.renderer, null);
    }

    pub fn allocateSpace(self: *Frame, w: i32, h: i32) void {
        if (w > self.width) self.width = w + 2 * self.padding;
        if (h > self.height - self.cursor_y) self.height = self.cursor_y + h + self.padding;
    }

    pub fn heading(self: *Frame, name: [:0]const u8) !void {
        if (self.cursor_y == self.padding) {
            self.cursor_y = 1;
        }
        const renderer = self.window.renderer;
        const title_rect = sdl.Rect{
            .x = self.pos_x + 1,
            .y = self.pos_y + self.cursor_y,
            .w = self.width - 2,
            .h = 42,
        };
        try renderer.setDrawColorRGBA(60, 60, 70, 255);
        try renderer.fillRect(title_rect);

        try self.text(
            name,
            self.padding + 2,
            self.cursor_y,
        );

        self.cursor_x = self.padding;
        self.cursor_y += self.padding + 42;
    }

    pub fn image(self: *Frame, texture: *sdl.Texture, w: i32, h: i32) !void {
        self.allocateSpace(w, h);
        const rect = sdl.Rect{
            .x = self.pos_x + self.cursor_x,
            .y = self.pos_y + self.cursor_y,
            .w = w,
            .h = h,
        };

        try sdl.renderCopy(self.window.renderer, texture, null, &rect);

        self.cursor_y += h + self.padding;
    }

    pub fn button(self: *Frame, label: []const u8, w: i32, h: i32) !bool {
        self.allocateSpace(w, h);
        _ = label;
        const rect = sdl.Rect{
            .x = self.pos_x + self.cursor_x,
            .y = self.pos_y + self.cursor_y,
            .w = w,
            .h = h,
        };

        var mouse_x: c_int = 0;
        var mouse_y: c_int = 0;
        const mouse_state = sdl.getMouseState(&mouse_x, &mouse_y);
        const is_mouse_down = mouse_state != 0;

        const is_hovered = mouse_x >= rect.x and mouse_x < rect.x + rect.w and
            mouse_y >= rect.y and mouse_y < rect.y + rect.h;

        var is_clicked = false;

        if (is_hovered and is_mouse_down) {
            try self.window.renderer.setDrawColorRGBA(80, 100, 150, 255);
            is_clicked = true;
        } else if (is_hovered) {
            try self.window.renderer.setDrawColorRGBA(60, 80, 120, 255);
        } else {
            try self.window.renderer.setDrawColorRGBA(50, 50, 60, 255);
        }

        try self.window.renderer.fillRect(rect);
        try self.window.renderer.setDrawColorRGBA(100, 100, 110, 255);
        try sdl.renderDrawRect(self.window.renderer, rect);

        self.cursor_y += h + self.padding;

        return is_clicked;
    }

    pub fn text(self: *Frame, msg: [:0]const u8, x: i32, y: i32) !void {
        const font = try self.window.font.get_font(38);
        const surf = try font.renderTextBlended(msg, Color.WHITE.IntoSDLColor());
        const tex = try sdl.createTextureFromSurface(self.window.renderer, surf);
        try sdl.renderCopy(self.window.renderer, tex, null, &.{
            .x = self.pos_x + x,
            .y = self.pos_y + y,
            .w = surf.clip_rect.w,
            .h = surf.clip_rect.h,
        });
    }

    pub fn textArea(self: *Frame) components.TextArea {
        return components.TextArea.init(self);
    }
};
