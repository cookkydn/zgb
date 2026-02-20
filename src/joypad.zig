const CPU = @import("cpu.zig").CPU;
const sdl = @import("zsdl2");
const std = @import("std");
pub const Joypad = struct {
    start: bool,
    select: bool,
    a: bool,
    b: bool,
    up: bool,
    down: bool,
    left: bool,
    right: bool,
    pub fn init() Joypad {
        return .{
            .start = false,
            .select = false,
            .a = false,
            .b = false,
            .up = false,
            .down = false,
            .left = false,
            .right = false,
        };
    }

    pub fn get_cpu(self: *Joypad) *CPU {
        return @alignCast(@fieldParentPtr("joypad", self));
    }

    pub fn handle_events(self: *Joypad) void {
        var ev: sdl.Event = undefined;
        self.get_cpu().mem.data[0xFF00] |= 0xF;
        const select_buttons = self.get_cpu().mem.data[0xFF00] & 0b00100000 == 0;
        const select_d_pad = self.get_cpu().mem.data[0xFF00] & 0b00010000 == 0;
        if (sdl.pollEvent(&ev)) {
            if (ev.type == sdl.EventType.quit) {
                self.get_cpu().quit = true;
            } else if (ev.type == sdl.EventType.mousebuttondown) {
                self.get_cpu().pause = true;
            } else if (ev.type == sdl.EventType.mousebuttonup) {
                self.get_cpu().pause = false;
            } else if (ev.type == sdl.EventType.keydown) {
                std.debug.print("KEYDOWN: {} = {}", .{ ev.key.keysym.sym, sdl.Keycode.@"return" });
                switch (ev.key.keysym.sym) {
                    sdl.Keycode.kp_enter, sdl.Keycode.@"return" => {
                        self.start = true;
                    },
                    sdl.Keycode.up => {
                        self.up = true;
                    },
                    sdl.Keycode.down => {
                        self.down = true;
                    },
                    sdl.Keycode.right => {
                        self.right = true;
                    },
                    sdl.Keycode.left => {
                        self.left = true;
                    },
                    sdl.Keycode.lshift => {
                        self.select = true;
                    },
                    sdl.Keycode.a => {
                        self.b = true;
                    },
                    sdl.Keycode.s => {
                        self.a = true;
                    },
                    else => {},
                }
            } else if (ev.type == sdl.EventType.keyup) {
                std.debug.print("KEYUP: {} = {}", .{ ev.key.keysym.sym, sdl.Keycode.@"return" });
                switch (ev.key.keysym.sym) {
                    sdl.Keycode.kp_enter, sdl.Keycode.@"return" => {
                        self.start = false;
                    },
                    sdl.Keycode.up => {
                        self.up = false;
                    },
                    sdl.Keycode.down => {
                        self.down = false;
                    },
                    sdl.Keycode.right => {
                        self.right = false;
                    },
                    sdl.Keycode.left => {
                        self.left = false;
                    },
                    sdl.Keycode.lshift => {
                        self.select = false;
                    },
                    sdl.Keycode.a => {
                        self.b = false;
                    },
                    sdl.Keycode.s => {
                        self.a = false;
                    },
                    else => {},
                }
            }
        }
        if (select_buttons) {
            self.get_cpu().mem.data[0xFF00] &= if (self.start) @as(u8, 0b11110111) else 0xFF;
            self.get_cpu().mem.data[0xFF00] &= if (self.select) @as(u8, 0b11111011) else 0xFF;
            self.get_cpu().mem.data[0xFF00] &= if (self.b) @as(u8, 0b11111101) else 0xFF;
            self.get_cpu().mem.data[0xFF00] &= if (self.a) @as(u8, 0b11111110) else 0xFF;
        }
        if (select_d_pad) {
            self.get_cpu().mem.data[0xFF00] &= if (self.down) @as(u8, 0b11110111) else 0xFF;
            self.get_cpu().mem.data[0xFF00] &= if (self.up) @as(u8, 0b11111011) else 0xFF;
            self.get_cpu().mem.data[0xFF00] &= if (self.left) @as(u8, 0b11111101) else 0xFF;
            self.get_cpu().mem.data[0xFF00] &= if (self.right) @as(u8, 0b11111110) else 0xFF;
        }
        // if (!self.get_cpu().pause) {
        //     std.debug.print("Joypad: 0b{b:0>8} select_buttons: {} select_d_pad: {} ", .{ self.get_cpu().mem.data[0xFF00], select_buttons, select_d_pad });
        // }
    }
};
