const sapp = @import("sokol").app;
const alu = @import("../cpu/arithmetics.zig");
pub const Joypad = struct {
    p1_joyp: u8 = 0x3F,

    press_state: packed struct(u8) {
        up: bool = false,
        down: bool = false,
        left: bool = false,
        right: bool = false,
        a: bool = false,
        b: bool = false,
        start: bool = false,
        select: bool = false,
    } = .{},

    pub fn handle_event(self: *Joypad, ev: [*c]const sapp.Event) void {
        const is_down = ev.*.type == sapp.EventType.KEY_DOWN;
        const is_up = ev.*.type == sapp.EventType.KEY_UP;
        if (is_down or is_up) {
            switch (ev.*.key_code) {
                .UP => if (is_down) self.press(.up) else self.release(.up),
                .DOWN => if (is_down) self.press(.down) else self.release(.down),
                .LEFT => if (is_down) self.press(.left) else self.release(.left),
                .RIGHT => if (is_down) self.press(.right) else self.release(.right),

                .X => if (is_down) self.press(.a) else self.release(.a),
                .Z => if (is_down) self.press(.b) else self.release(.b),

                .ENTER => if (is_down) self.press(.start) else self.release(.start),
                .SPACE => if (is_down) self.press(.select) else self.release(.select),

                else => {},
            }
        }
        self.update_reg();
        return;
    }

    pub fn press(self: *Joypad, joy_btn: JoypadBtn) void {
        switch (joy_btn) {
            .up => self.press_state.up = true,
            .down => self.press_state.down = true,
            .left => self.press_state.left = true,
            .right => self.press_state.right = true,
            .a => self.press_state.a = true,
            .b => self.press_state.b = true,
            .start => self.press_state.start = true,
            .select => self.press_state.select = true,
        }
    }

    pub fn release(self: *Joypad, joy_btn: JoypadBtn) void {
        switch (joy_btn) {
            .up => self.press_state.up = false,
            .down => self.press_state.down = false,
            .left => self.press_state.left = false,
            .right => self.press_state.right = false,
            .a => self.press_state.a = false,
            .b => self.press_state.b = false,
            .start => self.press_state.start = false,
            .select => self.press_state.select = false,
        }
    }

    pub fn update_reg(self: *Joypad) void {
        var action_bits: u8 = 0x0F;
        var dpad_bits: u8 = 0x0F;

        if (self.press_state.start) action_bits &= ~(@as(u8, 1) << 3); // 0b0111
        if (self.press_state.select) action_bits &= ~(@as(u8, 1) << 2); // 0b1011
        if (self.press_state.b) action_bits &= ~(@as(u8, 1) << 1); // 0b1101
        if (self.press_state.a) action_bits &= ~(@as(u8, 1) << 0); // 0b1110

        if (self.press_state.down) dpad_bits &= ~(@as(u8, 1) << 3); // 0b0111
        if (self.press_state.up) dpad_bits &= ~(@as(u8, 1) << 2); // 0b1011
        if (self.press_state.left) dpad_bits &= ~(@as(u8, 1) << 1); // 0b1101
        if (self.press_state.right) dpad_bits &= ~(@as(u8, 1) << 0); // 0b1110

        var lower_nibble: u8 = 0x0F;

        if (self.p1_joyp & 0x20 == 0) {
            lower_nibble &= action_bits;
        }

        if (self.p1_joyp & 0x10 == 0) {
            lower_nibble &= dpad_bits;
        }

        self.p1_joyp = 0xC0 | (self.p1_joyp & 0x30) | lower_nibble;
    }
};

pub const JoypadBtn = union(enum) {
    up,
    down,
    left,
    right,
    a,
    b,
    start,
    select,
};
