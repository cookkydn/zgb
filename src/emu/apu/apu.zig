const constants = @import("../const.zig");
const std = @import("std");

// Audio processing unig
pub const APU = struct {
    audio_registers: [0x30]u8,
    /// Master_control
    nr52: u8 = 0,
    /// Panning
    nr51: u8 = 0,
    /// master_volume_and_panning
    nr50: u8 = 0,

    // -- Channel 1 (Pulse with period sweep) --
    /// Sweep
    nr10: u8 = 0,
    /// Length timer & duty cycle
    nr11: u8 = 0,
    /// Volume & enveloppe
    nr12: u8 = 0,
    /// Period low
    nr13: u8 = 0,
    /// Period high & control
    nr14: u8 = 0,

    // -- Channel 2 (Pulse) --
    /// Length timer & duty cycle
    nr21: u8 = 0,
    /// Volume & enveloppe
    nr22: u8 = 0,
    /// Period low
    nr23: u8 = 0,
    /// Period high & control
    nr24: u8 = 0,

    allocator: std.mem.Allocator,

    sample_counter: u32 = 0,

    buffer: [2048]f32 = undefined,
    buffer_index: usize = 0,
    frame_seq: i32 = 0,
    frame_step: u8 = 0,

    ch1_freq_timer: i32 = 0,
    ch1_duty_step: u8 = 0,
    ch1_length_counter: u8 = 0,
    ch1_volume: u8 = 0,
    ch1_env_timer: u8 = 0,
    ch1_sweep_timer: u8 = 0,

    ch2_freq_timer: i32 = 0,
    ch2_duty_step: u8 = 0,
    ch2_length_counter: u8 = 0,
    ch2_volume: u8 = 0,
    ch2_env_timer: u8 = 0,

    pub fn init(all: std.mem.Allocator) APU {
        return .{
            .audio_registers = .{0x00} ** 0x30,
            .allocator = all,
        };
    }

    pub fn deinit(self: *APU) void {
        _ = self;
        return;
    }

    pub fn tick(self: *APU, ticks: u16) void {
        self.frame_seq -= @as(i32, ticks);
        while (self.frame_seq <= 0) {
            self.frame_seq += 8192;
            self.frame_step = (self.frame_step + 1) % 8;

            // --- Canal 1 ---

            if (self.frame_step % 2 == 0) {
                if (self.is_ch1_length_enabled() and self.ch1_length_counter > 0) {
                    self.ch1_length_counter -= 1;
                    if (self.ch1_length_counter == 0) {
                        self.nr52 &= 0xFE;
                    }
                }
            }
            if (self.frame_step == 2 or self.frame_step == 6) {
                if (self.get_ch1_sweep_pace() != 0) {
                    if (self.ch1_sweep_timer > 0) {
                        self.ch1_sweep_timer -= 1;
                    }

                    if (self.ch1_sweep_timer == 0) {
                        self.ch1_sweep_timer = self.get_ch1_sweep_pace();
                        const shift = self.get_ch1_sweep_step();
                        const current_freq: u11 = self.get_ch1_freq();
                        const freq_mod: u11 = current_freq >> shift;
                        var new_freq: u16 = current_freq;

                        switch (self.get_ch1_sweep_dir()) {
                            .ADD => new_freq += freq_mod,
                            .SUB => {
                                if (current_freq >= freq_mod) {
                                    new_freq -= freq_mod;
                                } else {
                                    new_freq = 0;
                                }
                            },
                        }

                        if (new_freq > 0x7FF) {
                            self.nr52 &= 0xFE;
                        } else if (shift != 0) {
                            self.nr13 = @as(u8, @truncate(new_freq & 0xFF));
                            self.nr14 = (self.nr14 & 0xF8) | @as(u8, @truncate((new_freq >> 8) & 0x07));
                        }
                    }
                }
            }
            if (self.frame_step == 7) {
                if (self.get_ch1_envelope_pace() != 0) {
                    if (self.ch1_env_timer > 0) {
                        self.ch1_env_timer -= 1;
                    }

                    if (self.ch1_env_timer == 0) {
                        self.ch1_env_timer = self.get_ch1_envelope_pace();
                        switch (self.get_ch1_env_dir()) {
                            .INCREASE => {
                                if (self.ch1_volume < 15) self.ch1_volume += 1;
                            },
                            .DECREASE => {
                                if (self.ch1_volume > 0) self.ch1_volume -= 1;
                            },
                        }
                    }
                }
            }

            // --- Canal 2 ---

            if (self.frame_step % 2 == 0) {
                if (self.is_ch2_length_enabled() and self.ch2_length_counter > 0) {
                    self.ch2_length_counter -= 1;
                    if (self.ch2_length_counter == 0) {
                        self.nr52 &= 0xFD;
                    }
                }
            }
            if (self.frame_step == 7) {
                if (self.get_ch2_envelope_pace() != 0) {
                    if (self.ch2_env_timer > 0) {
                        self.ch2_env_timer -= 1;
                    }

                    if (self.ch2_env_timer == 0) {
                        self.ch2_env_timer = self.get_ch2_envelope_pace();
                        switch (self.get_ch2_env_dir()) {
                            .INCREASE => {
                                if (self.ch2_volume < 15) self.ch2_volume += 1;
                            },
                            .DECREASE => {
                                if (self.ch2_volume > 0) self.ch2_volume -= 1;
                            },
                        }
                    }
                }
            }
        }

        self.ch1_freq_timer -= @as(i32, ticks);
        while (self.ch1_freq_timer <= 0) {
            self.ch1_freq_timer += (2048 - @as(i32, self.get_ch1_freq())) * 4;
            self.ch1_duty_step = (self.ch1_duty_step + 1) % 8;
        }

        self.ch2_freq_timer -= @as(i32, ticks);
        while (self.ch2_freq_timer <= 0) {
            self.ch2_freq_timer += (2048 - @as(i32, self.get_ch2_freq())) * 4;
            self.ch2_duty_step = (self.ch2_duty_step + 1) % 8;
        }

        self.sample_counter += @as(u32, ticks) * constants.sample_rate;

        while (self.sample_counter >= constants.cpu_freq) {
            self.sample_counter -= constants.cpu_freq;

            const ch1_val: f32 = blk: {
                if (!self.is_on()) break :blk 0;
                if (!self.is_ch1_on()) break :blk 0;
                break :blk if (self.get_ch1_wave_duty()[self.ch1_duty_step]) @as(f32, @floatFromInt(self.ch1_volume)) / 15 else 0;
            };

            const ch2_val: f32 = blk: {
                if (!self.is_on()) break :blk 0;
                if (!self.is_ch2_on()) break :blk 0;
                break :blk if (self.get_ch2_wave_duty()[self.ch2_duty_step]) @as(f32, @floatFromInt(self.ch2_volume)) / 15 else 0;
            };

            self.buffer[self.buffer_index] = (ch1_val + ch2_val) / 2;
            self.buffer[self.buffer_index + 1] = (ch1_val + ch2_val) / 2;
            self.buffer_index += 2;
        }
    }

    fn is_on(self: *APU) bool {
        return self.nr52 & 0x80 > 0;
    }

    pub fn turn_off(self: *APU) void {
        // Reset channel 1
        self.nr10 = 0;
        self.nr11 = 0;
        self.nr12 = 0;
        self.nr13 = 0;
        self.nr14 = 0;

        // Reset channel 2
        self.nr21 = 0;
        self.nr22 = 0;
        self.nr23 = 0;
        self.nr24 = 0;

        self.nr52 = 0;
    }

    fn is_ch1_on(self: *APU) bool {
        return self.nr52 & 0x1 > 0;
    }

    fn is_ch2_on(self: *APU) bool {
        return self.nr52 & 0x2 > 0;
    }

    pub fn trigger_ch1(self: *APU) void {
        self.nr52 = self.nr52 | 1;
        if (self.ch1_length_counter == 0) self.ch1_length_counter = 64;
        self.ch1_freq_timer = (2048 - @as(i32, self.get_ch1_freq())) * 4;
        self.ch1_volume = (self.nr12 & 0xF0) >> 4;
    }

    pub fn trigger_ch2(self: *APU) void {
        self.nr52 = self.nr52 | 2;
        if (self.ch2_length_counter == 0) self.ch2_length_counter = 64;
        self.ch2_freq_timer = (2048 - @as(i32, self.get_ch2_freq())) * 4;
        self.ch2_volume = (self.nr22 & 0xF0) >> 4;
    }

    fn get_ch1_wave_duty(self: *APU) [8]bool {
        const duty: u2 = @truncate((self.nr11 & 0xC0) >> 6);
        return switch (duty) {
            0b00 => .{ true, true, true, true, true, true, true, false },
            0b01 => .{ false, true, true, true, true, true, true, false },
            0b10 => .{ false, true, true, true, true, false, false, false },
            0b11 => .{ true, false, false, false, false, false, false, true },
        };
    }

    fn get_ch2_wave_duty(self: *APU) [8]bool {
        const duty: u2 = @truncate((self.nr21 & 0xC0) >> 6);
        return switch (duty) {
            0b00 => .{ true, true, true, true, true, true, true, false },
            0b01 => .{ false, true, true, true, true, true, true, false },
            0b10 => .{ false, true, true, true, true, false, false, false },
            0b11 => .{ true, false, false, false, false, false, false, true },
        };
    }

    fn get_ch1_freq(self: *APU) u11 {
        return @as(u11, self.nr13) | (@as(u11, self.nr14 & 0x7) << 8);
    }

    fn is_ch1_length_enabled(self: *APU) bool {
        return self.nr14 & 0x40 != 0;
    }

    fn is_ch1_env_enabled(self: *APU) bool {
        return self.get_ch1_envelope_pace() != 0;
    }

    fn get_ch1_envelope_pace(self: *APU) u3 {
        return @truncate(self.nr12 & 0x07);
    }

    fn get_ch1_sweep_pace(self: *APU) u3 {
        return @truncate((self.nr10 & 0x70) >> 4);
    }

    fn get_ch1_sweep_dir(self: *APU) SweepDir {
        return @enumFromInt(@as(u1, @truncate((self.nr10 & 0x08) >> 3)));
    }

    fn get_ch1_sweep_step(self: *APU) u3 {
        return @truncate(self.nr10 & 0x07);
    }

    fn get_ch1_env_dir(self: *APU) EnvelopeDir {
        return @enumFromInt(@as(u1, @truncate((self.nr12 & 0x08) >> 3)));
    }

    fn get_ch2_freq(self: *APU) u11 {
        return @as(u11, self.nr23) | (@as(u11, self.nr24 & 0x7) << 8);
    }

    fn is_ch2_length_enabled(self: *APU) bool {
        return self.nr24 & 0x40 != 0;
    }

    fn is_ch2_env_enabled(self: *APU) bool {
        return self.get_ch2_envelope_pace() != 0;
    }

    fn get_ch2_envelope_pace(self: *APU) u3 {
        return @truncate(self.nr22 & 0x07);
    }

    fn get_ch2_env_dir(self: *APU) EnvelopeDir {
        return @enumFromInt(@as(u1, @truncate((self.nr22 & 0x08) >> 3)));
    }
};

const EnvelopeDir = enum(u1) {
    DECREASE = 0,
    INCREASE = 1,
};

const SweepDir = enum(u1) {
    ADD = 0,
    SUB = 1,
};
