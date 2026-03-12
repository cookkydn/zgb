// Audio processing unig
pub const APU = struct {
    audio_registers: [0x17]u8,

    pub fn init() APU {
        return .{
            .audio_registers = .{0x00} ** 0x17,
        };
    }

    pub fn deinit(self: *APU) void {
        _ = self;
        return;
    }
};
