pub const CpuState = struct {
    stopped: bool,
    halted: bool,
    ime: IMEState,

    pub fn init() CpuState {
        return .{
            .halted = false,
            .stopped = false,
            .ime = .DISABLED,
        };
    }
};

pub const IMEState = enum {
    ENABLED,
    DISABLED,
    /// Meaning will be enabled next iteration
    ENABLED_NEXT,
};
