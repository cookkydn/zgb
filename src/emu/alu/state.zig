pub const CpuState = struct {
    stopped: bool = false,
    halted: bool = false,
    ime: IMEState = .DISABLED,
};

pub const IMEState = enum {
    DISABLED,
    ENABLED,
    /// Meaning will be enabled next iteration
    ENABLED_NEXT,
};
