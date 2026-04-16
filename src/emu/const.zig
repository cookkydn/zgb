// Interrupts src
pub const v_blank_src = 0x40;
pub const stat_src = 0x48;
pub const timer_src = 0x50;
pub const serial_src = 0x58;
pub const joypad_src = 0x60;

// VIDEO constants
pub const screen_width = 160;
pub const screen_height = 144;

pub const argb_color_palette = struct {
    pub const white_off = 0xFF9CBC0F;
    pub const white = 0xFF9CBC0F;
    pub const light_gray = 0xFF8BAC0F;
    pub const dark_gray = 0xFF306230;
    pub const black = 0xFF10380F;
};

// Cpu
pub const cpu_freq: u32 = 4194304;

// Sound
pub const sample_rate: u32 = 48000;
