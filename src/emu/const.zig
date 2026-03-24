// Interrupts src
pub const V_BLANK_SRC = 0x40;
pub const STAT_SRC = 0x48;
pub const TIMER_SRC = 0x50;
pub const SERIAL_SRC = 0x58;
pub const JOYPAD_SRC = 0x60;

// VIDEO constants
pub const SCREEN_WIDTH = 160;
pub const SCREEN_HEIGHT = 144;

pub const ARGB_COLOR_PALETTE = struct {
    pub const WHITE_OFF = 0xFF9CBC0F;
    pub const WHITE = 0xFF9CBC0F;
    pub const LIGHT_GRAY = 0xFF8BAC0F;
    pub const DARK_GRAY = 0xFF306230;
    pub const BLACK = 0xFF10380F;
};

// Cartridge header
pub const CartridgeType = 0x0147;

// Memory registers
pub const IF = 0xFF0F;
pub const IE = 0xFFFF;
