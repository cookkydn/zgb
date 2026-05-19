const ig = @import("cimgui");
const std = @import("std");

/// Formats and prints text to ImGui without dynamic allocation.
/// Uses a 64-byte stack buffer, which is extremely fast and safe for UI.
///
/// Example:
/// ```zig
/// ui.fmt.print("Cycles: {d}", .{ emu.cycles });
/// ```
pub fn print(comptime format: []const u8, args: anytype) void {
    var buf: [64]u8 = undefined;
    const text_slice = std.fmt.bufPrintSentinel(
        &buf,
        format,
        args,
        0,
    ) catch |err| {
        std.log.warn("ImGui formatting error: {}", .{err});
        return;
    };
    ig.igTextUnformatted(text_slice.ptr);
}

/// Formatting function with a compile-time configurable buffer size.
/// Uses a stack-allocated buffer to ensure zero dynamic allocation and high performance.
pub fn printSize(comptime buf_size: usize, comptime format: []const u8, args: anytype) void {
    var buf: [buf_size]u8 = undefined;
    const text_slice = std.fmt.bufPrintSentinel(
        &buf,
        format,
        args,
        0,
    ) catch |err| {
        std.log.warn("ImGui formatting error: {}", .{err});
        return;
    };
    ig.igTextUnformatted(text_slice.ptr);
}

/// Prints text with a specific color.
/// Automatically handles the push/pop of the ImGui style color.
///
/// Example:
/// ```zig
/// ui.fmt.colored(0xFF0000FF, "Error: {}", .{ err });
/// ```
pub fn colored(color: u32, comptime format: []const u8, args: anytype) void {
    ig.igPushStyleColor(@intCast(ig.ImGuiCol_Text), color);
    defer ig.igPopStyleColor(1);
    print(format, args);
}

/// Prints disabled (grayed-out) text.
pub fn disabled(comptime format: []const u8, args: anytype) void {
    colored(0xFF888888, format, args); // Standard dark gray
}

/// Prints a bullet point followed by formatted text.
pub fn bullet(comptime format: []const u8, args: anytype) void {
    ig.igBullet();
    print(format, args);
}

/// Prints an 8-bit value in hexadecimal (e.g., 0xAF).
pub fn hex8(value: u8) void {
    print("0x{X:0>2}", .{value});
}

/// Prints a 16-bit value in hexadecimal (e.g., 0xFF40).
pub fn hex16(value: u16) void {
    print("0x{X:0>4}", .{value});
}

/// Prints an 8-bit value in binary (e.g., 0b10101111).
pub fn bin8(value: u8) void {
    print("0b{b:0>8}", .{value});
}

/// Prints a decimal value.
pub fn dec(value: anytype) void {
    print("{d}", .{value});
}

/// Prints a boolean value with semantic colors (Green for true, Red for false).
pub fn boolean(value: bool) void {
    if (value) {
        colored(0xFF4CAF50, "True", .{});
    } else {
        colored(0xFFF44336, "False", .{});
    }
}

/// Prints a key-value pair for an 8-bit hex value.
/// Example: "A: 0x01"
pub fn kvHex8(name: [*c]const u8, value: u8) void {
    ig.igTextUnformatted(name);
    ig.igSameLine();
    hex8(value);
}

/// Prints a key-value pair for a 16-bit hex value.
/// Example: "HL: 0x014D"
pub fn kvHex16(name: [*c]const u8, value: u16) void {
    ig.igTextUnformatted(name);
    ig.igSameLine();
    hex16(value);
}

/// Prints a key-value pair for a binary value.
/// Example: "LCDC: 0b10010001"
pub fn kvBin8(name: [*c]const u8, value: u8) void {
    ig.igTextUnformatted(name);
    ig.igSameLine();
    bin8(value);
}

/// Prints a key-value pair for a boolean value with colored output.
/// Example: "IME: True" (in green)
pub fn kvBool(name: [*c]const u8, value: bool) void {
    ig.igTextUnformatted(name);
    ig.igSameLine();
    boolean(value);
}

/// Prints a generic key-value pair with a dynamically formatted string value.
///
/// Example:
/// ```zig
/// ui.fmt.kvString("MBC Type", "{s}", .{ cart.mbc_name })
/// ```
pub fn kvString(name: [*c]const u8, comptime format: []const u8, args: anytype) void {
    ig.igTextUnformatted(name);
    ig.igSameLine();
    print(format, args);
}

/// Prints a 16-bit address range. Example: "[0x0000 - 0x3FFF]"
pub fn addressRange(start: u16, end: u16) void {
    print("[0x{X:0>4} - 0x{X:0>4}]", .{ start, end });
}

/// Prints a human-readable memory size.
/// Converts raw bytes to B, KB, or MB. Perfect for Cartridge ROM/RAM specs.
pub fn memorySize(bytes: usize) void {
    if (bytes >= 1024 * 1024) {
        print("{d} MB", .{bytes / (1024 * 1024)});
    } else if (bytes >= 1024) {
        print("{d} KB", .{bytes / 1024});
    } else {
        print("{d} B", .{bytes});
    }
}
