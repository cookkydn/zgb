const ig = @import("cimgui");

/// Draws a standard clickable button.
/// Returns true if the button was clicked during this frame.
///
/// Example:
/// ```zig
/// if (ui.inputs.button("Step Into")) { emu.step(); }
/// ```
pub fn button(label: [*c]const u8) bool {
    return ig.igButton(label);
}

/// Draws a standard checkbox.
/// Directly modifies the referenced boolean value.
/// Returns true if the value was toggled during this frame.
///
/// Example:
/// ```zig
/// ui.inputs.checkbox("Show Background", &emu.ppu.show_bg);
/// ```
pub fn checkbox(label: [*c]const u8, v: *bool) bool {
    return ig.igCheckbox(label, v);
}

/// Draws a horizontal slider for an integer value.
/// Great for volume controls or frame skip settings.
///
/// Example:
/// ```zig
/// ui.inputs.sliderInt("Volume", &app.volume, 0, 100);
/// ```
pub fn sliderInt(label: [*c]const u8, v: *c_int, v_min: c_int, v_max: c_int) bool {
    return ig.igSliderInt(label, v, v_min, v_max, "%d", ig.ImGuiSliderFlags_None);
}

/// Draws a horizontal slider for a floating-point value.
/// Great for display scaling or audio frequency tweaks.
pub fn sliderFloat(label: [*c]const u8, v: *f32, v_min: f32, v_max: f32) bool {
    return ig.igSliderFloat(label, v, v_min, v_max, "%.3f", ig.ImGuiSliderFlags_None);
}

/// Draws a text input field using a Zig slice for the buffer.
/// `flags` should be a combination of `InputTextFlag` values (e.g., to force hex).
///
/// Example:
/// ```zig
/// var addr_buf: [5]u8 = [_]u8{0} ** 5;
/// if (ui.inputs.inputText("Go to Address", &addr_buf, ui.flags.InputTextFlag.AddressInput)) {
///     // Handle the new address...
/// }
/// ```
pub fn inputText(label: [*c]const u8, buf: []u8, flags: c_int) bool {
    return ig.igInputText(label, buf.ptr, buf.len, flags, null, null);
}

/// Draws a dropdown menu (Combo box).
/// `items_separated_by_zeros` must be a single string where items are separated by `\0`,
/// with a double `\0\0` at the very end.
///
/// Example:
/// ```zig
/// var speed_idx: c_int = 0;
/// ui.inputs.combo("Emulation Speed", &speed_idx, "1x\0 2x\0 Max\0\0");
/// ```
pub fn combo(label: [*c]const u8, current_item: *c_int, items_separated_by_zeros: [*c]const u8) bool {
    return ig.igCombo_Str(label, current_item, items_separated_by_zeros, -1);
}
