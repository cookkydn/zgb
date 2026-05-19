const sg = @import("sokol").gfx;
const simgui = @import("sokol").imgui;
const ig = @import("cimgui");
const std = @import("std");
const zeroes = std.mem.zeroes;

pub const Texture = @This();

image: sg.Image,
view: sg.View,
sampler: sg.Sampler,
width: usize,
height: usize,

/// Initializes a new dynamic texture with the given dimensions.
///
/// Example: `const screen = ui.texture.Texture.init(160, 144);`
pub fn init(width: usize, height: usize) Texture {
    var im_desc = zeroes(sg.ImageDesc);
    im_desc.width = @intCast(width);
    im_desc.height = @intCast(height);
    im_desc.pixel_format = .SRGB8A8;
    im_desc.usage = .{ .stream_update = true };

    const image = sg.makeImage(im_desc);

    var sp_desc = zeroes(sg.SamplerDesc);
    sp_desc.min_filter = .NEAREST;
    sp_desc.mag_filter = .NEAREST;
    sp_desc.wrap_u = .CLAMP_TO_EDGE;
    sp_desc.wrap_v = .CLAMP_TO_EDGE;

    var v_desc = zeroes(sg.ViewDesc);
    v_desc.texture = .{ .image = image };

    return .{
        .image = image,
        .sampler = sg.makeSampler(sp_desc),
        .view = sg.makeView(v_desc),
        .width = width,
        .height = height,
    };
}

/// Destroys the underlying Sokol image. Must be called when the application shuts down.
pub fn deinit(self: Texture) void {
    sg.destroyImage(self.image);
    sg.destroySampler(self.sampler);
    sg.destroyView(self.view);
}

/// Updates the texture with new pixel data.
/// `pixels` must be a slice containing exactly (width * height) bytes.
///
/// Example: `screen.update(&emu.ppu.framebuffer);`
pub fn update(self: Texture, pixels: []const u32) void {
    // Prevent Sokol crashes by ensuring the slice size matches the texture capacity
    std.debug.assert(pixels.len == self.width * self.height);

    var img_data = zeroes(sg.ImageData);

    img_data.mip_levels[0] = sg.Range{
        .ptr = pixels.ptr,
        .size = pixels.len * 4,
    };
    sg.updateImage(self.image, img_data);
}

/// Draws the texture in the current ImGui window.
/// `scale` multiplies the native resolution (e.g., scale=2.0 for 320x288).
pub fn draw(self: Texture, scale: f32) void {
    const w = @as(f32, @floatFromInt(self.width)) * scale;
    const h = @as(f32, @floatFromInt(self.height)) * scale;
    self.drawSized(w, h);
}

/// Draws the texture with a specific explicit size in pixels.
pub fn drawSized(self: Texture, width: f32, height: f32) void {
    // Convert the Sokol image handle into an ImGui Texture ID
    const tex_id = simgui.imtextureid(self.view);
    const tex_ref = ig.ImTextureRef{ ._TexID = tex_id, ._TexData = null };
    ig.igImage(
        tex_ref,
        .{ .x = width, .y = height },
    );
}

/// Draws a specific sub-region of the texture using normalized UV coordinates.
/// `uv0` and `uv1` must be between 0.0 and 1.0.
pub fn drawRegion(self: Texture, width: f32, height: f32, uv0_x: f32, uv0_y: f32, uv1_x: f32, uv1_y: f32) void {
    const tex_id = simgui.imtextureid(self.view);

    ig.igImageEx(
        tex_id,
        .{ .x = width, .y = height },
        .{ .x = uv0_x, .y = uv0_y },
        .{ .x = uv1_x, .y = uv1_y },
    );
}

/// Extracts and draws a single 8x8 tile from this texture.
/// Assuming this texture holds a full VRAM tilemap (e.g., 128x192 pixels),
/// this function will calculate the UVs to render only the requested tile.
///
/// `tile_x` and `tile_y` are grid coordinates (e.g., column 2, row 5).
/// `display_size` is the final rendered size (e.g., 32.0 to zoom it 4x).
pub fn drawTile(self: Texture, tile_x: usize, tile_y: usize, display_size: f32) void {
    const tile_w = 8.0;
    const tile_h = 8.0;

    const tex_w = @as(f32, @floatFromInt(self.width));
    const tex_h = @as(f32, @floatFromInt(self.height));

    const pixel_x = @as(f32, @floatFromInt(tile_x)) * tile_w;
    const pixel_y = @as(f32, @floatFromInt(tile_y)) * tile_h;

    // Convert exact pixel coordinates to normalized UV coordinates (0.0 -> 1.0)
    const uv0_x = pixel_x / tex_w;
    const uv0_y = pixel_y / tex_h;
    const uv1_x = (pixel_x + tile_w) / tex_w;
    const uv1_y = (pixel_y + tile_h) / tex_h;

    self.drawRegion(display_size, display_size, uv0_x, uv0_y, uv1_x, uv1_y);
}
