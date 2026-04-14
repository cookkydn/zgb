const sg = @import("sokol").gfx;
const simgui = @import("sokol").imgui;
const ig = @import("cimgui");

pub const Texture = struct {
    image: sg.Image = .{},
    view: sg.View = .{},
    sampler: sg.Sampler = .{},
    width: i32,
    height: i32,

    pub fn init(width: i32, height: i32) Texture {
        const img_desc = sg.ImageDesc{
            .width = width,
            .height = height,
            .pixel_format = .SRGB8A8,
            .usage = .{ .stream_update = true },
            .label = "screen_image",
        };
        const image = sg.makeImage(img_desc);

        const smp_desc = sg.SamplerDesc{
            .min_filter = .NEAREST,
            .mag_filter = .NEAREST,
            .wrap_u = .CLAMP_TO_EDGE,
            .wrap_v = .CLAMP_TO_EDGE,
            .label = "screen_sampler",
        };
        const sampler = sg.makeSampler(smp_desc);

        const view_desc = sg.ViewDesc{
            .texture = .{ .image = image },
        };
        const view = sg.makeView(view_desc);

        return .{
            .image = image,
            .view = view,
            .sampler = sampler,
            .width = width,
            .height = height,
        };
    }

    pub fn update(self: *Texture, pixels_ptr: anytype) void {
        var image_data = sg.ImageData{};
        image_data.mip_levels[0] = sg.Range{
            .ptr = pixels_ptr,
            .size = @as(usize, @intCast(self.width * self.height * 4)),
        };
        sg.updateImage(self.image, image_data);
    }

    pub fn imTextureId(self: Texture) ig.ImTextureID {
        return simgui.imtextureid(self.view);
    }

    pub fn deinit(self: *Texture) void {
        sg.destroyView(self.view);
        sg.destroySampler(self.sampler);
        sg.destroyImage(self.image);
    }
};
