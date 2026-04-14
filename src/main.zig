const emu = @import("emu/root.zig");
const ig = @import("cimgui");
const sokol = @import("sokol");
const slog = sokol.log;
const sg = sokol.gfx;
const sgl = sokol.gl;
const sapp = sokol.app;
const sglue = sokol.glue;
const simgui = sokol.imgui;
const sgimgui = sokol.sgimgui;
const sgaudio = sokol.audio;
const std = @import("std");
const AppState = @import("app.zig").AppState;
const Decompiler = @import("decompiler.zig").Decompiler;
const RomBrowser = @import("panels/rom-browser.zig").RomBrowser;

var app_ref: *AppState = undefined;

pub fn main() void {
    const allocator = AppState.alloc_impl.allocator();
    var app = AppState.init(allocator);
    app_ref = &app;
    sapp.run(.{
        .init_userdata_cb = init,
        .frame_userdata_cb = app_wrapper("frame", null),
        .cleanup_userdata_cb = app_wrapper("deinit", null),
        .event_userdata_cb = app_wrapper("event", AppState.Event),
        .window_title = "ZGB",
        .width = 1440,
        .height = 900,
        .user_data = &app,
        .icon = .{
            .sokol_default = true,
        },
        .logger = .{
            .func = slog.func,
        },
    });
}

export fn init(user_data: ?*anyopaque) void {
    const app: *AppState = @ptrCast(@alignCast(user_data.?));
    app.init_sokol();

    // app.decompiler = Decompiler.init(app.allocator, &app.emu.cpu.bus);
    // app.rom_list = RomList.init(app.allocator, &app.emu.cpu);
    // app.rom_list.read_rom_folder() catch @panic("Failed to read ROM folder");
    // app.debug = DebugWindow.init(&app.emu.cpu);
}

pub const panic = std.debug.FullPanic(crash);

pub fn crash(msg: []const u8, first_trace_addr: ?usize) noreturn {
    app_ref.emu.print_performance_report();
    std.debug.defaultPanic(msg, first_trace_addr);
}

fn GetCallbackType(comptime OptArgType: ?type) type {
    if (OptArgType) |T| {
        return *const fn (T, ?*anyopaque) callconv(.c) void;
    } else {
        return *const fn (?*anyopaque) callconv(.c) void;
    }
}

pub fn app_wrapper(comptime method_name: []const u8, comptime OptArgType: ?type) GetCallbackType(OptArgType) {
    if (OptArgType) |ArgType| {
        return struct {
            pub fn cb(arg: ArgType, user_data: ?*anyopaque) callconv(.c) void {
                const ptr: *AppState = @ptrCast(@alignCast(user_data.?));
                @field(AppState, method_name)(ptr, arg);
            }
        }.cb;
    } else {
        return struct {
            pub fn cb(user_data: ?*anyopaque) callconv(.c) void {
                const ptr: *AppState = @ptrCast(@alignCast(user_data.?));
                @field(AppState, method_name)(ptr);
            }
        }.cb;
    }
}
