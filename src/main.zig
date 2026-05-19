const AppState = @import("app.zig").AppState;
const sokol = @import("sokol");
const std = @import("std");
const slog = sokol.log;
const sapp = sokol.app;

var app_ref: *AppState = undefined;

pub fn main(init: std.process.Init) void {
    const allocator = AppState.alloc_impl.allocator();
    const io = init.io;
    var app = AppState.init(allocator, io);
    app_ref = &app;
    sapp.run(.{
        .init_userdata_cb = init_app,
        .frame_userdata_cb = appWrapper("frame", null),
        .cleanup_userdata_cb = appWrapper("deinit", null),
        .event_userdata_cb = appWrapper("event", AppState.Event),
        .window_title = "ZGB",
        .width = 930,
        .height = 860,
        .user_data = &app,
        .icon = .{
            .sokol_default = true,
        },
        .logger = .{
            .func = slog.func,
        },
    });
}

export fn init_app(user_data: ?*anyopaque) void {
    const app: *AppState = @ptrCast(@alignCast(user_data.?));
    app.initSokol();
}

fn GetCallbackType(comptime OptArgType: ?type) type {
    if (OptArgType) |T| {
        return *const fn (T, ?*anyopaque) callconv(.c) void;
    } else {
        return *const fn (?*anyopaque) callconv(.c) void;
    }
}

pub fn appWrapper(comptime method_name: []const u8, comptime OptArgType: ?type) GetCallbackType(OptArgType) {
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
