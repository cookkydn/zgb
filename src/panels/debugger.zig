const std = @import("std");
const AppState = @import("../app.zig").AppState;
const AddressList = std.ArrayList(u16);
const Allocator = std.mem.Allocator;

const WORKING_SIZE = 0xFFFF + 1;

pub const Debugger = struct {
    visible: bool = true,
    active: bool = false,
    allocator: Allocator,
    breakpoints: AddressList,

    pub fn init(all: Allocator) !Debugger {
        const breakpoints = try AddressList.initCapacity(all, 5);
        return .{
            .breakpoints = breakpoints,
            .allocator = all,
        };
    }

    pub fn deinit(self: *Debugger) void {
        self.breakpoints.clearAndFree(self.allocator);
    }
    pub fn draw(self: *Debugger, app: *AppState) void {
        if (!self.visible) return;
        _ = app;
    }
};
