const ui = @import("ui");
const CPU = @import("emu").CPU;
pub const RegisterView = struct { ctx: ui.UiContext, console: ui.TextConsole };

pub fn create_view() !RegisterView {
    var registers_view = try ui.UiContext.init(.{
        .title = "Registers",
        .h = 180,
        .w = 120,
    });
    const text_console = ui.TextConsole.init(&registers_view, 20, ui.Color.WHITE);
    return .{ .console = text_console, .ctx = registers_view };
}

pub fn render_view(view: *RegisterView, cpu: *CPU) !void {
    try view.ctx.set_dpi_scale();
    try view.ctx.begin_frame();
    view.console.resetCursor();
    view.console.print("AF: {x:0>4}", .{cpu.registers.get_af()});
    view.console.print("BC: {x:0>4}", .{cpu.registers.get_bc()});
    view.console.print("DE: {x:0>4}", .{cpu.registers.get_de()});
    view.console.print("HL: {x:0>4}", .{cpu.registers.get_hl()});
    view.console.print("SP: {x:0>4}", .{cpu.registers.sp});
    view.console.print("PC: {x:0>4}", .{cpu.registers.pc});
    view.console.print("DOT: {}", .{cpu.bus.ppu.dots});
    try view.ctx.end_frame();
}
