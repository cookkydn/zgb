const ui = @import("ui");
const emu = @import("emu");
pub const InstructionView = struct {
    ctx: ui.UiContext,
    console: ui.TextConsole,
    instructions: [10]?emu.Instruction,
};

pub fn create_view() !InstructionView {
    var registers_view = try ui.UiContext.init(.{
        .title = "Instruction",
        .h = 250,
        .w = 600,
    });
    const text_console = ui.TextConsole.init(&registers_view, 20, ui.Color.WHITE);
    return .{ .console = text_console, .ctx = registers_view, .instructions = .{null} ** 10 };
}

pub fn update_list(view: *InstructionView, instruction: emu.Instruction) void {
    for (0..9) |i| {
        view.instructions[i] = view.instructions[i + 1];
    }
    view.instructions[9] = instruction;
}

pub fn render_view(view: *InstructionView) !void {
    try view.ctx.set_dpi_scale();
    try view.ctx.begin_frame();
    view.console.resetCursor();
    for (0..10) |i| {
        if (view.instructions[i]) |instr| view.console.print("{f}", .{instr});
    }
    try view.ctx.end_frame();
}
