const std = @import("std");
const sdl = @import("zsdl2");
const instr_view = @import("views/instructions.zig");
const reg_view = @import("views/registers.zig");
// const Emulator = @import("emulator.zig").Emulator;
// const view = @import("video/view.zig");
// const sdl = @import("zsdl2");
// const Ctx = @import("video/view.zig").Ctx;
// const render_lcd = @import("views/lcd.zig").render_lcd;
const ui = @import("ui");

const emu = @import("emu");
const App = @import("app.zig").App;

pub fn main() !void {
    var app = App.init();
    defer app.deinit();
    var cpu = emu.CPU.init(.DMG0);
    var reg_v = try reg_view.create_view();
    var instr_v = try instr_view.create_view();
    var canvas = try ui.Canvas.init(app.window, 160, 144);
    reg_v.console.ui_ctx = &reg_v.ctx;
    instr_v.console.ui_ctx = &instr_v.ctx;
    defer cpu.deinit();
    try cpu.bus.loadCartridge("tetris.gb");

    var frame_rect: sdl.Rect = .{ .x = 400, .y = 100, .w = 160 * 4, .h = 144 * 4 };

    var ev: sdl.Event = undefined;
    while (true) {
        if (sdl.pollEvent(&ev)) {
            if (ev.type == sdl.EventType.quit) {
                break;
            }
        }
        try reg_view.render_view(&reg_v, &cpu);
        try instr_view.render_view(&instr_v);
        try canvas.draw(&cpu.bus.ppu.frame_buffer, &frame_rect);
        app.window.present();
        for (0..70224) |_| {
            const instr = emu.Instruction.from_bus(&cpu.bus);
            const cycles_taken = cpu.execute_instruction(instr);
            instr_view.update_list(&instr_v, instr);
            cpu.bus.ppu.tick(cycles_taken);
            cpu.bus.timer.tick(cycles_taken);
            cpu.interrupts.handle_interrupts();
        }
    }
    // var fps_counter = try ui.UiContext.init(.{
    //     .title = "Zgb",
    //     .w = 600,
    //     .h = 600,
    // });

    // var mem_info = try ui.UiContext.init(.{
    //     .title = "Zgb - Mem",
    //     .w = 400,
    //     .h = 600,
    // });
    // var mem_info_text = ui.TextConsole.init(&mem_info, 20, ui.Color.WHITE);

    // fps_counter.set_resizable(true);
    // const allocator = std.heap.page_allocator;
    // var emulator = try Emulator.init(allocator);
    // try emulator.start();
    // var ev: sdl.Event = undefined;
    // try emulator.load_cart("./carts/tetris.gb");
    // while (true) {
    //     if (sdl.pollEvent(&ev)) {
    //         if (ev.type == sdl.EventType.quit) {
    //             break;
    //         } else if (ev.type == sdl.EventType.keydown) {
    //             switch (ev.key.keysym.sym) {
    //                 sdl.Keycode.a => {
    //                     std.debug.print(
    //                         "scale: {any}\n",
    //                         .{ui.UiContext.get_scale(fps_counter.window, fps_counter.renderer)},
    //                     );
    //                 },
    //                 else => {},
    //             }
    //         }
    //     }
    //     if (emulator.cpu.quit) {
    //         break;
    //     }
    //     emulator.step();
    //     try fps_counter.set_dpi_scale();
    //     try fps_counter.begin_frame();
    //     try fps_counter.draw_filled_rect(10, 40, 580, 40, ui.Color.BLUE);
    //     try render_lcd(&fps_counter, &emulator, allocator);
    //     try fps_counter.end_frame();

    //     try mem_info.set_dpi_scale();
    //     try mem_info.begin_frame();
    //     mem_info_text.resetCursor();
    //     mem_info_text.print("AF: {x:0>4}", .{emulator.cpu.registers.get_af()});
    //     mem_info_text.print("BC: {x:0>4}", .{emulator.cpu.registers.get_bc()});
    //     mem_info_text.print("DE: {x:0>4}", .{emulator.cpu.registers.get_de()});
    //     mem_info_text.print("HL: {x:0>4}", .{emulator.cpu.registers.get_hl()});
    //     mem_info_text.print("SP: {x:0>4}", .{emulator.cpu.registers.sp});
    //     mem_info_text.print("PC: {x:0>4}", .{emulator.cpu.registers.pc});
    //     try mem_info.end_frame();
    // }
}
