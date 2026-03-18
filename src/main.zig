const std = @import("std");
const sdl = @import("zsdl2");

const emu = @import("emu");
const App = @import("app.zig").App;
const ui = @import("ui");

pub fn main() !void {
    var cpu = emu.CPU.init(.DMG0);
    var window = try ui.Window.init("Zbg", 1280, 720);
    var screen_frame = ui.Frame.init(&window);
    var registers_frame = ui.Frame.init(&window);
    registers_frame.pos_x = 700;
    registers_frame.pos_y = 20;
    const texture = try sdl.createTexture(
        window.renderer,
        sdl.PixelFormatEnum.argb8888,
        sdl.TextureAccess.streaming,
        @intCast(160),
        @intCast(144),
    );
    const pitch: c_int = @intCast(160 * @sizeOf(u32));
    screen_frame.pos_x = 20;
    screen_frame.pos_y = 20;
    defer window.deinit();
    defer cpu.deinit();
    try cpu.bus.loadCartridge("tetris.gb");

    while (window.is_running) {
        var event: sdl.Event = undefined;
        while (window.pollEvents(&event)) {

            // if (debugger.processEvent(&event)) continue;
            // emu.handleInput(&event);
        }

        for (0..70224) |_| {
            const instr = emu.Instruction.from_bus(&cpu.bus);
            const cycles_taken = cpu.execute_instruction(instr);
            cpu.bus.ppu.tick(cycles_taken);
            cpu.bus.timer.tick(cycles_taken);
            cpu.interrupts.handle_interrupts();
        }

        try window.beginFrame();

        if (try screen_frame.begin()) {
            try sdl.updateTexture(
                texture,
                null,
                @constCast(&cpu.bus.ppu.frame_buffer),
                pitch,
            );
            try screen_frame.heading("Rendering output");
            try screen_frame.image(texture, 160 * 4, 144 * 4);
            try screen_frame.end();
        }

        if (try registers_frame.begin()) {
            try registers_frame.heading("Registers");
            var textarea = registers_frame.textArea();
            try textarea.print("PC: 0x{x:0>4}", .{cpu.registers.pc});
            try textarea.render();
            try registers_frame.end();
        }

        window.endFrame();
    }
}
