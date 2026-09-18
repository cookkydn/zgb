const emu = @import("emu/root.zig");
const ig = @import("cimgui");
const Decompiler = @import("decompiler.zig").Decompiler;
// const LayoutManager = @import("ui/layout.zig").LayoutManager;
const SettingsPanel = @import("panels/settings.zig").SettingsPanel;
const VramViewer = @import("panels/vram-viewer.zig").VramViewer;

const std = @import("std");
const Allocator = std.mem.Allocator;

// -- Sokol imports --
const sokol = @import("sokol");
const sapp = sokol.app;
const simgui = sokol.imgui;
const sgaudio = sokol.audio;

// -- Global state --
pub const AppState = struct {
    pub fn event(self: *AppState, ev: Event) void {
        _ = simgui.handleEvent(ev.*);
        if (ig.igGetIO().*.WantCaptureKeyboard) return;
        self.emu.gb.joypad.handleEvent(ev);
    }
};

pub const Emulator = struct {
    pub fn pushSound(self: *Emulator) void {
        const apu = &self.gb.apu;
        if (apu.buffer_index >= apu.buffer.len) {
            apu.buffer_index = 0;
            for (apu.buffer, 0..) |_, i| {
                apu.buffer[i] *= self.volume;
            }
            _ = sgaudio.push(&apu.buffer[0], apu.buffer.len / 2);
        }
    }

    pub fn frameEmu(self: *Emulator) void {
        if (!self.pause) {
            while (self.skip_boot and self.gb.bus.is_bios_loaded) {
                var cycles_taken: u16 = 4;
                if (!self.gb.cpu.state.halted) {
                    const instr = emu.Instruction.fromBus(&self.gb.bus);
                    cycles_taken = self.gb.cpu.execute_instruction(instr, &self.gb.bus);
                }
                cycles_taken += self.gb.cpu.handleInterrupts(&self.gb.bus);
                self.gb.ppu.tick(cycles_taken);
                self.gb.timer.tick(cycles_taken);
                self.gb.apu.tick(cycles_taken);
                self.gb.apu.buffer_index = 0;
            }

            self.cycle_acc += sapp.frameDuration() * cpu_freq;
            if (self.cycle_acc > cpu_freq / 10.0) {
                self.cycle_acc /= 10;
                if (self.is_overloaded == false) {
                    self.overload_count += 1;
                    self.is_overloaded = true;
                }
            } else {
                self.is_overloaded = false;
            }
            while (self.cycle_acc > 0) {
                var cycles_taken: u16 = 4;
                if (!self.gb.cpu.state.halted) {
                    const instr = emu.Instruction.fromBus(&self.gb.bus);
                    cycles_taken = self.gb.cpu.execute_instruction(instr, &self.gb.bus);
                }
                cycles_taken += self.gb.cpu.handleInterrupts(&self.gb.bus);
                self.gb.ppu.tick(cycles_taken);
                self.gb.timer.tick(cycles_taken);
                self.gb.apu.tick(cycles_taken);
                self.pushSound();
                self.cycle_acc -= @floatFromInt(cycles_taken);
            }
        }
    }
};
