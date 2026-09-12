const std = @import("std");
const Build = std.Build;
const OptimizeMode = std.builtin.OptimizeMode;
const ResolvedTarget = Build.ResolvedTarget;
const Dependency = Build.Dependency;
const zlinter = @import("zlinter");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Linter
    {
        const lint_cmd = b.step("lint", "Lint source code.");
        lint_cmd.dependOn(step: {
            // Swap in and out whatever rules you see fit from RULES.md
            var builder = zlinter.builder(b, .{ .optimize = .ReleaseFast });
            builder.addRule(.{
                .builtin = .field_naming,
            }, .{
                .struct_field_min_len = .{ .len = 1, .severity = .warning },
                .union_field_min_len = .{ .len = 1, .severity = .warning },
                .enum_field_min_len = .{ .len = 1, .severity = .warning },
            });
            builder.addRule(.{ .builtin = .field_ordering }, .{});
            builder.addRule(.{
                .builtin = .declaration_naming,
            }, .{
                .decl_name_min_len = .{ .len = 1, .severity = .warning },
            });
            builder.addRule(.{ .builtin = .function_naming }, .{});
            builder.addRule(.{ .builtin = .file_naming }, .{});
            builder.addRule(.{ .builtin = .import_ordering }, .{});
            builder.addRule(.{ .builtin = .switch_case_ordering }, .{});
            builder.addRule(.{ .builtin = .no_unused }, .{});
            builder.addRule(.{ .builtin = .no_deprecated }, .{});
            builder.addRule(.{ .builtin = .no_orelse_unreachable }, .{});
            break :step builder.build();
        });
    }
    // CPU testing
    {
        const cpu_test_mod = b.addModule("cpu_test", .{
            .root_source_file = b.path("src/test_cpu.zig"),
            .optimize = optimize,
            .target = target,
        });
        const cpu_test_mod_options = b.addOptions();
        cpu_test_mod.addOptions("build_options", cpu_test_mod_options);
        const cpu_test_exe = b.addExecutable(.{
            .name = "ZGB-cpu-test",
            .root_module = cpu_test_mod,
        });
        const test_cpu_cmd = b.step("test_cpu", "Run the CPU test");
        const run_cpu_tests = b.addRunArtifact(cpu_test_exe);
        test_cpu_cmd.dependOn(&run_cpu_tests.step);
    }

    const dvui_dep = b.dependency("dvui", .{ .target = target, .optimize = optimize, .backend = .sdl3 });

    const mod_main = b.createModule(.{
        .root_source_file = b.path("src/dvui-main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Emu
    const emu_mod = b.addModule("emu", .{
        .root_source_file = b.path("src/emu/root.zig"),
    });

    mod_main.addImport("emu", emu_mod);

    const mod_options = b.addOptions();
    mod_main.addOptions("build_options", mod_options);

    const exe = b.addExecutable(.{
        .name = "zgb",
        .root_module = mod_main,
    });
    exe.root_module.addImport("dvui", dvui_dep.module("dvui_sdl3"));
    exe.root_module.addImport("sdl-backend", dvui_dep.module("sdl3"));

    b.installArtifact(exe);

    // Running the app
    const run_exe = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run the application");

    run_step.dependOn(&run_exe.step);
}
