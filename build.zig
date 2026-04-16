const std = @import("std");
const Build = std.Build;
const OptimizeMode = std.builtin.OptimizeMode;
const ResolvedTarget = Build.ResolvedTarget;
const Dependency = Build.Dependency;
const cimgui = @import("cimgui");
const zlinter = @import("zlinter");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const cimgui_conf = cimgui.getConfig(true);

    const dep_sokol = b.dependency("sokol", .{
        .target = target,
        .optimize = optimize,
        .with_sokol_imgui = true,
        .with_tracing = true,
    });
    const dep_cimgui = b.dependency("cimgui", .{
        .target = target,
        .optimize = optimize,
    });

    dep_sokol.artifact("sokol_clib").root_module.addIncludePath(dep_cimgui.path(cimgui_conf.include_dir));

    const mod_main = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "sokol", .module = dep_sokol.module("sokol") },
            .{ .name = "cimgui", .module = dep_cimgui.module(cimgui_conf.module_name) },
        },
    });

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

    const mod_options = b.addOptions();
    mod_main.addOptions("build_options", mod_options);

    const exe = b.addExecutable(.{
        .name = "zgb",
        .root_module = mod_main,
    });

    b.installArtifact(exe);

    // Running the app
    const run_exe = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run the application");

    run_step.dependOn(&run_exe.step);
}
