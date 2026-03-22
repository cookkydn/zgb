const std = @import("std");
const Build = std.Build;
const OptimizeMode = std.builtin.OptimizeMode;
const ResolvedTarget = Build.ResolvedTarget;
const Dependency = Build.Dependency;
const sokol = @import("sokol");
const cimgui = @import("cimgui");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const opt_docking = b.option(bool, "docking", "Build with docking support") orelse false;
    const cimgui_conf = cimgui.getConfig(opt_docking);

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
            .{ .name = cimgui_conf.module_name, .module = dep_cimgui.module(cimgui_conf.module_name) },
        },
    });

    const mod_options = b.addOptions();

    mod_options.addOption(bool, "docking", opt_docking);
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
