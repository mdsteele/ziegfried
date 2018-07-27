const Builder = @import("std").build.Builder;

pub fn build(b: &Builder) void {
    const mode = b.standardReleaseOptions();

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&b.addTest("src/test.zig").step);

    const lib_step = b.step("library", "Build static library");
    const lib = b.addStaticLibrary("ziegfried", "src/ziegfried/index.zig");
    lib_step.dependOn(&lib.step);

    b.default_step.dependOn(test_step);
}
