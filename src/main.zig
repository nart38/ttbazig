const std = @import("std");
const panic = std.debug.panic;

pub fn main() !void {
    const argv = std.os.argv;
    if (argv.len != 2) {
        panic("Usage: teeny myprogram.tt");
    }
}
