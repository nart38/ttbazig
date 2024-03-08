const std = @import("std");
const panic = std.debug.panic;
const lexer = @import("lexer.zig");
// const parser = @import("parser.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);

    if (argv.len != 2) {
        panic("\nERROR: Incorrect usage.\nUsage: teeny myprogram.tt", .{});
    }
    const source_file_path: []const u8 = argv[1][0..];
    var file = try std.fs.cwd().openFile(source_file_path, .{});
    defer file.close();

    const reader = file.reader();
    var source = std.ArrayList(u8).init(allocator);
    defer source.deinit();

    reader.streamUntilDelimiter(source.writer(), 0, null) catch |err| switch (err) {
        error.EndOfStream => {},
        else => return err,
    };
}
