const std = @import("std");
// TODO: set std.debug.panic to public panic function only when debug build.
// builtin.strip_debug_info can be used to check if this is a debug build.
const panic = std.debug.panic;
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const emitter = @import("emit.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);

    if (argv.len != 2) {
        panic("\nERROR: Incorrect usage.\nUsage: teeny myprogram.tt\n", .{});
    }
    const source_file_path: []const u8 = argv[1][0..];
    var file = try std.fs.cwd().openFile(source_file_path, .{});
    defer file.close();

    const reader = file.reader();
    var text = std.ArrayList(u8).init(allocator);
    defer text.deinit();

    reader.streamUntilDelimiter(text.writer(), 0, null) catch |err| switch (err) {
        error.EndOfStream => {},
        else => return err,
    };

    const source: []const u8 = try text.toOwnedSlice();
    defer allocator.free(source);

    // Initialize lexer and parser.
    var lex = lexer.Lexer.init(source);
    var emit = emitter.Emitter.init(allocator, "out.c");
    defer emit.deinit();

    var pars = parser.Parser.init(allocator, lex, emit);
    defer pars.deinit();
    try pars.program();
    try pars.emitter.writeFile();
    std.debug.print("Compilation completed with no error.\n", .{});
}
