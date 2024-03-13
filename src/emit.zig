const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const panic = std.debug.panic;

pub const Emitter = struct {
    const Self = @This();

    full_path: []const u8,
    header: ArrayList(u8),
    code: ArrayList(u8),

    pub fn init(a: Allocator, full_path: []const u8) Emitter {
        return Emitter{
            .full_path = full_path,
            .header = ArrayList(u8).init(a),
            .code = ArrayList(u8).init(a),
        };
    }

    pub fn deinit(self: Self) void {
        self.header.deinit();
        self.code.deinit();
    }

    pub fn emit(self: *Self, code: []const u8) void {
        self.code.appendSlice(code) catch |err| switch (err) {
            else => panic("Allocater Error: {any}", .{err}),
        };
    }

    pub fn emitLine(self: *Self, code: []const u8) void {
        self.code.appendSlice(code) catch |err| switch (err) {
            else => panic("Allocater Error: {any}", .{err}),
        };
        self.code.append('\n') catch |err| switch (err) {
            else => panic("Allocater Error: {any}", .{err}),
        };
    }

    pub fn headerLine(self: *Self, code: []const u8) void {
        self.header.appendSlice(code) catch |err| switch (err) {
            else => panic("Allocater Error: {any}", .{err}),
        };
        self.header.append('\n') catch |err| switch (err) {
            else => panic("Allocater Error: {any}", .{err}),
        };
    }

    pub fn head(self: *Self, code: []const u8) void {
        self.header.appendSlice(code) catch |err| switch (err) {
            else => panic("Allocater Error: {any}", .{err}),
        };
    }

    pub fn writeFile(self: *Self) !void {
        const file = try std.fs.cwd().createFile(self.full_path, .{});
        defer file.close();
        const header = try self.header.toOwnedSlice();
        const code = try self.code.toOwnedSlice();
        try file.writeAll(header);
        try file.writeAll(code);
    }
};
