const std = @import("std");
const panic = std.debug.panic;
const isDigit = std.ascii.isDigit;
const isAlphanumeric = std.ascii.isAlphanumeric;
const testing = std.testing;

pub const Lexer = struct {
    const Self = @This();
    source: []const u8,
    cur_char: u8,
    cur_pos: usize,

    pub fn init(source: []const u8) Lexer {
        var lexer = Lexer{
            .source = source,
            .cur_char = source[0],
            .cur_pos = 0,
        };
        // lexer.nextChar();
        return lexer;
    }

    fn nextChar(self: *Self) void {
        self.cur_pos += 1;
        if (self.cur_pos >= self.source.len) {
            self.cur_char = 0;
        } else {
            self.cur_char = self.source[self.cur_pos];
        }
    }

    fn peek(self: Self) u8 {
        if (self.cur_pos + 1 >= self.source.len) {
            return 0;
        }
        return self.source[self.cur_pos + 1];
    }

    fn skipWhiteSpace(self: *Self) void {
        while (self.cur_char == ' ' or self.cur_char == '\t' or self.cur_char == '\r') {
            self.nextChar();
        }
    }

    fn skipComment(self: *Self) void {
        if (self.cur_char == '#') {
            while (self.cur_char >= '\n') {
                self.nextChar();
            }
        }
    }

    // Return the next token.
    pub fn getToken(self: *Self) Token {
        self.skipWhiteSpace();
        self.skipComment();

        const token = switch (self.cur_char) {
            '+' => Token.init(self.source[self.cur_pos .. self.cur_pos + 1], TokenKind.plus),
            '-' => Token.init(self.source[self.cur_pos .. self.cur_pos + 1], TokenKind.minus),
            '*' => Token.init(self.source[self.cur_pos .. self.cur_pos + 1], TokenKind.asterisk),
            '/' => Token.init(self.source[self.cur_pos .. self.cur_pos + 1], TokenKind.slash),

            '=' => blk: {
                if (self.peek() == '=') {
                    const start = self.cur_pos;
                    self.nextChar();
                    break :blk Token.init(self.source[start .. self.cur_pos + 1], TokenKind.eqeq);
                } else {
                    break :blk Token.init(self.source[self.cur_pos .. self.cur_pos + 1], TokenKind.eq);
                }
            },

            '>' => blk: {
                if (self.peek() == '=') {
                    const start = self.cur_pos;
                    self.nextChar();
                    break :blk Token.init(self.source[start .. self.cur_pos + 1], TokenKind.gteq);
                } else {
                    break :blk Token.init(self.source[self.cur_pos .. self.cur_pos + 1], TokenKind.gt);
                }
            },

            '<' => blk: {
                if (self.peek() == '=') {
                    const start = self.cur_pos;
                    self.nextChar();
                    break :blk Token.init(self.source[start .. self.cur_pos + 1], TokenKind.lteq);
                } else {
                    break :blk Token.init(self.source[self.cur_pos .. self.cur_pos + 1], TokenKind.lt);
                }
            },

            '!' => blk: {
                const start = self.cur_pos;
                if (self.peek() == '=') {
                    self.nextChar();
                    break :blk Token.init(self.source[start .. self.cur_pos + 1], TokenKind.noteq);
                } else {
                    panic("Lexer Error: expected '!=', found: '{s}'", .{self.source[start .. self.cur_pos + 1]});
                }
            },

            '"' => blk: {
                // Get characters between quotations.
                self.nextChar();
                const start = self.cur_pos;
                // FIXME: What happens if quotations never closed?
                while (self.cur_char != '"') {
                    // Special characters not allowed
                    if (self.cur_char == '\\' or self.cur_char == '\r' or self.cur_char == '\n' or self.cur_char == '\t' or self.cur_char == '%') {
                        panic("Lexer Error: Illegal character in string: '{s}'", .{self.source[start .. self.cur_pos + 1]});
                    }
                    self.nextChar();
                }
                break :blk Token.init(self.source[start..self.cur_pos], TokenKind.string);
            },

            '0'...'9' => blk: {
                const start = self.cur_pos;
                while (isDigit(self.peek())) {
                    self.nextChar();
                    if (self.peek() == '.') { // Decimal.
                        self.nextChar();
                        if (!isDigit(self.cur_char)) {
                            panic("Lexer Error: Illegal number: {s}", .{self.source[start .. self.cur_pos + 1]});
                        }
                        while (isDigit(self.peek())) {
                            self.nextChar();
                        }
                    }
                }
                break :blk Token.init(self.source[start .. self.cur_pos + 1], TokenKind.number);
            },

            'a'...'z', 'A'...'Z' => blk: {
                const start = self.cur_pos;
                while (isAlphanumeric(self.peek())) {
                    self.nextChar();
                }
                const tok_text = self.source[start .. self.cur_pos + 1];
                const tok_kind = Token.checkIfKeyword(tok_text) orelse TokenKind.ident;

                break :blk Token.init(tok_text, tok_kind);
            },

            '\n' => Token.init(self.source[self.cur_pos .. self.cur_pos + 1], TokenKind.newline),
            0 => Token.init("", TokenKind.eof),
            else => panic("Lexer Error: Unknown Token", .{}),
        };

        self.nextChar();
        return token;
    }
};

pub const Token = struct {
    const Self = @This();
    text: []const u8,
    kind: TokenKind,

    pub fn init(text: []const u8, token_kind: TokenKind) Token {
        return Token{
            .text = text,
            .kind = token_kind,
        };
    }

    pub fn checkIfKeyword(text: []const u8) ?TokenKind {
        inline for (@typeInfo(TokenKind).Enum.fields) |field| {
            if (std.mem.eql(u8, field.name, text) and field.value >= 100 and field.value < 200) {
                return @field(TokenKind, field.name);
            }
        }
        return null;
    }
};

pub const TokenKind = enum(u16) {
    eof = 0,
    newline,
    number,
    ident,
    string,
    // Keywords. Keywords are all capitalized to make things easier.
    LABEL = 100,
    GOTO,
    PRINT,
    INPUT,
    LET,
    IF,
    THEN,
    ENDIF,
    WHILE,
    REPEAT,
    ENDWHILE,
    // Operators.
    eq = 200,
    plus,
    minus,
    asterisk,
    slash,
    eqeq,
    noteq,
    lt,
    lteq,
    gt,
    gteq,
};

test "Lexer" {
    const src = "IF+-123 foo*THEN/9";
    var lex = Lexer.init(src);
    var tk = lex.getToken() catch unreachable;
    try testing.expectEqual(TokenKind.IF, tk.kind);
    tk = lex.getToken() catch unreachable;
    try testing.expectEqual(TokenKind.plus, tk.kind);
    tk = lex.getToken() catch unreachable;
    try testing.expectEqual(TokenKind.minus, tk.kind);
    tk = lex.getToken() catch unreachable;
    try testing.expectEqual(TokenKind.number, tk.kind);
    tk = lex.getToken() catch unreachable;
    try testing.expectEqual(TokenKind.ident, tk.kind);
    tk = lex.getToken() catch unreachable;
    try testing.expectEqual(TokenKind.asterisk, tk.kind);
    tk = lex.getToken() catch unreachable;
    try testing.expectEqual(TokenKind.THEN, tk.kind);
    tk = lex.getToken() catch unreachable;
    try testing.expectEqual(TokenKind.slash, tk.kind);
    tk = lex.getToken() catch unreachable;
    try testing.expectEqual(TokenKind.number, tk.kind);
    tk = lex.getToken() catch unreachable;
    try testing.expectEqual(TokenKind.eof, tk.kind);
}
