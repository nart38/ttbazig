const std = @import("std");
const lexer = @import("lexer.zig");
const BufSet = std.BufSet;
const Allocator = std.mem.Allocator;
const panic = std.debug.panic;

pub const Parser = struct {
    const Self = *@This();
    lex: lexer.Lexer,
    cur_token: lexer.Token,
    peek_token: lexer.Token,

    symbols: BufSet, // Varaibles declared.
    labels_declared: BufSet, // Keep track of all the labels declared.
    labels_gotoed: BufSet, // Keep track of only the labels gotoed. So we can check if they exist or not.

    pub fn init(a: Allocator, l: lexer.Lexer) Parser {
        var p = Parser{
            .lex = l,
            .cur_token = undefined,
            .peek_token = undefined,

            .symbols = BufSet.init(a),
            .labels_declared = BufSet.init(a),
            .labels_gotoed = BufSet.init(a),
        };
        p.nextToken();
        p.nextToken();
        return p;
    }

    // Free allocated memory for the BufSets.
    pub fn deinit(self: Self) void {
        self.symbols.deinit();
        self.labels_gotoed.deinit();
        self.labels_declared.deinit();
    }

    fn checkToken(self: Self, kind: lexer.TokenKind) bool {
        return kind == self.cur_token.kind;
    }

    fn checkPeek(self: Self, kind: lexer.TokenKind) bool {
        return kind == self.peek_token.kind;
    }

    fn match(self: Self, kind: lexer.TokenKind) void {
        if (!self.checkToken(kind))
            panic("Expected {s}, got {s}.\n", .{ @tagName(kind), @tagName(self.cur_token.kind) });

        self.nextToken();
    }

    fn nextToken(self: *Parser) void {
        self.cur_token = self.peek_token;
        self.peek_token = self.lex.getToken();
    }

    fn isComparisonOperator(self: Self) bool {
        switch (self.cur_token.kind) {
            .eqeq, .noteq, .gt, .gteq, .lt, .lteq => return true,
            else => return false,
        }
    }

    // Production rules

    // program ::= {statement}
    pub fn program(self: *Parser) !void {
        std.debug.print("PROGRAM\n", .{});

        // Skip unnecessary newlines.
        while (self.checkToken(lexer.TokenKind.newline)) {
            self.nextToken();
        }

        // Parse all statemnets.
        while (!self.checkToken(lexer.TokenKind.eof)) {
            try self.statement();
        }

        // Check if all the labels gotoed is declared.
        var i = self.labels_gotoed.iterator();
        var item = i.next();
        while (item) |label| {
            if (!self.labels_declared.contains(label.*))
                panic("Attempting to GOTO to undeclared label: {s}\n", .{label});
        }
    }

    fn statement(self: *Parser) !void {
        switch (self.cur_token.kind) {
            // "PRINT" (expression | string)
            .PRINT => {
                std.debug.print("STATEMENT-PRINT\n", .{});
                self.nextToken();
                switch (self.cur_token.kind) {
                    .string => self.nextToken(),
                    else => self.expression(),
                }
            },

            // "IF" comparison "THEN" {statement} "ENDIF"
            .IF => {
                std.debug.print("STATEMENT-IF\n", .{});
                self.nextToken();
                self.comparison();

                self.match(lexer.TokenKind.THEN);
                self.nl();
                while (!self.checkToken(lexer.TokenKind.ENDIF)) {
                    try self.statement();
                }
                self.match(lexer.TokenKind.ENDIF);
            },

            // "WHILE" comparison "REPEAT" {statement} "ENDWHILE"
            .WHILE => {
                std.debug.print("STATEMENT-WHILE\n", .{});
                self.nextToken();
                self.comparison();

                self.match(lexer.TokenKind.REPEAT);
                self.nl();
                while (!self.checkToken(lexer.TokenKind.ENDWHILE)) {
                    try self.statement();
                }
                self.match(lexer.TokenKind.ENDWHILE);
            },

            // "LABEL" ident
            .LABEL => {
                std.debug.print("STATEMENT-LABEL\n", .{});
                self.nextToken();

                if (self.labels_declared.contains(self.cur_token.text))
                    panic("Label already exists: {s}\n", .{self.cur_token.text});

                try self.labels_declared.insert(self.cur_token.text);
                self.match(lexer.TokenKind.ident);
            },

            // "GOTO" ident
            .GOTO => {
                std.debug.print("STATEMENT-GOTO\n", .{});
                self.nextToken();
                try self.labels_gotoed.insert(self.cur_token.text);
                self.match(lexer.TokenKind.ident);
            },

            // "LET" ident "=" expression
            .LET => {
                std.debug.print("STATEMENT-LET\n", .{});
                self.nextToken();

                if (!self.symbols.contains(self.cur_token.text))
                    try self.symbols.insert(self.cur_token.text);

                self.match(lexer.TokenKind.ident);
                self.match(lexer.TokenKind.eq);
                self.expression();
            },

            // "INPUT" ident
            .INPUT => {
                std.debug.print("STATEMENT-INPUT\n", .{});
                self.nextToken();

                if (!self.symbols.contains(self.cur_token.text))
                    try self.symbols.insert(self.cur_token.text);

                self.match(lexer.TokenKind.ident);
            },

            // Not a valid statement.
            else => panic("Invalid statement at: {s} ({s})\n", .{ self.cur_token.text, @tagName(self.cur_token.kind) }),
        }
        self.nl();
    }

    // comparison ::= expression (("==" | "!=" | ">" | ">=" | "<" | "<=") expression)+
    fn comparison(self: Self) void {
        std.debug.print("STATEMENT-INPUT\n", .{});

        self.expression();
        if (self.isComparisonOperator()) {
            self.nextToken();
            self.expression();
        } else {
            panic("Expected comparison operator at: {s}\n", .{self.cur_token.text});
        }

        while (self.isComparisonOperator()) {
            self.nextToken();
            self.expression();
        }
    }

    // expression ::= term {( "-" | "+" ) term}
    fn expression(self: Self) void {
        std.debug.print("EXPRESSION\n", .{});

        self.term();
        while (self.checkToken(lexer.TokenKind.plus) or self.checkToken(lexer.TokenKind.minus)) {
            self.nextToken();
            self.term();
        }
    }

    // term ::= unary {( "/" | "*" ) unary}
    fn term(self: Self) void {
        std.debug.print("TERM\n", .{});

        self.unary();
        while (self.checkToken(lexer.TokenKind.asterisk) or self.checkToken(lexer.TokenKind.slash)) {
            self.nextToken();
            self.unary();
        }
    }

    // unary ::= ["+" | "-"] primary
    fn unary(self: Self) void {
        std.debug.print("UNARY\n", .{});
        if (self.checkToken(lexer.TokenKind.plus) or self.checkToken(lexer.TokenKind.minus)) {
            self.nextToken();
        }

        self.primary();
    }

    // primary ::= number | ident
    fn primary(self: Self) void {
        std.debug.print("PRIMARY ({s})\n", .{self.cur_token.text});

        switch (self.cur_token.kind) {
            .number => self.nextToken(),
            .ident => {
                if (!self.symbols.contains(self.cur_token.text))
                    panic("Referencing variable before assignment: {s}\n", .{self.cur_token.text});

                self.nextToken();
            },
            else => panic("Unexpected token at {s}\n", .{self.cur_token.text}),
        }
    }

    // nl ::= '\n'+
    fn nl(self: Self) void {
        std.debug.print("NEWLINE\n", .{});
        self.match(lexer.TokenKind.newline);
        while (self.checkToken(lexer.TokenKind.newline)) {
            self.nextToken();
        }
    }
};
