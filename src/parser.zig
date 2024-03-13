const std = @import("std");
const lexer = @import("lexer.zig");
const emitter = @import("emit.zig");
const BufSet = std.BufSet;
const Allocator = std.mem.Allocator;
const panic = std.debug.panic;

pub const Parser = struct {
    const Self = *@This();
    lex: lexer.Lexer,
    emitter: emitter.Emitter,
    cur_token: lexer.Token,
    peek_token: lexer.Token,

    symbols: BufSet, // Varaibles declared.
    labels_declared: BufSet, // Keep track of all the labels declared.
    labels_gotoed: BufSet, // Keep track of only the labels gotoed. So we can check if they exist or not.

    pub fn init(a: Allocator, l: lexer.Lexer, e: emitter.Emitter) Parser {
        var p = Parser{
            .lex = l,
            .emitter = e,
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
        self.emitter.deinit();
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
        self.emitter.headerLine("#include<stdio.h>");
        self.emitter.headerLine("int main(void){");

        // Skip unnecessary newlines.
        while (self.checkToken(lexer.TokenKind.newline)) {
            self.nextToken();
        }

        // Parse all statemnets.
        while (!self.checkToken(lexer.TokenKind.eof)) {
            try self.statement();
        }

        // Wrap things up.
        self.emitter.emitLine("return 0;");
        self.emitter.emitLine("}");

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
                self.nextToken();
                switch (self.cur_token.kind) {
                    .string => {
                        self.emitter.emit("printf(\"");
                        self.emitter.emit(self.cur_token.text);
                        self.emitter.emitLine("\\n\");");
                        self.nextToken();
                    },
                    else => {
                        self.emitter.emit("printf(\"%.2f\\n\", (float)(");
                        self.expression();
                        self.emitter.emitLine("));");
                    },
                }
            },

            // "IF" comparison "THEN" {statement} "ENDIF"
            .IF => {
                self.nextToken();
                self.emitter.emit("if(");
                self.comparison();

                self.match(lexer.TokenKind.THEN);
                self.nl();
                self.emitter.emitLine("){");

                while (!self.checkToken(lexer.TokenKind.ENDIF)) {
                    try self.statement();
                }
                self.match(lexer.TokenKind.ENDIF);
                self.emitter.emitLine("}");
            },

            // "WHILE" comparison "REPEAT" {statement} "ENDWHILE"
            .WHILE => {
                self.nextToken();
                self.emitter.emit("while(");
                self.comparison();

                self.match(lexer.TokenKind.REPEAT);
                self.nl();
                self.emitter.emitLine("){");

                while (!self.checkToken(lexer.TokenKind.ENDWHILE)) {
                    try self.statement();
                }
                self.match(lexer.TokenKind.ENDWHILE);
                self.emitter.emitLine("}");
            },

            // "LABEL" ident
            .LABEL => {
                self.nextToken();

                if (self.labels_declared.contains(self.cur_token.text))
                    panic("Label already exists: {s}\n", .{self.cur_token.text});

                try self.labels_declared.insert(self.cur_token.text);
                self.emitter.emit(self.cur_token.text);
                self.emitter.emitLine(":");
                self.match(lexer.TokenKind.ident);
            },

            // "GOTO" ident
            .GOTO => {
                self.nextToken();
                try self.labels_gotoed.insert(self.cur_token.text);
                self.emitter.emit("goto");
                self.emitter.emit(self.cur_token.text);
                self.emitter.emitLine(";");
                self.match(lexer.TokenKind.ident);
            },

            // "LET" ident "=" expression
            .LET => {
                self.nextToken();

                if (!self.symbols.contains(self.cur_token.text)) {
                    try self.symbols.insert(self.cur_token.text);
                    self.emitter.head("float ");
                    self.emitter.head(self.cur_token.text);
                    self.emitter.headerLine(";");
                }

                self.emitter.emit(self.cur_token.text);
                self.emitter.emit(" = ");
                self.match(lexer.TokenKind.ident);
                self.match(lexer.TokenKind.eq);

                self.expression();
                self.emitter.emitLine(";");
            },

            // "INPUT" ident
            .INPUT => {
                self.nextToken();

                if (!self.symbols.contains(self.cur_token.text))
                    try self.symbols.insert(self.cur_token.text);
                self.emitter.head("float ");
                self.emitter.head(self.cur_token.text);
                self.emitter.headerLine(";");

                self.emitter.emit("if(0 == scanf(\"%f\", &");
                self.emitter.emit(self.cur_token.text);
                self.emitter.emitLine(")) {");
                self.emitter.emit(self.cur_token.text);
                self.emitter.emitLine(" = 0;");
                self.emitter.emit("scanf(\"%");
                self.emitter.emitLine("*s\");");
                self.emitter.emitLine("}");
                self.match(lexer.TokenKind.ident);
            },

            // Not a valid statement.
            else => panic("Invalid statement at: {s} ({s})\n", .{ self.cur_token.text, @tagName(self.cur_token.kind) }),
        }
        self.nl();
    }

    // comparison ::= expression (("==" | "!=" | ">" | ">=" | "<" | "<=") expression)+
    fn comparison(self: Self) void {
        self.expression();
        if (self.isComparisonOperator()) {
            self.emitter.emit(self.cur_token.text);
            self.nextToken();
            self.expression();
        } else {
            panic("Expected comparison operator at: {s}\n", .{self.cur_token.text});
        }

        while (self.isComparisonOperator()) {
            self.emitter.emit(self.cur_token.text);
            self.nextToken();
            self.expression();
        }
    }

    // expression ::= term {( "-" | "+" ) term}
    fn expression(self: Self) void {
        self.term();
        while (self.checkToken(lexer.TokenKind.plus) or self.checkToken(lexer.TokenKind.minus)) {
            self.emitter.emit(self.cur_token.text);
            self.nextToken();
            self.term();
        }
    }

    // term ::= unary {( "/" | "*" ) unary}
    fn term(self: Self) void {
        self.unary();
        while (self.checkToken(lexer.TokenKind.asterisk) or self.checkToken(lexer.TokenKind.slash)) {
            self.emitter.emit(self.cur_token.text);
            self.nextToken();
            self.unary();
        }
    }

    // unary ::= ["+" | "-"] primary
    fn unary(self: Self) void {
        if (self.checkToken(lexer.TokenKind.plus) or self.checkToken(lexer.TokenKind.minus)) {
            self.emitter.emit(self.cur_token.text);
            self.nextToken();
        }

        self.primary();
    }

    // primary ::= number | ident
    fn primary(self: Self) void {
        switch (self.cur_token.kind) {
            .number => {
                self.emitter.emit(self.cur_token.text);
                self.nextToken();
            },
            .ident => {
                if (!self.symbols.contains(self.cur_token.text))
                    panic("Referencing variable before assignment: {s}\n", .{self.cur_token.text});

                self.emitter.emit(self.cur_token.text);
                self.nextToken();
            },
            else => panic("Unexpected token at {s}\n", .{self.cur_token.text}),
        }
    }

    // nl ::= '\n'+
    fn nl(self: Self) void {
        self.match(lexer.TokenKind.newline);
        while (self.checkToken(lexer.TokenKind.newline)) {
            self.nextToken();
        }
    }
};
