const std = @import("std");
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();
const printf = std.debug.print;

const TokenType = enum { CurlyLeft, CurlyRight, SquareLeft, SquareRight, Comma, Colon, Literal, Keyword, Null };

const TokenTypeLiteral = union(enum) { Number: f64, String: []const u8, Bool: bool, Null: void, None: void };

const Token = struct {
    kind: TokenType,
    literal: TokenTypeLiteral,
    lexeme: []const u8,
    line: usize,
    column: usize,

    pub fn print(self: Token) void {
        printf("Type: {s}, Lexeme: {s}, Line {d}, Col {d}\n", .{ @tagName(self.kind), self.lexeme, self.line, self.column });
    }

    pub fn empty() Token {
        return Token{
            .kind = TokenType.Null,
            .literal = TokenTypeLiteral{ .Null = undefined },
            .lexeme = "",
            .line = 0,
            .column = 0,
        };
    }
};

const LexerError = error{ InvalidCharacter, InvalidNumber, UnterminatedString, InvalidKeyword };

const Lexer = struct {
    text: []const u8,
    current_char: usize = 0,
    start_char: usize = 0,
    current_line: usize = 1,
    current_column: usize = 1,

    fn token(self: Lexer, kind: TokenType, literal: TokenTypeLiteral) Token {
        return Token{ .kind = kind, .literal = literal, .lexeme = self.consume(), .line = self.current_line, .column = self.current_column - (self.current_char - self.start_char) };
    }

    fn isAtEnd(self: Lexer) bool {
        return self.current_char >= self.text.len;
    }

    fn current(self: Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.text[self.current_char];
    }

    fn advance(self: *Lexer) u8 {
        if (self.isAtEnd()) return 0;

        const char = self.current();
        self.current_char += 1;
        self.current_column += 1;
        return char;
    }

    fn consume(self: Lexer) []const u8 {
        return self.text[self.start_char..self.current_char];
    }

    fn match(self: *Lexer, target: u8) bool {
        if (self.isAtEnd()) return false;

        const res = self.current() == target;
        if (res) _ = self.advance();
        return res;
    }

    fn peek(self: Lexer) u8 {
        if (self.current_char + 1 < self.text.len) {
            return self.text[self.current_char + 1];
        } else return 0;
    }

    pub fn scan(input: []const u8) ![]Token {
        var tokens = std.ArrayList(Token).init(allocator);
        var lexer = Lexer{ .text = input };

        while (!lexer.isAtEnd()) {
            const char = lexer.advance();

            if (char == '\n') {
                lexer.current_column = 0;
                lexer.current_line += 1;
                continue;
            } else if (std.ascii.isWhitespace(char)) {
                continue;
            }

            lexer.start_char = lexer.current_char - 1;

            const kind = switch (char) {
                '{' => TokenType.CurlyLeft,
                '}' => TokenType.CurlyRight,
                '[' => TokenType.SquareLeft,
                ']' => TokenType.SquareRight,
                ',' => TokenType.Comma,
                ':' => TokenType.Colon,
                '"', '-', '.' => TokenType.Literal,

                else => blk: {
                    if (std.ascii.isAlphanumeric(char)) {
                        break :blk TokenType.Literal;
                    }
                    return LexerError.InvalidCharacter;
                },
            };

            const literal = switch (kind) {
                .Literal => blk: {
                    if (char == '"') {
                        break :blk try lexer.scanString();
                    } else if (std.ascii.isDigit(char) or char == '-' or char == '.') {
                        break :blk try lexer.scanNumber();
                    } else {
                        break :blk try lexer.scanKeyword();
                    }
                },
                else => .Null,
            };

            const t = lexer.token(kind, literal);
            try tokens.append(t);
        }

        return tokens.items;
    }

    fn scanString(self: *Lexer) LexerError!TokenTypeLiteral {
        while (!self.isAtEnd()) : (_ = self.advance()) {
            if (self.match('\\') and self.match('"')) {
                self.current_char += 2;
                self.current_column += 2;
            } else if (self.match('"')) {
                const str = self.consume();
                // const trimmed = str[1 .. str.len - 1];
                return TokenTypeLiteral{ .String = str };
            }
        }

        return LexerError.UnterminatedString;
    }

    fn scanNumber(self: *Lexer) !TokenTypeLiteral {
        var decimal = false;
        while (!self.isAtEnd()) {
            const char = self.current();

            if (char == '.') {
                if (decimal) {
                    return LexerError.InvalidNumber;
                } else decimal = true;
            } else if (!std.ascii.isDigit(char)) {
                break;
            }

            _ = self.advance();
        }

        const str = self.consume();
        const num = try std.fmt.parseFloat(f64, str);
        return .{ .Number = num };
    }

    fn scanKeyword(self: *Lexer) !TokenTypeLiteral {
        while (!self.isAtEnd()) : (_ = self.advance()) {
            if (!std.ascii.isAlphanumeric(self.current())) {
                break;
            }
        }

        const str = self.consume();
        if (std.mem.eql(u8, str, "true")) {
            return TokenTypeLiteral{ .Bool = true };
        } else if (std.mem.eql(u8, str, "false")) {
            return TokenTypeLiteral{ .Bool = false };
        } else if (std.mem.eql(u8, str, "null")) {
            return TokenTypeLiteral{ .Null = undefined };
        }

        return TokenTypeLiteral{ .None = undefined };
    }
};

const JsonValueTag = enum { Number, String, Key, Bool, Null, List, Object };

const JsonObjectEntry = struct {
    key: []const u8,
    value: JsonValue,
};

const JsonValue = union(JsonValueTag) {
    Number: f64,
    String: []const u8,
    Key: []const u8,
    Bool: bool,
    Null: void,
    List: []JsonValue,
    Object: []JsonObjectEntry,

    pub fn print(self: JsonValue) void {
        switch (self) {
            .Number => printf("{d}", .{self.Number}),
            .String, .Key => |str| printf("{s}", .{str}),
            .Bool => printf("{}", .{self.Bool}),
            .Null => printf("null", .{}),
            .List => {
                printf("[", .{});
                for (self.List) |value| {
                    printf("", .{});
                    value.print();
                    printf(",", .{});
                }
                printf("]", .{});
            },
            .Object => {
                printf("{{", .{});
                for (self.Object) |entry| {
                    printf("{s}: ", .{entry.key});
                    entry.value.print();
                    printf(",", .{});
                }
                printf("}}", .{});
            },
        }
    }
};

const ParserError = error{ InvalidToken, InvalidKeyword, UnterminatedList, MissingListEntrySeparator, UnterminatedObject, MissingKeyValueSeparator, InvalidObjectKey, MissingObjectEntrySeparator, MemoryError };

const Parser = struct {
    tokens: []Token,
    start_token: usize = 0,
    current_token: usize = 0,

    fn isAtEnd(self: Parser) bool {
        return self.current_token >= self.tokens.len;
    }

    fn current(self: Parser) Token {
        if (self.isAtEnd()) return Token.empty();
        return self.tokens[self.current_token];
    }

    fn advance(self: *Parser) Token {
        if (self.isAtEnd()) return Token.empty();

        const token = self.current();
        self.current_token += 1;
        return token;
    }

    fn match(self: *Parser, target: TokenType) bool {
        if (self.isAtEnd()) return false;

        const res = self.current().kind == target;
        if (res) _ = self.advance();
        return res;
    }

    pub fn parse(tokens: []Token) !JsonValue {
        var parser = Parser{ .tokens = tokens };
        return parser.parseValue();
    }

    fn parseValue(self: *Parser) ParserError!JsonValue {
        const token = self.advance();

        const value = switch (token.kind) {
            TokenType.CurlyLeft => try self.parseObject(),
            TokenType.SquareLeft => try self.parseList(),
            TokenType.Literal => blk: {
                break :blk switch (token.literal) {
                    .None => JsonValue{ .Key = token.lexeme },
                    .Null => JsonValue{ .Null = undefined },
                    .Bool => |val| JsonValue{ .Bool = val },
                    .Number => |val| JsonValue{ .Number = val },
                    .String => |val| JsonValue{ .String = val },
                };
            },

            else => {
                return ParserError.InvalidToken;
            },
        };

        return value;
    }

    fn parseList(self: *Parser) !JsonValue {
        var list = std.ArrayList(JsonValue).init(allocator);

        // empty list
        if (self.match(TokenType.SquareRight)) {
            return JsonValue{ .List = list.items };
        }

        while (!self.isAtEnd()) {
            const value = try self.parseValue();
            switch (value) {
                .Key => return ParserError.InvalidKeyword,
                else => {},
            }

            list.append(value) catch {
                return ParserError.MemoryError;
            };

            const comma_found = self.match(TokenType.Comma);

            if (self.match(TokenType.SquareRight)) {
                return JsonValue{ .List = list.items };
            } else if (!comma_found) {
                return ParserError.MissingListEntrySeparator;
            }
        }

        return ParserError.UnterminatedList;
    }

    fn parseObject(self: *Parser) !JsonValue {
        var list = std.ArrayList(JsonObjectEntry).init(allocator);

        // empty object
        if (self.match(TokenType.CurlyRight)) {
            return JsonValue{ .Object = list.items };
        }

        while (!self.isAtEnd()) {
            const maybeKey = try self.parseValue();
            const key = switch (maybeKey) {
                .Number => |num| std.fmt.allocPrint(allocator, "{d}", .{num}) catch {
                    return ParserError.InvalidObjectKey;
                },
                .String => |str| str[1 .. str.len - 1],
                .Key => |str| str,
                .Bool => |b| std.fmt.allocPrint(allocator, "{}", .{b}) catch {
                    return ParserError.InvalidObjectKey;
                },
                else => return ParserError.InvalidObjectKey,
            };

            if (!self.match(TokenType.Colon)) {
                return ParserError.MissingKeyValueSeparator;
            }

            const value = try self.parseValue();
            switch (value) {
                .Key => return ParserError.InvalidKeyword,
                else => {},
            }

            const entry = JsonObjectEntry{ .key = key, .value = value };
            list.append(entry) catch {
                return ParserError.MemoryError;
            };

            const comma_found = self.match(TokenType.Comma);

            if (self.match(TokenType.CurlyRight)) {
                return JsonValue{ .Object = list.items };
            } else if (!comma_found) {
                return ParserError.MissingObjectEntrySeparator;
            }
        }

        return ParserError.UnterminatedObject;
    }
};

fn parse_json(input: []const u8) !JsonValue {
    const tokens = try Lexer.scan(input);
    return try Parser.parse(tokens);
}

pub fn main() !void {
    var args = try std.process.argsWithAllocator(allocator);
    _ = args.skip();
    const filepath = args.next() orelse {
        printf("Usage: filepath", .{});
        std.process.exit(1);
    };

    const file = try std.fs.openFileAbsolute(filepath, .{});
    const buffer = try file.readToEndAlloc(allocator, 256 * 1024);

    const value = try parse_json(buffer);
    value.print();
}
