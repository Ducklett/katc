

typedef struct lexer {
	char *text;
	u16 text_length;
	u32 index;
} lexer;

static inline bool isWhitespace(char c) { return c == ' ' || c == '\t'; }

static inline bool isNewline(char c) { return c == '\n' || c == '\r'; }

static inline bool isNumber(char c) { return c >= '0' && c <= '9'; }

static inline bool isLetter(char c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); }

static inline bool isIdentifierStart(char c) { return isLetter(c) || c == '_'; }
static inline bool isIdentifier(char c) { return isLetter(c) || isNumber(c) || c == '_'; }

static inline int parse_numeric_char(char c) { return c - 48; }
static inline int parse_hexadecimal_char(char c) {
	if (c >= '0' && c <= '9') return c - 48;
	if (c >= 'a' && c <= 'f') return c - 87;
	if (c >= 'A' && c <= 'F') return c - 55;
	return 0;
}
static inline int parse_binary_char(char c) { return c - 48; }

bool span_compare(char* text, textspan span, const char* comp) {

	for (int i = 0; i < span.length; i++)
		if (text[span.start + i] != comp[i]) return false;

	if (comp[span.length] != '\0') return false;

	return true;
}

char lexer_current(lexer *l) {
	if (l->index >= l->text_length) return '\0';
	return l->text[l->index];
}

char lexer_peek(lexer *l, int n) {
	if (l->index + n >= l->text_length) return '\0';
	return l->text[l->index + n];
}

char lexer_move_next(lexer *l) {
	if (l->index >= l->text_length) return '\0';
	return l->text[l->index++];
}

char lexer_match(lexer *l, diagnosticContainer *d,  char expected) {
	if (l->index >= l->text_length) return '\0';
	char matched = l->text[l->index++];
	if (matched != expected) report_diagnostic(d, unexpectedCharacterDiagnostic, textspan_create(l->index - 1, 1), matched, expected, 0);
	return matched;
}

static inline node lex_basic_token(lexer *l, enum syntaxKind kind, u8 length) {
	node t = {
		.kind = kind,
		.span = textspan_create(l->index, length),
		.data = 0,
	};

	while (length > 0) {
		lexer_move_next(l);
		length--;
	}
	return t;
}

node lexer_lex_token(lexer *l, diagnosticContainer *d) {
	node t = {0};
	char current = lexer_current(l);
	int start;

	switch (current) {
	case '/': {
		if (lexer_peek(l,1) == '/') {
			// single line comment
			t.kind = singleLineComment;
			start =  l->index;
			while (!isNewline(lexer_current(l)) && lexer_current(l) != '\0') lexer_move_next(l);
			t.span = textspan_create(start, l->index - start);
		} else if (lexer_peek(l,1) == '*') {
			// multi line comment
			t.kind = multiLineComment;
			start =  l->index;

			// move past the initial /*
			lexer_move_next(l);
			lexer_move_next(l);

			int nestLevel=0;
			while (true) {
				char current = lexer_current(l);
				char lookahead = lexer_peek(l,1);
				if (current == '\0') {
					report_diagnostic(d, unterminatedCommentDiagnostic, textspan_create(start, l->index - start), 0, 0, 0);
					break;
				} else if (current == '/' && lookahead == '*') {
					nestLevel++;
					lexer_move_next(l);
					lexer_move_next(l);
				} else if (nestLevel > 0 && current == '*' && lookahead == '/') {
					nestLevel--;
					lexer_move_next(l);
					lexer_move_next(l);
				} else if (current == '*' && lookahead == '/') {
					lexer_move_next(l);
					lexer_move_next(l);
					break;
				} else {
					lexer_move_next(l);
				}
			}
			t.span = textspan_create(start, l->index - start);
		} else if (lexer_peek(l,1) == '=') return lex_basic_token(l, slashEqualsToken, 2);
		else return lex_basic_token(l, divisionOperator, 1);

	} break;

	case '\0': return lex_basic_token(l, endOfFileToken, 1);

	case '+': if (lexer_peek(l,1) == '+') return lex_basic_token(l, plusPlusOperator, 2);
			  else if (lexer_peek(l,1) == '=') return lex_basic_token(l, plusEqualsToken, 2);
			  else return lex_basic_token(l, plusOperator, 1);
	case '-': if (lexer_peek(l,1) == '-') return lex_basic_token(l, minusMinusOperator, 2);
			  else if (lexer_peek(l,1) == '=') return lex_basic_token(l, minusEqualsToken, 2);
			  return lex_basic_token(l, minusOperator, 1);

	case '*': if (lexer_peek(l,1) == '=') return lex_basic_token(l, starEqualsToken, 2);
			  else return lex_basic_token(l, multipliationOperator, 1);
	case '%': if (lexer_peek(l,1) == '=') return lex_basic_token(l, percentEqualsToken, 2);
			  else return lex_basic_token(l, modulusOperator, 1);

	case '!': if (lexer_peek(l,1) == '=') return lex_basic_token(l, bangEqualsOperator, 2);
			  else return lex_basic_token(l, bangOperator, 1);
	case '=': if (lexer_peek(l,1) == '=') return lex_basic_token(l, euqualsEqualsOperator, 2);
			  else return lex_basic_token(l, equalsToken, 1);
	case '<': if (lexer_peek(l,1) == '=') return lex_basic_token(l, lessEqualsOperator, 2);
			  else if (lexer_peek(l,1) == '<' && lexer_peek(l,2) == '=') return lex_basic_token(l, lessLessEqualsToken, 3);
			  else if (lexer_peek(l,1) == '<') return lex_basic_token(l, lessLessOperator, 2);
			  else return lex_basic_token(l, lessOperator, 1);
	case '>': if (lexer_peek(l,1) == '=') return lex_basic_token(l, greaterEqualsOperator, 2);
			  else if (lexer_peek(l,1) == '>' && lexer_peek(l,2) == '=') return lex_basic_token(l, greaterGreaterEqualsToken, 3);
			  else if (lexer_peek(l,1) == '>') return lex_basic_token(l, greaterGreaterOperator, 2);
			  else return lex_basic_token(l, greaterOperator, 1);

	case '&': if (lexer_peek(l,1) == '&') return lex_basic_token(l, ampersandAmpersandOperator, 2);
			  else if (lexer_peek(l,1) == '=') return lex_basic_token(l, ampersandEqualsToken, 2);
			  else return lex_basic_token(l, ampersandOperator, 1);
	case '|': if (lexer_peek(l,1) == '|') return lex_basic_token(l, pipePipeOperator, 2);
			  else if (lexer_peek(l,1) == '=') return lex_basic_token(l, pipeEqualsToken, 2);
			  else return lex_basic_token(l, pipeOperator, 1);

	case '~': return lex_basic_token(l, tildeOperator, 1);
	case '^': if (lexer_peek(l,1) == '=') return lex_basic_token(l, caretEqualsToken, 2);
			  else return lex_basic_token(l, caretOperator, 1);

	case '?': return lex_basic_token(l, questionmarkToken, 1);
	case ':': return lex_basic_token(l, colonToken, 1);
	case ';': return lex_basic_token(l, semicolonToken, 1);
	case ',': return lex_basic_token(l, commaToken, 1);
	case '.': if (lexer_peek(l,1) == '.') return lex_basic_token(l, dotDotToken, 2);
			  else return lex_basic_token(l, dotToken, 1);

	case '(': return lex_basic_token(l, openParenthesisToken, 1);
	case ')': return lex_basic_token(l, closeParenthesisToken, 1);
	case '{': return lex_basic_token(l, openCurlyToken, 1);
	case '}': return lex_basic_token(l, closeCurlyToken, 1);

	case '\'':
	case '"': {
		bool isChar = current == '\'';
		char startEndChar = isChar ? '\'' : '"';
		t.kind = isChar ? charLiteral : stringLiteral;
		start =  l->index;
		char *sb = NULL;

		lexer_match(l, d, startEndChar);

		while (true) {
			char current = lexer_current(l);
			char lookahead = lexer_peek(l, 1);

			switch(current) {
				case '\\': {
					if (lookahead == 'r') sb_push(sb, '\r');
					else if (lookahead == 'n') sb_push(sb, '\n');
					else if (lookahead == '"') sb_push(sb, '"');
					else if (lookahead == '\'') sb_push(sb, '\'');
					else if (lookahead == '0') sb_push(sb, '\0');
					else if (lookahead == '\\') sb_push(sb, '\\');
					else sb_push(sb, lookahead);

					lexer_move_next(l);
					lexer_move_next(l);
					break;
				}
				case '\0': case '\r': case '\n': goto end;
				default:
					if (current == startEndChar) goto end;
					sb_push(sb, current);
					lexer_move_next(l);
					break;
			}
		}
		end:

		lexer_match(l, d, startEndChar);
		t.span = textspan_create(start, l->index - start);

		int len = sb_count(sb);

		if (isChar) {
			if (len == 0) report_diagnostic(d, charEmptyDiagnostic, t.span, 0, 0, 0);
			else if (len > 1) report_diagnostic(d, charTooLongDiagnostic, t.span, 0, 0, 0);
			t.charValue = len==0 ? 0 : sb[0];
		} else {
			if (len > 0) {
				t.stringValue = allocate_string(sb, len, string_arena);
			}
		}
		sb_free(sb);
	} break;

	case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
		int value = 0;
		t.kind = numberLiteral;
		start =  l->index;

		#define BASE10 0
		#define BASE16 1
		#define BASE2 2
		u8 radix = BASE10;
		if (lexer_peek(l,1) == 'x') radix = BASE16;
		if (lexer_peek(l,1) == 'b') radix = BASE2;

		if (radix != BASE10) {
			// skip the 0x and 0b prefix
			lexer_move_next(l);
			lexer_move_next(l);
		} else if (lexer_current(l) == '0' && isNumber(lexer_peek(l,1))) {
			int leadingZeroCount=1;
			while(lexer_peek(l,leadingZeroCount) == '0') leadingZeroCount++;

			report_diagnostic(d, leadingZerosOnBase10NumberDiagnostic, textspan_create(l->index,leadingZeroCount), 0, 0, 0);
			t.kind=badToken;
		}

		bool foundIllegalCharacter=false;
		while (isNumber(lexer_current(l)) || lexer_current(l) == '_' || (radix == BASE16 && isLetter(lexer_current(l)) )) {
			char nextNum = lexer_move_next(l);
			if (nextNum == '_') continue;
			switch(radix) {
				case BASE10: value = value * 10 + parse_numeric_char(nextNum); break;
				case BASE16: {
					u8 v = parse_hexadecimal_char(nextNum);
					if (!foundIllegalCharacter && nextNum != '0' && v == 0) {
						foundIllegalCharacter=true;
						report_diagnostic(d, invalidHexadecimalNumberDiagnostic, textspan_create(l->index-1,1), 0, 0, 0);
						t.kind=badToken;
					}
					value = (value << 4) + v; 
				} break;
				case BASE2: {
					if (!foundIllegalCharacter && nextNum != '0' && nextNum != '1') {
						foundIllegalCharacter=true;
						report_diagnostic(d, invalidBinaryNumberDiagnostic, textspan_create(l->index-1,1), 0, 0, 0);
						t.kind=badToken;
					}
					value = (value << 1) + parse_binary_char(nextNum); 
				} break;
			}
		}
		t.span = textspan_create(start, l->index - start);
		t.numValue = value;
		break;
	}

	case ' ': case '\t':
		t.kind = whitespaceToken;
		start =  l->index;
		while (isWhitespace(lexer_current(l))) lexer_move_next(l);
		t.span = textspan_create(start, l->index - start);
		break;

	case '\n': case '\r':
		t.kind = newlineToken;
		start =  l->index;
		while (isNewline(lexer_current(l))) lexer_move_next(l);
		t.span = textspan_create(start, l->index - start);
		break;

	default:
		if (isIdentifierStart(current)) {
			t.kind = identifierToken;
			start =  l->index;
			while (isIdentifier(lexer_current(l))) lexer_move_next(l);
			t.span = textspan_create(start, l->index - start);

			if (span_compare(l->text, t.span, "true")) {
				t.kind = trueKeyword;
				t.boolValue = true;
			}
			else if (span_compare(l->text, t.span, "false")) {
				t.kind = falseKeyword;
				t.boolValue = false;
			}
			else if (span_compare(l->text, t.span, "if")) t.kind = ifKeyword;
			else if (span_compare(l->text, t.span, "else")) t.kind = elseKeyword;
			else if (span_compare(l->text, t.span, "switch")) t.kind = switchKeyword;
			else if (span_compare(l->text, t.span, "case")) t.kind = caseKeyword;
			else if (span_compare(l->text, t.span, "default")) t.kind = defaultKeyword;
			else if (span_compare(l->text, t.span, "while")) t.kind = whileKeyword;
			else if (span_compare(l->text, t.span, "for")) t.kind = forKeyword;
			else if (span_compare(l->text, t.span, "in")) t.kind = inKeyword;
			else if (span_compare(l->text, t.span, "break")) t.kind = breakKeyword;
			else if (span_compare(l->text, t.span, "continue")) t.kind = continueKeyword;
			else if (span_compare(l->text, t.span, "fn")) t.kind = fnKeyword;
			else if (span_compare(l->text, t.span, "namespace")) t.kind = namespaceKeyword;
			else if (span_compare(l->text, t.span, "enum")) t.kind = enumKeyword;
			break;
		}
		t.kind = badToken;
		t.span = textspan_create(l->index, 1);
		lexer_move_next(l);
		report_diagnostic(d, badTokenDiagnostic, t.span, 0, 0, 0);
		break;
	}

	return t;
}
