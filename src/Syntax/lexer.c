
typedef struct lexer {
	char *text;
	u16 text_length;
	u32 index;
} lexer;

inline bool isWhitespace(char c) { return c == ' ' || c == '\t'; }

inline bool isNewline(char c) { return c == '\n' || c == '\r'; }

inline bool isNumber(char c) { return c >= '0' && c <= '9'; }

inline bool isLetter(char c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); }

inline bool isIdentifierStart(char c) { return isLetter(c) || c == '_'; }
inline bool isIdentifier(char c) { return isLetter(c) || isNumber(c) || c == '_'; }

bool span_compare(char* text, node *token, char* comp) {

	for (int i = 0; i < token->span.length; i++)
		if (text[token->span.start + i] != comp[i]) return false;

	if (comp[token->span.length] != '\0') return false;

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

inline node lex_basic_token(lexer *l, enum syntaxKind kind, u8 length) {
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

	case '\0': return lex_basic_token(l, endOfFileToken, 1);

	case '+': return lex_basic_token(l, plusOperator, 1);
	case '-': return lex_basic_token(l, minusOperator, 1);
	case '*': return lex_basic_token(l, multipliationOperator, 1);
	case '/': return lex_basic_token(l, divisionOperator, 1);
	case '%': return lex_basic_token(l, modulusOperator, 1);

	case '!':
		if (lexer_peek(l,1) == '=') return lex_basic_token(l, bangEqualsOperator, 2);
		else return lex_basic_token(l, bangOperator, 1);
	case '=':
		if (lexer_peek(l,1) == '=') return lex_basic_token(l, euqualsEqualsOperator, 2);
		else return lex_basic_token(l, equalsToken, 1);
	case '<':
		if (lexer_peek(l,1) == '=') return lex_basic_token(l, lessEqualsOperator, 2);
		else return lex_basic_token(l, lessOperator, 1);
	case '>':
		if (lexer_peek(l,1) == '=') return lex_basic_token(l, greaterEqualsOperator, 2);
		else return lex_basic_token(l, greaterOperator, 1);
	case '&':
		if (lexer_peek(l,1) == '&') return lex_basic_token(l, ampersandAmpersandOperator, 2);
	case '|':
		if (lexer_peek(l,1) == '|') return lex_basic_token(l, pipePipeOperator, 2);

	case ':': return lex_basic_token(l, colonToken, 1);

	case '(': return lex_basic_token(l, openParenthesisToken, 1);
	case ')': return lex_basic_token(l, closeParenthesisToken, 1);
	case '{': return lex_basic_token(l, openCurlyToken, 1);
	case '}': return lex_basic_token(l, closeCurlyToken, 1);

	case '"':
		t.kind = stringLiteral;
		start =  l->index;

		// TODO: store the value
		lexer_match(l, d, '"'); // open "

		while (true) {
			char current = lexer_current(l);
			char lookahead = lexer_peek(l, 1);
			switch(current) {
				case '\\':
					lexer_move_next(l);
					lexer_move_next(l);
					break;
				case '\0': case '\r': case '\n': case '"': goto end;
				default: lexer_move_next(l); break;
			}
		}
		end:

		lexer_match(l, d, '"'); // close "
		t.span = textspan_create(start, l->index - start);
		break;

	case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
		t.kind = numberLiteral;
		start =  l->index;
		while (isNumber(lexer_current(l))) lexer_move_next(l);
		t.span = textspan_create(start, l->index - start);
		break;

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

			if (span_compare(l->text, &t, "true")) t.kind = trueKeyword;
			else if (span_compare(l->text, &t, "false")) t.kind = falseKeyword;
			else if (span_compare(l->text, &t, "if")) t.kind = ifKeyword;
			else if (span_compare(l->text, &t, "else")) t.kind = elseKeyword;
			else if (span_compare(l->text, &t, "while")) t.kind = whileKeyword;
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
