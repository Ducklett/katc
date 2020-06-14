
typedef struct lexer
{
	char *text;
	int text_length;
	int index;
} lexer;

inline bool isWhitespace(char c)
{
	return c == ' ' || c == '\t';
}

inline bool isNewline(char c)
{
	return c == '\n' || c == '\r';
}

inline bool isNumber(char c)
{
	return c >= '0' && c <= '9';
}

inline bool isLetter(char c)
{
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

char lexer_current(lexer *l)
{
	if (l->index >= l->text_length)
		return '\0';
	return l->text[l->index];
}

char lexer_move_next(lexer *l)
{
	if (l->index >= l->text_length)
		return '\0';
	return l->text[l->index++];
}

inline node lex_basic_token(lexer *l, enum syntaxKind kind, int length)
{
	node t = {
		.kind = kind,
		.text_start = l->index,
		.text_length = length,
	};

	while (length > 0)
	{
		lexer_move_next(l);
		length--;
	}
	return t;
}

node lexer_lex_token(lexer *l, diagnosticContainer *d)
{
	node t = {0};
	char current = lexer_current(l);

	switch (current)
	{

	case '\0':
		return lex_basic_token(l, endOfFileToken, 1);

	case '+':
		return lex_basic_token(l, plusOperator, 1);
	case '-':
		return lex_basic_token(l, minusOperator, 1);
	case '*':
		return lex_basic_token(l, multipliationOperator, 1);
	case '/':
		return lex_basic_token(l, divisionOperator, 1);
	case '%':
		return lex_basic_token(l, modulusOperator, 1);

	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		t.kind = numberLiteral;
		t.text_start = l->index;
		while (isNumber(lexer_current(l)))
			lexer_move_next(l);
		t.text_length = l->index - t.text_start;
		break;

	case ' ':
	case '\t':
		t.kind = whitespaceToken;
		t.text_start = l->index;
		while (isWhitespace(lexer_current(l)))
			lexer_move_next(l);
		t.text_length = l->index - t.text_start;
		break;

	case '\n':
	case '\r':
		t.kind = newlineToken;
		t.text_start = l->index;
		while (isNewline(lexer_current(l)))
			lexer_move_next(l);
		t.text_length = l->index - t.text_start;
		break;

	default:
		t.kind = badToken;
		t.text_start = l->index;
		t.text_length = 1;
		lexer_move_next(l);
		report_diagnostic(d, badTokenDiagnostic, t.text_start, t.text_length, 0, 0, 0);
		break;
	}
	return t;
}
