typedef struct parser {
	lexer lexer;
	node root;
	binaryExpressionNode binaryExpressions[1024];
	parenthesizedExpressionNode parenthesizedExpressions[1024];
	u16 nodeIndex;
	u16 binaryExpressionIndex;
	u16 parenthesizedExpressionIndex;
} parser;

node parser_parse_expression(parser *p, diagnosticContainer *d);
node parser_parse_binary_expression(parser *p, diagnosticContainer *d, i8 parentPrecedence);
node parser_parse_primary_expression(parser *p, diagnosticContainer *d);

node parser_next_token(parser *p, diagnosticContainer *d) {
	while(true) {
		node t = lexer_lex_token(&p->lexer, d);
		if (t.kind == whitespaceToken || t.kind == newlineToken) continue;
		return t;
	}
}

node parser_match_token(parser *p, diagnosticContainer *d, enum syntaxKind expectedKind) {
	while(true) {
		node t = lexer_lex_token(&p->lexer, d);
		if (t.kind == whitespaceToken || t.kind == newlineToken) continue;
		if (t.kind != expectedKind) {
			// badToken already gets reported by the lexer
			if (t.kind != badToken) report_diagnostic(d, unexpectedTokenDiagnostic, t.text_start, t.text_length, t.kind, expectedKind, 0);
			t.kind=errorToken;
		}
		return t;
	}
}

void parser_parse(parser *p, diagnosticContainer *d) {
	p->root = parser_parse_expression(p, d);
	parser_match_token(p, d, endOfFileToken);
}

node parser_parse_expression(parser *p, diagnosticContainer *d) { return parser_parse_binary_expression(p, d,-2); }

node parser_parse_binary_expression(parser *p, diagnosticContainer *d, i8 parentPrecedence) {
	node left = parser_parse_primary_expression(p, d);

	if (left.kind == endOfFileToken || left.kind == errorToken) return left;

	while (true) {
		node operator = parser_next_token(p, d);

		// not a binary expression, return self
		if (operator.kind == endOfFileToken || operator.kind == badToken) return left;

		if (operator.kind == closeParenthesisToken) {
			p->lexer.index--;
			return left;
		}

		i8 precedence = getOperatorPrecedence(operator.kind);

		if (precedence == -1 || precedence <= parentPrecedence) {
			// reached the end, go back to before the operator was lexed
			// TODO: don't re-lex
			p->lexer.index-= operator.text_length;
			return left;
		}

		node right = parser_parse_binary_expression(p, d, precedence);

		binaryExpressionNode exprData = { left, operator, right, };

		u16 index = p->binaryExpressionIndex;
		p->binaryExpressions[p->binaryExpressionIndex++] = exprData;

		node exprNode = {
			.kind = binaryExpression,
			.text_start = left.text_start,
			.text_length = (right.text_start - left.text_start) + right.text_length,
			.data = &(p->binaryExpressions[index]), 
		};

		left = exprNode;
	}

	return left;
}

node parser_parse_primary_expression(parser *p, diagnosticContainer *d) {
	node token = parser_next_token(p, d);

	if (token.kind == numberLiteral) return token;

	if (token.kind == openParenthesisToken) {
		node expr = parser_parse_expression(p, d);
		node closeParen = parser_match_token(p, d, closeParenthesisToken);

		parenthesizedExpressionNode exprData = { token, expr, closeParen };

		u16 index = p->parenthesizedExpressionIndex;
		p->parenthesizedExpressions[p->parenthesizedExpressionIndex++] = exprData;

		node exprNode = {
			.kind = parenthesizedExpression,
			.text_start = token.text_start,
			.text_length = (closeParen.text_start - token.text_start) + closeParen.text_length,
			.data = &(p->parenthesizedExpressions[index]), 
		};

		return exprNode;
	}
}
