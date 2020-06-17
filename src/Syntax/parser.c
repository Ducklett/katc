#define MAX_LOOKAHEAD 10

typedef struct parser {
	lexer lexer;
	node token_buffer[MAX_LOOKAHEAD];	// ring buffer that caches token lookaheads
	node root;
	node nodes[1024];	// stores the nodes from arbitrarily sized sequences like paramater lists and block statements
	variableDeclarationNode variableDeclarations[1024];
	variableAssignmentNode variableAssignments[1024];
	blockStatementNode blockStatements[1024];
	unaryExpressionNode unaryExpressions[1024];
	binaryExpressionNode binaryExpressions[1024];
	parenthesizedExpressionNode parenthesizedExpressions[1024];
	u8 tokenBufferIndex;
	u16 nodeIndex;
	u16 variableDeclaratonIndex;
	u16 variableAssignmentIndex;
	u16 blockIndex;
	u16 unaryExpressionIndex;
	u16 binaryExpressionIndex;
	u16 parenthesizedExpressionIndex;
} parser;

node parser_next_token(parser *p, diagnosticContainer *d);

node parser_parse_statement(parser *p, diagnosticContainer *d);
node parser_parse_block_statement(parser *p, diagnosticContainer *d);
node parser_parse_variable_declaration(parser *p, diagnosticContainer *d);
node parser_parse_variable_assignment(parser *p, diagnosticContainer *d);

node parser_parse_expression(parser *p, diagnosticContainer *d);
node parser_parse_binary_expression(parser *p, diagnosticContainer *d, i8 parentPrecedence);
node parser_parse_primary_expression(parser *p, diagnosticContainer *d);

node parser_parse_token(parser *p, diagnosticContainer *d) {
	while(true) {
		node t = lexer_lex_token(&p->lexer, d);
		if (t.kind == whitespaceToken || t.kind == newlineToken) continue;
		return t;
	}
}

node parser_peek(parser *p, diagnosticContainer *d, u8 n) {
	u8 index;
	for (int i = 0; i <= n; i++) {
		index = (p->tokenBufferIndex + i) % MAX_LOOKAHEAD;
		if (p->token_buffer[index].kind == emptyToken) {
			p->token_buffer[index] = parser_parse_token(p, d);
		}
	}
	return p->token_buffer[index];
}

inline node parser_current(parser *p, diagnosticContainer *d) { return parser_peek(p, d, 0); }

node parser_next_token(parser *p, diagnosticContainer *d) {
	node t = parser_current(p, d);
	p->token_buffer[p->tokenBufferIndex].kind = emptyToken;
	p->tokenBufferIndex = (p->tokenBufferIndex + 1) % MAX_LOOKAHEAD;
	return t;
}

node parser_match_token(parser *p, diagnosticContainer *d, enum syntaxKind expectedKind) {

	node t = parser_current(p, d);
	if (t.kind != expectedKind) {
		// badToken already gets reported by the lexer
		if (t.kind != badToken) report_diagnostic(d, unexpectedTokenDiagnostic, t.text_start, t.text_length, t.kind, expectedKind, 0);
		t.kind=errorToken;
	}
	p->token_buffer[p->tokenBufferIndex].kind = emptyToken;
	p->tokenBufferIndex = (p->tokenBufferIndex + 1) % MAX_LOOKAHEAD;
	return t;
}

void parser_parse(parser *p, diagnosticContainer *d) {
	p->root = parser_parse_statement(p, d);
	parser_match_token(p, d, endOfFileToken);
}

node parser_parse_statement(parser *p, diagnosticContainer *d) {
	node l1 = parser_peek(p, d, 0);
	enum syntaxKind l1kind  = l1.kind;

	node l2 = parser_peek(p, d, 1);
	enum syntaxKind l2kind  = l2.kind;

	if (l1kind == openCurlyToken) return parser_parse_block_statement(p, d);
	if (l1kind == identifierToken && l2kind == colonToken) return parser_parse_variable_declaration(p, d);
	if (l1kind == identifierToken && l2kind == equalsToken) return parser_parse_variable_assignment(p, d);
	return parser_parse_expression(p, d);
}

node parser_parse_block_statement(parser *p, diagnosticContainer *d) {
	node openCurly = parser_match_token(p, d, openCurlyToken);
	node closeCurly;

	// TODO: add dynamic arrays so this doesn't have to be fixed size
	node nodes[100];
	int nodeIndex = 0;

	while (true) {
		node token = parser_current(p, d);

		if (token.kind == closeCurlyToken) {
			closeCurly = parser_next_token(p, d);
			break;
		}

		if (token.kind == endOfFileToken) {
			report_diagnostic(d, unexpectedTokenDiagnostic, token.text_start, token.text_length, token.kind, closeCurlyToken, 0);
			closeCurly = parser_next_token(p, d);
			break;
		}

		node exprNode = parser_parse_statement(p, d);
		nodes[nodeIndex++] = exprNode;
	}

	// TODO: some kind of memcpy is probably faster
	u16 startIndex = p->nodeIndex;
	u16 statementCount = nodeIndex;
	for (int i = 0; i < statementCount; i++) {
		p->nodes[p->nodeIndex++] = nodes[i];
	}

	u16 blockIndex = p->blockIndex;
	blockStatementNode blockData = { openCurly, &(p->nodes[startIndex]), statementCount, closeCurly, };
	p->blockStatements[p->blockIndex++] = blockData;

	node blockNode = {
		.kind = blockStatement,
		.text_start = openCurly.text_start,
		.text_length = (closeCurly.text_start -  openCurly.text_start) - closeCurly.text_length,
		.data = &(p->blockStatements[blockIndex]),
	};
	return blockNode;
}

node parser_parse_variable_declaration(parser *p, diagnosticContainer *d) {
	node identifier = parser_match_token(p, d, identifierToken);
	node colon = parser_match_token(p, d, colonToken);
	node equals = parser_match_token(p, d, equalsToken);
	node expression = parser_parse_expression(p, d);

	variableDeclarationNode declData = { identifier, colon, equals, expression };

	u16 index = p->variableDeclaratonIndex;
	p->variableDeclarations[p->variableDeclaratonIndex++] = declData;

	node declNode = {
		.kind = variableDeclaration,
		.text_start = identifier.text_start,
		.text_length = (expression.text_start - identifier.text_start) + expression.text_length,
		.data = &(p->variableDeclarations[index]), 
	};

	return declNode;
}

node parser_parse_variable_assignment(parser *p, diagnosticContainer *d) {
	node identifier = parser_match_token(p, d, identifierToken);
	node equals = parser_match_token(p, d, equalsToken);
	node expression = parser_parse_expression(p, d);

	variableAssignmentNode assData = { identifier,  equals, expression };

	u16 index = p->variableAssignmentIndex;
	p->variableAssignments[p->variableAssignmentIndex++] = assData;

	node assNode = {
		.kind = variableAssignment,
		.text_start = identifier.text_start,
		.text_length = (expression.text_start - identifier.text_start) + expression.text_length,
		.data = &(p->variableAssignments[index]), 
	};

	return assNode;
}

node parser_parse_expression(parser *p, diagnosticContainer *d) { return parser_parse_binary_expression(p, d,-2); }

node parser_parse_binary_expression(parser *p, diagnosticContainer *d, i8 parentPrecedence) {
	node unaryOp = parser_current(p, d);
	i8 unaryPrecedence =  getUnaryOperatorPrecedence(unaryOp.kind);

	// found a unary operator
	if (unaryPrecedence != -1) parser_next_token(p, d);

	node left = parser_parse_primary_expression(p, d);

	if (left.kind == endOfFileToken || left.kind == errorToken) return left;

	while (true) {
		node operator = parser_current(p, d);

		i8 precedence = getBinaryOperatorPrecedence(operator.kind);

		if (unaryPrecedence != -1 && unaryPrecedence > precedence) {
			// ensure it doesn't get used a second time
			unaryPrecedence = -1;

			unaryExpressionNode exprData = { unaryOp, left, };

			u16 index = p->unaryExpressionIndex;
			p->unaryExpressions[p->unaryExpressionIndex++] = exprData;

			node exprNode = {
				.kind = unaryExpression,
				.text_start = unaryOp.text_start,
				.text_length = (left.text_start - unaryOp.text_start) + left.text_length,
				.data = &(p->unaryExpressions[index]), 
			};

			left = exprNode;
		}

		if (precedence == -1 || precedence <= parentPrecedence) {
			return left;
		}

		operator = parser_next_token(p, d);

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
	node current = parser_current(p, d);

	if (current.kind == numberLiteral) return parser_next_token(p, d);

	if (current.kind == identifierToken) return parser_next_token(p, d);

	if (current.kind == openParenthesisToken) {
		node expr = parser_parse_expression(p, d);
		node closeParen = parser_match_token(p, d, closeParenthesisToken);

		parenthesizedExpressionNode exprData = { current, expr, closeParen };

		u16 index = p->parenthesizedExpressionIndex;
		p->parenthesizedExpressions[p->parenthesizedExpressionIndex++] = exprData;

		node exprNode = {
			.kind = parenthesizedExpression,
			.text_start = current.text_start,
			.text_length = (closeParen.text_start - current.text_start) + closeParen.text_length,
			.data = &(p->parenthesizedExpressions[index]), 
		};

		return exprNode;
	}

	// TODO: better error handling here
	if (current.kind != badToken) report_diagnostic(d, unexpectedTokenDiagnostic, current.text_start, current.text_length, current.kind, numberLiteral, 0);
	return parser_next_token(p, d);
}
