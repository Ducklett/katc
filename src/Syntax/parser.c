#define MAX_LOOKAHEAD 10

typedef struct parser {
	lexer lexer;
	node token_buffer[MAX_LOOKAHEAD];	// ring buffer that caches token lookaheads
	node root;
	node nodes[1024];	// stores the nodes from arbitrarily sized sequences like paramater lists and block statements
	variableDeclarationNode variableDeclarations[1024];
	variableAssignmentNode variableAssignments[1024];
	functionCallNode functionCalls[1024];
	blockStatementNode blockStatements[1024];
	ifStatementNode ifStatements[1024];
	caseStatementNode caseStatements[1024];
	caseBranchNode caseBranches[1024];
	whileLoopNode whileLoops[1024];
	forLoopNode forLoops[1024];
	unaryExpressionNode unaryExpressions[1024];
	binaryExpressionNode binaryExpressions[1024];
	parenthesizedExpressionNode parenthesizedExpressions[1024];
	rangeExpressionNode rangeExpressions[1024];
	u8 tokenBufferIndex;
	u16 nodeIndex;
	u16 variableDeclaratonIndex;
	u16 variableAssignmentIndex;
	u16 functionCallIndex;
	u16 blockIndex;
	u16 ifStatementIndex;
	u16 caseStatementIndex;
	u16 caseBranchIndex;
	u16 whileLoopIndex;
	u16 forLoopIndex;
	u16 unaryExpressionIndex;
	u16 binaryExpressionIndex;
	u16 parenthesizedExpressionIndex;
	u16 rangeExpressionIndex;
} parser;

node parser_next_token(parser *p, diagnosticContainer *d);

node parser_parse_statement(parser *p, diagnosticContainer *d);
node parser_parse_block_statement(parser *p, diagnosticContainer *d);
node parser_parse_if_statement(parser *p, diagnosticContainer *d);
node parser_parse_case_statement(parser *p, diagnosticContainer *d);
node parser_parse_case_branch(parser *p, diagnosticContainer *d);
node parser_parse_while_loop(parser *p, diagnosticContainer *d);
node parser_parse_for_loop(parser *p, diagnosticContainer *d);
node parser_parse_variable_declaration(parser *p, diagnosticContainer *d);
node parser_parse_variable_assignment(parser *p, diagnosticContainer *d);
node parser_parse_function_call(parser *p, diagnosticContainer *d);

node parser_parse_expression(parser *p, diagnosticContainer *d);
node parser_parse_binary_expression(parser *p, diagnosticContainer *d, i8 parentPrecedence);
node parser_parse_primary_expression(parser *p, diagnosticContainer *d);
node parser_parse_range_expression(parser *p, diagnosticContainer *d);

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

static inline node parser_current(parser *p, diagnosticContainer *d) { return parser_peek(p, d, 0); }

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
		if (t.kind != badToken) report_diagnostic(d, unexpectedTokenDiagnostic, t.span, t.kind, expectedKind, 0);
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

	node res;

	switch(l1kind) {
	case openCurlyToken: res = parser_parse_block_statement(p, d); break;
	case ifKeyword: res = parser_parse_if_statement(p, d); break;
	case caseKeyword: res = parser_parse_case_statement(p, d); break;
	case whileKeyword: res = parser_parse_while_loop(p, d); break;
	case forKeyword: res = parser_parse_for_loop(p, d); break;
	case identifierToken:
		if  (l2kind == colonToken) {
			res = parser_parse_variable_declaration(p, d);
			break;
		}
		else if (l2kind == equalsToken) {
			res = parser_parse_variable_assignment(p, d);
			break;
		}
		else if  (l2kind == openParenthesisToken) {
			res = parser_parse_function_call(p, d);
			break;
		}
	default: res = parser_parse_expression(p, d); break;
	}

	while (parser_current(p, d).kind == semicolonToken) parser_next_token(p, d);

	return res;
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
			report_diagnostic(d, unexpectedTokenDiagnostic, token.span, token.kind, closeCurlyToken, 0);
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
		.span = textspan_from_bounds(&openCurly, &closeCurly),
		.data = &(p->blockStatements[blockIndex]),
	};
	return blockNode;
}

node parser_parse_if_statement(parser *p, diagnosticContainer *d) {
	node ifToken = parser_match_token(p, d, ifKeyword);
	node condition = parser_parse_expression(p, d);
	node thenStatement = parser_parse_statement(p, d);

	node elseToken = {0};
	node elseStatement = {0};
	if (parser_current(p, d).kind == elseKeyword) {
		elseToken = parser_match_token(p, d, elseKeyword);
		elseStatement = parser_parse_statement(p, d);
	} 

	ifStatementNode ifData = { ifToken, condition, thenStatement, elseToken, elseStatement };

	u16 index = p->ifStatementIndex;
	p->ifStatements[p->ifStatementIndex++] = ifData;

	node lastToken = elseToken.kind == emptyToken ? thenStatement : elseStatement;

	node ifNode = {
		.kind = ifStatement,
		.span = textspan_from_bounds(&ifToken, &lastToken),
		.data = &(p->ifStatements[index]), 
	};

	return ifNode;
}

node parser_parse_case_branch(parser *p, diagnosticContainer *d) {
	node condition = parser_current(p,d).kind == defaultKeyword
		? parser_next_token(p, d)
		: parser_parse_expression(p, d);

	node colon = parser_match_token(p, d, colonToken);
	node thenStatement = parser_parse_statement(p, d);

	caseBranchNode bData = { condition, colon, thenStatement };

	u16 index = p->caseBranchIndex;
	p->caseBranches[p->caseBranchIndex++] = bData;

	node bNode = {
		.kind = caseBranch,
		.span = textspan_from_bounds(&condition, &thenStatement),
		.data = &(p->caseBranches[index]), 
	};

	return bNode;
}

node parser_parse_case_statement(parser *p, diagnosticContainer *d) {
	node caseToken = parser_match_token(p, d, caseKeyword);
	node openCurly = parser_match_token(p, d, openCurlyToken);

	node branches[100];
	u8 branchCount=0;
	while(parser_current(p, d).kind != closeCurlyToken)
		branches[branchCount++] = parser_parse_case_branch(p, d);

	node closeCurly = parser_match_token(p, d, closeCurlyToken);

	node* branchesStart = &(p->nodes[p->nodeIndex]);
	for (int i=0; i<branchCount; i++)
		p->nodes[p->nodeIndex++] = branches[i];

	caseStatementNode caseData = { caseToken, openCurly, branchesStart, branchCount, closeCurly };

	u16 index = p->caseStatementIndex;
	p->caseStatements[p->caseStatementIndex++] = caseData;

	node caseNode = {
		.kind = caseStatement,
		.span = textspan_from_bounds(&caseToken, &closeCurly),
		.data = &(p->caseStatements[index]), 
	};

	return caseNode;
}

node parser_parse_while_loop(parser *p, diagnosticContainer *d) {
	node whileToken = parser_match_token(p, d, whileKeyword);
	node condition = parser_parse_expression(p, d);
	node block = parser_parse_statement(p, d);

	whileLoopNode whileData = { whileToken, condition, block };

	u16 index = p->whileLoopIndex;
	p->whileLoops[p->whileLoopIndex++] = whileData;

	node whileNode = {
		.kind = whileLoop,
		.span = textspan_from_bounds(&whileToken, &block),
		.data = &(p->whileLoops[index]), 
	};

	return whileNode;
}

node parser_parse_for_loop(parser *p, diagnosticContainer *d) {
	node openParen = {0};
	node closeParen = {0};
	node comma = {0};
	node key = {0};

	node forToken = parser_match_token(p, d, forKeyword);

	bool hasParens = parser_current(p, d).kind == openParenthesisToken;

	if (hasParens) openParen = parser_match_token(p, d, openParenthesisToken);

	node value = parser_match_token(p, d, identifierToken);

	bool hasKey = parser_current(p, d).kind == commaToken;

	if (hasKey) {
		comma = parser_match_token(p, d, commaToken);
		key = parser_match_token(p, d, identifierToken);
	}

	node inToken = parser_match_token(p, d, inKeyword);

	node range = parser_parse_range_expression(p, d);

	if (hasParens) closeParen = parser_match_token(p, d, closeParenthesisToken);

	node block = parser_parse_statement(p, d);

	forLoopNode forData = { forToken, openParen, value, comma, key, inToken, range, closeParen, block };

	u16 index = p->forLoopIndex;
	p->forLoops[p->forLoopIndex++] = forData;

	node forNode = {
		.kind = forLoop,
		.span = textspan_from_bounds(&forToken, &block),
		.data = &(p->forLoops[index]), 
	};

	return forNode;
}

node parser_parse_variable_declaration(parser *p, diagnosticContainer *d) {
	node identifier = parser_match_token(p, d, identifierToken);
	node colon = parser_match_token(p, d, colonToken);

	node type = {0};
	if (parser_current(p, d).kind == identifierToken) {
		type = parser_next_token(p, d);
	}

	node equals = parser_match_token(p, d, equalsToken);
	node expression = parser_parse_expression(p, d);

	variableDeclarationNode declData = { identifier, colon, type, equals, expression };

	u16 index = p->variableDeclaratonIndex;
	p->variableDeclarations[p->variableDeclaratonIndex++] = declData;

	node declNode = {
		.kind = variableDeclaration,
		.span = textspan_from_bounds(&identifier, &expression),
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
		.span = textspan_from_bounds(&identifier, &expression),
		.data = &(p->variableAssignments[index]), 
	};

	return assNode;
}

node parser_parse_function_call(parser *p, diagnosticContainer *d) {
	node arguments[20];
	u8 argumentCount = 0;

	node identifier = parser_match_token(p, d, identifierToken);
	node openParen = parser_match_token(p, d, openParenthesisToken);

	if (parser_current(p,d).kind == closeParenthesisToken) goto end;
	while (true) {

		arguments[argumentCount++] = parser_parse_expression(p, d);

		node cur = parser_current(p,d);
		if (cur.kind == closeParenthesisToken || cur.kind == endOfFileToken) break;

		arguments[argumentCount++] = parser_match_token(p, d, commaToken);
	}
	end:
	if (true) {}
	node closeParen = parser_match_token(p, d, closeParenthesisToken);

	int argStart = p->nodeIndex;
	for (int i = 0; i<argumentCount;i++) p->nodes[p->nodeIndex++] = arguments[i];


	functionCallNode fnData = { identifier,  openParen, &(p->nodes[argStart]), argumentCount, closeParen };

	u16 index = p->functionCallIndex;
	p->functionCalls[p->functionCallIndex++] = fnData;

	node fnNode = {
		.kind = callExpression,
		.span = textspan_from_bounds(&identifier, &closeParen),
		.data = &(p->functionCalls[index]), 
	};

	return fnNode;
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
				.span = textspan_from_bounds(&unaryOp, &left),
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
			.span = textspan_from_bounds(&left, &right),
			.data = &(p->binaryExpressions[index]), 
		};

		left = exprNode;
	}

	return left;
}

node parser_parse_range_expression(parser *p, diagnosticContainer *d) {
	node from  = parser_match_token(p, d, numberLiteral);
	node dotDot  = parser_match_token(p, d, dotDotToken);
	node to  = parser_match_token(p, d, numberLiteral);

	rangeExpressionNode exprData = { from, dotDot, to };

	u16 index = p->rangeExpressionIndex;
	p->rangeExpressions[p->rangeExpressionIndex++] = exprData;

	node exprNode = {
		.kind = rangeExpression,
		.span = textspan_from_bounds(&from, &to),
		.data = &(p->rangeExpressions[index]), 
	};

	return exprNode;
}

node parser_parse_primary_expression(parser *p, diagnosticContainer *d) {
	node current = parser_current(p, d);
	node lookahead = parser_peek(p, d, 1);

	if (current.kind == numberLiteral && lookahead.kind == dotDotToken)
		return parser_parse_range_expression(p, d);

	switch (current.kind) {
	case identifierToken: {
		if (lookahead.kind == openParenthesisToken) return parser_parse_function_call(p, d);
		else return parser_next_token(p, d);
	}

	case numberLiteral:
	case stringLiteral:
	case trueKeyword:
	case falseKeyword:
		return parser_next_token(p, d);

	case openParenthesisToken: {
		node openParen = parser_next_token(p, d);
		node expr = parser_parse_expression(p, d);
		node closeParen = parser_match_token(p, d, closeParenthesisToken);

		parenthesizedExpressionNode exprData = { openParen, expr, closeParen };

		u16 index = p->parenthesizedExpressionIndex;
		p->parenthesizedExpressions[p->parenthesizedExpressionIndex++] = exprData;

		node exprNode = {
			.kind = parenthesizedExpression,
			.span = textspan_from_bounds(&current, &closeParen),
			.data = &(p->parenthesizedExpressions[index]), 
		};

		return exprNode;
	}
	default: 
		// TODO: better error handling here
		if (current.kind != badToken) report_diagnostic(d, unexpectedTokenDiagnostic, current.span, current.kind, numberLiteral, 0);
		return parser_next_token(p, d);
	}
}

int create_syntaxtree(char* text, u64 length, parser* p, diagnosticContainer* d)  {

	lexer l = { .text = text, .text_length = length, .index = 0, };
	p->lexer = l;

	printf("Input: %s\n", text);
	{
		benchmark_start();
		parser_parse(p, d);
		benchmark_end("Parsing");
	}

	return d->index==0;
}
