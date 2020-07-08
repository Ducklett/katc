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
node parser_parse_file_statement(parser *p, diagnosticContainer *d);
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
		if (t.kind == whitespaceToken
			|| t.kind == newlineToken
			|| t.kind == singleLineComment
			|| t.kind == multiLineComment) continue;
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
	p->root = parser_parse_file_statement(p, d);
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
		} else if (isAssignmentOperator(l2kind)) {
			res = parser_parse_variable_assignment(p, d);
		} else {
			res = parser_parse_expression(p, d);
		} break;
	default: res = parser_parse_expression(p, d); break;
	}

	while (parser_current(p, d).kind == semicolonToken) parser_next_token(p, d);

	return res;
}

node parser_parse_file_statement(parser *p, diagnosticContainer *d) {
	node *nodes = NULL;

	while (true) {
		node token = parser_current(p, d);

		if (token.kind == endOfFileToken) break;

		node exprNode = parser_parse_statement(p, d);
		sb_push(nodes, exprNode);
	}

	// TODO: some kind of memcpy is probably faster
	u16 startIndex = p->nodeIndex;
	u16 statementCount = sb_count(nodes);
	for (int i = 0; i < statementCount; i++) {
		p->nodes[p->nodeIndex++] = nodes[i];
	}

	sb_free(nodes);

	node voidNode = {0};

	u16 blockIndex = p->blockIndex;
	p->blockStatements[p->blockIndex++] =
		(blockStatementNode){ voidNode, &(p->nodes[startIndex]), statementCount, voidNode, };

	return (node) { fileStatement, textspan_from_bounds(&voidNode, &voidNode), .data = &(p->blockStatements[blockIndex]), };
}

node parser_parse_block_statement(parser *p, diagnosticContainer *d) {
	node openCurly = parser_match_token(p, d, openCurlyToken);
	node closeCurly;

	node *nodes = NULL;

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
		sb_push(nodes, exprNode);
	}

	// TODO: some kind of memcpy is probably faster
	u16 startIndex = p->nodeIndex;
	u16 statementCount = sb_count(nodes);
	for (int i = 0; i < statementCount; i++) {
		p->nodes[p->nodeIndex++] = nodes[i];
	}

	sb_free(nodes);

	u16 blockIndex = p->blockIndex;
	p->blockStatements[p->blockIndex++] =
		(blockStatementNode){ openCurly, &(p->nodes[startIndex]), statementCount, closeCurly, };

	return (node) { blockStatement, textspan_from_bounds(&openCurly, &closeCurly), .data = &(p->blockStatements[blockIndex]), };
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

	u16 index = p->ifStatementIndex;
	p->ifStatements[p->ifStatementIndex++] =
		(ifStatementNode){ ifToken, condition, thenStatement, elseToken, elseStatement };

	node lastToken = elseToken.kind == emptyToken ? thenStatement : elseStatement;

	return (node) { ifStatement, textspan_from_bounds(&ifToken, &lastToken), .data = &(p->ifStatements[index]), };
}

node parser_parse_case_branch(parser *p, diagnosticContainer *d) {
	node condition = parser_current(p,d).kind == defaultKeyword
		? parser_next_token(p, d)
		: parser_parse_expression(p, d);

	node colon = parser_match_token(p, d, colonToken);
	node thenStatement = parser_parse_statement(p, d);

	u16 index = p->caseBranchIndex;
	p->caseBranches[p->caseBranchIndex++] = 
		(caseBranchNode){ condition, colon, thenStatement };

	return (node) { caseBranch, textspan_from_bounds(&condition, &thenStatement), .data = &(p->caseBranches[index]), };
}

node parser_parse_case_statement(parser *p, diagnosticContainer *d) {
	node caseToken = parser_match_token(p, d, caseKeyword);
	node openCurly = parser_match_token(p, d, openCurlyToken);

	node *branches = NULL;

	while(true) {
		enum syntaxKind currentKind = parser_current(p, d).kind;
		if (currentKind == closeCurlyToken || currentKind == endOfFileToken) break;
		sb_push(branches, parser_parse_case_branch(p, d));
	}

	node closeCurly = parser_match_token(p, d, closeCurlyToken);

	node* branchesStart = &(p->nodes[p->nodeIndex]);
	for (int i=0; i < sb_count(branches); i++)
		p->nodes[p->nodeIndex++] = branches[i];

	u16 index = p->caseStatementIndex;
	p->caseStatements[p->caseStatementIndex++] =
		(caseStatementNode){ caseToken, openCurly, branchesStart, sb_count(branches), closeCurly };

	sb_free(branches);

	return (node) { caseStatement, textspan_from_bounds(&caseToken, &closeCurly), .data = &(p->caseStatements[index]), };
}

node parser_parse_while_loop(parser *p, diagnosticContainer *d) {
	node whileToken = parser_match_token(p, d, whileKeyword);
	node condition = parser_parse_expression(p, d);
	node block = parser_parse_statement(p, d);

	u16 index = p->whileLoopIndex;
	p->whileLoops[p->whileLoopIndex++] =
		(whileLoopNode){ whileToken, condition, block };

	return (node) { whileLoop, textspan_from_bounds(&whileToken, &block), .data = &(p->whileLoops[index]), };
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

	u16 index = p->forLoopIndex;
	p->forLoops[p->forLoopIndex++] =
		(forLoopNode){ forToken, openParen, value, comma, key, inToken, range, closeParen, block };

	return (node) { forLoop, textspan_from_bounds(&forToken, &block), .data = &(p->forLoops[index]), };
}

node parser_parse_variable_declaration(parser *p, diagnosticContainer *d) {
	node identifier = parser_match_token(p, d, identifierToken);
	node colon = parser_match_token(p, d, colonToken);

	enum syntaxKind mutKind = parser_current(p, d).kind;
	if (mutKind == identifierToken) mutKind = parser_peek(p, d, 1).kind;
	bool hasInitializer = mutKind == colonToken || mutKind == equalsToken;

	node type = {0};
	if (parser_current(p, d).kind == identifierToken || !hasInitializer) {
		type = parser_match_token(p, d, identifierToken);
	}

	node equals = {0}; 
	node expression = {0}; 

	if (hasInitializer) {
		equals = parser_next_token(p, d);
		expression = parser_parse_statement(p, d);
	}

	u16 index = p->variableDeclaratonIndex;
	p->variableDeclarations[p->variableDeclaratonIndex++] =
		(variableDeclarationNode){ identifier, colon, type, equals, expression };

	return (node) { variableDeclaration, textspan_from_bounds(&identifier, &expression), .data = &(p->variableDeclarations[index]), };
}

node parser_parse_variable_assignment(parser *p, diagnosticContainer *d) {
	node identifier = parser_match_token(p, d, identifierToken);
	node equals = parser_next_token(p, d);

	if (!isAssignmentOperator(equals.kind) && equals.kind != badToken) {
		report_diagnostic(d, notAnAssignmentOperatorDiagnostic, equals.span, equals.kind, 0, 0);
	}

	node expression = parser_parse_statement(p, d);

	u16 index = p->variableAssignmentIndex;
	p->variableAssignments[p->variableAssignmentIndex++] =
		(variableAssignmentNode){ identifier, equals, expression };

	return (node) { variableAssignment, textspan_from_bounds(&identifier, &expression), .data = &(p->variableAssignments[index]), };
}

node parser_parse_function_call(parser *p, diagnosticContainer *d) {
	node *arguments = NULL;

	node identifier = parser_match_token(p, d, identifierToken);
	node openParen = parser_match_token(p, d, openParenthesisToken);

	if (parser_current(p,d).kind == closeParenthesisToken) goto end;
	while (true) {

		sb_push(arguments, parser_parse_expression(p, d));

		node cur = parser_current(p,d);
		if (cur.kind == closeParenthesisToken || cur.kind == endOfFileToken) break;

		sb_push(arguments, parser_match_token(p, d, commaToken));
	}
	end: ;
	node closeParen = parser_match_token(p, d, closeParenthesisToken);

	int argStart = p->nodeIndex;
	for (int i = 0; i < sb_count(arguments); i++) p->nodes[p->nodeIndex++] = arguments[i];

	u16 index = p->functionCallIndex;
	p->functionCalls[p->functionCallIndex++] =
		(functionCallNode){ identifier,  openParen, &(p->nodes[argStart]), sb_count(arguments), closeParen };

	sb_free(arguments);

	return (node) { callExpression, textspan_from_bounds(&identifier, &closeParen), .data = &(p->functionCalls[index]), };
}

node parser_parse_expression(parser *p, diagnosticContainer *d) { return parser_parse_binary_expression(p, d,-2); }

node parser_parse_binary_expression(parser *p, diagnosticContainer *d, i8 parentPrecedence) {

	bool unaryOnLeft = true;
	node unaryOp = parser_current(p, d);
	i8 unaryPrecedence =  getUnaryOperatorPrecedence(unaryOp.kind);

	// found a unary operator
	if (unaryPrecedence != -1) parser_next_token(p, d);

	node left = parser_parse_primary_expression(p, d);

	// try to find a right unary expression if a left one was not found
	if (unaryPrecedence == -1) {
		unaryOp = parser_current(p, d);
		if (unaryOp.kind == plusPlusOperator || unaryOp.kind == minusMinusOperator) {
			unaryPrecedence =  getUnaryOperatorPrecedence(unaryOp.kind);
			parser_next_token(p, d);
			unaryOnLeft = false;
		} 
	} else if (left.kind == endOfFileToken || left.kind == errorToken) return left;



	while (true) {
		node operator = parser_current(p, d);

		i8 precedence = getBinaryOperatorPrecedence(operator.kind);

		if (unaryPrecedence != -1 && unaryPrecedence > precedence) {
			// ensure it doesn't get used a second time
			unaryPrecedence = -1;

			u16 index = p->unaryExpressionIndex;
			p->unaryExpressions[p->unaryExpressionIndex++] =
				(unaryExpressionNode){ unaryOp, left, unaryOnLeft };

			left = (node) { unaryExpression, textspan_from_bounds(&unaryOp, &left), .data = &(p->unaryExpressions[index]), };
		}

		if (precedence == -1 || precedence <= parentPrecedence) {
			return left;
		}

		operator = parser_next_token(p, d);

		node right = parser_parse_binary_expression(p, d, precedence);

		u16 index = p->binaryExpressionIndex;
		p->binaryExpressions[p->binaryExpressionIndex++] =
			(binaryExpressionNode){ left, operator, right, };

		left = (node) { binaryExpression, textspan_from_bounds(&left, &right), .data = &(p->binaryExpressions[index]), };
	}

	return left;
}

node parser_parse_range_expression(parser *p, diagnosticContainer *d) {

	node from  = parser_parse_expression(p, d);
	node dotDot  = parser_match_token(p, d, dotDotToken);
	node to  = parser_parse_expression(p, d);

	u16 index = p->rangeExpressionIndex;
	p->rangeExpressions[p->rangeExpressionIndex++] =
		(rangeExpressionNode){ from, dotDot, to };

	return (node) { rangeExpression, textspan_from_bounds(&from, &to), .data = &(p->rangeExpressions[index]), };
}

node parser_parse_primary_expression(parser *p, diagnosticContainer *d) {
	node current = parser_current(p, d);
	node lookahead = parser_peek(p, d, 1);

	switch (current.kind) {
	case identifierToken:
		if (lookahead.kind == openParenthesisToken) return parser_parse_function_call(p, d);
		else return parser_next_token(p, d);

	case numberLiteral:
	case stringLiteral:
	case charLiteral:
	case trueKeyword:
	case falseKeyword:
		return parser_next_token(p, d);

	case openParenthesisToken: {
		node openParen = parser_next_token(p, d);
		node expr = parser_parse_expression(p, d);
		node closeParen = parser_match_token(p, d, closeParenthesisToken);

		u16 index = p->parenthesizedExpressionIndex;
		p->parenthesizedExpressions[p->parenthesizedExpressionIndex++] =
			(parenthesizedExpressionNode){ openParen, expr, closeParen };

		return (node) { parenthesizedExpression, textspan_from_bounds(&current, &closeParen), .data = &(p->parenthesizedExpressions[index]), };
	}
	default: 
		if (current.kind != badToken) report_diagnostic(d, illegalPrimaryExpressionDiagnostic, current.span, current.kind, 0, 0);
		return parser_next_token(p, d);
	}
}

int create_syntaxtree(char* text, u64 length, parser* p, diagnosticContainer* d)  {

	p->lexer = (lexer){ .text = text, .text_length = length, .index = 0, .string_arena = arena_create() };
	if (p->lexer.string_arena == NULL) panic("memory allocation for lexer.string_arena failed\n");

	//printf("Input: %s\n", text);
	{
		benchmark_start();
		parser_parse(p, d);
		benchmark_end("Parsing");
	}

	return d->index==0;
}
