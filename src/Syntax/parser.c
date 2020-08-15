#define MAX_LOOKAHEAD 10

typedef struct parser {
	lexer lexer;
	node token_buffer[MAX_LOOKAHEAD];	// ring buffer that caches token lookaheads
	node root;
	u8 tokenBufferIndex;
	enum syntaxKind parentKind;
} parser;

node parser_next_token(parser *p, diagnosticContainer *d);

node parser_parse_statement(parser *p, diagnosticContainer *d);
node parser_parse_block_statement(parser *p, diagnosticContainer *d, bool isFileStatement);
node parser_parse_if_statement(parser *p, diagnosticContainer *d);
node parser_parse_case_statement(parser *p, diagnosticContainer *d);
node parser_parse_case_branch(parser *p, diagnosticContainer *d);
node parser_parse_switch_statement(parser *p, diagnosticContainer *d);
node parser_parse_switch_branch(parser *p, diagnosticContainer *d);
node parser_parse_while_loop(parser *p, diagnosticContainer *d);
node parser_parse_for_loop(parser *p, diagnosticContainer *d);
node parser_parse_symbol_reference(parser *p, diagnosticContainer *d, bool isIdentifier);
node parser_parse_function_declaration(parser *p, diagnosticContainer *d);
node parser_parse_variable_declaration(parser *p, diagnosticContainer *d);
node parser_parse_variable_assignment(parser *p, diagnosticContainer *d, node identifier);
node parser_parse_array_access(parser *p, diagnosticContainer *d, node identifier);
node parser_parse_function_call(parser *p, diagnosticContainer *d);
node parser_parse_namespace_declaration(parser *p, diagnosticContainer *d);
node parser_parse_enum_declaration(parser *p, diagnosticContainer *d);
node parser_parse_typedef_declaration(parser *p, diagnosticContainer *d);
node parser_parse_extern_declaration(parser *p, diagnosticContainer *d);
node parser_parse_return_statement(parser *p, diagnosticContainer *d);
node parser_parse_struct_declaration(parser *p, diagnosticContainer *d);

node parser_parse_expression(parser *p, diagnosticContainer *d);
node parser_parse_binary_expression(parser *p, diagnosticContainer *d, i8 parentPrecedence);
node parser_parse_primary_expression(parser *p, diagnosticContainer *d);
node parser_parse_range_expression(parser *p, diagnosticContainer *d, bool allowEarlyExit);

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

bool parser_next_token_is_on_new_line(parser *p, diagnosticContainer *d) {
	node cur = parser_peek(p, d, 0);
	for (int i=cur.span.start-1;i>=0;i--) {
		if (i==0) return true;
		char c = p->lexer.text[i];
		if (isNewline(c)) return true;
		if (isWhitespace(c)) continue;
		return false;
	}

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
	p->root = parser_parse_block_statement(p, d, true);
	parser_match_token(p, d, endOfFileToken);
}

// when given the array capacity int[10][20], the parser will produce a result of ((int 10) 20)
// this function reverses the capacities so it becomes ((int 20) 10)
// void reverse_array_capacity(node *n) {
// 	if (n->kind != arrayKind) return;

// 	arrayKindNode *an = (arrayKindNode*)n->data;
// 	if (an->identifier.kind != arrayKind) return;

// 	arrayKindNode *can = (arrayKindNode*)an->identifier.data;

// 	node temp = an->capacity;
// 	an->capacity = can->capacity;
// 	can->capacity = temp;

// 	reverse_array_capacity(&an->identifier);
// }

node parser_parse_type(parser *p, diagnosticContainer *d) {
	node identifier = parser_parse_symbol_reference(p,d,true);

	while (parser_current(p,d).kind == openBracketToken) {

		node openBracket = parser_match_token(p,d,openBracketToken);
		// TODO: make capacity optional and infer capacity from assignment
		node capacity = {0};
		if (parser_current(p,d).kind != closeBracketToken) {
			capacity = parser_parse_expression(p,d);
		}
		node closeBracket = parser_match_token(p,d,closeBracketToken);

		arrayKindNode *ar = arena_malloc(parser_arena, sizeof(arrayKindNode));
		*ar = (arrayKindNode){ identifier, openBracket, capacity, closeBracket };

		identifier = (node) { arrayKind, textspan_from_bounds(&identifier, &closeBracket), .data = ar, };
		// reverse_array_capacity(&identifier);
	}
	return identifier;
}
node parser_parse_statement(parser *p, diagnosticContainer *d) {
	node l1 = parser_peek(p, d, 0);
	enum syntaxKind l1kind  = l1.kind;

	node l2 = parser_peek(p, d, 1);
	enum syntaxKind l2kind  = l2.kind;

	node res;

	switch(l1kind) {
	case openCurlyToken: res = parser_parse_block_statement(p, d, false); break;
	case ifKeyword: res = parser_parse_if_statement(p, d); break;
	case caseKeyword: res = parser_parse_case_statement(p, d); break;
	case switchKeyword: res = parser_parse_switch_statement(p, d); break;
	case whileKeyword: res = parser_parse_while_loop(p, d); break;
	case forKeyword: res = parser_parse_for_loop(p, d); break;
	case breakKeyword: res = parser_next_token(p, d); break;
	case continueKeyword: res = parser_next_token(p, d); break;
	case fnKeyword: res = parser_parse_function_declaration(p, d); break;
	case namespaceKeyword: res = parser_parse_namespace_declaration(p, d); break;
	case structKeyword: res = parser_parse_struct_declaration(p, d); break;
	case enumKeyword: res = parser_parse_enum_declaration(p, d); break;
	case typedefKeyword: res = parser_parse_typedef_declaration(p, d); break;
	case externKeyword: res = parser_parse_extern_declaration(p, d); break;
	case returnKeyword: res = parser_parse_return_statement(p, d); break;
	case identifierToken:
		if  (l2kind == colonToken) {
			res = parser_parse_variable_declaration(p, d);
		} else {
			res = parser_parse_expression(p, d);
		} break;
	default: res = parser_parse_expression(p, d); break;
	}

	switch(p->parentKind) {
		case 0: break;
		case forLoop: break;
		case whileLoop: break;

		case functionDeclaration:
			if (
				res.kind == namespaceDeclaration ||
				(res.kind == breakKeyword || res.kind == continueKeyword))
					report_diagnostic(d, notAllowedInContextDiagnostic, res.span, res.kind, p->parentKind, 0); break;
		case namespaceDeclaration:
			if (res.kind != functionDeclaration && res.kind != variableDeclaration && res.kind != enumDeclaration && res.kind != structDeclaration && res.kind != typedefDeclaration)
				report_diagnostic(d, notAllowedInContextDiagnostic, res.span, res.kind, p->parentKind, 0); break;
		case structDeclaration:
			if (res.kind != variableDeclaration) report_diagnostic(d, notAllowedInContextDiagnostic, res.span, res.kind, p->parentKind, 0);
			break;
		case externDeclaration:
			if (res.kind != variableDeclaration && res.kind != functionDeclaration && res.kind != structDeclaration) report_diagnostic(d, notAllowedInContextDiagnostic, res.span, res.kind, p->parentKind, 0);
			break;
		case fileStatement:
			if (res.kind == breakKeyword || res.kind == continueKeyword)
					report_diagnostic(d, notAllowedInContextDiagnostic, res.span, res.kind, p->parentKind, 0); break;
		default:
			printf("unexpected parent kind %s\n", syntaxKindText[p->parentKind]);
			exit(1);
	}
	return res;
}

enum syntaxKind parser_push_context(parser *p, enum syntaxKind kind) {
	enum syntaxKind parent = p->parentKind;
	p->parentKind = kind;
	return parent;
}

void parser_pop_context(parser *p, enum syntaxKind parent) {
	p->parentKind = parent;
}

node parser_parse_block_statement(parser *p, diagnosticContainer *d, bool isFileStatement) {
	node openCurly = isFileStatement?(node){0}:parser_match_token(p, d, openCurlyToken);
	node closeCurly;

	enum syntaxKind statementKind = isFileStatement ? fileStatement : blockStatement;

	node *nodes = NULL;

	bool isFirst = true;
	while (true) {
		node token = parser_current(p, d);

		if (token.kind == (isFileStatement?endOfFileToken:closeCurlyToken)) {
			closeCurly = isFileStatement?(node){0}:parser_next_token(p, d);
			break;
		}

		if (!isFileStatement && token.kind == endOfFileToken) {
			report_diagnostic(d, unexpectedTokenDiagnostic, token.span, token.kind, closeCurlyToken, 0);
			closeCurly = isFileStatement?(node){0}:parser_next_token(p, d);
			break;
		}

		if (!isFirst) {
			node c = parser_current(p,d);
			if (!parser_next_token_is_on_new_line(p,d) && c.kind != semicolonToken) {
				if (c.kind != badToken) report_diagnostic(d, unexpectedTokenDiagnostic, c.span, c.kind, semicolonToken, 0);
			}
			while (parser_current(p, d).kind == semicolonToken) parser_next_token(p, d);
		}

		if (parser_current(p, d).kind == (isFileStatement?endOfFileToken:closeCurlyToken)) {
			closeCurly = isFileStatement?(node){0}:parser_next_token(p, d);
			break;
		}

		node exprNode = parser_parse_statement(p, d);

		isFirst = false;

		sb_push(nodes, exprNode);
	}

	u16 statementCount = sb_count(nodes);
	size_t statementsSize = sizeof(node) * statementCount;
	node* nodeStorage = arena_malloc(parser_arena, statementsSize);
	memcpy(nodeStorage, nodes, statementsSize);

	sb_free(nodes);

	blockStatementNode *block = arena_malloc(parser_arena, sizeof(blockStatementNode));
	*block = (blockStatementNode){ openCurly, nodeStorage, statementCount, closeCurly, };

	return (node) { blockStatement, textspan_from_bounds(&openCurly, &closeCurly), .data = block, };
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

	ifStatementNode *ifNode = arena_malloc(parser_arena, sizeof(ifStatementNode));
	*ifNode = (ifStatementNode){ ifToken, condition, thenStatement, elseToken, elseStatement };

	node lastToken = elseToken.kind == emptyToken ? thenStatement : elseStatement;

	return (node) { ifStatement, textspan_from_bounds(&ifToken, &lastToken), .data = ifNode, };
}

node parser_parse_case_branch(parser *p, diagnosticContainer *d) {
	node condition = parser_current(p,d).kind == defaultKeyword
		? parser_next_token(p, d)
		: parser_parse_expression(p, d);

	node colon = parser_match_token(p, d, colonToken);
	node thenStatement = parser_parse_statement(p, d);

	caseBranchNode *cbNode = arena_malloc(parser_arena, sizeof(caseBranchNode));
	*cbNode = (caseBranchNode){ condition, colon, thenStatement };

	return (node) { caseBranch, textspan_from_bounds(&condition, &thenStatement), .data = cbNode, };
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

	u16 branchCount = sb_count(branches);
	size_t branchesSize = sizeof(node) * branchCount;
	node* branchesStart = arena_malloc(parser_arena, branchesSize);
	memcpy(branchesStart, branches, branchesSize);

	caseStatementNode *caseNode = arena_malloc(parser_arena, sizeof(caseStatementNode));
	*caseNode = (caseStatementNode){ caseToken, openCurly, branchesStart, branchCount, closeCurly };

	sb_free(branches);

	return (node) { caseStatement, textspan_from_bounds(&caseToken, &closeCurly), .data = caseNode, };
}

node parser_parse_switch_branch(parser *p, diagnosticContainer *d) {
	bool isDefault = parser_current(p,d).kind == defaultKeyword;
	node caseToken = isDefault
		? parser_next_token(p, d)
		: parser_match_token(p, d, caseKeyword);

	node condition = isDefault
		? (node){0}
		: parser_parse_range_expression(p, d, true);

	node colon = parser_match_token(p, d, colonToken);
	node thenStatement = parser_parse_statement(p, d);

	switchBranchNode *sbNode = arena_malloc(parser_arena, sizeof(switchBranchNode));
	*sbNode = (switchBranchNode){ caseToken, condition, colon, thenStatement };

	return (node) { switchBranch, textspan_from_bounds(&condition, &thenStatement), .data = sbNode, };
}

node parser_parse_switch_statement(parser *p, diagnosticContainer *d) {
	node switchToken = parser_match_token(p, d, switchKeyword);
	node targetExpression = parser_parse_expression(p, d);
	node openCurly = parser_match_token(p, d, openCurlyToken);

	node *branches = NULL;

	while(true) {
		enum syntaxKind currentKind = parser_current(p, d).kind;
		if (currentKind == closeCurlyToken || currentKind == endOfFileToken) break;
		sb_push(branches, parser_parse_switch_branch(p, d));
	}

	node closeCurly = parser_match_token(p, d, closeCurlyToken);

	u16 branchCount = sb_count(branches);
	size_t branchesSize = sizeof(node) * branchCount;
	node* branchesStart = arena_malloc(parser_arena, branchesSize);
	memcpy(branchesStart, branches, branchesSize);

	switchStatementNode *switchNode = arena_malloc(parser_arena, sizeof(switchStatementNode));
	*switchNode = (switchStatementNode){ switchToken, targetExpression, openCurly, branchesStart, branchCount, closeCurly };

	sb_free(branches);

	return (node) { switchStatement, textspan_from_bounds(&switchToken, &closeCurly), .data = switchNode, };
}

node parser_parse_while_loop(parser *p, diagnosticContainer *d) {
	node whileToken = parser_match_token(p, d, whileKeyword);
	node condition = parser_parse_expression(p, d);

	enum syntaxKind prevKind = parser_push_context(p, whileLoop);
	node block = parser_parse_statement(p, d);
	parser_pop_context(p,prevKind);

	whileLoopNode *wnode = arena_malloc(parser_arena, sizeof(whileLoopNode));
	*wnode = (whileLoopNode){ whileToken, condition, block };

	return (node) { whileLoop, textspan_from_bounds(&whileToken, &block), .data = wnode, };
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

	node range = parser_parse_range_expression(p, d, true);

	if (hasParens) closeParen = parser_match_token(p, d, closeParenthesisToken);

	enum syntaxKind prevKind = parser_push_context(p, forLoop);
	node block = parser_parse_statement(p, d);
	parser_pop_context(p,prevKind);

	forLoopNode *forNode = arena_malloc(parser_arena, sizeof(forLoopNode));
	*forNode = (forLoopNode){ forToken, openParen, value, comma, key, inToken, range, closeParen, block };

	return (node) { forLoop, textspan_from_bounds(&forToken, &block), .data = forNode, };
}

node parser_parse_variable_declaration(parser *p, diagnosticContainer *d) {
	node identifier = parser_match_token(p, d, identifierToken);
	node colon = parser_match_token(p, d, colonToken);

	node type = {0};
	if (parser_current(p, d).kind == identifierToken) {
		type = parser_parse_type(p,d);
	}

	enum syntaxKind mutKind = parser_current(p, d).kind;
	bool hasInitializer = mutKind == colonToken || mutKind == equalsToken;

	if (type.kind == NULL && ((p->parentKind == namespaceDeclaration && mutKind == equalsToken) || !hasInitializer) ) {
		report_diagnostic(d, variableMustHaveTypeInCurrentContextDiagnostic, identifier.span, 0,0, 0);
	}

	node equals = {0}; 
	node expression = {0}; 

	if (hasInitializer) {
		equals = parser_next_token(p, d);
		expression = parser_parse_expression(p, d);
	}

	variableDeclarationNode *var = arena_malloc(parser_arena, sizeof(variableDeclarationNode));
	*var = (variableDeclarationNode){ identifier, colon, type, equals, expression };

	return (node) { variableDeclaration, textspan_from_bounds(&identifier, &expression), .data = var, };
}

node parser_parse_array_access(parser *p, diagnosticContainer *d, node identifier) {
	node openBracket = parser_match_token(p, d, openBracketToken);
	node index = parser_parse_expression(p,d);
	node closeBracket = parser_match_token(p, d, closeBracketToken);

	arrayAccessNode *access = arena_malloc(parser_arena, sizeof(arrayAccessNode));
	*access = (arrayAccessNode){ identifier, openBracket, index, closeBracket };

	return (node) { arrayAccessExpression, textspan_from_bounds(&identifier, &closeBracket), .data = access };
}

node parser_parse_variable_assignment(parser *p, diagnosticContainer *d, node identifier) {
	node equals = parser_next_token(p, d);

	if (!isAssignmentOperator(equals.kind) && equals.kind != badToken) {
		report_diagnostic(d, notAnAssignmentOperatorDiagnostic, equals.span, equals.kind, 0, 0);
	}

	node expression = parser_parse_statement(p, d);

	variableAssignmentNode *assignment = arena_malloc(parser_arena, sizeof(variableAssignmentNode));
	*assignment = (variableAssignmentNode){ identifier, equals, expression };

	return (node) { variableAssignment, textspan_from_bounds(&identifier, &expression), .data = assignment };
}

node parse_typed_identifier(parser *p, diagnosticContainer *d) {
	node refNode = {0};
	if (parser_current(p,d).kind == refKeyword) refNode = parser_match_token(p, d, refKeyword);
	node identifier = parser_match_token(p, d, identifierToken);
	node colon = parser_match_token(p, d, colonToken);
	node type = parser_parse_type(p, d);

	typedIdentifierNode *id = arena_malloc(parser_arena, sizeof(typedIdentifierNode));
	*id = (typedIdentifierNode){ refNode, identifier, colon, type };

	return (node) { typedIdentifier, textspan_from_bounds(&identifier, &type), .data = id, };
}

node* parse_function_parameters(parser *p, diagnosticContainer *d, u16 *paramCount) {
	node *parameters = NULL;

	if (parser_current(p,d).kind == closeParenthesisToken) goto end;
	while (true) {

		sb_push(parameters, parse_typed_identifier(p, d));

		node cur = parser_current(p,d);
		if (cur.kind == closeParenthesisToken || cur.kind == endOfFileToken) break;

		sb_push(parameters, parser_match_token(p, d, commaToken));
	}
	end: ;

	*paramCount = sb_count(parameters);
	size_t paramSize = sizeof(node) * *paramCount;
	node* paramStorage = arena_malloc(parser_arena, paramSize);
	memcpy(paramStorage, parameters, paramSize);

	sb_free(parameters);

	return paramStorage;
}

node parser_parse_function_declaration(parser *p, diagnosticContainer *d) {
	node fnToken = parser_match_token(p, d, fnKeyword);
	node identifier = parser_match_token(p, d, identifierToken);
	node openParen = parser_match_token(p, d, openParenthesisToken);
	u16 paramCount;
	node* parameters = parse_function_parameters(p, d, &paramCount);
	node closeParen = parser_match_token(p, d, closeParenthesisToken);

	node thickArrow = {0};
	node thinArrow = {0};
	node returnType = {0};
	node block = {0};
	if (parser_current(p,d).kind == equalsGreaterToken) {
		// arrow function
		thickArrow = parser_match_token(p, d, equalsGreaterToken);
		enum syntaxKind prevKind = parser_push_context(p, functionDeclaration);
		block = parser_parse_expression(p, d);
		parser_pop_context(p,prevKind);
	} else {
		if (parser_current(p,d).kind == minusGreaterToken) {
			thinArrow = parser_match_token(p, d, minusGreaterToken);
			returnType = parser_parse_type(p, d);
		}
		// block function
		// if no block is provided it is a forward declaration
		if (parser_current(p,d).kind == openCurlyToken) {
			enum syntaxKind prevKind = parser_push_context(p, functionDeclaration);
			block = parser_parse_block_statement(p, d, false);
			parser_pop_context(p,prevKind);
		}
	}

	functionDeclarationNode *fnode = arena_malloc(parser_arena, sizeof(functionDeclarationNode));
	*fnode = (functionDeclarationNode){
			fnToken, identifier, openParen,
			parameters, paramCount, closeParen,
			thinArrow, returnType, thickArrow,
			block };

	return (node) { functionDeclaration, textspan_from_bounds(&fnToken, &block), .data = fnode, };
}

node* parse_function_arguments(parser *p, diagnosticContainer *d, u16 *argCount) {
	node *arguments = NULL;

	if (parser_current(p,d).kind == closeParenthesisToken) goto end;
	while (true) {


		if (parser_current(p,d).kind == identifierToken && parser_peek(p, d, 1).kind == colonToken) {
			node name = parser_match_token(p, d, identifierToken);
			node colon = parser_match_token(p, d, colonToken);
			node expr = parser_parse_expression(p, d);

			namedArgumentNode* argNode = arena_malloc(parser_arena, sizeof(namedArgumentNode));
			*argNode = (namedArgumentNode){ name, colon, expr };
			sb_push(arguments, ((node){ namedArgument, textspan_from_bounds(&name, &expr), .data = argNode }));
		} else {
			sb_push(arguments, parser_parse_expression(p, d));
		}

		node cur = parser_current(p,d);
		if (cur.kind == closeParenthesisToken || cur.kind == endOfFileToken) break;

		sb_push(arguments, parser_match_token(p, d, commaToken));
	}
	end: ;

	*argCount = sb_count(arguments);
	size_t argSize = sizeof(node) * *argCount;
	node* argStorage = arena_malloc(parser_arena, argSize);
	memcpy(argStorage, arguments, argSize);

	sb_free(arguments);

	return argStorage;
}

node parser_parse_function_call(parser *p, diagnosticContainer *d) {

	node identifier = parser_match_token(p, d, identifierToken);
	node openParen = parser_match_token(p, d, openParenthesisToken);
	u16 argCount;
	node *argStorage = parse_function_arguments(p, d, &argCount);
	node closeParen = parser_match_token(p, d, closeParenthesisToken);

	functionCallNode *call = arena_malloc(parser_arena, sizeof(functionCallNode));
	*call = (functionCallNode){ identifier,  openParen, argStorage, argCount, closeParen };

	return (node) { callExpression, textspan_from_bounds(&identifier, &closeParen), .data = call };
}

node parser_parse_namespace_declaration(parser *p, diagnosticContainer *d) {
	node namespaceToken = parser_match_token(p, d, namespaceKeyword);
	node identifier = parser_parse_symbol_reference(p, d, true);

	enum syntaxKind prevKind = parser_push_context(p, namespaceDeclaration);
	node block = parser_parse_block_statement(p, d, false);
	parser_pop_context(p,prevKind);

	namespaceDeclarationNode *nnode = arena_malloc(parser_arena, sizeof(namespaceDeclarationNode));
	*nnode = (namespaceDeclarationNode){ namespaceToken, identifier, block };

	return (node) { namespaceDeclaration, textspan_from_bounds(&namespaceToken, &block), .data = nnode, };
}

node parser_parse_struct_declaration(parser *p, diagnosticContainer *d) {
	node structToken = parser_match_token(p, d, structKeyword);
	node identifier = parser_match_token(p, d, identifierToken);

	enum syntaxKind prevKind = parser_push_context(p, structDeclaration);
	node block = parser_parse_block_statement(p, d, false);
	parser_pop_context(p,prevKind);

	structDeclarationNode *nnode = arena_malloc(parser_arena, sizeof(structDeclarationNode));
	*nnode = (structDeclarationNode){ structToken, identifier, block };

	return (node) { structDeclaration, textspan_from_bounds(&structToken, &block), .data = nnode, };
}

node parser_parse_enum_declaration(parser *p, diagnosticContainer *d) {
	node enumToken = parser_match_token(p, d, enumKeyword);
	node identifier = parser_match_token(p,d,identifierToken);
	node openCurly = parser_match_token(p,d,openCurlyToken);

	enum syntaxKind prevKind = parser_push_context(p, enumDeclaration);

	node *enums = NULL;

	if (parser_current(p,d).kind == closeCurlyToken) goto end;
	while (true) {

		sb_push(enums, parser_match_token(p, d, identifierToken));

		node cur = parser_current(p,d);
		if (cur.kind == closeCurlyToken || cur.kind == endOfFileToken) break;

		sb_push(enums, parser_match_token(p, d, commaToken));

		// support trailing comma
		if (parser_current(p,d).kind == closeCurlyToken) break;
	}
	end: ;

	u8 enumCount = sb_count(enums);
	size_t enumSize = sizeof(node) * enumCount;
	node* enumStorage = arena_malloc(parser_arena, enumSize);
	memcpy(enumStorage, enums, enumSize);

	sb_free(enums);

	parser_pop_context(p,prevKind);

	node closeCurly = parser_match_token(p,d,closeCurlyToken);

	enumDeclarationNode *enode = arena_malloc(parser_arena, sizeof(enumDeclarationNode));
	*enode = (enumDeclarationNode){ enumToken, identifier, openCurly,  enumStorage, enumCount, closeCurly };

	return (node) { enumDeclaration, textspan_from_bounds(&enumToken, &closeCurly), .data = enode, };
}

node parser_parse_typedef_declaration(parser *p, diagnosticContainer *d) {
	node typedefToken = parser_match_token(p, d, typedefKeyword);
	node identifier = parser_match_token(p,d,identifierToken);
	node colon1 = parser_match_token(p,d,colonToken);
	node colon2 = parser_match_token(p,d,colonToken);
	node type = parser_parse_type(p,d);

	typedefDeclarationNode *tnode = arena_malloc(parser_arena, sizeof(typedefDeclarationNode));
	*tnode = (typedefDeclarationNode){ typedefToken, identifier, colon1, colon2, type};

	return (node) { typedefDeclaration, textspan_from_bounds(&typedefToken, &type), .data = tnode, };
}

node parser_parse_extern_declaration(parser *p, diagnosticContainer *d) {
	node externToken = parser_match_token(p, d, externKeyword);
	node identifier = parser_match_token(p,d,identifierToken);
	node colon1 = parser_match_token(p,d,colonToken);
	node colon2 = parser_match_token(p,d,colonToken);
	node libraryName = parser_match_token(p, d, stringLiteral);

	enum syntaxKind prevKind = parser_push_context(p, externDeclaration);
	node body = parser_parse_block_statement(p,d, false);
	parser_pop_context(p,prevKind);

	externDeclarationNode *enode = arena_malloc(parser_arena, sizeof(externDeclarationNode));
	*enode = (externDeclarationNode){ externToken, identifier, colon1, colon2, libraryName, body };

	return (node) { externDeclaration, textspan_from_bounds(&externToken, &body), .data = enode, };
}

node parser_parse_return_statement(parser *p, diagnosticContainer *d) {
	node returnNode = parser_match_token(p,d,returnKeyword);
	node expression = parser_parse_expression(p,d);

	returnStatementNode *rnode = arena_malloc(parser_arena, sizeof(returnStatementNode));
	*rnode = (returnStatementNode){ returnNode, expression };

	return (node) { returnStatement, textspan_from_bounds(&returnNode, &expression), .data = rnode, };
}

node parser_parse_expression(parser *p, diagnosticContainer *d) { return parser_parse_binary_expression(p, d,-2); }

node parser_parse_ternary_expression(parser *p, diagnosticContainer *d, node *condition) {
	node questionmark = parser_match_token(p,d,questionmarkToken);
	node thenStatement = parser_parse_expression(p,d);
	node colon = parser_match_token(p,d,colonToken);
	node elseStatement = parser_parse_expression(p,d);

	ternaryExpressionNode *ternaryNode = arena_malloc(parser_arena, sizeof(ternaryExpressionNode));
	*ternaryNode = (ternaryExpressionNode){ *condition, questionmark, thenStatement, colon, elseStatement };

	return (node) { ternaryExpression, textspan_from_bounds(condition, &elseStatement), .data = ternaryNode, };
}

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

			unaryExpressionNode *unaryNode = arena_malloc(parser_arena, sizeof(unaryExpressionNode));
			*unaryNode = (unaryExpressionNode){ unaryOp, left, unaryOnLeft };

			left = (node) { unaryExpression, textspan_from_bounds(&unaryOp, &left), .data = unaryNode, };
		}

		if (precedence == -1 || precedence <= parentPrecedence) {
			return left;
		}

		if (operator.kind == questionmarkToken) return parser_parse_ternary_expression(p, d, &left);

		operator = parser_next_token(p, d);

		node right = parser_parse_binary_expression(p, d, precedence);

		binaryExpressionNode *binaryNode = arena_malloc(parser_arena, sizeof(binaryExpressionNode));
		*binaryNode = (binaryExpressionNode){ left, operator, right, };

		left = (node) { binaryExpression, textspan_from_bounds(&left, &right), .data = binaryNode, };
	}

	return left;
}

node parser_parse_range_expression(parser *p, diagnosticContainer *d, bool allowEarlyExit) {

	node from  = parser_parse_expression(p, d);
	if (allowEarlyExit && parser_current(p, d).kind != dotDotToken) return from;

	node dotDot  = parser_match_token(p, d, dotDotToken);
	node to  = parser_parse_expression(p, d);

	rangeExpressionNode *rangeNode = arena_malloc(parser_arena, sizeof(rangeExpressionNode));
	*rangeNode = (rangeExpressionNode){ from, dotDot, to };

	return (node) { rangeExpression, textspan_from_bounds(&from, &to), .data = rangeNode, };
}

node parser_parse_symbol_reference(parser *p, diagnosticContainer *d, bool isIdentifier) {

	node left;

	if (!isIdentifier && parser_peek(p,d,1).kind == openParenthesisToken) left = parser_parse_function_call(p, d);
	else left = parser_match_token(p, d, identifierToken);

	if (parser_current(p, d).kind != dotToken) return left;

	node dotNode = parser_match_token(p, d, dotToken);

	node right = parser_parse_symbol_reference(p, d, isIdentifier);

	binaryExpressionNode *binaryNode = arena_malloc(parser_arena, sizeof(binaryExpressionNode));
	*binaryNode = (binaryExpressionNode){ left, dotNode, right, };

	return (node) { symbolReferenceExpression, textspan_from_bounds(&left, &right), .data = binaryNode, };
}

node parser_parse_array_literal(parser *p, diagnosticContainer *d) {
	node openBracket = parser_match_token(p,d, openBracketToken);

	node *values = NULL;

	if (parser_current(p,d).kind == closeBracketToken) goto end;
	while (true) {

		sb_push(values, parser_parse_expression(p,d));

		node cur = parser_current(p,d);
		if (cur.kind == closeBracketToken || cur.kind == endOfFileToken) break;

		sb_push(values, parser_match_token(p, d, commaToken));

		// support trailing comma
		if (parser_current(p,d).kind == closeBracketToken) break;
	}
	end: ;

	u8 valueCount = sb_count(values);
	size_t valueSize = sizeof(node) * valueCount;
	node* valueStorage = arena_malloc(parser_arena, valueSize);
	memcpy(valueStorage, values, valueSize);

	sb_free(values);

	node closeBracket = parser_match_token(p,d,closeBracketToken);

	arrayLiteralNode *anode = arena_malloc(parser_arena, sizeof(arrayLiteralNode));
	*anode = (arrayLiteralNode){ openBracket, valueStorage, valueCount, closeBracket };

	return (node) { arrayLiteral, textspan_from_bounds(&openBracket, &closeBracket), .data = anode, };
}

node parser_parse_primary_expression(parser *p, diagnosticContainer *d) {
	node current = parser_current(p, d);
	node lookahead = parser_peek(p, d, 1);

	switch (current.kind) {
	case identifierToken: {
		node n = parser_parse_symbol_reference(p,d,false);

		if (parser_current(p,d).kind == openBracketToken) {
			n = parser_parse_array_access(p, d, n);
		}

		if (isAssignmentOperator(parser_current(p,d).kind)) {
			n = parser_parse_variable_assignment(p, d, n);
		}
		return n;
	}

	case openBracketToken: return parser_parse_array_literal(p,d);
	case numberLiteral:
	case floatLiteral:
	case stringLiteral:
	case charLiteral:
	case trueKeyword:
	case falseKeyword:
		return parser_next_token(p, d);

	case openParenthesisToken: {
		node openParen = parser_next_token(p, d);
		node expr = parser_parse_expression(p, d);
		node closeParen = parser_match_token(p, d, closeParenthesisToken);

		parenthesizedExpressionNode *parenNode = arena_malloc(parser_arena, sizeof(parenthesizedExpressionNode));
		*parenNode = (parenthesizedExpressionNode){ openParen, expr, closeParen };

		return (node) { parenthesizedExpression, textspan_from_bounds(&current, &closeParen), .data = parenNode, };
	}
	default: 
		if (current.kind != badToken) report_diagnostic(d, illegalPrimaryExpressionDiagnostic, current.span, current.kind, 0, 0);
		return parser_next_token(p, d);
	}
}

int create_syntaxtree(char* text, u64 length, parser* p, diagnosticContainer* d)  {

	p->lexer = (lexer){ .text = text, .text_length = length, .index = 0 };

	string_arena = arena_create();
	if (string_arena == NULL) panic("memory allocation for string_arena failed\n");

	//printf("Input: %s\n", text);
	{
		benchmark_start();
		parser_parse(p, d);
		benchmark_end("Parsing");
	}

	return d->index==0;
}
