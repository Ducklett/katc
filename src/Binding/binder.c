astNode bind_expression(node *n, ast *tree);
astNode bind_expression_internal(node *n, ast* tree);
astNode bind_expression_of_type(node *n, ast *tree, enum astType expectedType, textspan errorSpan);
astNode bind_block_statement(node *n, ast *tree);
astNode bind_if_statement(node *n, ast *tree);
astNode bind_case_branch(node *n, ast *tree);
astNode bind_case_statement(node *n, ast *tree);
astNode bind_while_loop(node *n, ast *tree);
astNode bind_for_loop(node *n, ast *tree);
astNode bind_unary_expression(node *n, ast *tree);
astNode bind_binary_expression(node *n, ast *tree);
astNode bind_call_expression(node *n, ast *tree);
astNode bind_variable_declaration(node *n, ast *tree);
astNode bind_variable_assignment(node *n, ast *tree);
static inline int push_scope(ast *tree);
static inline void pop_scope(ast *tree, int parentScopeIndex);
variableSymbol* declare_variable(ast *tree, textspan nameSpan, enum astType variableType, u8 flags);
variableSymbol* find_variable_in_scope(textspan nameSpan, ast *tree);
astNode fold_binary_expression(typedOperator *op, int left, int right);
astNode fold_unary_expression(enum astUnaryOperator op, astNode *boundOperand);
astNode fold_cast_expression(enum astType from, enum astType to, i64 value);
bool check_bounds(astNode n, diagnosticContainer *d, textspan span);
astNode cast_expression(node *n, ast *tree, enum astType toType, bool isExplicit);

int bind_tree(ast* tree) {

	benchmark_start();
	tree->root = bind_expression(&tree->parser.root, tree);
	benchmark_end("Binding");
	return tree->diagnostics.index == 0;
}

astNode bind_expression_of_type(node *n, ast* tree, enum astType expectedType, textspan errorSpan) {

	astNode outNode = cast_expression(n, tree, expectedType, /*isExplicit:*/ false);

	if (outNode.type != errorType && outNode.kind == literalKind) {
		if (!check_bounds(outNode, &tree->diagnostics, n->span)) {
			outNode.type = errorType;
		}
	}

	return outNode;
}

astNode bind_expression(node *n, ast* tree) { return bind_expression_of_type(n, tree, 0, n->span); }
astNode bind_expression_internal(node *n, ast* tree) {
	switch(n->kind) {
		case fileStatement:
		case blockStatement: return bind_block_statement(n, tree);
		case ifStatement: return bind_if_statement(n, tree);
		case caseStatement: return bind_case_statement(n, tree);
		case whileLoop: return bind_while_loop(n, tree);
		case forLoop: return bind_for_loop(n, tree);

		case falseKeyword:
		case trueKeyword: return (astNode){ literalKind, boolType, .boolValue = n->boolValue };

		case numberLiteral: return (astNode){ literalKind, intType, .numValue = n->numValue };
		case stringLiteral: return (astNode){ literalKind, stringType, .stringValue = n->stringValue };
		case charLiteral: return (astNode){ literalKind, charType, .charValue = n->charValue };
		
		case parenthesizedExpression: return bind_expression(&((parenthesizedExpressionNode*)n->data)->expression, tree);

		case identifierToken: {
			variableSymbol *variable = find_variable_in_scope(n->span, tree);

			bool hasErrors = variable == 0;

			if (feature_constantfolding &&
				!hasErrors &&
				!(variable->flags & VARIABLE_MUTABLE) &&
				(variable->flags & VARIABLE_VALUE_KNOWN)) {

				return (astNode){ literalKind, variable->type, .numValue = variable->numValue };
			}

			if (!hasErrors &&  !(variable->flags & VARIABLE_INITIALIZED)) {
				hasErrors=true;
				report_diagnostic(&tree->diagnostics, variableNotInitializedDiagnostic, n->span, (u64)variable->name, 0, 0);
			}

			return (astNode){ variableReferenceKind, hasErrors ? errorType : variable->type, .data = variable };
		}

		case unaryExpression: return bind_unary_expression(n, tree);
		case binaryExpression: return bind_binary_expression(n, tree);
		case callExpression: return bind_call_expression(n, tree);
		case variableDeclaration: return bind_variable_declaration(n, tree);
		case variableAssignment: return bind_variable_assignment(n, tree);

		default: {
			fprintf(stderr, "%sUnhandled node of type %s in binder%s", TERMRED, syntaxKindText[n->kind], TERMRESET);
			exit(1);
		}
	}
}

astNode bind_block_statement(node *n, ast *tree) {
	astNode *boundStatements = NULL;

	blockStatementNode bn = *(blockStatementNode*)n->data;

	int parentScopeIndex = push_scope(tree);

	for (int i = 0; i < bn.statementsCount; i++) {
		sb_push(boundStatements, bind_expression(&bn.statements[i], tree));
	}
	
	pop_scope(tree, parentScopeIndex);

	astNode* nodesStart = &tree->nodes[tree->nodesIndex];
	for (int i = 0; i < sb_count(boundStatements); i++) {
		tree->nodes[tree->nodesIndex++] = boundStatements[i];
	}

	int index = tree->blockStatementsIndex;
	tree->blockStatements[tree->blockStatementsIndex++] =
		(blockStatementAst){ nodesStart, sb_count(boundStatements) };

	sb_free(boundStatements);

	return (astNode){ n->kind == blockStatement ? blockStatementKind : fileStatementKind, voidType, .data = &tree->blockStatements[index] };
}

astNode bind_if_statement(node *n, ast *tree) {

	ifStatementNode in = *(ifStatementNode*)n->data;

	astNode boundCondition = bind_expression_of_type(&in.condition, tree, boolType, in.condition.span);
	astNode boundthen = bind_expression(&in.thenExpression, tree);
	astNode boundElse = in.elseExpression.kind == 0
		? (astNode){0}
		: bind_expression(&in.elseExpression, tree);

	int index = tree->ifStatementsIndex;
	tree->ifStatements[tree->ifStatementsIndex++] =
		(ifStatementAst){ boundCondition, boundthen, boundElse }; 

	enum astType ifType = boundthen.type == boundElse.type ? boundthen.type : voidType;

	return (astNode){ ifStatementKind, ifType, .data = &tree->ifStatements[index] };
}

astNode bind_case_branch(node *n, ast *tree) {
	caseBranchNode cn = *(caseBranchNode*)n->data;

	astNode boundCondition = cn.condition.kind == defaultKeyword
		? (astNode){0}
		: bind_expression_of_type(&cn.condition, tree, boolType, cn.condition.span);

	astNode boundthen = bind_expression(&cn.thenExpression, tree);

	int index = tree->caseBranchesIndex;
	tree->caseBranches[tree->caseBranchesIndex++] =
		(caseBranchAst){ boundCondition, boundthen  }; 

	return (astNode){ caseBranchKind, boundthen.type, .data = &tree->caseBranches[index] };
}

astNode bind_case_statement(node *n, ast *tree) {

	astNode *boundBranches = NULL;

	caseStatementNode cn = *(caseStatementNode*)n->data;

	int parentScopeIndex = push_scope(tree);

	enum astType caseType;

	for (int i = 0; i < cn.branchCount; i++) {
		sb_push(boundBranches, bind_case_branch(&cn.branches[i], tree));
		if (i == 0) caseType = boundBranches[i].type;
		else if (caseType != boundBranches[i].type) caseType = voidType;
	}
	
	pop_scope(tree, parentScopeIndex);

	if (sb_count(boundBranches) == 0) {
		report_diagnostic(&tree->diagnostics, emptyCaseStatementDiagnostic, n->span, 0, 0, 0);
	}

	astNode* nodesStart = &tree->nodes[tree->nodesIndex];
	for (int i = 0; i < sb_count(boundBranches); i++) {
		tree->nodes[tree->nodesIndex++] = boundBranches[i];
	}

	int index = tree->caseStatementsIndex;
	tree->caseStatements[tree->caseStatementsIndex++] =
		(caseStatementAst){ nodesStart, sb_count(boundBranches) };
	
	sb_free(boundBranches);

	return (astNode){ caseStatementKind, caseType, .data = &tree->caseStatements[index], };
}

astNode bind_while_loop(node *n, ast *tree) {

	whileLoopNode wn = *(whileLoopNode*)n->data;

	astNode boundCondition = bind_expression_of_type(&wn.condition, tree, boolType, wn.condition.span);
	astNode boundBlock = bind_expression(&wn.block, tree);

	int index = tree->whileLoopIndex;
	tree->whileLoops[tree->whileLoopIndex++] = 
		(whileLoopAst){ boundCondition, boundBlock };

	return (astNode){ whileLoopKind, voidType, .data = &tree->whileLoops[index] };
}

astNode bind_range_expression(node *n, ast *tree) {
	rangeExpressionNode rn = *(rangeExpressionNode*)n->data;

	astNode from = bind_expression(&rn.start, tree);

	enum astType type = from.type;

	if (from.kind != literalKind) {
		report_diagnostic(&tree->diagnostics, nonConstantDiagnostic, rn.start.span, 0, 0, 0);
	}


	if (!isNumberType(type) && type != charType ) {
		report_diagnostic(&tree->diagnostics, illegalRangeDiagnostic, rn.start.span, type, 0, 0);
	}

	astNode to = bind_expression_of_type(&rn.end, tree, type, rn.end.span);

	if (to.kind != literalKind) {
		report_diagnostic(&tree->diagnostics, nonConstantDiagnostic, rn.end.span, 0, 0, 0);
	}

	int index = tree->rangeIndex;

	if (type == intType) {
		tree->ranges[tree->rangeIndex++] = 
		 	(rangeExpressionAst){ .fromInt = from.numValue, .toInt = to.numValue };
	} else {
		tree->ranges[tree->rangeIndex++] = 
		 	(rangeExpressionAst){ .fromChar = from.charValue, .toChar = to.charValue };
	}

	return (astNode){ rangeExpressionKind, type, .data = &tree->ranges[index] };
}

astNode bind_for_loop(node *n, ast *tree) {

	forLoopNode fn = *(forLoopNode*)n->data;

	astNode range = bind_range_expression(&fn.range, tree);

	u8 flags = VARIABLE_MUTABLE | VARIABLE_INITIALIZED;
	variableSymbol *valueVar = declare_variable(tree, fn.value.span, range.type, flags);
	variableSymbol *keyVar = fn.key.kind == 0 ? 0 : declare_variable(tree, fn.key.span, intType, flags);
	astNode boundBlock = bind_expression(&fn.block, tree);

	int index = tree->forLoopIndex;
	tree->forLoops[tree->forLoopIndex++] = 
		 (forLoopAst){ valueVar, keyVar, range, boundBlock };

	return (astNode){ forLoopKind, voidType, .data = &tree->forLoops[index] };
}

astNode bind_unary_expression(node *n, ast *tree) {
	unaryExpressionNode un = *(unaryExpressionNode*)n->data;

	astNode boundOperand = bind_expression(&un.operand, tree);

	bool hasErrors = boundOperand.type == errorType || boundOperand.type == unresolvedType;

	if (un.operator.kind == plusPlusOperator || un.operator.kind == minusMinusOperator) {
		if (boundOperand.kind != variableReferenceKind) {
			hasErrors=true;
			report_diagnostic(&tree->diagnostics, illegalIncrementOrDecrementDiagnostic, un.operand.span, 0, 0, 0);
		}
	}

	enum astUnaryOperator op = get_unary_operator(un.operator.kind, boundOperand.type, un.left);

	if (!op) {
		hasErrors=true;
		report_diagnostic(&tree->diagnostics, undefinedUnaryOperatorDiagnostic, un.operator.span, un.operator.kind, boundOperand.type, 0);
	}

	if (!hasErrors && feature_constantfolding && boundOperand.kind == literalKind) {
		return fold_unary_expression(op, &boundOperand);
	}

	if (op == identityOp) return boundOperand;

	int index = tree->unaryExpressionsIndex;
	tree->unaryExpressions[tree->unaryExpressionsIndex++] =
		(unaryExpressionAst){ op, boundOperand };

	return (astNode){ unaryExpressionKind , boundOperand.type, .data = &tree->unaryExpressions[index] };
}

astNode bind_binary_expression(node *n, ast *tree) {
	binaryExpressionNode bn = *(binaryExpressionNode*)n->data;

	astNode boundLeft = bind_expression(&bn.left, tree);
	astNode boundRight = bind_expression(&bn.right, tree);

	typedOperator op = get_binary_operator(bn.operator.kind, boundLeft.type, boundRight.type);

	// silence errors if the problem lies elsewhere
	bool hasErrors = boundLeft.type == errorType || boundLeft.type == unresolvedType ||
					boundRight.type == errorType || boundRight.type == unresolvedType;

	if (!op.operator && !hasErrors) {
		report_diagnostic(&tree->diagnostics, undefinedBinaryOperatorDiagnostic, n->span, bn.operator.kind, boundLeft.type, boundRight.type);
		hasErrors = true;
	}

	if (!hasErrors && feature_constantfolding && boundLeft.kind == literalKind && boundRight.kind == literalKind) {
		return fold_binary_expression(&op, boundLeft.numValue, boundRight.numValue);
	}

	int index = tree->binaryExpressionsIndex;
	tree->binaryExpressions[tree->binaryExpressionsIndex++] =
		(binaryExpressionAst){ op.operator, boundLeft, boundRight };

	return (astNode){ binaryExpressionKind, hasErrors ? errorType : op.type, .data = &tree->binaryExpressions[index] };
}

astNode bind_call_expression(node *n, ast *tree) {
	functionCallNode cn = *(functionCallNode*)n->data;

	u8 cast = 0;
	for (int i=0;i<=charType;i++) {
		if (span_compare(tree->text, cn.identifier.span, astTypeText[i])) {
			cast = i;
			break;
		}
	}

	if (cast > 2) {
		if (cn.argumentCount != 1) {
			textspan errSpan = cn.argumentCount == 0
				?  textspan_from_bounds(&cn.openParen, &cn.closeParen)
				:  textspan_from_bounds(&cn.arguments[1], &cn.arguments[cn.argumentCount-1]);

			report_diagnostic(&tree->diagnostics, oneArgumentCastDiagnostic, errSpan, 0, 0, 0);
			return (astNode){ castExpressionKind, errorType, .data = 0 };
		}

		return cast_expression(&cn.arguments[0], tree, cast, true);
	}

	if (!span_compare(tree->text, cn.identifier.span, "print")) {
		fprintf(stderr, "only print function supported currently\n");
		exit(1);
	}

	astNode *arguments = NULL;

	// skip the comma tokens
	for (int i=0;i<cn.argumentCount;i+=2) {
		if (i == 0) {
			sb_push(arguments, bind_expression_of_type(&cn.arguments[i], tree, stringType, cn.arguments[i].span));
		} else {
			sb_push(arguments, bind_expression(&cn.arguments[i],tree));
		}
	}

	astNode* nodesStart = &tree->nodes[tree->nodesIndex];
	for (int i = 0; i < sb_count(arguments); i++) {
		tree->nodes[tree->nodesIndex++] = arguments[i];
	}

	int index = tree->functionCallIndex;
	tree->functionCalls[tree->functionCallIndex++] =
		 (callExpressionAst){ nodesStart, sb_count(arguments) };

	sb_free(arguments);

	return (astNode){ callExpressionKind , voidType, .data = &tree->functionCalls[index] };
}

astNode cast_expression(node *n, ast *tree, enum astType toType, bool isExplicit) {
	astNode bn = bind_expression_internal(n, tree);

	bool hasErrors = bn.type == errorType;

	u8 castType = getCastInformation(bn.type, toType);

	if (hasErrors || castType == CAST_IDENTITY || toType <= 2) return bn;

	if (!hasErrors && castType == CAST_ILLEGAL) {
		hasErrors = true;
		report_diagnostic(&tree->diagnostics, illegalCastDiagnostic, n->span, bn.type, toType, 0);
	}

	if (!hasErrors && castType == CAST_EXPLICIT && !isExplicit) {
		hasErrors = true;
		report_diagnostic(&tree->diagnostics, illegalImplicitCastDiagnostic, n->span, bn.type, toType, 0);
	}

	if (hasErrors) return (astNode){ castExpressionKind, errorType, .data = 0 };

	if (feature_constantfolding && bn.kind == literalKind) {
		if (isExplicit) return fold_cast_expression(bn.type, toType, bn.numValue);
		// implicit casts should keep their literal value so they can be bounds checked
		bn.type = toType;
		return bn;
	}

	tree->nodes[tree->nodesIndex++] = bn;

	return (astNode){ castExpressionKind, toType, .data = &tree->nodes[tree->nodesIndex-1] };
}

astNode bind_variable_declaration(node *n, ast *tree) {

	variableDeclarationNode vn = *(variableDeclarationNode*)n->data;

	astNode boundInitializer = {0};
	bool hasInitializer = vn.expression.kind != 0;

	u8 flags = vn.mutabilityIndicator.kind == equalsToken || !hasInitializer
		? VARIABLE_MUTABLE
		: 0;

	enum astType type =  0;
	if (vn.type.kind != 0) type = resolve_type_from_span(tree, vn.type.span);

	if (hasInitializer) {
		boundInitializer = type != 0
			? bind_expression_of_type(&vn.expression, tree, type, vn.expression.span)
			: bind_expression(&vn.expression, tree);

		if (type == 0) type = boundInitializer.type;

		flags |= VARIABLE_INITIALIZED;
	} 

	variableSymbol *variable = declare_variable(tree, vn.identifier.span, type, flags);

	if (variable != 0 && boundInitializer.kind == literalKind) {
		variable->flags |= VARIABLE_VALUE_KNOWN;
		variable->numValue = boundInitializer.numValue;
	}

	int index = tree->variableDeclarationIndex;
	tree->variableDeclarations[tree->variableDeclarationIndex++] =
		(variableDeclarationAst){ variable, boundInitializer };

	return (astNode){ variableDeclarationKind , type, .data = &tree->variableDeclarations[index] };
}

astNode bind_variable_assignment(node *n, ast *tree) {

	variableAssignmentNode an = *(variableAssignmentNode*)n->data;
	enum syntaxKind opKind = getBinaryOperatorFromAssignmentOperator(an.assignmentOperator.kind);

	astNode boundExpression = bind_expression(&an.expression, tree);

	variableSymbol *variable = find_variable_in_scope(an.identifier.span, tree);

	bool hasErrors = variable == 0 || boundExpression.type == errorType || boundExpression.type == unresolvedType;

	typedOperator op = {0};
	if (opKind != 0 && !hasErrors) {
		op = get_binary_operator(opKind, variable->type, boundExpression.type);
	}

	if (opKind != 0 && !op.operator) {
		report_diagnostic(&tree->diagnostics, undefinedBinaryOperatorDiagnostic, n->span, opKind, variable->type, boundExpression.type);
		hasErrors = true;
	}

	if (!hasErrors) {
		if (opKind == 0 && variable->type != boundExpression.type) {
			report_diagnostic(&tree->diagnostics, cannotAssignDiagnostic, an.identifier.span, variable->type, boundExpression.type, 0);
		} else if (!(variable->flags & VARIABLE_MUTABLE)) {
			report_diagnostic(&tree->diagnostics, cannotAssignConstantDiagnostic, an.expression.span, (u64)&an.identifier.span, 0, 0);
		} else if (!(variable->flags & VARIABLE_INITIALIZED)) {
			variable->flags |= VARIABLE_INITIALIZED;
		}
	}

	int index = tree->variableAssignmentIndex;
	tree->variableAssignments[tree->variableAssignmentIndex++] =
		(variableAssignmentAst){ variable, boundExpression, op.operator };

	return (astNode){ variableAssignmentKind, variable == 0 ? errorType : variable->type, .data = &tree->variableAssignments[index] };
}

static inline int push_scope(ast *tree) {
	int parentScopeIndex = tree->currentScopeIndex;
	tree->currentScopeIndex = sb_count(tree->scopes);

	scope*  newScope = arena_malloc(binder_arena, sizeof(scope));
	sb_push(tree->scopes, newScope);

	if (parentScopeIndex != tree->currentScopeIndex) {
		newScope->parentScope = tree->scopes[parentScopeIndex];
	} 
	return parentScopeIndex;
}

static inline void pop_scope(ast *tree, int parentScopeIndex) {
	tree->currentScopeIndex = parentScopeIndex;
}


variableSymbol* declare_variable(ast *tree, textspan nameSpan, enum astType variableType, u8 flags) {
	if (variableType == voidType) {
		report_diagnostic(&tree->diagnostics, variableCannotBeVoidDiagnostic, nameSpan, 0, 0, 0);
		return 0;
	}

	scope *currentScope = tree->scopes[tree->currentScopeIndex];

	for (int i = 0; i < sb_count(currentScope->variables); i++) {
		if (span_compare(tree->text, nameSpan, currentScope->variables[i]->name)) {
			report_diagnostic(&tree->diagnostics, redeclarationOfVariableDiagnostic, nameSpan, (u64)currentScope->variables[i]->name, 0, 0);
			return 0;
		}
	}

	variableSymbol *variable = arena_malloc(binder_arena, sizeof(variableSymbol));
	sb_push(currentScope->variables, variable);

	variable->name = ast_substring(tree->text, nameSpan, string_arena);
	variable->type = variableType;
	variable->flags = flags;

	return variable;
}

variableSymbol* find_variable_in_scope_internal(textspan nameSpan, ast *tree, scope *currentScope);
variableSymbol* find_variable_in_scope(textspan nameSpan, ast *tree) {
	return find_variable_in_scope_internal(nameSpan, tree, tree->scopes[tree->currentScopeIndex]);
}
variableSymbol* find_variable_in_scope_internal(textspan nameSpan, ast *tree, scope *currentScope) {

	for (int i = 0; i < sb_count(currentScope->variables); i++) {
		if (span_compare(tree->text, nameSpan, currentScope->variables[i]->name)) {
			return currentScope->variables[i];
		}
	}

	if (currentScope->parentScope == 0) {
		report_diagnostic(&tree->diagnostics, referenceToUndefinedVariableDiagnostic, nameSpan, 0, 0, 0);
		return 0;
	}

	return find_variable_in_scope_internal(nameSpan, tree, currentScope->parentScope);
}

astNode fold_binary_expression(typedOperator *op, int left, int right) {
	int v;
	switch(op->operator) {
		case addOp:            v = left +  right; break; 
		case subtractOp:       v = left -  right; break; 
		case multiplyOp:       v = left *  right; break;
		case divideOp:         v = left /  right; break;
		case moduloOp:         v = left %  right; break;

		case equalOp:          v = left == right; break;
		case inEqualOp:        v = left != right; break;
		case lessOp:           v = left <  right; break;
		case greaterOp:        v = left >  right; break;
		case lessOrEqualOp:    v = left <= right; break;  
		case greaterOrEqualOp: v = left >= right; break;     

		case shiftLeftOp:      v = left << right; break;
		case shiftRightOp:     v = left >> right; break;
		case bitwiseAndOp:     v = left &  right; break;
		case bitwiseXorOp:     v = left ^  right; break;
		case bitwiseOrOp:      v = left |  right; break;

		case logicalAndOp:     v = left && right; break;
		case logicalOrOp:      v = left || right; break;
		default:
			fprintf(stderr, "%sUnhandled binary operator of type %s in binder%s", TERMRED, astBinaryText[op->operator], TERMRESET);
			exit(1);
	}

	return (astNode){ literalKind, op->type, .numValue = v };
}

astNode fold_unary_expression(enum astUnaryOperator op, astNode *boundOperand) {
	int v;
	int operand = boundOperand->numValue;
	switch(op) {
	case logicalNegationOp: v = !operand; break;
	case bitwiseNegationOp: v = ~operand; break;
	case negationOp:        v = -operand; break;
	case identityOp:        v =  operand; break;
	default:
		fprintf(stderr, "%sUnhandled unary operator of type %s in binder%s", TERMRED, astUnaryText[op], TERMRESET);
		exit(1);
	}
	return (astNode){ literalKind , boundOperand->type, .numValue = v };
}

astNode fold_cast_expression(enum astType from, enum astType to, i64 value) {
	switch (to) {
	case intType : value = (int )value; break;
	case u8Type  : value = (u8  )value; break;
	case u16Type : value = (u16 )value; break;
	case u32Type : value = (u32 )value; break;
	case u64Type : value = (u64 )value; break;
	case i8Type  : value = (i8  )value; break;
	case i16Type : value = (i16 )value; break;
	case i32Type : value = (i32 )value; break;
	case i64Type : value = (i64 )value; break;
	case boolType: value = (bool)value; break;
	case charType: value = (char)value; break;
	default:
		fprintf(stderr, "%sUnhandled type %s in fold_cast_expression%s", TERMRED, astTypeText[to], TERMRESET);
		exit(1);
	}

	return (astNode){ literalKind, to, .numValue = value };
}

bool check_bounds(astNode n, diagnosticContainer *d, textspan span) {
	if (n.kind != literalKind) {
		fprintf(stderr, "%scheck_bounds called on non-literal%s\n", TERMRED, TERMRESET);
		exit(1);
	}

	i64 value = n.numValue;
	bool errored=false;
	bool overflow=false;

	switch(n.type) {
		case charType: {
			if (value > CHAR_MAX ) { errored=true; overflow=true; }
			else if (value < CHAR_MIN) { errored=true; } } break;
		case intType: {
			if (value > INT_MAX ) { errored=true; overflow=true; }
			else if (value < INT_MIN) { errored=true; } } break;
		case i8Type: {
			if (value > I8_MAX ) { errored=true; overflow=true; }
			else if (value < I8_MIN) { errored=true; } } break;
		case i16Type: {
			if (value > I16_MAX ) { errored=true; overflow=true; }
			else if (value < I16_MIN) { errored=true; } } break;
		case i32Type: {
			if (value > I32_MAX ) { errored=true; overflow=true; }
			else if (value < I32_MIN) { errored=true; } } break;
		case u8Type: {
			if (value > U8_MAX ) { errored=true; overflow=true; }
			else if (value < U8_MIN) { errored=true; } } break;
		case u16Type: {
			if (value > U16_MAX ) { errored=true; overflow=true; }
			else if (value < U16_MIN) { errored=true; } } break;
		case u32Type: {
			if (value > U32_MAX ) { errored=true; overflow=true; }
			else if (value < U32_MIN) { errored=true; } } break;

		case i64Type:
		case u64Type:
		case stringType:
		case boolType: break;

		default:
			fprintf(stderr, "%sUnhandled type %s in check_bounds%s", TERMRED, astTypeText[n.type], TERMRESET);
			exit(1);
	}

	if (errored) {
		report_diagnostic(d, valueOutOfBoundsDiagnostic, span, n.type, overflow, 0);
	}

	return !errored;
}
