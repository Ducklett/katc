astNode bind_expression(node *n, ast *tree);
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

int bind_tree(ast* tree) {

	benchmark_start();
	tree->root = bind_expression(&tree->parser.root, tree);
	benchmark_end("Binding");
	return tree->diagnostics.index == 0;
}

astNode bind_expression_of_type(node *n, ast* tree, enum astType expectedType, textspan errorSpan) {
	astNode outNode = bind_expression(n, tree);

	if (expectedType != 0 && outNode.type != errorType && expectedType != outNode.type) {
		// TODO: proper casting
		if ((isNumberType(expectedType) && isNumberType(outNode.type))) {
			outNode.type = expectedType;
		} else {
			report_diagnostic(&tree->diagnostics, cannotConvertDiagnostic, errorSpan, outNode.type, expectedType, 0);
		}
	}

	return outNode;
}

astNode bind_expression(node *n, ast* tree) {
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
			if (feature_constantfolding &&
				variable != 0 &&
				!(variable->flags & VARIABLE_MUTABLE) &&
				(variable->flags & VARIABLE_VALUE_KNOWN)) {

				return (astNode){ literalKind, variable->type, .numValue = variable->numValue };
			}
			return (astNode){ variableReferenceKind, variable == 0 ? errorType : variable->type, .data = variable };
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
	astNode boundStatements[100];
	int statementCount = 0;

	blockStatementNode bn = *(blockStatementNode*)n->data;

	int parentScopeIndex = push_scope(tree);

	for (int i = 0; i < bn.statementsCount; i++) {
		boundStatements[statementCount++] = bind_expression(&bn.statements[i], tree);
	}
	
	pop_scope(tree, parentScopeIndex);

	astNode* nodesStart = &tree->nodes[tree->nodesIndex];
	for (int i = 0; i < statementCount; i++) {
		tree->nodes[tree->nodesIndex++] = boundStatements[i];
	}

	int index = tree->blockStatementsIndex;
	tree->blockStatements[tree->blockStatementsIndex++] =
		(blockStatementAst){ nodesStart, statementCount };

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

	astNode boundBranches[100];
	int branchCount = 0;

	caseStatementNode cn = *(caseStatementNode*)n->data;

	int parentScopeIndex = push_scope(tree);

	enum astType caseType;

	for (int i = 0; i < cn.branchCount; i++) {
		boundBranches[branchCount++] = bind_case_branch(&cn.branches[i], tree);
		if (i == 0) caseType = boundBranches[i].type;
		else if (caseType != boundBranches[i].type) caseType = voidType;
	}
	
	pop_scope(tree, parentScopeIndex);

	if (branchCount == 0) {
		report_diagnostic(&tree->diagnostics, emptyCaseStatementDiagnostic, n->span, 0, 0, 0);
	}

	astNode* nodesStart = &tree->nodes[tree->nodesIndex];
	for (int i = 0; i < branchCount; i++) {
		tree->nodes[tree->nodesIndex++] = boundBranches[i];
	}

	int index = tree->caseStatementsIndex;
	tree->caseStatements[tree->caseStatementsIndex++] =
		(caseStatementAst){ nodesStart, branchCount };

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

	if (!span_compare(tree->text, cn.identifier.span, "print")) {
		fprintf(stderr, "only print function supported currently\n");
		exit(1);
	}

	astNode arguments[128];
	u8 argumentCount=0;
	// skip the comma tokens
	for (int i=0;i<cn.argumentCount;i+=2) {
		if (i == 0) {
			arguments[argumentCount++] = bind_expression_of_type(&cn.arguments[i], tree, stringType, cn.arguments[i].span);
		} else {
			arguments[argumentCount++] = bind_expression(&cn.arguments[i],tree);
		}
	}

	astNode* nodesStart = &tree->nodes[tree->nodesIndex];
	for (int i = 0; i < argumentCount; i++) {
		tree->nodes[tree->nodesIndex++] = arguments[i];
	}

	int index = tree->functionCallIndex;
	tree->functionCalls[tree->functionCallIndex++] =
		 (callExpressionAst){ nodesStart, argumentCount };

	return (astNode){ callExpressionKind , voidType, .data = &tree->functionCalls[index] };
}

astNode bind_variable_declaration(node *n, ast *tree) {

	variableDeclarationNode vn = *(variableDeclarationNode*)n->data;

	astNode boundInitializer = vn.type.kind != 0
		? bind_expression_of_type(&vn.expression, tree, resolve_type_from_span(tree, vn.type.span), vn.expression.span)
		: bind_expression(&vn.expression, tree);

	u8 flags = VARIABLE_INITIALIZED;
	if (vn.mutabilityIndicator.kind == equalsToken) flags |= VARIABLE_MUTABLE;

	variableSymbol *variable = declare_variable(tree, vn.identifier.span, boundInitializer.type, flags);

	if (variable != 0 && boundInitializer.kind == literalKind) {
		variable->flags |= VARIABLE_VALUE_KNOWN;
		variable->numValue = boundInitializer.numValue;
	}

	int index = tree->variableDeclarationIndex;
	tree->variableDeclarations[tree->variableDeclarationIndex++] =
		(variableDeclarationAst){ variable, boundInitializer };

	return (astNode){ variableDeclarationKind , boundInitializer.type, .data = &tree->variableDeclarations[index] };
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

	if (!op.operator) {
		report_diagnostic(&tree->diagnostics, undefinedBinaryOperatorDiagnostic, n->span, opKind, variable->type, boundExpression.type);
		hasErrors = true;
	}

	if (!hasErrors) {
		if (opKind == 0 && variable->type != boundExpression.type) {
			report_diagnostic(&tree->diagnostics, cannotAssignDiagnostic, an.identifier.span, variable->type, boundExpression.type, 0);
		} else if (!(variable->flags & VARIABLE_MUTABLE)) {
			report_diagnostic(&tree->diagnostics, cannotAssignConstantDiagnostic, an.expression.span, (u64)&an.identifier.span, 0, 0);
		}
	}

	int index = tree->variableAssignmentIndex;
	tree->variableAssignments[tree->variableAssignmentIndex++] =
		(variableAssignmentAst){ variable, boundExpression, op.operator };

	return (astNode){ variableAssignmentKind, variable == 0 ? errorType : variable->type, .data = &tree->variableAssignments[index] };
}

static inline int push_scope(ast *tree) {
	int parentScopeIndex = tree->currentScopeIndex;
	tree->currentScopeIndex = tree->scopesIndex;
	scope*  newScope = &tree->scopes[tree->scopesIndex++];

	if (parentScopeIndex != tree->currentScopeIndex) {
		newScope->parentScope = &tree->scopes[parentScopeIndex];
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

	scope *currentScope = &tree->scopes[tree->currentScopeIndex];

	for (int i = 0; i < currentScope->variableCount; i++) {
		if (span_compare(tree->text, nameSpan, currentScope->variables[i].name)) {
			report_diagnostic(&tree->diagnostics, redeclarationOfVariableDiagnostic, nameSpan, (u64)currentScope->variables[i].name, 0, 0);
			return 0;
		}
	}

	variableSymbol *variable = &currentScope->variables[currentScope->variableCount++];

	for (int i= 0;i<nameSpan.length;i++) {
			variable->name[i] = tree->text[i+nameSpan.start];
	}
	variable->type = variableType;
	variable->flags = flags;

	return variable;
}

variableSymbol* find_variable_in_scope_internal(textspan nameSpan, ast *tree, scope *currentScope);
variableSymbol* find_variable_in_scope(textspan nameSpan, ast *tree) {
	return find_variable_in_scope_internal(nameSpan, tree, &tree->scopes[tree->currentScopeIndex]);
}
variableSymbol* find_variable_in_scope_internal(textspan nameSpan, ast *tree, scope *currentScope) {

	for (int i = 0; i < currentScope->variableCount; i++) {
		if (span_compare(tree->text, nameSpan, currentScope->variables[i].name)) {
			return &currentScope->variables[i];
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
