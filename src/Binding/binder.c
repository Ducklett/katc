
#define FLAG_NONE 0
#define FLAG_RECURSE 1
#define FLAG_THROW 2

astNode bind_expression(node *n, ast *tree);
astNode bind_expression_internal(node *n, ast* tree);
astNode bind_expression_of_type(node *n, ast *tree, astType expectedType, textspan errorSpan);
astNode bind_variable_reference(node *n, ast *tree, scope *variableScope);
astNode bind_block_statement(node *n, ast *tree, scope *withScope);
astNode bind_if_statement(node *n, ast *tree);
astNode bind_case_branch(node *n, ast *tree);
astNode bind_case_statement(node *n, ast *tree);
astNode bind_switch_branch(node *n, ast *tree, astType caseType);
astNode bind_switch_statement(node *n, ast *tree);
astNode bind_range_expression(node *n, ast *tree);
astNode bind_while_loop(node *n, ast *tree);
astNode bind_for_loop(node *n, ast *tree);
astNode bind_function_declaration(node *n, ast *tree);
astNode bind_namespace_declaration(node *n, ast *tree);
astNode bind_enum_declaration(node *n, ast *tree);
astNode bind_unary_expression(node *n, ast *tree);
astNode bind_binary_expression(node *n, ast *tree);
astNode bind_call_expression(node *n, ast *tree, scope *functionScope);
astNode bind_variable_declaration(node *n, ast *tree);
astNode bind_variable_assignment(node *n, ast *tree);
scope* create_scope(ast *tree);
scope* set_current_scope(ast *tree, scope* s);
scope* push_scope(ast *tree);
scope* push_scope_and_get_reference(ast *tree, scope **outScope);
void pop_scope(ast *tree, scope* parentScope);
void declare_builtin_function(ast *tree, char* name);
astSymbol* find_symbol_in_scope_internal(textspan nameSpan, ast *tree, scope *currentScope, u8 symbolKind, u8 flags);
astNode resolve_symbol_reference(node *n, ast *tree);
astSymbol* declare_namespace(ast *tree, node *n, u8 flags);
astSymbol* declare_function(ast *tree, textspan nameSpan, u8 flags, node *body, astSymbol **parameters, u16 parameterCount, scope *functionScope);
astSymbol* declare_variable(ast *tree, textspan nameSpan, astType variableType, u8 flags);
astSymbol* declare_enum(ast *tree, textspan nameSpan, node *enums, int enumCount, u8 flags);
astSymbol* declare_enum_member(ast *tree, textspan nameSpan, astSymbol *enumSymbol, u8 value);
astSymbol* find_function_in_scope(textspan nameSpan, ast *tree, scope *currentScope, bool recurse);
astSymbol* find_variable_in_scope(textspan nameSpan, ast *tree, scope *currentScope);
astSymbol* find_enum_in_scope(node *n, ast *tree, scope *currentScope);
astSymbol* find_enum_type_in_scope(textspan nameSpan, ast *tree, scope *currentScope);
astNode fold_binary_expression(typedOperator *op, int left, int right);
astNode fold_unary_expression(enum astUnaryOperator op, astNode *boundOperand);
astNode fold_cast_expression(astType from, astType to, i64 value);
bool check_bounds(astNode n, diagnosticContainer *d, textspan span);
astNode cast_expression(node *n, ast *tree, astType toType, bool isExplicit);

int bind_tree(ast* tree, node *root) {

	benchmark_start();
	tree->root = bind_expression(root, tree);
	benchmark_end("Binding");
	return tree->diagnostics.index == 0;
}

astNode bind_expression_of_type(node *n, ast* tree, astType expectedType, textspan errorSpan) {

	astNode outNode = cast_expression(n, tree, expectedType, /*isExplicit:*/ false);

	if (outNode.type.kind != errorType && outNode.kind == literalKind) {
		if (!check_bounds(outNode, &tree->diagnostics, n->span)) {
			outNode.type = primitive_type_from_kind(errorType);
		}
	}

	return outNode;
}

astNode bind_expression(node *n, ast* tree) { return bind_expression_of_type(n, tree, (astType){0}, n->span); }
astNode bind_expression_internal(node *n, ast* tree) {
	switch(n->kind) {
		case fileStatement:
		case blockStatement: return bind_block_statement(n, tree, NULL);
		case ifStatement: return bind_if_statement(n, tree);
		case caseStatement: return bind_case_statement(n, tree);
		case switchStatement: return bind_switch_statement(n, tree);
		case rangeExpression: return bind_range_expression(n, tree);
		case whileLoop: return bind_while_loop(n, tree);
		case forLoop: return bind_for_loop(n, tree);
		case functionDeclaration: return bind_function_declaration(n, tree);
		case namespaceDeclaration: return bind_namespace_declaration(n, tree);
		case enumDeclaration: return bind_enum_declaration(n, tree);

		case breakKeyword: return (astNode){ breakKind, primitive_type_from_kind(voidType), .data = 0 };
		case continueKeyword: return (astNode){ continueKind, primitive_type_from_kind(voidType), .data = 0 };

		case falseKeyword:
		case trueKeyword: return (astNode){ literalKind, primitive_type_from_kind(boolType), .boolValue = n->boolValue };

		case numberLiteral: return (astNode){ literalKind, primitive_type_from_kind(intType), .numValue = n->numValue };
		case stringLiteral: return (astNode){ literalKind, primitive_type_from_kind(stringType), .stringValue = n->stringValue };
		case charLiteral: return (astNode){ literalKind, primitive_type_from_kind(charType), .charValue = n->charValue };
		
		case parenthesizedExpression: return bind_expression(&((parenthesizedExpressionNode*)n->data)->expression, tree);

		case identifierToken: return bind_variable_reference(n,tree,NULL);

		case symbolReferenceExpression: return resolve_symbol_reference(n, tree);

		case unaryExpression: return bind_unary_expression(n, tree);
		case binaryExpression: return bind_binary_expression(n, tree);
		case callExpression: return bind_call_expression(n, tree, NULL);
		case variableDeclaration: return bind_variable_declaration(n, tree);
		case variableAssignment: return bind_variable_assignment(n, tree);

		default: {
			fprintf(stderr, "%sUnhandled node of type %s in binder%s", TERMRED, syntaxKindText[n->kind], TERMRESET);
			exit(1);
		}
	}
}

astNode bind_variable_reference(node *n, ast *tree, scope *variableScope) {
	astSymbol *variable = find_variable_in_scope(n->span, tree, variableScope);

	bool hasErrors = variable == 0;

	if (feature_constantfolding &&
		!hasErrors &&
		!(variable->flags & VARIABLE_MUTABLE) &&
		(variable->flags & VARIABLE_VALUE_KNOWN)) {
		if (variable->type.kind == stringType)
			return (astNode){ literalKind, variable->type, .stringValue = variable->stringValue };
		else return (astNode){ literalKind, variable->type, .numValue = variable->numValue };
	}

	if (!hasErrors &&  !(variable->flags & VARIABLE_INITIALIZED)) {
		hasErrors=true;
		report_diagnostic(&tree->diagnostics, variableNotInitializedDiagnostic, n->span, (u64)variable->name, 0, 0);
	}

	return (astNode){ variableReferenceKind, hasErrors ? primitive_type_from_kind(errorType) : variable->type, .data = variable };
}

astNode bind_enum_reference(node *n, ast *tree, scope *enumScope) {

	astSymbol *enumRef = find_enum_in_scope(n, tree, enumScope);

	bool  hasErrors = enumRef == 0;

	return (astNode){ enumReferenceKind,  hasErrors ? primitive_type_from_kind(errorType) : enumRef->type, .numValue = hasErrors ? 0 : enumRef->numValue };
}

astNode bind_block_statement(node *n, ast *tree, scope *withScope) {
	astNode *boundStatements = NULL;

	blockStatementNode bn = *(blockStatementNode*)n->data;

	scope *parentScope = withScope == NULL ? push_scope(tree) : set_current_scope(tree, withScope);

	for (int i = 0; i < bn.statementsCount; i++) {
		sb_push(boundStatements, bind_expression(&bn.statements[i], tree));
	}
	
	pop_scope(tree, parentScope);

	u16 nodesCount = sb_count(boundStatements);
	size_t nodesSize = nodesCount * sizeof(astNode);
	astNode* nodesStorage = arena_malloc(binder_arena, nodesSize);
	memcpy(nodesStorage, boundStatements, nodesSize);

	blockStatementAst *blockNode = arena_malloc(binder_arena, sizeof(blockStatementAst));
	*blockNode = (blockStatementAst){ nodesStorage, nodesCount };

	sb_free(boundStatements);

	return (astNode){ n->kind == blockStatement ? blockStatementKind : fileStatementKind, primitive_type_from_kind(voidType), .data = blockNode };
}

astNode bind_if_statement(node *n, ast *tree) {

	ifStatementNode in = *(ifStatementNode*)n->data;

	astNode boundCondition = bind_expression_of_type(&in.condition, tree, primitive_type_from_kind(boolType), in.condition.span);
	astNode boundthen = bind_expression(&in.thenExpression, tree);
	astNode boundElse = in.elseExpression.kind == 0
		? (astNode){0}
		: bind_expression(&in.elseExpression, tree);


	ifStatementAst *ifNode = arena_malloc(binder_arena, sizeof(ifStatementAst));
	*ifNode = (ifStatementAst){ boundCondition, boundthen, boundElse }; 

	// TODO: better equality check to also accounts for enums
	astType ifType = boundthen.type.kind == boundElse.type.kind ? boundthen.type : primitive_type_from_kind(voidType);

	return (astNode){ ifStatementKind, ifType, .data = ifNode };
}

astNode bind_case_branch(node *n, ast *tree) {
	caseBranchNode cn = *(caseBranchNode*)n->data;

	astNode boundCondition = cn.condition.kind == defaultKeyword
		? (astNode){0}
		: bind_expression_of_type(&cn.condition, tree, primitive_type_from_kind(boolType), cn.condition.span);

	astNode boundthen = bind_expression(&cn.thenExpression, tree);

	caseBranchAst *branchNode = arena_malloc(binder_arena, sizeof(caseBranchAst));
	*branchNode = (caseBranchAst){ boundCondition, boundthen }; 

	return (astNode){ caseBranchKind, boundthen.type, .data = branchNode };
}

astNode bind_case_statement(node *n, ast *tree) {

	astNode *boundBranches = NULL;

	caseStatementNode cn = *(caseStatementNode*)n->data;

	scope *parentScope = push_scope(tree);

	astType caseType;

	for (int i = 0; i < cn.branchCount; i++) {
		sb_push(boundBranches, bind_case_branch(&cn.branches[i], tree));
		if (i == 0) caseType = boundBranches[i].type;
		else if (caseType.kind != boundBranches[i].type.kind) caseType = primitive_type_from_kind(voidType);
	}
	
	pop_scope(tree, parentScope);

	if (sb_count(boundBranches) == 0) {
		report_diagnostic(&tree->diagnostics, emptyCaseStatementDiagnostic, n->span, 0, 0, 0);
	}

	u16 nodesCount = sb_count(boundBranches);
	size_t nodesSize = nodesCount * sizeof(astNode);
	astNode* nodesStorage = arena_malloc(binder_arena, nodesSize);
	memcpy(nodesStorage, boundBranches, nodesSize);

	caseStatementAst *caseNode = arena_malloc(binder_arena, sizeof(caseStatementAst));
	*caseNode = (caseStatementAst){ nodesStorage, nodesCount };
	
	sb_free(boundBranches);

	return (astNode){ caseStatementKind, caseType, .data = caseNode, };
}

astNode bind_switch_branch(node *n, ast *tree, astType caseType) {
	switchBranchNode cn = *(switchBranchNode*)n->data;

	astNode boundCondition = cn.condition.kind == 0
		? (astNode){0}
		: bind_expression_of_type(&cn.condition, tree, caseType, cn.condition.span);

	if (boundCondition.kind != rangeExpressionKind && boundCondition.kind != literalKind && boundCondition.type.kind > 2) {
		report_diagnostic(&tree->diagnostics, nonConstantDiagnostic, cn.condition.span, 0, 0, 0);
		boundCondition.type = primitive_type_from_kind(errorType);
	} 

	astNode boundthen = bind_expression(&cn.thenExpression, tree);

	switchBranchAst *branchNode = arena_malloc(binder_arena, sizeof(switchBranchAst));
	*branchNode = (switchBranchAst){ boundCondition, boundthen }; 

	return (astNode){ switchBranchKind, boundthen.type, .data = branchNode };
}

astNode bind_switch_statement(node *n, ast *tree) {

	astNode *boundBranches = NULL;
	int *branchValues = NULL;

	switchStatementNode cn = *(switchStatementNode*)n->data;

	astNode boundTarget = bind_expression(&cn.targetExpression, tree);

	if (boundTarget.type.kind == stringType || boundTarget.type.kind == boolType) {
		report_diagnostic(&tree->diagnostics, invalidSwitchTypeDiagnostic, cn.targetExpression.span, boundTarget.type.kind, 0, 0);
	}

	scope *parentScope = push_scope(tree);

	astType caseType = boundTarget.type;

	for (int i = 0; i < cn.branchCount; i++) {
		astNode bn = bind_switch_branch(&cn.branches[i], tree, caseType);
		sb_push(boundBranches, bn);
		astNode bc = (*(switchBranchAst*)bn.data).condition;
		if (bc.kind == literalKind) {
			bool hasDup = false;
			for (int j=0;j<sb_count(branchValues); j++) {
				if (branchValues[j] == bc.numValue) {
					report_diagnostic(&tree->diagnostics, duplicateSwitchValueDiagnostic, cn.branches[i].span, 0, 0, 0);
					hasDup=true;
					break;
				}
			}
			if (!hasDup) sb_push(branchValues, bc.numValue);
		} else if (bc.kind == rangeExpressionKind) {
			rangeExpressionAst rn = *(rangeExpressionAst*)bc.data;
			for (int v=rn.fromInt; v <= rn.toInt; v++) {
				bool hasDup = false;
				for (int j=0;j<sb_count(branchValues); j++) {
					if (branchValues[j] == v) {
						report_diagnostic(&tree->diagnostics, duplicateSwitchValueDiagnostic, cn.branches[i].span, 0, 0, 0);
						hasDup=true;
						break;
					}
				}
				if (!hasDup) sb_push(branchValues, v);
			}
		}
	}

	pop_scope(tree, parentScope);

	if (sb_count(boundBranches) == 0) {
		report_diagnostic(&tree->diagnostics, emptyCaseStatementDiagnostic, n->span, 0, 0, 0);
	}

	u16 nodesCount = sb_count(boundBranches);
	size_t nodesSize = nodesCount * sizeof(astNode);
	astNode* nodesStorage = arena_malloc(binder_arena, nodesSize);
	memcpy(nodesStorage, boundBranches, nodesSize);

	switchStatementAst *switchNode = arena_malloc(binder_arena, sizeof(switchStatementAst));
	*switchNode = (switchStatementAst){ boundTarget, nodesStorage, nodesCount };
	
	sb_free(boundBranches);

	return (astNode){ switchStatementKind, caseType, .data = switchNode, };
}

astNode bind_while_loop(node *n, ast *tree) {

	whileLoopNode wn = *(whileLoopNode*)n->data;

	astNode boundCondition = bind_expression_of_type(&wn.condition, tree, primitive_type_from_kind(boolType), wn.condition.span);
	astNode boundBlock = bind_expression(&wn.block, tree);

	whileLoopAst *whileNode = arena_malloc(binder_arena, sizeof(whileLoopAst));
	*whileNode = (whileLoopAst){ boundCondition, boundBlock };

	return (astNode){ whileLoopKind, primitive_type_from_kind(voidType), .data = whileNode };
}

astNode bind_range_expression(node *n, ast *tree) {
	rangeExpressionNode rn = *(rangeExpressionNode*)n->data;

	astNode from = bind_expression(&rn.start, tree);

	astType type = from.type;

	if (from.kind != literalKind) {
		report_diagnostic(&tree->diagnostics, nonConstantDiagnostic, rn.start.span, 0, 0, 0);
	}


	if (!isNumberType(type.kind) && type.kind != charType ) {
		report_diagnostic(&tree->diagnostics, illegalRangeDiagnostic, rn.start.span, type.kind, 0, 0);
	}

	astNode to = bind_expression_of_type(&rn.end, tree, type, rn.end.span);

	if (to.kind != literalKind) {
		report_diagnostic(&tree->diagnostics, nonConstantDiagnostic, rn.end.span, 0, 0, 0);
	}

	rangeExpressionAst *rangeNode = arena_malloc(binder_arena, sizeof(rangeExpressionAst));

	if (type.kind == intType) {
		*rangeNode = (rangeExpressionAst){ .fromInt = from.numValue, .toInt = to.numValue };
	} else {
		*rangeNode = (rangeExpressionAst){ .fromChar = from.charValue, .toChar = to.charValue };
	}

	return (astNode){ rangeExpressionKind, type, .data = rangeNode };
}

astNode bind_for_loop(node *n, ast *tree) {

	forLoopNode fn = *(forLoopNode*)n->data;

	astNode range = bind_range_expression(&fn.range, tree);

	u8 flags = VARIABLE_MUTABLE | VARIABLE_INITIALIZED;
	astSymbol *valueVar = declare_variable(tree, fn.value.span, range.type, flags);
	astSymbol *keyVar = fn.key.kind == 0 ? 0 : declare_variable(tree, fn.key.span, primitive_type_from_kind(intType), flags);
	astNode boundBlock = bind_expression(&fn.block, tree);

	forLoopAst *forNode = arena_malloc(binder_arena, sizeof(forLoopAst));
	*forNode = (forLoopAst){ valueVar, keyVar, range, boundBlock };

	return (astNode){ forLoopKind, primitive_type_from_kind(voidType), .data = forNode };
}

astNode bind_function_declaration(node *n, ast *tree) {

	functionDeclarationNode fn = *(functionDeclarationNode*)n->data;

	u8 flags = 0;

	scope *functionScope;
	scope *parentScope = push_scope_and_get_reference(tree, &functionScope);

	astSymbol **params = NULL;

	u16 nodesCount = (fn.parameterCount+1)/2;

	bool hasErrors = false;
	astSymbol** nodesStorage = NULL;

	for (int i=0;i<fn.parameterCount;i+=2) {
		typedIdentifierNode id = *(typedIdentifierNode*)fn.parameters[i].data;
		astSymbol *param = declare_variable(tree, id.identifier.span, primitive_type_from_kind(resolve_primitive_type_from_span(tree, id.type.span)), VARIABLE_INITIALIZED);
		if (param == NULL) {
			hasErrors = true;
			goto end;
		}
		sb_push(params, param);
	}

	size_t nodesSize = nodesCount * sizeof(astSymbol*);
	nodesStorage = arena_malloc(binder_arena, nodesSize);
	memcpy(nodesStorage, params, nodesSize);

	end:;
	sb_free(params);
	pop_scope(tree, parentScope);

	astSymbol *function = declare_function(tree, fn.identifier.span, flags, hasErrors?NULL:&fn.body, nodesStorage, nodesCount, functionScope);

	return (astNode){ functionDeclarationKind , primitive_type_from_kind(voidType), .data = function };
}

astNode bind_namespace_declaration(node *n, ast *tree) {

	namespaceDeclarationNode fn = *(namespaceDeclarationNode*)n->data;

	u8 flags = 0;
	astSymbol *namespace = declare_namespace(tree, &fn.identifier, flags);

	astSymbol *parentNamespace = tree->currentNamespace;
	tree->currentNamespace = namespace;

	namespaceAst *ns = arena_malloc(binder_arena, sizeof(namespaceAst));
	*ns = (namespaceAst){ namespace, bind_block_statement(&fn.body, tree, namespace->namespaceScope) };

	tree->currentNamespace = parentNamespace;

	return (astNode){ namespaceDeclarationKind , primitive_type_from_kind(voidType), .data = ns };
}

astNode bind_enum_declaration(node *n, ast *tree) {

	enumDeclarationNode en = *(enumDeclarationNode*)n->data;

	u8 flags = 0;
	astSymbol *enumSymbol = declare_enum(tree, en.identifier.span, en.enums, en.enumCount, flags);

	return (astNode){ enumDeclarationKind , primitive_type_from_kind(voidType), .data = enumSymbol };
}

astNode bind_unary_expression(node *n, ast *tree) {
	unaryExpressionNode un = *(unaryExpressionNode*)n->data;

	astNode boundOperand = bind_expression(&un.operand, tree);

	bool hasErrors = boundOperand.type.kind == errorType || boundOperand.type.kind == unresolvedType;

	if (un.operator.kind == plusPlusOperator || un.operator.kind == minusMinusOperator) {
		if (boundOperand.kind != variableReferenceKind) {
			hasErrors=true;
			report_diagnostic(&tree->diagnostics, illegalIncrementOrDecrementDiagnostic, un.operand.span, 0, 0, 0);
		}
	}

	enum astUnaryOperator op = get_unary_operator(un.operator.kind, boundOperand.type.kind, un.left);

	if (!op) {
		hasErrors=true;
		report_diagnostic(&tree->diagnostics, undefinedUnaryOperatorDiagnostic, un.operator.span, un.operator.kind, boundOperand.type.kind, 0);
	}

	if (!hasErrors && feature_constantfolding && boundOperand.kind == literalKind) {
		return fold_unary_expression(op, &boundOperand);
	}

	if (op == identityOp) return boundOperand;

	unaryExpressionAst *unaryNode = arena_malloc(binder_arena, sizeof(unaryExpressionAst));
	*unaryNode = (unaryExpressionAst){ op, boundOperand };

	return (astNode){ unaryExpressionKind , boundOperand.type, .data = unaryNode };
}

astNode bind_binary_expression(node *n, ast *tree) {
	binaryExpressionNode bn = *(binaryExpressionNode*)n->data;

	astNode boundLeft = bind_expression(&bn.left, tree);
	astNode boundRight = bind_expression(&bn.right, tree);

	typedOperator op = get_binary_operator(bn.operator.kind, boundLeft.type, boundRight.type);

	// silence errors if the problem lies elsewhere
	bool hasErrors = boundLeft.type.kind == errorType || boundLeft.type.kind == unresolvedType ||
					boundRight.type.kind == errorType || boundRight.type.kind == unresolvedType;

	if (!op.operator && !hasErrors) {
		report_diagnostic(&tree->diagnostics, undefinedBinaryOperatorDiagnostic, n->span, bn.operator.kind, boundLeft.type.kind, boundRight.type.kind);
		hasErrors = true;
	}

	if (!hasErrors && feature_constantfolding && boundLeft.kind == literalKind && boundRight.kind == literalKind) {
		return fold_binary_expression(&op, boundLeft.numValue, boundRight.numValue);
	}

	binaryExpressionAst *binaryNode = arena_malloc(binder_arena, sizeof(binaryExpressionAst));
	*binaryNode = (binaryExpressionAst){ op.operator, boundLeft, boundRight };

	return (astNode){ binaryExpressionKind, hasErrors ? primitive_type_from_kind(errorType) : op.type, .data = binaryNode };
}

astNode bind_call_expression(node *n, ast *tree, scope *functionScope) {
	functionCallNode cn = *(functionCallNode*)n->data;

	astType castType = {0};
	bool isCast = false;
	for (int i=0;i<=charType;i++) {
		if (span_compare(tree->text, cn.identifier.span, astKindText[i])) {
			if (i>2) {
				castType = primitive_type_from_kind(i);
				break;
			}
		}
	}

	if (!isCast) {
		astSymbol *enumSymbol = find_enum_type_in_scope(cn.identifier.span, tree, functionScope == NULL ? tree->currentScope : functionScope);
		if (enumSymbol != NULL) {
			isCast = true;
			castType = enumSymbol->type;
		}
	}

	if (isCast) {
		if (cn.argumentCount != 1) {
			textspan errSpan = cn.argumentCount == 0
				?  textspan_from_bounds(&cn.openParen, &cn.closeParen)
				:  textspan_from_bounds(&cn.arguments[1], &cn.arguments[cn.argumentCount-1]);

			report_diagnostic(&tree->diagnostics, oneArgumentCastDiagnostic, errSpan, 0, 0, 0);
			return (astNode){ castExpressionKind, primitive_type_from_kind(errorType), .data = 0 };
		}

		return cast_expression(&cn.arguments[0], tree, castType, true);
	}

	astSymbol *function = functionScope == NULL
		? find_function_in_scope(cn.identifier.span, tree, tree->currentScope, true)
		: find_function_in_scope(cn.identifier.span, tree, functionScope, false);

	callExpressionAst *callNode = NULL;

	bool hasErrors = function == NULL || function->type.kind == errorType;

	if (hasErrors) goto end;

	astNode *arguments = NULL;

	u16 nodesCount = (cn.argumentCount+1)/2;

	bool isPrint = !strcmp(function->name, "print");

	functionSymbolData *fd = function->functionData;

	if (fd->parameterCount != nodesCount && !isPrint) {
		report_diagnostic(&tree->diagnostics, argCountDoensntMatchDiagnostic, cn.identifier.span, (u64)function->name, 0, 0);
		hasErrors = true;
		goto end;
	}

	// skip the comma tokens
	for (int i=0;i<cn.argumentCount;i+=2) {
		if (isPrint) {
			if (i==0) {
				sb_push(arguments, bind_expression_of_type(&cn.arguments[i], tree, primitive_type_from_kind(stringType), cn.arguments[i].span));
			} else {
				sb_push(arguments, bind_expression(&cn.arguments[i], tree));
			}
		} else {
			sb_push(arguments, bind_expression_of_type(&cn.arguments[i], tree, fd->parameters[i/2]->type, cn.arguments[i].span));
		}
	}


	size_t nodesSize = nodesCount * sizeof(astNode);
	astNode* nodesStorage = arena_malloc(binder_arena, nodesSize);
	memcpy(nodesStorage, arguments, nodesSize);

	callNode = arena_malloc(binder_arena, sizeof(callExpressionAst));
	*callNode = (callExpressionAst){ function, nodesStorage, nodesCount };

	sb_free(arguments);

	end:;

	return (astNode){ callExpressionKind , primitive_type_from_kind(voidType), .data = callNode };
}

astNode cast_expression(node *n, ast *tree, astType toType, bool isExplicit) {
	astNode bn = bind_expression_internal(n, tree);

	bool hasErrors = bn.type.kind == errorType;

	u8 castKind = getCastInformation(bn.type, toType);

	if (hasErrors || castKind == CAST_IDENTITY || toType.kind <= 2) return bn;

	if (!hasErrors && castKind == CAST_ILLEGAL) {
		hasErrors = true;
		report_diagnostic(&tree->diagnostics, illegalCastDiagnostic, n->span, bn.type.kind, toType.kind, 0);
	}

	if (!hasErrors && castKind == CAST_EXPLICIT && !isExplicit) {
		hasErrors = true;
		report_diagnostic(&tree->diagnostics, illegalImplicitCastDiagnostic, n->span, bn.type.kind, toType.kind, 0);
	}

	if (hasErrors) return (astNode){ castExpressionKind, primitive_type_from_kind(errorType), .data = 0 };

	if (feature_constantfolding && bn.kind == literalKind) {
		if (isExplicit) return fold_cast_expression(bn.type, toType, bn.numValue);
		// implicit casts should keep their literal value so they can be bounds checked
		bn.type = toType;
		return bn;
	}

	astNode *bNode = arena_malloc(binder_arena, sizeof(astNode));
	*bNode = bn;

	return (astNode){ castExpressionKind, toType, .data = bNode };
}

astNode bind_variable_declaration(node *n, ast *tree) {

	variableDeclarationNode vn = *(variableDeclarationNode*)n->data;

	astNode boundInitializer = {0};
	bool hasInitializer = vn.expression.kind != 0;

	u8 flags = vn.mutabilityIndicator.kind == equalsToken || !hasInitializer
		? VARIABLE_MUTABLE
		: 0;

	if (tree->currentNamespace == NULL || tree->currentNamespace->symbolKind == SYMBOL_NAMESPACE)
		flags |= VARIABLE_GLOBAL;

	astType type =  {0};
	if (vn.type.kind != 0) type.kind = resolve_primitive_type_from_span(tree, vn.type.span);

	if (hasInitializer) {
		boundInitializer = type.kind != 0
			? bind_expression_of_type(&vn.expression, tree, type, vn.expression.span)
			: bind_expression(&vn.expression, tree);

		if (type.kind == 0) type = boundInitializer.type;

		flags |= VARIABLE_INITIALIZED;
	} else if (flags & VARIABLE_GLOBAL) {
		flags |= VARIABLE_INITIALIZED;
		boundInitializer = (astNode){literalKind , type, .data = 0 };
	}

	astSymbol *variable = declare_variable(tree, vn.identifier.span, type, flags);

	if (variable != 0 && boundInitializer.kind == literalKind) {
		variable->flags |= VARIABLE_VALUE_KNOWN;

		if (variable->type.kind == stringType) {
			variable->stringValue = boundInitializer.stringValue;
		} else {
			variable->numValue = boundInitializer.numValue;
		}
	}

	variableDeclarationAst *declNode = arena_malloc(binder_arena, sizeof(variableDeclarationAst));
	*declNode = (variableDeclarationAst){ variable, boundInitializer };

	return (astNode){ variableDeclarationKind, type, .data = declNode };
}

astNode bind_variable_assignment(node *n, ast *tree) {

	variableAssignmentNode an = *(variableAssignmentNode*)n->data;
	enum syntaxKind opKind = getBinaryOperatorFromAssignmentOperator(an.assignmentOperator.kind);

	astNode boundExpression = bind_expression(&an.expression, tree);

	astSymbol *variable = find_variable_in_scope(an.identifier.span, tree, NULL);

	bool hasErrors = variable == 0 || boundExpression.type.kind == errorType || boundExpression.type.kind == unresolvedType;

	typedOperator op = {0};
	if (opKind != 0 && !hasErrors) {
		op = get_binary_operator(opKind, variable->type, boundExpression.type);
	}

	if (opKind != 0 && !op.operator) {
		report_diagnostic(&tree->diagnostics, undefinedBinaryOperatorDiagnostic, n->span, opKind, variable->type.kind, boundExpression.type.kind);
		hasErrors = true;
	}

	if (!hasErrors) {
		if (opKind == 0 && variable->type.kind != boundExpression.type.kind) {
			report_diagnostic(&tree->diagnostics, cannotAssignDiagnostic, an.identifier.span, variable->type.kind, boundExpression.type.kind, 0);
		} else if (!(variable->flags & VARIABLE_MUTABLE)) {
			report_diagnostic(&tree->diagnostics, cannotAssignConstantDiagnostic, an.expression.span, (u64)&an.identifier.span, 0, 0);
		} else if (!(variable->flags & VARIABLE_INITIALIZED)) {
			variable->flags |= VARIABLE_INITIALIZED;
		}
	}

	variableAssignmentAst *assignmentNode = arena_malloc(binder_arena, sizeof(variableAssignmentAst));
	*assignmentNode = (variableAssignmentAst){ variable, boundExpression, op.operator };

	return (astNode){ variableAssignmentKind, variable == 0 ? errorType : variable->type, .data = assignmentNode };
}

scope* create_scope(ast *tree) {
	scope*  newScope = arena_malloc(binder_arena, sizeof(scope));
	sb_push(tree->scopes, newScope);

	if (tree->currentScope == NULL) {
		declare_builtin_function(tree, "print");
	} else {
		newScope->parentScope = tree->currentScope;
	}

	return newScope;
}

// set scope s as the active scope and return the parent scope
// you can then pop the scope again by calling pop_scope(parent)
scope* set_current_scope(ast *tree, scope* s) {
	scope* parentScope = tree->currentScope;
	tree->currentScope = s;
	return parentScope;
}

// create a new scope and make it the active one
scope* push_scope(ast *tree) {
	return push_scope_and_get_reference(tree, NULL);
}

scope* push_scope_and_get_reference(ast *tree, scope **outScope) {
	scope*  newScope = create_scope(tree);
	if (outScope != NULL) *outScope = newScope;
	return set_current_scope(tree, newScope);
}

void pop_scope(ast *tree, scope *parentScope) {
	tree->currentScope = parentScope;
}

void declare_builtin_function(ast *tree, char* name) {

	scope *currentScope = tree->scopes[0];

	u8 len = strlen(name)+1;
	char *aName = arena_malloc(string_arena, sizeof(char)*len);
	strcpy(aName, name);

	astSymbol *function = arena_malloc(binder_arena, sizeof(astSymbol));
	sb_push(currentScope->symbols, function);

	functionSymbolData *fd = arena_malloc(binder_arena, sizeof(functionSymbolData));
	*fd = (functionSymbolData){ 0, 0, voidType, 0 };

	function->symbolKind = SYMBOL_FUNCTION;
	function->name = aName;
	function->type = primitive_type_from_kind(voidType);
	function->flags = 0;
	function->functionData = fd;
}

astSymbol* declare_function(ast *tree, textspan nameSpan, u8 flags, node *body, astSymbol **parameters, u16 parameterCount, scope *functionScope) {

	scope *currentScope = tree->currentScope;

	for (int i = 0; i < sb_count(currentScope->symbols); i++) {
		if (span_compare(tree->text, nameSpan, currentScope->symbols[i]->name)) {
			report_diagnostic(&tree->diagnostics, redeclarationOfSymbolDiagnostic, nameSpan, (u64)currentScope->symbols[i]->name, 0, 0);
			return 0;
		}
	}

	astSymbol *function = arena_malloc(binder_arena, sizeof(astSymbol));
	sb_push(currentScope->symbols, function);

	functionSymbolData *fd = arena_malloc(binder_arena, sizeof(functionSymbolData));

	*fd = (functionSymbolData){ parameters, parameterCount, primitive_type_from_kind(voidType), (astNode){0} };

	function->symbolKind = SYMBOL_FUNCTION;

	function->name = ast_substring(tree->text, nameSpan, string_arena);
	function->parentNamespace = tree->currentNamespace;

	function->type = primitive_type_from_kind(body == NULL ? errorType : voidType);
	function->flags = flags;
	function->functionData = fd;

	if (body != NULL) {
		astSymbol *parentNamespace = tree->currentNamespace;
		tree->currentNamespace = function;
		astNode boundBody = bind_block_statement(body, tree, functionScope);
		fd->body = boundBody;
		tree->currentNamespace = parentNamespace;
	}

	return function;
}

astSymbol* declare_variable(ast *tree, textspan nameSpan, astType variableType, u8 flags) {
	if (variableType.kind == voidType) {
		report_diagnostic(&tree->diagnostics, variableCannotBeVoidDiagnostic, nameSpan, 0, 0, 0);
		variableType.kind = errorType;
	}

	scope *currentScope = tree->currentScope;

	for (int i = 0; i < sb_count(currentScope->symbols); i++) {
		if (span_compare(tree->text, nameSpan, currentScope->symbols[i]->name)) {
			report_diagnostic(&tree->diagnostics, redeclarationOfSymbolDiagnostic, nameSpan, (u64)currentScope->symbols[i]->name, 0, 0);
			return 0;
		}
	}

	astSymbol *variable = arena_malloc(binder_arena, sizeof(astSymbol));
	sb_push(currentScope->symbols, variable);

	variable->symbolKind = SYMBOL_VARIABLE;
	variable->name = ast_substring(tree->text, nameSpan, string_arena);
	variable->parentNamespace = tree->currentNamespace;
	variable->type = variableType;
	variable->flags = flags;

	return variable;
}

// bypass pseudo namespaces like functions and returns the underlying "real" namespace
astSymbol* find_real_namespace_of_symbol(astSymbol *s);
astSymbol* find_real_namespace_of_symbol(astSymbol *s) {
	if (s == NULL || s->symbolKind == SYMBOL_NAMESPACE) return s;
	return find_real_namespace_of_symbol(s->parentNamespace);
}

astNode resolve_symbol_reference(node *n, ast *tree) {

	scope **scopesToSearch = NULL;
	astSymbol *currentNamespace = find_real_namespace_of_symbol(tree->currentNamespace);

	if (currentNamespace != NULL) sb_push(scopesToSearch, currentNamespace->namespaceScope);

	sb_push(scopesToSearch, tree->scopes[0]);

	scope *outScope = NULL;

	textspan deepestNamespan = n->span;
	i8 depth=-1;

	for (int i=0;i<sb_count(scopesToSearch);i++) {
		bool foundInScope = true;
		node *nSearch = n;
		outScope = scopesToSearch[i];
		u8 currentDepth=0;
		while (nSearch->kind == symbolReferenceExpression) {
			binaryExpressionNode *bn = (binaryExpressionNode*)nSearch->data;
			node namespace = bn->left;
			textspan namespan = namespace.span;

			astSymbol *symbol  = find_symbol_in_scope_internal(namespan, tree, outScope, SYMBOL_ANY, FLAG_NONE);

			if (symbol == NULL) {
				foundInScope = false;
				if (currentDepth > depth) {
					depth = currentDepth;
					deepestNamespan = namespan;
				}
				break;
			} else if (symbol->symbolKind == SYMBOL_ENUM) {
				nSearch->kind = enumReferenceExpression;
			} else {
				nSearch = &bn->right;
				outScope = symbol->namespaceScope;
				currentDepth++;
			}
		}

		if (foundInScope) {
			n = nSearch;
			break;
		} else outScope = NULL;
	}

	sb_free(scopesToSearch);

	if (outScope == NULL) {
		report_diagnostic(&tree->diagnostics, referenceToUndefinedVariableDiagnostic, deepestNamespan, 0, 0, 0);
		return (astNode){ missingKind, primitive_type_from_kind(errorType), .data = NULL };
	}

	switch(n->kind) {
		case enumReferenceExpression: return bind_enum_reference(n, tree, outScope);
		case callExpression: return bind_call_expression(n, tree, outScope);
		case identifierToken: return bind_variable_reference(n, tree, outScope);
		default: {
			fprintf(stderr, "%sUnhandled node of type %s in binder symbol resolver%s", TERMRED, syntaxKindText[n->kind], TERMRESET);
			exit(1);
		}
	}
}

astSymbol* declare_namespace(ast *tree, node *n, u8 flags) {
	scope *currentScope = tree->scopes[0];
	astSymbol *symbol = tree->currentNamespace;

	textspan namespan;

	while (true) {
		if(n->kind == symbolReferenceExpression) {
			binaryExpressionNode bn = *(binaryExpressionNode*)n->data;
			namespan = bn.left.span;
			n = &bn.right;
		} else {
			namespan = n->span;
			n = NULL;
		}

		astSymbol *newSymbol  = find_symbol_in_scope_internal(namespan, tree, currentScope, SYMBOL_NAMESPACE, FLAG_NONE);
		if (newSymbol != NULL) {
			symbol = newSymbol;
		} else {
			// CReate namespace
			astSymbol *ns = arena_malloc(binder_arena, sizeof(astSymbol));
			sb_push(currentScope->symbols, ns);

			ns->symbolKind = SYMBOL_NAMESPACE;
			ns->parentNamespace = symbol;
			ns->name = ast_substring(tree->text, namespan, string_arena);
			ns->type = primitive_type_from_kind(voidType);
			ns->flags = flags;
			ns->namespaceScope = create_scope(tree);
			symbol = ns;
		}

		currentScope = symbol->namespaceScope;

		if (n == NULL) return symbol;
	}
}

astSymbol* declare_enum(ast *tree, textspan nameSpan, node *enums, int enumCount, u8 flags) {
	scope *currentScope = tree->currentScope;

	for (int i = 0; i < sb_count(currentScope->symbols); i++) {
		if (span_compare(tree->text, nameSpan, currentScope->symbols[i]->name)) {
			report_diagnostic(&tree->diagnostics, redeclarationOfSymbolDiagnostic, nameSpan, (u64)currentScope->symbols[i]->name, 0, 0);
			return 0;
		}
	}

	astSymbol *enumSymbol = arena_malloc(binder_arena, sizeof(astSymbol));
	sb_push(currentScope->symbols, enumSymbol);

	enumSymbol->symbolKind = SYMBOL_ENUM;
	enumSymbol->name = ast_substring(tree->text, nameSpan, string_arena);
	enumSymbol->parentNamespace = tree->currentNamespace;
	enumSymbol->type = (astType){ enumType, enumSymbol };
	enumSymbol->flags = flags;
	enumSymbol->namespaceScope = create_scope(tree);

	for (int i=0;i<enumCount;i+=2) {
		declare_enum_member(tree, enums[i].span, enumSymbol, i/2);
	}

	return enumSymbol;
}

astSymbol* declare_enum_member(ast *tree, textspan nameSpan, astSymbol *enumSymbol, u8 value) {
	scope *currentScope = enumSymbol->namespaceScope;

	for (int i = 0; i < sb_count(currentScope->symbols); i++) {
		if (span_compare(tree->text, nameSpan, currentScope->symbols[i]->name)) {
			report_diagnostic(&tree->diagnostics, redeclarationOfSymbolDiagnostic, nameSpan, (u64)currentScope->symbols[i]->name, 0, 0);
			return 0;
		}
	}

	astSymbol *member = arena_malloc(binder_arena, sizeof(astSymbol));
	sb_push(currentScope->symbols, member);

	member->symbolKind = SYMBOL_ENUM_MEMBER;
	member->name = ast_substring(tree->text, nameSpan, string_arena);
	member->parentNamespace = enumSymbol;
	member->type = (astType){ enumType, enumSymbol };
	member->flags = 0;
	member->numValue = value;

	return member;
}

astSymbol* find_variable_in_scope(textspan nameSpan, ast *tree, scope *currentScope) {
	return find_symbol_in_scope_internal(nameSpan, tree, currentScope == NULL ? tree->currentScope : currentScope, SYMBOL_VARIABLE, (currentScope == NULL ? FLAG_RECURSE : FLAG_NONE) | FLAG_THROW);
}

astSymbol* find_enum_in_scope(node *n, ast *tree, scope *currentScope) {
	binaryExpressionNode bn = *(binaryExpressionNode*)n->data;
	astSymbol *enumSymbol = find_symbol_in_scope_internal(bn.left.span, tree, currentScope, SYMBOL_ENUM, FLAG_NONE | FLAG_THROW);
	if (enumSymbol == NULL) return NULL;
	return find_symbol_in_scope_internal(bn.right.span, tree, enumSymbol->namespaceScope, SYMBOL_ENUM_MEMBER, FLAG_NONE | FLAG_THROW);
}

astSymbol* find_enum_type_in_scope(textspan nameSpan, ast *tree, scope *currentScope) {
	return find_symbol_in_scope_internal(nameSpan, tree, currentScope, SYMBOL_ENUM, FLAG_NONE);
}

astSymbol* find_function_in_scope(textspan nameSpan, ast *tree, scope *currentScope, bool recurse) {
	return find_symbol_in_scope_internal(nameSpan, tree, currentScope, SYMBOL_FUNCTION, recurse | FLAG_THROW);
}

astSymbol* find_symbol_in_scope_internal(textspan nameSpan, ast *tree, scope *currentScope, u8 symbolKind, u8 flags) {
	for (int i = 0; i < sb_count(currentScope->symbols); i++) {
		if (span_compare(tree->text, nameSpan, currentScope->symbols[i]->name) && (currentScope->symbols[i]->symbolKind == symbolKind || symbolKind == SYMBOL_ANY) ) {
			return currentScope->symbols[i];
		}
	}

	if (currentScope->parentScope == 0 || !(flags & FLAG_RECURSE)) {
		if (flags & FLAG_THROW) report_diagnostic(&tree->diagnostics, referenceToUndefinedVariableDiagnostic, nameSpan, 0, 0, 0);
		return 0;
	}

	return find_symbol_in_scope_internal(nameSpan, tree, currentScope->parentScope, symbolKind, flags);
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

astNode fold_cast_expression(astType from, astType to, i64 value) {
	switch (to.kind) {
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
	case enumType: break;
	default:
		fprintf(stderr, "%sUnhandled type %s in fold_cast_expression%s", TERMRED, astKindText[to.kind], TERMRESET);
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

	switch(n.type.kind) {
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
		case enumType: {
			if (value >= sb_count(n.type.declaration->namespaceScope->symbols)  ) { errored=true; overflow=true; }
			else if (value < 0) { errored=true; } } break;

		case i64Type:
		case u64Type:
		case stringType:
		case boolType: break;

		default:
			fprintf(stderr, "%sUnhandled type %s in check_bounds%s", TERMRED, astKindText[n.type.kind], TERMRESET);
			exit(1);
	}

	if (errored) {
		report_diagnostic(d, valueOutOfBoundsDiagnostic, span, n.type.kind, overflow, 0);
	}

	return !errored;
}
