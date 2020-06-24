astNode bind_expression(node *n, ast *tree);
astNode bind_block_statement(node *n, ast *tree);
astNode bind_unary_expression(node *n, ast *tree);
astNode bind_binary_expression(node *n, ast *tree);
astNode bind_variable_declaration(node *n, ast *tree);
variableSymbol* find_variable_in_scope(textspan nameSpan, ast *tree);

int bind_tree(ast* tree) {
    tree->root = bind_expression(&tree->parser.root, tree);
    return tree->diagnostics.index == 0;
}

astNode bind_expression(node *n, ast* tree) {
    switch(n->kind) {
        case blockStatement: return bind_block_statement(n, tree);

        case falseKeyword:
        case trueKeyword: {
            astNode num = { literalKind, boolType, .boolValue = n->boolValue };
            return num;
        }
        case numberLiteral: {
            astNode num = { literalKind, intType, .numValue = n->numValue };
            return num;
        }

        case parenthesizedExpression: {
		    parenthesizedExpressionNode pn = *(parenthesizedExpressionNode*)n->data;
            return bind_expression(&pn.expression, tree);
        }

        case identifierToken: {
            variableSymbol *variable = find_variable_in_scope(n->span, tree);
            astNode varRef = { variableReferenceKind, variable == 0 ? errorType : variable->type, .data = variable };
            return varRef;
        }

        case unaryExpression: return bind_unary_expression(n, tree);
        case binaryExpression: return bind_binary_expression(n, tree);
        case variableDeclaration: return bind_variable_declaration(n, tree);

        default: {
            TERMRED();
            printf("Unhandled node of type %s in binder", syntaxKindText[n->kind]);
            TERMRESET();
            exit(1);
        }
    }
}

astNode bind_block_statement(node *n, ast *tree) {
    astNode boundStatements[100];
    int statementCount = 0;

	blockStatementNode bn = *(blockStatementNode*)n->data;

    int parentScopeIndex = tree->currentScopeIndex;
    tree->currentScopeIndex = tree->scopesIndex;
    scope*  newScope = &tree->scopes[tree->scopesIndex++];

    if (parentScopeIndex != tree->currentScopeIndex) {
        newScope->parentScope = &tree->scopes[parentScopeIndex];
    } 

    for (int i = 0; i < bn.statementsCount; i++) {
        boundStatements[statementCount++] = bind_expression(&bn.statements[i], tree);
    }
    
    tree->currentScopeIndex = parentScopeIndex;

    astNode* nodesStart = &tree->nodes[tree->nodesIndex];
    for (int i = 0; i < statementCount; i++) {
        tree->nodes[tree->nodesIndex++] = boundStatements[i];
    }

    int index = tree->blockStatementsIndex;
    blockStatementAst blockData = { nodesStart, statementCount };
    tree->blockStatements[tree->blockStatementsIndex++] = blockData;

    astNode blockNode = { blockStatementKind,  .data = &tree->blockStatements[index] };

    return blockNode;
}

astNode bind_unary_expression(node *n, ast *tree) {
	unaryExpressionNode un = *(unaryExpressionNode*)n->data;

    astNode boundOperand = bind_expression(&un.operand, tree);

    enum astUnaryOperator op = get_unary_operator(un.operator.kind, boundOperand.type);

    if (!op) {
        report_diagnostic(&tree->diagnostics, undefinedUnaryOperatorDiagnostic, un.operator.span, un.operator.kind, boundOperand.type, 0);
    }

    if (op == identityOp) return boundOperand;

    int index = tree->unaryExpressionsIndex;
    unaryExpressionAst unaryData = { op, boundOperand };
    tree->unaryExpressions[tree->unaryExpressionsIndex++] = unaryData;

    astNode unaryNode = { unaryExpressionKind , boundOperand.type, .data = &tree->unaryExpressions[index] };

    return unaryNode;
}

astNode bind_binary_expression(node *n, ast *tree) {
	binaryExpressionNode bn = *(binaryExpressionNode*)n->data;

    astNode boundLeft = bind_expression(&bn.left, tree);
    astNode boundRight = bind_expression(&bn.right, tree);

    enum astBinaryOperator op = get_binary_operator(bn.operator.kind, boundLeft.type, boundRight.type);

    // silence errors if the problem lies elsewhere
    bool hasErrors = boundLeft.type == errorType || boundLeft.type == unresolvedType ||
                    boundRight.type == errorType || boundRight.type == unresolvedType;

    if (!op && !hasErrors) {
        report_diagnostic(&tree->diagnostics, undefinedBinaryOperatorDiagnostic, n->span, bn.operator.kind, boundLeft.type, boundRight.type);
    }

    int index = tree->binaryExpressionsIndex;
    binaryExpressionAst binaryData = { op, boundLeft, boundRight };
    tree->binaryExpressions[tree->binaryExpressionsIndex++] = binaryData;

    // TODO: actually have a return type for the operator
    astNode binaryNode = { binaryExpressionKind , boundLeft.type, .data = &tree->binaryExpressions[index] };

    return binaryNode;
}

astNode bind_variable_declaration(node *n, ast *tree) {

	variableDeclarationNode vn = *(variableDeclarationNode*)n->data;

    astNode boundInitializer = bind_expression(&vn.expression, tree);

    if (vn.type.kind != 0) {
        printf("No explicit types for now\n");
        exit(1);
    } 

    scope *currentScope = &tree->scopes[tree->currentScopeIndex];
    textspan nameSpan = vn.identifier.span;

    variableSymbol *variable = 0;

    bool success=true;
    for (int i = 0; i < currentScope->variableCount; i++) {
        if (span_compare(tree->text, nameSpan, currentScope->variables[i].name)) {
            report_diagnostic(&tree->diagnostics, redeclarationOfVariableDiagnostic, n->span, (u32)currentScope->variables[i].name, 0, 0);
            success=false;
            break;
        }
    }

    if (success) {
        variable = &currentScope->variables[currentScope->variableCount++];

        for (int i= 0;i<nameSpan.length;i++) {
            variable->name[i] = tree->text[i+nameSpan.start];
        }
        variable->type = boundInitializer.type;
    }

    int index = tree->variableDeclarationIndex;
    variableDeclarationAst varData = { variable, boundInitializer };

    tree->variableDeclarations[tree->variableDeclarationIndex++] = varData;

    astNode varNode = { variableDeclarationKind , boundInitializer.type, .data = &tree->variableDeclarations[index] };

    return varNode;
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