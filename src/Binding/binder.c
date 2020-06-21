astNode bind_expression(node *n, ast *tree);
astNode bind_unary_expression(node *n, ast *tree);

int bind_tree(ast* tree) {
    tree->root = bind_expression(&tree->parser.root, tree);
    return tree->diagnostics.index == 0;
}

astNode bind_expression(node *n, ast* tree) {
    switch(n->kind) {
        case falseKeyword:
        case trueKeyword: {
            astNode num = { literalKind, boolType, .boolValue = n->boolValue };
            return num;
        }
        case numberLiteral: {
            astNode num = { literalKind, intType, .numValue = n->numValue };
            return num;
        }

        case unaryExpression: return bind_unary_expression(n, tree);

        default: {
            TERMRED();
            printf("Unhandled node of type %s in binder", syntaxKindText[n->kind]);
            TERMRESET();
            exit(1);
        }
    }
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