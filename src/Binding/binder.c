astNode bind_expression(node *n, diagnosticContainer *d);

int bind_tree(ast* tree) {
    tree->root = bind_expression(&tree->parser.root, &tree->diagnostics);
    return tree->diagnostics.index == 0;
}

astNode bind_expression(node *n, diagnosticContainer *d) {
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

        default: {
            TERMRED();
            printf("Unhandled node of type %s", syntaxKindText[n->kind]);
            TERMRESET();
            exit(1);
        }
    }
}