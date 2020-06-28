void emit_c_node(astNode *n, ast *tree);
static inline void emit_c_file(astNode *n, ast *tree);
static inline void emit_c_literal(astNode *n, ast *tree);
static inline void emit_c_callExpression(astNode *n, ast *tree);
char* escape_string_c(char *str);

void emit_c_from_ast(ast *tree) {
	emit_c_file(&tree->root, tree);
}

void emit_c_node(astNode *n, ast *tree) {
	switch(n->kind) {
	case literalKind: return emit_c_literal(n, tree);
	case callExpressionKind: return emit_c_callExpression(n, tree);
	default:
		TERMRED();
		printf("Unhandled node of type %s in c emitter", astKindText[n->kind]);
		TERMRESET();
		exit(1);
	}
}

void emit_c_file(astNode *n, ast *tree) {
	printf("#include <stdio.h>\n");
	printf("void main() {\n");

	blockStatementAst bn = *(blockStatementAst*)n->data;
	for (int i= 0; i < bn.statementsCount; i++) {
		printf("\t");
		emit_c_node(bn.statements + i, tree);
		printf(";\n");
	}
	printf("}\n");
}

static inline void emit_c_literal(astNode *n, ast *tree) {
	switch (n->type) {
	case intType: printf("%d", n->numValue); break;
	case boolType: printf("%s", n->boolValue ? "true" : "false"); break;
	case stringType: {
		char *escapedStr = escape_string_c(n->stringValue);
		printf("\"%s\"", escapedStr); break;
		free(escapedStr);
		free(n->stringValue);
	}
	default:
		TERMRED();
		printf("Unhandled type %s in c emitter", astTypeText[n->type]);
		TERMRESET();
		exit(1);
	}
}

static inline void emit_c_callExpression(astNode *n, ast *tree) {
	callExpressionAst cn = *(callExpressionAst*)n->data;
	printf("printf(");
	for (int i= 0; i < cn.argumentCount; i++) {
		emit_c_node(cn.arguments + i, tree);
		if (i != cn.argumentCount-1) printf(", ");
	}
	printf(")");
}

char* escape_string_c(char *str) {
	int escapedCharacters = 0;
	int i = 0;
	bool endFound = false;
	while (!endFound) {
		switch(str[i]) {
			case '\0': endFound=true; break;

			case '\r':
			case '\n':
			case '\\':
			case '\"':
				escapedCharacters++; /* FALLTHROUGH */
		
			default: i++;
		}
	}

	int length = i + escapedCharacters + 1;
	int oldlength = i;

	char *allocatedText = (char*)malloc(sizeof(char) * length);

	int index = 0;
	for (int j=0; j < oldlength; j++) {
		switch(str[j]) {
			case '\r':
				allocatedText[index++] = '\\';
				allocatedText[index++] = 'r';
				break;
			case '\n':
				allocatedText[index++] = '\\';
				allocatedText[index++] = 'n';
				break;
			case '\\':
				allocatedText[index++] = '\\';
				allocatedText[index++] = '\\';
			case '\"':
				allocatedText[index++] = '\\';
				allocatedText[index++] = '"';
				break;
		
			default: allocatedText[index++] = str[j];
		}
	}

	allocatedText[length-1] = '\0';
	return allocatedText;
}
