void emit_c_node(astNode *n, ast *tree);
static inline void emit_c_file(astNode *n, ast *tree);
static inline void emit_c_blockStatement(astNode *n, ast *tree);

static inline void emit_c_literal(astNode *n, ast *tree);
static inline void emit_c_binaryExpression(astNode *n, ast *tree);
static inline void emit_c_unaryExpression(astNode *n, ast *tree);
static inline void emit_c_callExpression(astNode *n, ast *tree);
static inline void emit_c_variableDeclaration(astNode *n, ast *tree);
static inline void emit_c_variableAssignment(astNode *n, ast *tree);
static inline void emit_c_variableReference(astNode *n, ast *tree);
char* escape_string_c(char *str);

static const char *cTypeText[] = {
	"errorType",
	"unresolved",
	"void",
	"int",
	"int",		// bool
	"char*",
};

static const char *cBinaryText[] = {
	"missingBinary",
	"+",
	"-",
	"*",
	"/",
	"%",
	"==",
	"!=",
	"<",
	">",
	"<=",
	">=",
	"&&",
	"||",
};

static const char *cUnaryText[] = {
	"missingUnary",
	"!",
	"-",
	"+",
};

void emit_c_from_ast(ast *tree) {
	emit_c_file(&tree->root, tree);
}

void emit_c_node(astNode *n, ast *tree) {
	switch(n->kind) {
	case blockStatementKind: return emit_c_blockStatement(n, tree);

	case literalKind: return emit_c_literal(n, tree);
	case binaryExpressionKind: return emit_c_binaryExpression(n, tree);
	case unaryExpressionKind: return emit_c_unaryExpression(n, tree);
	case callExpressionKind: return emit_c_callExpression(n, tree);
	case variableDeclarationKind: return emit_c_variableDeclaration(n, tree);
	case variableAssignmentKind: return emit_c_variableAssignment(n, tree);
	case variableReferenceKind: return emit_c_variableReference(n, tree);
	default:
		TERMRED();
		printf("Unhandled node of type %s in c emitter", astKindText[n->kind]);
		TERMRESET();
		exit(1);
	}
}

void emit_c_file(astNode *n, ast *tree) {
	printf("#include <stdio.h>\nvoid main() ");
	emit_c_blockStatement(n,tree);
}

static inline void emit_c_blockStatement(astNode *n, ast *tree) {
	printf("{\n");

	blockStatementAst bn = *(blockStatementAst*)n->data;
	for (int i= 0; i < bn.statementsCount; i++) {
		emit_c_node(bn.statements + i, tree);
		printf(";\n");
	}
	printf("}\n");
}

static inline void emit_c_literal(astNode *n, ast *tree) {
	switch (n->type) {
	case intType: printf("%d", n->numValue); break;
	case boolType: printf("%s", n->boolValue ? "1" : "0"); break;
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

static inline void emit_c_binaryExpression(astNode *n, ast *tree) {
	binaryExpressionAst bn = *(binaryExpressionAst*)n->data;
	printf("(");
	emit_c_node(&bn.left, tree);
	printf(" %s ", cBinaryText[bn.operator]);
	emit_c_node(&bn.right, tree);
	printf(")");
}

static inline void emit_c_unaryExpression(astNode *n, ast *tree) {
	unaryExpressionAst un = *(unaryExpressionAst*)n->data;
	printf("(");
	printf(cUnaryText[un.operator]);
	emit_c_node(&un.operand, tree);
	printf(")");
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

static inline void emit_c_variableDeclaration(astNode *n, ast *tree) {

	variableDeclarationAst dn = *(variableDeclarationAst*)n->data;

	printf("%s %s = ", cTypeText[dn.variable->type], dn.variable->name);
	emit_c_node(&dn.initalizer, tree);
}

static inline void emit_c_variableAssignment(astNode *n, ast *tree) {

	variableAssignmentAst an = *(variableAssignmentAst*)n->data;

	printf("%s = ", an.variable->name);
	emit_c_node(&an.expression, tree);
}

static inline void emit_c_variableReference(astNode *n, ast *tree) {
	printf(((variableSymbol*) n->data)->name);
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
