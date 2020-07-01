void emit_c_node(astNode *n, ast *tree);
static inline void emit_c_file(astNode *n, ast *tree);
static inline void emit_c_blockStatement(astNode *n, ast *tree);
static inline void emit_c_ifStatement(astNode *n, ast *tree);
static inline void emit_c_caseStatement(astNode *n, ast *tree);
static inline void emit_c_whileLoop(astNode *n, ast *tree);
static inline void emit_c_forLoop(astNode *n, ast *tree);

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
	"<<",
	">>",
	"&",
	"^",
	"|",
	"&&",
	"||",
};

static const char *cUnaryText[] = {
	"missingUnary",
	"!",
	"~",
	"-",
	"+",
	"++",
	"--",
	"++",
	"--",
};

FILE *fp;
void emit_c_from_ast(ast *tree) {
	fp = fopen("out.c", "w+");
	emit_c_file(&tree->root, tree);
	fclose(fp);
	system("tcc -run out.c");
	remove("out.c");
}

void emit_c_node(astNode *n, ast *tree) {
	switch(n->kind) {
	case blockStatementKind: return emit_c_blockStatement(n, tree);
	case ifStatementKind: return emit_c_ifStatement(n, tree);
	case caseStatementKind: return emit_c_caseStatement(n, tree);
	case whileLoopKind: return emit_c_whileLoop(n, tree);
	case forLoopKind: return emit_c_forLoop(n, tree);

	case literalKind: return emit_c_literal(n, tree);
	case binaryExpressionKind: return emit_c_binaryExpression(n, tree);
	case unaryExpressionKind: return emit_c_unaryExpression(n, tree);
	case callExpressionKind: return emit_c_callExpression(n, tree);
	case variableDeclarationKind: return emit_c_variableDeclaration(n, tree);
	case variableAssignmentKind: return emit_c_variableAssignment(n, tree);
	case variableReferenceKind: return emit_c_variableReference(n, tree);
	default:
		fprintf(stderr, "%sUnhandled node of type %s in c emitter%s", TERMRED, astKindText[n->kind], TERMRESET);
		exit(1);
	}
}

void emit_c_file(astNode *n, ast *tree) {
	fprintf(fp,"#include <stdio.h>\nvoid main() ");
	emit_c_blockStatement(n,tree);
}

static inline void emit_c_blockStatement(astNode *n, ast *tree) {
	fprintf(fp,"{\n");
	blockStatementAst bn = *(blockStatementAst*)n->data;
	for (int i= 0; i < bn.statementsCount; i++) {
		emit_c_node(bn.statements + i, tree);
		fprintf(fp,";\n");
	}
	fprintf(fp,"}\n");
}

static inline void emit_c_ifStatement(astNode *n, ast *tree) {
	ifStatementAst in = *(ifStatementAst*)n->data;
	fprintf(fp,"if (");
	emit_c_node(&in.condition, tree);
	fprintf(fp,")\n");
	emit_c_node(&in.thenStatement, tree);
	if (in.thenStatement.kind != blockStatementKind) fprintf(fp,";\n");
	if (in.elseStatement.kind != 0) {
		fprintf(fp," else \n");
		emit_c_node(&in.elseStatement, tree);
	}
}

static inline void emit_c_caseStatement(astNode *n, ast *tree) {
	caseStatementAst cn = *(caseStatementAst*)n->data;
	for (int i=0;i<cn.branchCount;i++) {
		caseBranchAst cb = *(caseBranchAst*)((cn.branches + i)->data);
		if (cb.condition.kind ==0) {
			fprintf(fp,"else ");
		} else if (i==0) {
			fprintf(fp,"if (");
			emit_c_node(&cb.condition, tree);
			fprintf(fp,") ");
		} else {
			fprintf(fp,"else if (");
			emit_c_node(&cb.condition, tree);
			fprintf(fp,") ");
		}
		emit_c_node(&cb.thenStatement, tree);
		fprintf(fp,";\n");
	}
}

static inline void emit_c_whileLoop(astNode *n, ast *tree) {
	whileLoopAst wn = *(whileLoopAst*)n->data;
	fprintf(fp,"while (");
	emit_c_node(&wn.condition, tree);
	fprintf(fp,") ");
	emit_c_node(&wn.block, tree);
}

static inline void emit_c_forLoop(astNode *n, ast *tree) {
	forLoopAst fn = *(forLoopAst*)n->data;
	rangeExpressionAst rn = *(rangeExpressionAst*)fn.range.data;

	if (fn.index == 0) {
		char* compOp = rn.to > rn.from ? "<=" : ">=";
		char* incOp = rn.to > rn.from ? "++" : "--";

		fprintf(fp,"for (");
		// int x = 1; 
		fprintf(fp,"%s %s = %d; ", cTypeText[fn.value->type], fn.value->name, rn.from);
		// x <= 100
		fprintf(fp,"%s %s %d; ", fn.value->name, compOp, rn.to);
		// x++
		fprintf(fp,"%s%s", fn.value->name, incOp);
		fprintf(fp,") ");
		emit_c_node(&fn.block, tree);
	} else {
		fprintf(fp,"for (int %s = 0; %s <= %d; %s++) {\n", fn.index->name, fn.index->name, abs(rn.from - rn.to), fn.index->name);

		if (rn.to > rn.from) fprintf(fp,"%s %s = i + %d;\n", cTypeText[fn.value->type], fn.value->name, rn.from);
		else fprintf(fp,"%s %s = %d - i;\n",cTypeText[fn.value->type], fn.value->name, rn.from);

		emit_c_node(&fn.block, tree);
		fprintf(fp,";\n");
		fprintf(fp,"} ");
	}
}

static inline void emit_c_literal(astNode *n, ast *tree) {
	switch (n->type) {
	case intType: fprintf(fp,"%d", n->numValue); break;
	case boolType: fprintf(fp,"%s", n->boolValue ? "1" : "0"); break;
	case stringType: {
		char *escapedStr = escape_string_c(n->stringValue);
		fprintf(fp,"\"%s\"", escapedStr); break;
		free(escapedStr);
		free(n->stringValue);
	}
	default:
		fprintf(fp,"%sUnhandled type %s in c emitter%s", TERMRED, astTypeText[n->type], TERMRESET);
		exit(1);
	}
}

static inline void emit_c_binaryExpression(astNode *n, ast *tree) {
	binaryExpressionAst bn = *(binaryExpressionAst*)n->data;
	fprintf(fp,"(");
	emit_c_node(&bn.left, tree);
	fprintf(fp," %s ", cBinaryText[bn.operator]);
	emit_c_node(&bn.right, tree);
	fprintf(fp,")");
}

static inline void emit_c_unaryExpression(astNode *n, ast *tree) {
	unaryExpressionAst un = *(unaryExpressionAst*)n->data;
	fprintf(fp,"(");
	if (un.operator == postIncrementOp || un.operator == postDecrementOp) {
		emit_c_node(&un.operand, tree);
		fprintf(fp,cUnaryText[un.operator]);
	} else {
		fprintf(fp,cUnaryText[un.operator]);
		emit_c_node(&un.operand, tree);
	}
	fprintf(fp,")");
}

static inline void emit_c_callExpression(astNode *n, ast *tree) {
	callExpressionAst cn = *(callExpressionAst*)n->data;
	fprintf(fp,"printf(");
	for (int i= 0; i < cn.argumentCount; i++) {
		emit_c_node(cn.arguments + i, tree);
		if (i != cn.argumentCount-1) fprintf(fp,", ");
	}
	fprintf(fp,")");
}

static inline void emit_c_variableDeclaration(astNode *n, ast *tree) {

	variableDeclarationAst dn = *(variableDeclarationAst*)n->data;

	fprintf(fp,"%s %s = ", cTypeText[dn.variable->type], dn.variable->name);
	emit_c_node(&dn.initalizer, tree);
}

static inline void emit_c_variableAssignment(astNode *n, ast *tree) {

	variableAssignmentAst an = *(variableAssignmentAst*)n->data;

	fprintf(fp,"%s = ", an.variable->name);
	emit_c_node(&an.expression, tree);
}

static inline void emit_c_variableReference(astNode *n, ast *tree) {
	fprintf(fp,((variableSymbol*) n->data)->name);
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
