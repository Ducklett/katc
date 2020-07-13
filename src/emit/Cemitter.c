void emit_c_node(astNode *n, ast *tree);
static inline void emit_c_file(astNode *n, ast *tree);
static inline void emit_c_function(astNode *n, ast *tree);
static inline void emit_c_blockStatement(astNode *n, ast *tree);
static inline void emit_c_ifStatement(astNode *n, ast *tree);
static inline void emit_c_caseStatement(astNode *n, ast *tree);
static inline void emit_c_switchStatement(astNode *n, ast *tree);
static inline void emit_c_whileLoop(astNode *n, ast *tree);
static inline void emit_c_forLoop(astNode *n, ast *tree);

static inline void emit_c_literal(astNode *n, ast *tree);
static inline void emit_c_binaryExpression(astNode *n, ast *tree);
static inline void emit_c_unaryExpression(astNode *n, ast *tree);
static inline void emit_c_callExpression(astNode *n, ast *tree);
static inline void emit_c_castExpression(astNode *n, ast *tree);
static inline void emit_c_variableDeclaration(astNode *n, ast *tree);
static inline void emit_c_variableAssignment(astNode *n, ast *tree);
static inline void emit_c_variableReference(astNode *n, ast *tree);
char* escape_string_c(char *str);

int c_indent=4;
enum astKind kindStack[128];
u8 kindIndex = 0;
enum astKind parentKind;

enum astKind needs_indent(enum astKind k) {
	return (
		k == blockStatementKind ||
		k == caseStatementKind  );
}

enum astKind needs_semicolon(enum astKind k) {
	return !(
		k == blockStatementKind ||
		k == forLoopKind        ||
		k == whileLoopKind      ||
		k == ifStatementKind    ||
		k == caseStatementKind  );
}
inline static void kindStack_push(enum astKind k) {
	kindStack[++kindIndex] = k;
	parentKind = kindStack[kindIndex-1];

	if (needs_indent(k)) c_indent+=4;
}

inline static void kindStack_pop() {
	if (needs_indent(kindStack[kindIndex])) c_indent-=4;
	--kindIndex;
	parentKind = kindStack[kindIndex];
}

static const char *cTypeText[] = {
	"errorType",
	"unresolved",
	"void",
	"int",            // int
	"unsigned char",  // u8
	"unsigned short", // u16
	"unsigned int",   // u32
	"unsigned long",  // u64
	"char",           // i8
	"short",          // i16
	"int",            // i32
	"long",           // i64
	"int",		      // bool
	"char*",          // string
	"char",           // char
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
void emit_c_from_ast(ast *tree, const char* outputName, bool run, bool emitSource) {
	const char* cFileName = (emitSource && outputName && !run) ? outputName : "out.c";
	fp = fopen(cFileName, "w+");
	emit_c_file(&tree->root, tree);
	fclose(fp);

	if (run) system("tcc -run out.c");
	else if (!emitSource) {
		if (outputName) {
			char* compileCommand = string_concat("tcc out.c -o ", outputName);
			system(compileCommand);
			free(compileCommand);
		} else system("tcc out.c");

		remove("out.c");
	}
}

void emit_c_node(astNode *n, ast *tree) {

	kindStack_push(n->kind);
	switch(n->kind) {
	case fileStatementKind:
	case blockStatementKind: emit_c_blockStatement(n, tree); break;
	case ifStatementKind: emit_c_ifStatement(n, tree); break;
	case caseStatementKind: emit_c_caseStatement(n, tree); break;
	case switchStatementKind: emit_c_switchStatement(n, tree); break;
	case whileLoopKind: emit_c_whileLoop(n, tree); break;
	case forLoopKind: emit_c_forLoop(n, tree); break;

	case breakKind: fprintf(fp, "break;\n"); break;
	case continueKind: fprintf(fp, "continue;\n"); break;

	case literalKind: emit_c_literal(n, tree); break;
	case binaryExpressionKind: emit_c_binaryExpression(n, tree); break;
	case unaryExpressionKind: emit_c_unaryExpression(n, tree); break;
	case callExpressionKind: emit_c_callExpression(n, tree); break;
	case castExpressionKind: emit_c_castExpression(n, tree); break;
	case variableDeclarationKind: emit_c_variableDeclaration(n, tree); break;
	case variableAssignmentKind: emit_c_variableAssignment(n, tree); break;
	case variableReferenceKind: emit_c_variableReference(n, tree); break;
	default:
		fprintf(stderr, "%sUnhandled node of type %s in c emitter%s", TERMRED, astKindText[n->kind], TERMRESET);
		exit(1);
	}
	kindStack_pop();
}

void emit_c_file(astNode *n, ast *tree) {
	fprintf(fp,"#include <stdio.h>\n\n");
	blockStatementAst bn = *(blockStatementAst*)n->data;
	for (int i= 0; i < bn.statementsCount; i++) {
		if (bn.statements[i].kind != functionDeclarationKind) continue;
		emit_c_function(bn.statements + i, tree);
		fprintf(fp,"\n");
	}
	fprintf(fp,"void main() ");
	emit_c_node(n,tree);
}

static inline void emit_c_function(astNode *n, ast *tree) {
	astSymbol vn = *(astSymbol*)n->data;
	fprintf(fp,"%*s%s %s() ", c_indent, "", cTypeText[vn.type], vn.name);
	emit_c_node(&vn.functionData->body,tree);
}

static inline void emit_c_blockStatement(astNode *n, ast *tree) {
	fprintf(fp,"%*s{\n", c_indent-4, "");
	blockStatementAst bn = *(blockStatementAst*)n->data;
	for (int i= 0; i < bn.statementsCount; i++) {
		if (bn.statements[i].kind == functionDeclarationKind) continue;
		fprintf(fp,"%*s", c_indent, "");
		emit_c_node(bn.statements + i, tree);
		if(needs_semicolon(bn.statements[i].kind)) fprintf(fp,";\n");
	}
	fprintf(fp,"%*s}\n", c_indent-4, "");
}

static inline void emit_c_ifStatement(astNode *n, ast *tree) {
	ifStatementAst in = *(ifStatementAst*)n->data;
	fprintf(fp,"if (");
	emit_c_node(&in.condition, tree);
	fprintf(fp,")\n");

	fprintf(fp,"%*s", c_indent, "");
	emit_c_node(&in.thenStatement, tree);
	if (needs_semicolon(in.thenStatement.kind)) fprintf(fp,";\n");
	if (in.elseStatement.kind != 0) {
		fprintf(fp,"%*s", c_indent, "");
		fprintf(fp,"else \n");
		fprintf(fp,"%*s", c_indent, "");
		emit_c_node(&in.elseStatement.kind, tree);
		if (needs_semicolon(in.elseStatement.kind)) fprintf(fp,";\n");
	}
}

static inline void emit_c_caseStatement(astNode *n, ast *tree) {
	caseStatementAst cn = *(caseStatementAst*)n->data;
	for (int i=0;i<cn.branchCount;i++) {
		caseBranchAst cb = *(caseBranchAst*)((cn.branches + i)->data);
		if (cb.condition.kind ==0) {
			fprintf(fp,"%*selse ", c_indent, "");
		} else if (i==0) {
			fprintf(fp,"if (");
			emit_c_node(&cb.condition, tree);
			fprintf(fp,") ");
		} else {
			fprintf(fp,"%*selse if (", c_indent, "");
			emit_c_node(&cb.condition, tree);
			fprintf(fp,") ");
		}
		emit_c_node(&cb.thenStatement, tree);
		fprintf(fp,";\n");
	}
}

static inline void emit_c_switchStatement(astNode *n, ast *tree) {
	switchStatementAst cn = *(switchStatementAst*)n->data;
	fprintf(fp,"switch(");
	emit_c_node(&cn.target, tree);
	fprintf(fp,") {\n");
	for (int i=0;i<cn.branchCount;i++) {
		switchBranchAst cb = *(switchBranchAst*)((cn.branches + i)->data);
		if (cb.condition.kind == 0) {
			fprintf(fp,"%*sdefault: ",c_indent,"");
		} else {
			if (cb.condition.kind == rangeExpressionKind) {
				rangeExpressionAst rn = *(rangeExpressionAst*)cb.condition.data;
				for (int i=rn.fromInt; i <= rn.toInt; i++) {
					if (cb.condition.type == charType) fprintf(fp,"%*scase '%c': ",c_indent,"", i);
					else fprintf(fp,"%*scase %d: ", c_indent, "", i);
				}
				fprintf(fp,"\n");
			} else {
				fprintf(fp,"%*scase ", c_indent, "");
				emit_c_node(&cb.condition, tree);
				fprintf(fp,": ");
			}
		}
		emit_c_node(&cb.thenStatement, tree);
		fprintf(fp,"; break;\n");
	}
	fprintf(fp,"}\n");
}

static inline void emit_c_whileLoop(astNode *n, ast *tree) {
	whileLoopAst wn = *(whileLoopAst*)n->data;
	fprintf(fp,"while (");
	emit_c_node(&wn.condition, tree);
	fprintf(fp,")\n");
	emit_c_node(&wn.block, tree);
}

static inline void emit_c_forLoop(astNode *n, ast *tree) {
	forLoopAst fn = *(forLoopAst*)n->data;
	rangeExpressionAst rn = *(rangeExpressionAst*)fn.range.data;

	if (fn.index == 0) {
		char* compOp = rn.toInt > rn.fromInt ? "<=" : ">=";
		char* incOp = rn.toInt > rn.fromInt ? "++" : "--";

		fprintf(fp,"for (");
		// int x = 1; 
		fprintf(fp,"%s %s = %d; ", cTypeText[fn.value->type], fn.value->name, rn.fromInt);
		// x <= 100
		fprintf(fp,"%s %s %d; ", fn.value->name, compOp, rn.toInt);
		// x++
		fprintf(fp,"%s%s", fn.value->name, incOp);
		fprintf(fp,")\n");

		if (fn.block.kind != blockStatementKind) fprintf(fp,"%*s", c_indent+4, "");
		emit_c_node(&fn.block, tree);
		if (needs_semicolon(fn.block.kind))fprintf(fp,";\n");
	} else {
		fprintf(fp,"for (int %s = 0; %s <= %d; %s++) {\n", fn.index->name, fn.index->name, abs(rn.fromInt - rn.toInt), fn.index->name);
		kindStack_push(blockStatementKind);

		fprintf(fp,"%*s", c_indent, "");
		if (rn.toInt > rn.fromInt) fprintf(fp,"%s %s = i + %d;\n", cTypeText[fn.value->type], fn.value->name, rn.fromInt);
		else fprintf(fp,"%s %s = %d - i;\n",cTypeText[fn.value->type], fn.value->name, rn.fromInt);

		if (fn.block.kind != blockStatementKind) fprintf(fp,"%*s", c_indent, "");
		emit_c_node(&fn.block, tree);
		if (needs_semicolon(fn.block.kind)) fprintf(fp,";\n");
		kindStack_pop();
		fprintf(fp,"%*s", c_indent, "");
		fprintf(fp,"}\n");
	}
}

static inline void emit_c_literal(astNode *n, ast *tree) {
	switch (n->type) {
	case u8Type: case u16Type: case u32Type: case u64Type: case i8Type: case i16Type: case i32Type: case i64Type:
	case intType: fprintf(fp,"%d", n->numValue); break;
	case boolType: fprintf(fp,"%s", n->boolValue ? "1" : "0"); break;
	case stringType: {
		char *escapedStr = escape_string_c(n->stringValue);
		fprintf(fp,"\"%s\"", escapedStr); break;
		free(escapedStr);
	}
	case charType: fprintf(fp,"'%c'", n->charValue); break;
	default:
		fprintf(stderr,"%sUnhandled type %s in c emitter%s", TERMRED, astTypeText[n->type], TERMRESET);
		exit(1);
	}
}

static inline void emit_c_binaryExpression(astNode *n, ast *tree) {
	binaryExpressionAst bn = *(binaryExpressionAst*)n->data;
	bool needsParen = parentKind == binaryExpressionKind || parentKind == unaryExpressionKind;
	if (needsParen) fprintf(fp,"(");
	emit_c_node(&bn.left, tree);
	fprintf(fp," %s ", cBinaryText[bn.operator]);
	emit_c_node(&bn.right, tree);
	if (needsParen) fprintf(fp,")");
}

static inline void emit_c_unaryExpression(astNode *n, ast *tree) {
	unaryExpressionAst un = *(unaryExpressionAst*)n->data;
	bool needsParen = parentKind == binaryExpressionKind || parentKind == unaryExpressionKind;
	if (needsParen) fprintf(fp,"(");
	if (un.operator == postIncrementOp || un.operator == postDecrementOp) {
		emit_c_node(&un.operand, tree);
		fprintf(fp,"%s", cUnaryText[un.operator]);
	} else {
		fprintf(fp,"%s", cUnaryText[un.operator]);
		emit_c_node(&un.operand, tree);
	}
	if (needsParen) fprintf(fp,")");
}

static inline void emit_c_callExpression(astNode *n, ast *tree) {
	callExpressionAst cn = *(callExpressionAst*)n->data;

	fprintf(fp,"printf(");
	for (int i= 0; i < cn.argumentCount; i++) {
		emit_c_node(cn.arguments + i, tree);
		if (cn.arguments[i].type == boolType) fprintf(fp, " ? \"true\" : \"false\"");
		if (i != cn.argumentCount-1) fprintf(fp,", ");
	}
	fprintf(fp,")");
}

static inline void emit_c_castExpression(astNode *n, ast *tree) {
	astNode cn = *(astNode*)n->data;
	fprintf(fp,"((%s)", cTypeText[n->type]);
	emit_c_node(&cn, tree);
	fprintf(fp,")");
}

static inline void emit_c_variableDeclaration(astNode *n, ast *tree) {

	variableDeclarationAst dn = *(variableDeclarationAst*)n->data;

	fprintf(fp,"%s %s = ", cTypeText[dn.variable->type], dn.variable->name);
	emit_c_node(&dn.initalizer, tree);
}

static inline void emit_c_variableAssignment(astNode *n, ast *tree) {

	variableAssignmentAst an = *(variableAssignmentAst*)n->data;

	fprintf(fp,"%s %s= ", an.variable->name, an.compoundOperator == 0 ? "" : cBinaryText[an.compoundOperator]);
	emit_c_node(&an.expression, tree);
}

static inline void emit_c_variableReference(astNode *n, ast *tree) {
	fprintf(fp, "%s", ((astSymbol*) n->data)->name);
}
