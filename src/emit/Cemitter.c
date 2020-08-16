void emit_c_node(astNode *n, ast *tree);
static inline void emit_c_file(astNode *n, ast *tree);
static inline void emit_c_functions_in_block(astNode *n, ast *tree, bool isNamespace);
static inline void emit_c_function(astNode *n, ast *tree);
static inline void emit_c_blockStatement(astNode *n, ast *tree);
static inline void emit_c_returnStatement(astNode *n, ast *tree);
static inline void emit_c_ifStatement(astNode *n, ast *tree);
static inline void emit_c_caseStatement(astNode *n, ast *tree);
static inline void emit_c_switchStatement(astNode *n, ast *tree);
static inline void emit_c_whileLoop(astNode *n, ast *tree);
static inline void emit_c_forLoop(astNode *n, ast *tree);

static inline void emit_c_literal(astNode *n, ast *tree);
static inline void emit_c_array_literal(astNode *n, ast *tree);
static inline void emit_c_binaryExpression(astNode *n, ast *tree);
static inline void emit_c_ternaryExpression(astNode *n, ast *tree);
static inline void emit_c_unaryExpression(astNode *n, ast *tree);
static inline void emit_c_callExpression(astNode *n, ast *tree);
static inline void emit_c_arrayAccess(astNode *n, ast *tree);
static inline void emit_c_constructorExpression(astNode *n, ast *tree);
static inline void emit_c_castExpression(astNode *n, ast *tree);
static inline void emit_c_variableDeclaration(astNode *n, ast *tree);
static inline void emit_c_enumDeclaration(astNode *n, ast *tree);
static inline void emit_c_structDeclaration(astNode *n, ast *tree);
static inline void emit_c_variableAssignment(astNode *n, ast *tree);
static inline void emit_c_variableReference(astNode *n, ast *tree);
static inline void emit_c_structReference(astNode *n, ast *tree);
char* escape_string_c(char *str);

int c_indent=0;
enum astSyntaxKind kindStack[128];
u8 kindIndex = 0;
enum astSyntaxKind parentKind;

enum astSyntaxKind needs_indent(enum astSyntaxKind k) {
	return (
		k == blockStatementKind ||
		k == caseStatementKind  );
}

enum astSyntaxKind needs_semicolon(enum astSyntaxKind k) {
	return !(
		k == blockStatementKind ||
		k == forLoopKind        ||
		k == whileLoopKind      ||
		k == ifStatementKind    ||
		k == caseStatementKind  );
}
inline static void kindStack_push(enum astSyntaxKind k) {
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
	"float",          // float
	"int",		      // bool
	"char*",          // string
	"char",           // char
	"enum",           // enum
	"struct",         // struct
	"",         	  // array
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

void print_c_type(astType t, astSymbol *identifier) {

	bool isReference = identifier != NULL && (identifier->flags & VARIABLE_REFERENCE);

	switch(t.kind) {
		case enumType:
			fprintf(fp, "%s ", cTypeText[t.kind]);
			printfSymbolReference(fp, t.declaration, "_");
			break;
		case structType:
			fprintf(fp, "%s ", cTypeText[t.kind]);
			printfSymbolReference(fp, t.declaration, "_");
			break;
		case arrayType:
			print_c_type(t.arrayInfo->ofType, NULL);
			if (identifier != NULL) {
				fprintf(fp, " ");
				if (isReference) fprintf(fp, "*");
				printfSymbolReference(fp, identifier, "_");
			}
			fprintf(fp, "[");
			u16 capacity = t.arrayInfo->capacity;
			if (capacity) fprintf(fp, "%d", capacity);
			fprintf(fp, "]");
			return;

		case voidType:
		case intType: case u8Type: case u16Type: case u32Type: case u64Type:
		case i8Type: case i16Type: case i32Type: case i64Type:
		case floatType: case boolType: case stringType: case charType:
			fprintf(fp, "%s", cTypeText[t.kind]); break;
		default: panic("Unhandled type %s in Cemitter -> print_c_type", astKindText[t.kind]);
	}

	if (identifier != NULL) {
		fprintf(fp, " ");
		if (isReference) fprintf(fp, "*");
		printfSymbolReference(fp, identifier, "_");
	}
}

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
	case externDeclarationKind: break;
	case fileStatementKind:
	case blockStatementKind: emit_c_blockStatement(n, tree); break;
	case returnStatementKind: emit_c_returnStatement(n, tree); break;
	case ifStatementKind: emit_c_ifStatement(n, tree); break;
	case caseStatementKind: emit_c_caseStatement(n, tree); break;
	case switchStatementKind: emit_c_switchStatement(n, tree); break;
	case whileLoopKind: emit_c_whileLoop(n, tree); break;
	case forLoopKind: emit_c_forLoop(n, tree); break;

	case breakKind: fprintf(fp, "break;\n"); break;
	case continueKind: fprintf(fp, "continue;\n"); break;

	case literalKind: emit_c_literal(n, tree); break;
	case arrayLiteralKind: emit_c_array_literal(n, tree); break;
	case binaryExpressionKind: emit_c_binaryExpression(n, tree); break;
	case ternaryExpressionKind: emit_c_ternaryExpression(n, tree); break;
	case unaryExpressionKind: emit_c_unaryExpression(n, tree); break;
	case callExpressionKind: emit_c_callExpression(n, tree); break;
	case arrayAccessKind: emit_c_arrayAccess(n, tree); break;
	case constructorExpressionKind: emit_c_constructorExpression(n, tree); break;
	case castExpressionKind: emit_c_castExpression(n, tree); break;
	case variableDeclarationKind: emit_c_variableDeclaration(n, tree); break;
	case variableAssignmentKind: emit_c_variableAssignment(n, tree); break;
	case variableReferenceKind: emit_c_variableReference(n, tree); break;
	case structReferenceKind: emit_c_structReference(n, tree); break;
	default: panic("Unhandled node of type %s in Cemitter -> emit_c_node", astSyntaxKindText[n->kind]);
	}
	kindStack_pop();
}

void emit_c_file(astNode *n, ast *tree) {
	fprintf(fp,"#include <stdio.h>\n#include <string.h>\n");
	for (int i = 0;i<sb_count(tree->externalLibraries);i++) {
		fprintf(fp, "#include <%s.h>\n", tree->externalLibraries[i]);
	}
	fprintf(fp, "\n");

	emit_c_functions_in_block(n, tree, false);
	fprintf(fp,"void main() ");
	emit_c_node(n,tree);
}

static inline void emit_c_functions_in_block(astNode *n, ast *tree, bool isNamespace) {
	blockStatementAst bn = *(blockStatementAst*)n->data;
	for (int i= 0; i < bn.statementsCount; i++) {
		enum astSyntaxKind kind = bn.statements[i].kind;
		if (kind == namespaceDeclarationKind) {
			astNode *b = &((namespaceAst*)bn.statements[i].data)->block;
			emit_c_functions_in_block(b, tree, true);
		} else if (kind == structDeclarationKind) {
			emit_c_structDeclaration(bn.statements + i, tree);
			fprintf(fp, ";\n");
		} else if (kind == variableDeclarationKind && isNamespace) {
			emit_c_variableDeclaration(bn.statements + i, tree);
			fprintf(fp, ";\n");
		} else if (kind == enumDeclarationKind) {
			emit_c_enumDeclaration(bn.statements + i, tree);
			fprintf(fp, ";\n");
		} else if (kind == functionDeclarationKind) {
			emit_c_function(bn.statements + i, tree);
			fprintf(fp,"\n");
		}
	}
}

static inline void emit_c_function(astNode *n, ast *tree) {
	astSymbol *vn = (astSymbol*)n->data;
	functionSymbolData *fd = vn->functionData;
	blockStatementAst *bn = (blockStatementAst*)fd->body.data;

	// emit local functions
	for (int i= 0; i < bn->statementsCount; i++) {
		if (bn->statements[i].kind != functionDeclarationKind) continue;
		emit_c_function(bn->statements + i, tree);
		fprintf(fp,"\n");
	}

	print_c_type(vn->functionData->returnType, vn);
	fprintf(fp,"(");
	for (int i=0;i<fd->parameterCount;i++) {

		astType t = fd->parameters[i]->type;

		print_c_type(t, fd->parameters[i]);

		fprintf (fp, "%s", i == fd->parameterCount-1?"":", ");
	}
	fprintf(fp, ") ");

	if (fd->body.kind) emit_c_node(&fd->body,tree);
}

static inline void emit_c_blockStatement(astNode *n, ast *tree) {
	fprintf(fp,"%*s{\n", c_indent-4, "");
	blockStatementAst bn = *(blockStatementAst*)n->data;
	for (int i= 0; i < bn.statementsCount; i++) {
		enum astSyntaxKind kind = bn.statements[i].kind;
		if (kind == functionDeclarationKind || kind == namespaceDeclarationKind || kind == enumDeclarationKind || kind == structDeclarationKind || kind == typeDeclarationKind) continue;
		fprintf(fp,"%*s", c_indent, "");
		emit_c_node(bn.statements + i, tree);
		if(needs_semicolon(bn.statements[i].kind)) fprintf(fp,";\n");
	}
	fprintf(fp,"%*s}\n", c_indent-4, "");
}

static inline void emit_c_returnStatement(astNode *n, ast *tree) {
	fprintf(fp,"return ");
	emit_c_node((astNode*)n->data, tree);
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
		emit_c_node(&in.elseStatement, tree);
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
					if (cb.condition.type.kind == charType) fprintf(fp,"%*scase '%c': ",c_indent,"", i);
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
	fprintf(fp,")%c", wn.block.kind == blockStatementKind ? '\n' : ' ');
	emit_c_node(&wn.block, tree);
	if (needs_semicolon(wn.block.kind)) fprintf(fp, ";\n");
}

static inline void emit_c_forLoop(astNode *n, ast *tree) {
	forLoopAst fn = *(forLoopAst*)n->data;

	enum astKind forKind = fn.value->type.kind;
	if (forKind == enumType) forKind = intType;

	if (fn.range.kind != rangeExpressionKind) {
		char* indexName = fn.index == 0 ? "_kat__index" : fn.index->name;

		fprintf(fp,"for (int %s = 0; %s <= ", indexName, indexName);
		if (fn.range.type.kind == arrayType) {
			fprintf(fp, "%d", fn.range.type.arrayInfo->capacity-1);
		} else if (fn.range.type.kind == stringType) {
			fprintf(fp, "strlen(");
			emit_c_node(&fn.range, tree);
			fprintf(fp, ")");
		} else {
			emit_c_node(&fn.range, tree);
		}
		fprintf(fp, "; %s++) {\n", indexName);
		kindStack_push(blockStatementKind);

		fprintf(fp,"%*s", c_indent, "");
		// for x in nums
		// x = nums[i]
		// TODO: cache the "range" node so it doesn't get recalculated on every iteration
		if (fn.range.type.kind == arrayType || fn.range.type.kind == stringType) {
			fprintf(fp,"%s %s = ", cTypeText[forKind], fn.value->name);
			emit_c_node(&fn.range, tree);
			fprintf(fp,"[%s];\n", indexName );
		}
		else fprintf(fp,"%s %s = %s;\n", cTypeText[forKind], fn.value->name, indexName);
		

		if (fn.block.kind != blockStatementKind) fprintf(fp,"%*s", c_indent, "");
		emit_c_node(&fn.block, tree);
		if (needs_semicolon(fn.block.kind)) fprintf(fp,";\n");
		kindStack_pop();
		fprintf(fp,"%*s", c_indent, "");
		fprintf(fp,"}\n");
	} else {
		rangeExpressionAst rn = *(rangeExpressionAst*)fn.range.data;
		if (fn.index == 0) {
			char* compOp = rn.toInt > rn.fromInt ? "<=" : ">=";
			char* incOp = rn.toInt > rn.fromInt ? "++" : "--";

			fprintf(fp,"for (");
			// int x = 1; 
			fprintf(fp,"%s %s = %d; ", cTypeText[forKind], fn.value->name, rn.fromInt);
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
			if (rn.toInt > rn.fromInt) fprintf(fp,"%s %s = i + %d;\n", cTypeText[forKind], fn.value->name, rn.fromInt);
			else fprintf(fp,"%s %s = %d - i;\n",cTypeText[forKind], fn.value->name, rn.fromInt);

			if (fn.block.kind != blockStatementKind) fprintf(fp,"%*s", c_indent, "");
			emit_c_node(&fn.block, tree);
			if (needs_semicolon(fn.block.kind)) fprintf(fp,";\n");
			kindStack_pop();
			fprintf(fp,"%*s", c_indent, "");
			fprintf(fp,"}\n");
		}
	}
}

static inline void emit_c_literal(astNode *n, ast *tree) {
	switch (n->type.kind) {
	case u8Type: case u16Type: case u32Type: case u64Type: case i8Type: case i16Type: case i32Type: case i64Type:
	case intType: fprintf(fp,"%d", n->numValue); break;
	case floatType: fprintf(fp,"%f", n->floatValue); break;
	case boolType: fprintf(fp,"%s", n->boolValue ? "1" : "0"); break;
	case stringType: {
		char *escapedStr = escape_string_c(n->stringValue);
		fprintf(fp,"\"%s\"", escapedStr); break;
		free(escapedStr);
	}
	case charType: fprintf(fp,"'%c'", n->charValue); break;
	case enumType: printfSymbolReference(fp, n->type.declaration->namespaceScope->symbols[n->numValue], "_"); break;
	case arrayType:
	case structType: fprintf(fp,"{0}"); break;
	default: panic("Unhandled type %s in Cemitter -> emit_c_literal", astKindText[n->type.kind]);
	}
}

static inline void emit_c_array_literal(astNode *n, ast *tree) {
	astNode* nodes = n->arrayValues;
	u16 capacity = n->type.arrayInfo->capacity;

	fprintf(fp, "{ ");
	for(int i=0;i<capacity;i++) {
		emit_c_node(&nodes[i], tree);
		fprintf(fp, ", ");
	}
	fprintf(fp, "}");
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

static inline void emit_c_ternaryExpression(astNode *n, ast *tree) {
	ternaryExpressionAst *tn = (ternaryExpressionAst*)n->data;
	bool needsParen = parentKind == binaryExpressionKind || parentKind == unaryExpressionKind;
	if (needsParen) fprintf(fp,"(");
	emit_c_node(&tn->condition, tree);
	if (needsParen) fprintf(fp,")");
	fprintf(fp," ? ");
	emit_c_node(&tn->thenExpression, tree);
	fprintf(fp," : ");
	emit_c_node(&tn->elseExpression, tree);
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
	callExpressionAst *cn = (callExpressionAst*)n->data;

	char* name = cn->function->name;

	functionSymbolData *fd = NULL;
	bool isPrint = (!strcmp(name, "print"));
	if (isPrint) fprintf(fp, "printf");
	else {
		printfSymbolReference(fp, cn->function, "_");
		fd = cn->function->functionData;
	}
	fprintf(fp,"(");

	for (int i= 0; i < cn->argumentCount; i++) {
		if (cn->arguments[i].type.kind == enumType) {
			if (isPrint) {
				printfSymbolReference(fp, cn->arguments[i].type.declaration, "_");
				fprintf(fp, "__TEXT[");
				emit_c_node(cn->arguments + i, tree);
				fprintf(fp, "]");
			} else {
				emit_c_node(cn->arguments + i, tree);
			}
		} else {
			bool isReference = fd != NULL && fd->parameters[i]->flags & VARIABLE_REFERENCE;
			if (isReference) fprintf(fp, "&(");
			emit_c_node(cn->arguments + i, tree);
			if (cn->arguments[i].type.kind == boolType) fprintf(fp, " ? \"true\" : \"false\"");
			if (isReference) fprintf(fp, ")");
		}
		if (i != cn->argumentCount-1) fprintf(fp,", ");
	}
	fprintf(fp,")");
}

static inline void emit_c_arrayAccess(astNode *n, ast *tree) {
	arrayAccessAst *an = (arrayAccessAst*)n->data;

	emit_c_node(&an->left, tree);
	fprintf(fp, "[");
	emit_c_node(&an->index, tree);
	fprintf(fp, "]");
}

static inline void emit_c_constructorExpression(astNode *n, ast *tree) {
	callExpressionAst *cn = (callExpressionAst*)n->data;

	char* name = cn->function->name;

	fprintf(fp,"((struct ");
	printfSymbolReference(fp, cn->function, "_");

	fprintf(fp,"){");

	for (int i= 0; i < cn->argumentCount; i++) {
		emit_c_node(cn->arguments + i, tree);
		if (cn->arguments[i].type.kind == boolType) fprintf(fp, " ? \"true\" : \"false\"");
		if (i != cn->argumentCount-1) fprintf(fp,", ");
	}
	fprintf(fp,"})");
}

static inline void emit_c_castExpression(astNode *n, ast *tree) {
	astNode cn = *(astNode*)n->data;
	fprintf(fp,"((%s)", cTypeText[n->type.kind]);
	emit_c_node(&cn, tree);
	fprintf(fp,")");
}

static inline void emit_c_variableDeclaration(astNode *n, ast *tree) {

	variableDeclarationAst *dn = (variableDeclarationAst*)n->data;

	if (feature_constantfolding && !(dn->variable->flags & VARIABLE_MUTABLE) && dn->variable->flags & VARIABLE_VALUE_KNOWN) return;

	print_c_type(dn->variable->type, dn->variable);

	if (dn->initalizer.kind != missingKind && parentKind != structDeclarationKind) {
		fprintf(fp," = ");
		emit_c_node(&dn->initalizer, tree);
	}
}

static inline void emit_c_enumDeclaration(astNode *n, ast *tree) {

	astSymbol *en = (astSymbol*)n->data;

	print_c_type(en->type, NULL);

	fprintf (fp, " { ");
	int count = sb_count(en->namespaceScope->symbols);
	for (int i=0;i<count;i++) {
		printfSymbolReference(fp, en->namespaceScope->symbols[i], "_");
		fprintf(fp, "%s ", i==count-1?"":",");
	}
	fprintf(fp, "};\n");


	fprintf(fp, "static const char * ");
	printfSymbolReference(fp, en, "_");
	fprintf(fp, "__TEXT[] = {");
	for (int i=0;i<count;i++) {
		fprintf(fp, "\"%s\"%s", en->namespaceScope->symbols[i]->name, i==count-1?"":",");
	}
	fprintf(fp, "};\n");
}

static inline void emit_c_structDeclaration(astNode *n, ast *tree) {


	kindStack_push(structDeclarationKind);

	structAst *sn = (structAst*)n->data;

	print_c_type(sn->structSymbol->type, NULL);
	fprintf (fp, " {\n");

	blockStatementAst bn = *(blockStatementAst*)sn->block.data;
	for (int i= 0; i < bn.statementsCount; i++) {
		emit_c_node(bn.statements + i, tree);
		fprintf (fp, ";\n");
	}
	fprintf(fp, "}");
	kindStack_pop();
}

static inline void emit_c_variableAssignment(astNode *n, ast *tree) {

	variableAssignmentAst *an = (variableAssignmentAst*)n->data;

	emit_c_node(&an->variable, tree);
	fprintf(fp," %s= ", an->compoundOperator == 0 ? "" : cBinaryText[an->compoundOperator]);
	emit_c_node(&an->expression, tree);
}

static inline void emit_c_variableReference(astNode *n, ast *tree) {
	astSymbol *varSymbol = (astSymbol*)n->data;
	bool isReference = varSymbol->flags & VARIABLE_REFERENCE;

	if (isReference) fprintf(fp, "(*");
	printfSymbolReference(fp, varSymbol, "_");
	if (isReference) fprintf(fp, ")");
}

static inline void emit_c_structReference(astNode *n, ast *tree) {

	structReferenceAst *sn = (structReferenceAst*)n->data;
	emit_c_node(&sn->left, tree);
	fprintf(fp, ".");
	emit_c_node(&sn->right, tree);
}