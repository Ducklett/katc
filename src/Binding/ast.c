enum astKind {
	missingKind,
	errorKind,
	literalKind,
	unaryExpressionKind,
	binaryExpressionKind,
	rangeExpressionKind,
	callExpressionKind,
	castExpressionKind,
	namespaceDeclarationKind,
	functionDeclarationKind,
	variableDeclarationKind,
	variableAssignmentKind,
	variableReferenceKind,
	fileStatementKind,
	blockStatementKind,
	ifStatementKind,
	caseStatementKind,
	caseBranchKind,
	switchStatementKind,
	switchBranchKind,
	whileLoopKind,
	forLoopKind,
	breakKind,
	continueKind,
	jumpKind,
};

static const char *astKindText[] = {
	"missing",
	"error",
	"literal",
	"unaryExpression",
	"binaryExpression",
	"rangeExpression",
	"callExpression",
	"castExpression",
	"namespaceDeclaration",
	"functionDeclaration",
	"variableDeclaration",
	"variableAssignment",
	"variableReference",
	"fileStatement",
	"blockStatement",
	"ifStatement",
	"caseStatement",
	"caseBranch",
	"switchStatement",
	"switchBranch",
	"whileLoop",
	"forLoop",
	"break",
	"continue",
	"jump",
};

enum astType {
	errorType,
	unresolvedType,
	voidType,
	intType,
	u8Type,
	u16Type,
	u32Type,
	u64Type,
	i8Type,
	i16Type,
	i32Type,
	i64Type,
	boolType,
	stringType,
	charType,
};

static const char *astTypeText[] = {
	"errorType",
	"unresolved",
	"void",
	"int",
	"u8",
	"u16",
	"u32",
	"u64",
	"i8",
	"i16",
	"i32",
	"i64",
	"bool",
	"string",
	"char",
};

bool isNumberType(enum astType t) {
	return (
		t == intType ||
		t == u8Type  ||
		t == u16Type ||
		t == u32Type ||
		t == u64Type ||
		t == i8Type  ||
		t == i16Type ||
		t == i32Type ||
		t == i64Type );
}

#define CAST_IDENTITY 0
#define CAST_IMPLICIT 1
#define CAST_EXPLICIT 2
#define CAST_ILLEGAL  3
u8 getCastInformation(enum astType from, enum astType to) {
	if (from == to) return CAST_IDENTITY;

	if (to == stringType || (from == stringType && to != boolType)) return CAST_ILLEGAL;

	if (from == intType && isNumberType(to)) return CAST_IMPLICIT;

	return CAST_EXPLICIT;
}

enum astBinaryOperator {
	missingBinaryOp,
	addOp,
	subtractOp,
	multiplyOp,
	divideOp,
	moduloOp,

	equalOp,
	inEqualOp,
	lessOp,
	greaterOp,
	lessOrEqualOp,
	greaterOrEqualOp,

	shiftLeftOp,
	shiftRightOp,
	bitwiseAndOp,
	bitwiseXorOp,
	bitwiseOrOp,

	logicalAndOp,
	logicalOrOp,
};

static const char *astBinaryText[] = {
	"missingBinary",
	"add",
	"subtract",
	"multiply",
	"divide",
	"modulo",
	"equal",
	"inEqual",
	"less",
	"greater",
	"lessOrEqual",
	"greaterOrEqual",
	"shiftLeft",
	"shiftRight",
	"bitwiseAnd",
	"bitwiseXor",
	"bitwiseOr",
	"logicalAnd",
	"logicalOr",
};

typedef struct typedOperator {
	enum astBinaryOperator operator;
	enum astType type;
} typedOperator;

typedOperator get_binary_operator(enum syntaxKind operatorToken, enum astType left, enum astType right) {
	if (operatorToken == plusOperator          && left == charType && isNumberType(right)) return (typedOperator){ addOp, charType };
	if (operatorToken == minusOperator         && left == charType && isNumberType(right)) return (typedOperator){ subtractOp, charType };
	if (operatorToken == euqualsEqualsOperator && left == charType && right == charType) return (typedOperator) { equalOp, boolType };
	if (operatorToken == bangEqualsOperator    && left == charType && right == charType) return (typedOperator){ inEqualOp, boolType };
	if (operatorToken == lessOperator          && left == charType && right == charType) return (typedOperator){ lessOp, boolType };
	if (operatorToken == greaterOperator       && left == charType && right == charType) return (typedOperator){ greaterOp, boolType };
	if (operatorToken == lessEqualsOperator    && left == charType && right == charType) return (typedOperator){ lessOrEqualOp, boolType };
	if (operatorToken == greaterEqualsOperator && left == charType && right == charType) return (typedOperator){ greaterOrEqualOp, boolType };

	if (operatorToken == plusOperator          && isNumberType(left) && isNumberType(right)) return (typedOperator){ addOp, left };
	if (operatorToken == minusOperator         && isNumberType(left) && isNumberType(right)) return (typedOperator){ subtractOp, left };
	if (operatorToken == multipliationOperator && isNumberType(left) && isNumberType(right)) return (typedOperator){ multiplyOp, left };
	if (operatorToken == divisionOperator      && isNumberType(left) && isNumberType(right)) return (typedOperator){ divideOp, left };
	if (operatorToken == modulusOperator       && isNumberType(left) && isNumberType(right)) return (typedOperator){ moduloOp, left };

	if (operatorToken == euqualsEqualsOperator && isNumberType(left) && right == left) return (typedOperator) { equalOp, boolType };
	if (operatorToken == bangEqualsOperator    && isNumberType(left) && right == left) return (typedOperator){ inEqualOp, boolType };

	if (operatorToken == euqualsEqualsOperator && left == boolType && right == boolType) return (typedOperator) { equalOp, boolType };
	if (operatorToken == bangEqualsOperator    && left == boolType && right == boolType) return (typedOperator){ inEqualOp, boolType };

	if (operatorToken == lessOperator          && isNumberType(left) && right == left) return (typedOperator){ lessOp, boolType };
	if (operatorToken == greaterOperator       && isNumberType(left) && right == left) return (typedOperator){ greaterOp, boolType };
	if (operatorToken == lessEqualsOperator    && isNumberType(left) && right == left) return (typedOperator){ lessOrEqualOp, boolType };
	if (operatorToken == greaterEqualsOperator && isNumberType(left) && right == left) return (typedOperator){ greaterOrEqualOp, boolType };

	if (operatorToken == lessLessOperator       && isNumberType(left) && isNumberType(right)) return (typedOperator){ shiftLeftOp, left };
	if (operatorToken == greaterGreaterOperator && isNumberType(left) && isNumberType(right)) return (typedOperator){ shiftRightOp, left };

	if (operatorToken == ampersandOperator && isNumberType(left) && right == left) return (typedOperator){ bitwiseAndOp, left };
	if (operatorToken == caretOperator     && isNumberType(left) && right == left) return (typedOperator){ bitwiseXorOp, left };
	if (operatorToken == pipeOperator      && isNumberType(left) && right == left) return (typedOperator){ bitwiseOrOp, left };

	if (operatorToken == ampersandOperator && left == boolType && right == boolType) return (typedOperator){ bitwiseAndOp, boolType };
	if (operatorToken == caretOperator && left == boolType && right == boolType) return (typedOperator){ bitwiseXorOp, boolType };
	if (operatorToken == pipeOperator && left == boolType && right == boolType) return (typedOperator){ bitwiseOrOp, boolType };

	if (operatorToken == ampersandAmpersandOperator && left == boolType && right == boolType) return (typedOperator){ logicalAndOp, boolType };
	if (operatorToken == pipePipeOperator && left == boolType && right == boolType) return (typedOperator) { logicalOrOp, boolType };

	return (typedOperator){0};
}

enum astUnaryOperator {
	missingUnaryOp,
	logicalNegationOp,
	bitwiseNegationOp,
	negationOp,
	identityOp,
	preIncrementOp,
	preDecrementOp,
	postIncrementOp,
	postDecrementOp,
};

static const char *astUnaryText[] = {
	"missingUnary",
	"logicalNegation",
	"bitwiseNegation",
	"negation",
	"identity",
	"preIncrement",
	"preDecrement",
	"postIncrement",
	"postDecrement",
};

enum astUnaryOperator get_unary_operator(enum syntaxKind operatorToken, enum astType type, bool left) {
	if (type == charType && operatorToken == plusPlusOperator && left) return preIncrementOp;
	if (type == charType && operatorToken == plusPlusOperator && !left) return postIncrementOp;
	if (type == charType && operatorToken == minusMinusOperator && left) return preDecrementOp;
	if (type == charType && operatorToken == minusMinusOperator && !left) return postDecrementOp;

	if (type == intType && operatorToken == plusPlusOperator && left) return preIncrementOp;
	if (type == intType && operatorToken == plusPlusOperator && !left) return postIncrementOp;
	if (type == intType && operatorToken == minusMinusOperator && left) return preDecrementOp;
	if (type == intType && operatorToken == minusMinusOperator && !left) return postDecrementOp;
	if (type == intType && operatorToken == tildeOperator && left) return bitwiseNegationOp;

	if (type == intType && operatorToken == plusOperator && left) return identityOp;
	if (type == intType && operatorToken == minusOperator && left) return negationOp;
	if (type == boolType && operatorToken == bangOperator && left) return logicalNegationOp;

	return missingUnaryOp;
}

typedef struct astNode {
	enum astKind kind;
	enum astType type;
	union {
		void* data; 
		int numValue; 
		bool boolValue; 
		char* stringValue; 
		char charValue; 
	};
} astNode;

#define VARIABLE_MUTABLE 1
#define VARIABLE_INITIALIZED 2
#define VARIABLE_VALUE_KNOWN 4

#define SYMBOL_VARIABLE 1
#define SYMBOL_FUNCTION 2
#define SYMBOL_NAMESPACE 3

struct astSymbol;
struct scope;

typedef struct functionSymbolData {
	struct astSymbol **parameters;
	u16 parameterCount;
	enum astType returnType;
	astNode body;
} functionSymbolData;

typedef struct astSymbol {
	char* name;
	struct astSymbol *parentNamespace;
	enum astType type;
	u8 symbolKind;
	u8 flags;
	union {
		void* data; 
		int numValue; 
		bool boolValue; 
		char* stringValue; 
		char charValue; 
		functionSymbolData *functionData;
		struct scope *namespaceScope;
	};
} astSymbol;

typedef struct unaryExpressionAst {
	enum astUnaryOperator operator;
	astNode operand;
} unaryExpressionAst;

typedef struct binaryExpressionAst {
	enum astBinaryOperator operator;
	astNode left;
	astNode right;
} binaryExpressionAst;

// TODO: range expression should eventually also support identifiers for start and end values
typedef struct rangeExpressionAst {
	union {
		i16 fromInt;
		char fromChar;
	};
	union {
		i16 toInt;
		char toChar;
	};
} rangeExpressionAst;

typedef struct callExpressionAst {
	astSymbol *function;
	astNode *arguments;
	u8 argumentCount;
} callExpressionAst;

typedef struct variableDeclarationAst {
	astSymbol* variable;
	astNode initalizer;
} variableDeclarationAst;

typedef struct variableAssignmentAst {
	astSymbol* variable;
	astNode expression;
	enum astBinaryOperator compoundOperator;
} variableAssignmentAst;

typedef struct blockStatementAst {
	astNode* statements;
	u16 statementsCount;
} blockStatementAst;

typedef struct ifStatementAst {
	astNode condition;
	astNode thenStatement;
	astNode elseStatement;		// optional
} ifStatementAst;

typedef struct caseBranchAst {
	astNode condition;
	astNode thenStatement;
} caseBranchAst;

typedef struct caseStatementAst {
	astNode* branches;
	u16 branchCount;
} caseStatementAst;

typedef struct switchBranchAst {
	astNode condition;
	astNode thenStatement;
} switchBranchAst;

typedef struct switchStatementAst {
	astNode target;
	astNode* branches;
	u16 branchCount;
} switchStatementAst;

typedef struct whileLoopAst {
	astNode condition;
	astNode block;
} whileLoopAst;

typedef struct forLoopAst {
	astSymbol* value;
	astSymbol* index;
	astNode range;
	astNode block;
} forLoopAst;

typedef struct namespaceAst {
	astSymbol* namespace;
	astNode block;
} namespaceAst;

typedef struct jumpAst {

} jumpAst;

typedef struct scope {
	struct scope* parentScope;
	astSymbol **symbols;
} scope;

typedef struct ast {
	char* text;
	u64 length;
	diagnosticContainer diagnostics;
	astNode root;
	scope **scopes;
	scope *currentScope;
	astSymbol *currentNamespace;
} ast;

enum astType resolve_type_from_span(ast *tree, textspan span) {
	char *text = tree->text;
	for (int i = intType; i<= charType; i++)
		if (span_compare(text, span, astTypeText[i])) return i;
	
	report_diagnostic(&tree->diagnostics, unresolvedTypeDiagnostic, span, 0, 0, 0);
	return 0;
}

int bind_tree(ast* tree, node *root);

int create_ast(const char* filename, ast *tree, parser *p, bool parseOnly) {
	{
		benchmark_start();
		tree->text = read_file(filename, &tree->length);
		benchmark_end("File read");
	}

	parser_arena = arena_create();
	if (parser_arena == NULL) panic("memory allocation for parser_arena failed\n");

	if (!create_syntaxtree(tree->text, tree->length, p, &tree->diagnostics)) {
		return 0;
	}

	if (parseOnly) return 1;

	binder_arena = arena_create();
	if (binder_arena == NULL) panic("memory allocation for binder_arena failed\n");
	return bind_tree(tree, &p->root);
}

// print the full path of a symbol reference
// foo.bar.baz()
void printfSymbolReference_internal(FILE *f, astSymbol *s, char* separator, bool entry);
void printfSymbolReference(FILE *f, astSymbol *s, char* separator) { return printfSymbolReference_internal(f, s, separator, true); }
void printfSymbolReference_internal(FILE *f, astSymbol *s, char* separator, bool entry) {
	if (s->parentNamespace != NULL) {
		printfSymbolReference_internal(f, s->parentNamespace, separator, false);
	}
	fprintf(f, "%s%s", s->name, entry ? "" : separator);
}
void print_ast_internal(char *text, astNode *root, int indent, bool verbose, bool newline);
void print_ast(char *text, astNode *root, int indent, bool verbose) { print_ast_internal(text, root, indent, verbose, true); }
void print_ast_internal(char *text, astNode *root, int indent, bool verbose, bool newline) {

	if (root->kind == literalKind) {
		if (root->type == voidType) return;
		if (verbose) {
			printf ("%*s(", indent, "");
			printf(TERMBLUE);
			switch (root->type) {
				case u8Type: case u16Type: case u32Type: case u64Type: case i8Type: case i16Type: case i32Type: case i64Type:
				case intType: printf ("%d", root->numValue); break;
				case boolType: printf ("%s", root->boolValue ? "true" : "false"); break;
				case stringType: printf ("\"%s\"", root->stringValue); break;
				case charType: printf ("'%c'", root->charValue); break;
				default:
					fprintf(stderr, "%sUnhandled type '%s' in print_ast%s", TERMRED, astTypeText[root->type], TERMRESET);
					exit(1);
			}
			printf ("%s :: %s%s%s)%s", TERMRESET, TERMYELLOW, astTypeText[root->type], TERMRESET, newline?"\n":"");
		} else {
			printf(TERMCYAN);
			switch (root->type) {
				case u8Type: case u16Type: case u32Type: case u64Type: case i8Type: case i16Type: case i32Type: case i64Type:
				case intType: printf ("%*s%d%s", indent, "", root->numValue, newline?"\n":""); break;
				case boolType: printf ("%*s%s%s", indent, "", root->boolValue?"true":"false", newline?"\n":""); break;
				case stringType: printf ("%*s\"%s\"%s", indent, "", root->stringValue, newline?"\n":""); break;
				case charType: printf ("%*s%c%s", indent, "", root->charValue, newline?"\n":""); break;
				default:
					fprintf(stderr, "%sUnhandled type '%s' in print_ast%s", TERMRED, astTypeText[root->type], TERMRESET);
					exit(1);
			}
			printf(TERMRESET);
		}
		return;
	}

	printf ("%*s(%s\n", indent, "", astKindText[root->kind]);

	indent += 4;

	switch(root->kind) {
	case fileStatementKind:
	case blockStatementKind: {
		blockStatementAst bn = *(blockStatementAst*)root->data;

		for(int i=0;i<bn.statementsCount;i++) {
			print_ast_internal(text, &bn.statements[i], indent, verbose, i!=bn.statementsCount-1);
		}
		break;
	}
	case ifStatementKind: {
		ifStatementAst in = *(ifStatementAst*)root->data;
		print_ast_internal(text, &in.condition, indent, verbose, true);
		bool hasElse = in.elseStatement.kind != 0;
		print_ast_internal(text, &in.thenStatement, indent, verbose, hasElse);
		if (hasElse) print_ast_internal(text, &in.elseStatement, indent, verbose, false);
		break;
	}
	case caseBranchKind: {
		caseBranchAst cn = *(caseBranchAst*)root->data;
		if (cn.condition.kind==0) {
			printf ("%*s%s%s%s\n", indent, "", TERMMAGENTA, "default", TERMRESET);
		} else {
			print_ast_internal(text, &cn.condition, indent, verbose, true);
		}
		print_ast_internal(text, &cn.thenStatement, indent, verbose, false);
		break;
	}
	case caseStatementKind: {
		caseStatementAst cn = *(caseStatementAst*)root->data;

		for(int i=0;i<cn.branchCount;i++) {
			print_ast_internal(text, &cn.branches[i], indent, verbose, i!=cn.branchCount-1);
		}
		break;
	}
	case switchBranchKind: {
		switchBranchAst cn = *(switchBranchAst*)root->data;
		if (cn.condition.kind==0) {
			printf ("%*s%s%s%s\n", indent, "", TERMMAGENTA, "default", TERMRESET);
		} else {
			print_ast_internal(text, &cn.condition, indent, verbose, true);
		}
		print_ast_internal(text, &cn.thenStatement, indent, verbose, false);
		break;
	}
	case switchStatementKind: {
		switchStatementAst cn = *(switchStatementAst*)root->data;

		print_ast_internal(text, &cn.target, indent, verbose, cn.branchCount != 0);

		for(int i=0;i<cn.branchCount;i++) {
			print_ast_internal(text, &cn.branches[i], indent, verbose, i!=cn.branchCount-1);
		}
		break;
	}
	case whileLoopKind: {
		whileLoopAst wn = *(whileLoopAst*)root->data;
		print_ast_internal(text, &wn.condition, indent, verbose, true);
		print_ast_internal(text, &wn.block, indent, verbose, false);
		break;
	}
	case forLoopKind: {
		forLoopAst fn = *(forLoopAst*)root->data;

		printf ("%*s%s%s %s%s\n", indent, "", TERMMAGENTA, fn.value->name, astTypeText[fn.value->type], TERMRESET);

		if (fn.index != 0) {
			printf ("%*s%s%s %s%s\n", indent, "", TERMMAGENTA, fn.index->name, astTypeText[fn.index->type], TERMRESET);
		}

		print_ast_internal(text, &fn.range, indent, verbose, true);
		print_ast_internal(text, &fn.block, indent, verbose, false);
		break;
	}
	case rangeExpressionKind: {

		rangeExpressionAst rn = *(rangeExpressionAst*)root->data;
		if (root->type == intType) printf ("%*s%s%d..%d%s", indent, "", TERMMAGENTA, rn.fromInt, rn.toInt, TERMRESET);
		else printf ("%*s%s'%d'..'%d'%s", indent, "", TERMMAGENTA, rn.fromChar, rn.toChar, TERMRESET);
		break;
	}
	case breakKind: break;
	case continueKind: break;
	case unaryExpressionKind: {
		unaryExpressionAst un = *(unaryExpressionAst*)root->data;

		printf ("%*s%s%s%s\n", indent, "", TERMMAGENTA, astUnaryText[un.operator], TERMRESET);
		print_ast_internal(text, &un.operand, indent, verbose, false);
		break;
	}
	case binaryExpressionKind: {
		binaryExpressionAst un = *(binaryExpressionAst*)root->data;

		printf ("%*s%s%s%s\n", indent, "", TERMMAGENTA, astBinaryText[un.operator], TERMRESET);
		print_ast_internal(text, &un.left, indent, verbose, true);
		print_ast_internal(text, &un.right, indent, verbose, false);
		break;
	}
	case callExpressionKind: {
		callExpressionAst cn = *(callExpressionAst*)root->data;

		printf ("%*s%s", indent, "", TERMMAGENTA);
		printfSymbolReference(stdout, cn.function, ".");
		printf ("%s\n", TERMRESET);
		for(int i=0;i<cn.argumentCount;i++) {
			print_ast_internal(text, &cn.arguments[i], indent, verbose, i!=cn.argumentCount-1);
		}
		break;
	}
	case castExpressionKind: {
		astNode en = *(astNode*)root->data;

		printf ("%*s%scast<%s>%s\n", indent, "", TERMMAGENTA, astTypeText[root->type], TERMRESET);
		print_ast_internal(text, &en, indent, verbose, false);
		break;
	}
	case namespaceDeclarationKind: {
		namespaceAst ns = *(namespaceAst*)root->data;

		printf ("%*s%s%s%s\n", indent, "", TERMMAGENTA, ns.namespace->name, TERMRESET);
		print_ast_internal(text, &ns.block, indent, verbose, false);
		break;
	}
	case functionDeclarationKind: {
		astSymbol vn = *(astSymbol*)root->data;
		functionSymbolData fd = *vn.functionData;

		printf("%s", TERMMAGENTA);
		printf ("%*s%s ", indent, "", astTypeText[vn.type]);
		printfSymbolReference(stdout, &vn, ".");
		printf ("(");
		for (int i=0;i<fd.parameterCount;i++) {
			printf (" %s: %s", fd.parameters[i]->name, astTypeText[fd.parameters[i]->type]);
		}
		printf(")%s\n", TERMRESET);
		print_ast_internal(text, &vn.functionData->body, indent, verbose, false);
		break;
	}
	case variableDeclarationKind: {
		variableDeclarationAst vn = *(variableDeclarationAst*)root->data;

		printf ("%*s%s%s %s (%s)%s\n", indent, "", TERMMAGENTA, vn.variable->name, astTypeText[vn.variable->type], (vn.variable->flags & VARIABLE_MUTABLE) ? "mutable" : "constant", TERMRESET);
		if (vn.initalizer.kind != 0) print_ast_internal(text, &vn.initalizer, indent, verbose, false);
		else printf ("%*s%s%s%s", indent, "", TERMMAGENTA, "<uninitialized>", TERMRESET);
		break;
	}
	case variableAssignmentKind: {
		variableAssignmentAst va = *(variableAssignmentAst*)root->data;

		printf ("%*s%s%s %s %s%s\n", indent, "", TERMMAGENTA,va.variable->name, astTypeText[va.variable->type], va.compoundOperator?astBinaryText[va.compoundOperator]:"equals",  TERMRESET);
		print_ast_internal(text, &va.expression, indent, verbose, false);
		break;
	}
	case variableReferenceKind: {
		astSymbol *vs = (astSymbol*)root->data;

		printf ("%*s%s%s ", indent, "", TERMMAGENTA);
		printfSymbolReference(stdout, vs, ".");
		printf ("%s%s", astTypeText[vs->type], TERMRESET);
		break;
	}
	default: {
		fprintf(stderr, "%sERROR: Unhandled case in print_ast for kind %s%s", TERMRED, astKindText[root->kind], TERMRESET);
		exit(1);
		break;
	}
	}

	printf (" )%s", newline?"\n":"");
}

void print_ast_graph_internal(char *text, astNode *root, FILE* fp, bool isRoot);
void print_ast_graph(char *text, astNode *root, FILE* fp) { print_ast_graph_internal(text, root, fp, true); }
void print_ast_graph_internal(char *text, astNode *root, FILE* fp, bool isRoot) {

	if (isRoot) {
		fprintf(fp, "digraph AST {\n");
		char* sourcetext = escape_string_c(text);
		fprintf (fp, "SOURCE[shape=\"box\" label=\"source code:\\n%s\"]\n", sourcetext);
		free(sourcetext);
	}

	if (root->kind == literalKind) {
		switch (root->type) {
			case voidType: fprintf(fp, "Label%p[label=\"%s\"]\n", root, "void"); break;
			case u8Type: case u16Type: case u32Type: case u64Type: case i8Type: case i16Type: case i32Type: case i64Type:
			case intType: fprintf (fp, "Label%p[label=\"%d\"]\n", root, root->numValue); break;
			case boolType: fprintf (fp, "Label%p[label=\"%s\"]\n", root, root->boolValue ? "true" : "false"); break;
			case stringType: {
				char* stringText = escape_string_c(root->stringValue);
				fprintf (fp, "Label%p[label=\"\\\"%s\\\"\"]\n", root, stringText); break;
				free(stringText);
			}
			case charType: fprintf (fp, "Label%p[label=\"'%c'\"]\n", root, root->charValue); break;
			default:
				fprintf(stderr, "%sUnhandled type '%s' in print_ast_graph%s", TERMRED, astTypeText[root->type], TERMRESET);
				exit(1);
		}
		return;
	}

	fprintf (fp, "Label%p[shape=diamond label=\"%s\\n", root, astKindText[root->kind]);
	#define ENDLABEL fprintf (fp, "\"]\n");

	switch(root->kind) {
	case fileStatementKind:
	case blockStatementKind: {
		blockStatementAst *bn = (blockStatementAst*)root->data;
		ENDLABEL
		for(int i=0;i<bn->statementsCount;i++) {
			fprintf (fp, "Label%p -> Label%p\n", root, &bn->statements[i]);
			print_ast_graph_internal(text, &bn->statements[i], fp, false);
		}
		break;
	}
	case ifStatementKind: {
		ifStatementAst *in = (ifStatementAst*)root->data;
		ENDLABEL
		print_ast_graph_internal(text, &in->condition, fp,  false);
		fprintf (fp, "Label%p -> Label%p\n", root, &in->condition);
		bool hasElse = in->elseStatement.kind != 0;
		print_ast_graph_internal(text, &in->thenStatement, fp, false);
		fprintf (fp, "Label%p -> Label%p\n", root, &in->thenStatement);
		if (hasElse) {
			print_ast_graph_internal(text, &in->elseStatement, fp, false);
			fprintf (fp, "Label%p -> Label%p\n", root, &in->elseStatement);
		}
		break;
	}
	case caseBranchKind: {
		caseBranchAst *cn = (caseBranchAst*)root->data;
		if (cn->condition.kind==0) {
			fprintf (fp, "%s", "default");
			ENDLABEL
		} else {
			ENDLABEL
			print_ast_graph_internal(text, &cn->condition, fp, false);
			fprintf (fp, "Label%p -> Label%p\n", root, &cn->condition);
		}
		print_ast_graph_internal(text, &cn->thenStatement, fp, false);
		fprintf (fp, "Label%p -> Label%p\n", root, &cn->thenStatement);
		break;
	}
	case caseStatementKind: {
		caseStatementAst *cn = (caseStatementAst*)root->data;
		ENDLABEL
		for(int i=0;i<cn->branchCount;i++) {
			print_ast_graph_internal(text, &cn->branches[i], fp, false);
			fprintf (fp, "Label%p -> Label%p\n", root, &cn->branches[i]);
		}
		break;
	}
	case switchBranchKind: {
		switchBranchAst *cn = (switchBranchAst*)root->data;
		if (cn->condition.kind==0) {
			fprintf (fp, "%s", "default");
			ENDLABEL
		} else {
			ENDLABEL
			print_ast_graph_internal(text, &cn->condition, fp, false);
			fprintf (fp, "Label%p -> Label%p\n", root, &cn->condition);
		}
		print_ast_graph_internal(text, &cn->thenStatement, fp, false);
			fprintf (fp, "Label%p -> Label%p\n", root, &cn->thenStatement);
		break;
	}
	case switchStatementKind: {
		switchStatementAst *cn = (switchStatementAst*)root->data;

		print_ast_graph_internal(text, &cn->target, fp, false);
			fprintf (fp, "Label%p -> Label%p\n", root, &cn->target);

		for(int i=0;i<cn->branchCount;i++) {
			print_ast_graph_internal(text, &cn->branches[i], fp, false);
			fprintf (fp, "Label%p -> Label%p\n", root, &cn->branches[i]);
		}
		break;
	}
	case whileLoopKind: {
		whileLoopAst *wn = (whileLoopAst*)root->data;
		ENDLABEL
		print_ast_graph_internal(text, &wn->condition, fp, false);
		fprintf (fp, "Label%p -> Label%p\n", root, &wn->condition);
		print_ast_graph_internal(text, &wn->block, fp, false);
		fprintf (fp, "Label%p -> Label%p\n", root, &wn->block);
		break;
	}
	case forLoopKind: {
		forLoopAst *fn = (forLoopAst*)root->data;

		fprintf (fp, "%s %s", fn->value->name, astTypeText[fn->value->type]);

		if (fn->index != 0) {
			fprintf (fp, "\\n%s %s", fn->index->name, astTypeText[fn->index->type]);
		}
		ENDLABEL
		print_ast_graph_internal(text, &fn->range, fp, false);
		print_ast_graph_internal(text, &fn->block, fp, false);
		fprintf (fp, "Label%p -> Label%p\n", root, &fn->range);
		fprintf (fp, "Label%p -> Label%p\n", root, &fn->block);
		break;
	}
	case rangeExpressionKind: {
		rangeExpressionAst rn = *(rangeExpressionAst*)root->data;
		if (root->type == intType) printf ("%d..%d", rn.fromInt, rn.toInt);
		else printf ("'%c'..'%c'", rn.fromChar, rn.toChar);
		ENDLABEL
		break;
	}
	case breakKind: ENDLABEL break;
	case continueKind: ENDLABEL break;
	case unaryExpressionKind: {
		unaryExpressionAst *un = (unaryExpressionAst*)root->data;

		printf ("%s", astUnaryText[un->operator]);
		ENDLABEL
		fprintf (fp, "Label%p -> Label%p\n", root, &un->operand);
		print_ast_graph_internal(text, &un->operand, fp, false);
		break;
	}
	case binaryExpressionKind: {
		binaryExpressionAst *un = (binaryExpressionAst*)root->data;

		printf ("%s", astBinaryText[un->operator]);
		ENDLABEL
		fprintf (fp, "Label%p -> Label%p\n", root, &un->left);
		fprintf (fp, "Label%p -> Label%p\n", root, &un->right);
		print_ast_graph_internal(text, &un->left, fp, false);
		print_ast_graph_internal(text, &un->right, fp, false);
		break;
	}
	case callExpressionKind: {
		callExpressionAst *cn = (callExpressionAst*)root->data;

		printfSymbolReference(fp, cn->function, ".");
		ENDLABEL
		for(int i=0;i<cn->argumentCount;i++) {
			fprintf (fp, "Label%p -> Label%p\n", root, &cn->arguments[i]);
			print_ast_graph_internal(text, &cn->arguments[i], fp, false);
		}
		break;
	}
	case castExpressionKind: {
		astNode *en = (astNode*)root->data;


		fprintf(fp, "cast<%s>", astTypeText[root->type]);
		ENDLABEL
		fprintf (fp, "Label%p -> Label%p\n", root, en);
		print_ast_graph_internal(text, en, fp, false);
		break;
	}
	case namespaceDeclarationKind: {
		namespaceAst *ns = (namespaceAst*)root->data;

		fprintf (fp, "%s", ns->namespace->name);
		ENDLABEL
		fprintf (fp, "Label%p -> Label%p\n", root, &ns->block);
		print_ast_graph_internal(text, &ns->block, fp, false);
		break;
	}
	case functionDeclarationKind: {
		astSymbol *vn = (astSymbol*)root->data;
		functionSymbolData *fd = vn->functionData;

		fprintf (fp, "%s ", astTypeText[vn->type]);

		printfSymbolReference(fp, vn, ".");
		fprintf (fp, "(");
		for (int i=0;i<fd->parameterCount;i++) {
			printf (" %s: %s", fd->parameters[i]->name, astTypeText[fd->parameters[i]->type]);
		}
		fprintf(fp, ")");
		ENDLABEL
		print_ast_graph_internal(text, &fd->body, fp, false);
		fprintf (fp, "Label%p -> Label%p\n", root, &fd->body);
		break;
	}
	case variableDeclarationKind: {
		variableDeclarationAst *vn = (variableDeclarationAst*)root->data;

		printf ("%s %s (%s)", vn->variable->name, astTypeText[vn->variable->type], (vn->variable->flags & VARIABLE_MUTABLE) ? "mutable" : "constant");
		if (vn->initalizer.kind != 0) {
			ENDLABEL
			print_ast_graph_internal(text, &vn->initalizer, fp, false);
			fprintf (fp, "Label%p -> Label%p\n", root, &vn->initalizer);
		}
		else {
			fprintf(fp, "%s", "<uninitialized>");
			ENDLABEL
		}
		break;
	}
	case variableAssignmentKind: {
		variableAssignmentAst *va = (variableAssignmentAst*)root->data;

		fprintf (fp, "%s %s (%s)", va->variable->name, astTypeText[va->variable->type], va->compoundOperator?astBinaryText[va->compoundOperator]:"equals");
		ENDLABEL
		print_ast_graph_internal(text, &va->expression, fp, false);
		fprintf (fp, "Label%p -> Label%p\n", root, &va->expression);
		break;
	}
	case variableReferenceKind: {
		astSymbol *vs = (astSymbol*)root->data;
		printfSymbolReference(fp, vs, ".");
		fprintf(fp, "%s", astTypeText[vs->type]);
		ENDLABEL
		break;
	}
	default: {
		fprintf(stderr, "%sERROR: Unhandled case in print_ast for kind %s%s", TERMRED, astKindText[root->kind], TERMRESET);
		exit(1);
		break;
	}
	}

	if (isRoot) fprintf(fp, "}\n");
}