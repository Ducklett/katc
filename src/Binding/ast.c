enum astKind {
	missingKind,
	errorKind,
	literalKind,
	unaryExpressionKind,
	binaryExpressionKind,
	rangeExpressionKind,
	callExpressionKind,
	castExpressionKind,
	variableDeclarationKind,
	variableAssignmentKind,
	variableReferenceKind,
	fileStatementKind,
	blockStatementKind,
	ifStatementKind,
	caseStatementKind,
	caseBranchKind,
	whileLoopKind,
	forLoopKind,
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
	"variableDeclaration",
	"variableAssignment",
	"variableReferenceKind",
	"fileStatement",
	"blockStatement",
	"ifStatement",
	"caseStatement",
	"caseBranch",
	"whileLoop",
	"forLoop",
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

	if (from == stringType && to != boolType) return CAST_ILLEGAL;

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

typedef struct variableSymbol {
	char name[128];
	enum astType type;
	u8 flags;
	union {
		void* data; 
		int numValue; 
		bool boolValue; 
		char* stringValue; 
		char charValue; 
	};
} variableSymbol;

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
	astNode *arguments;
	u8 argumentCount;
} callExpressionAst;

typedef struct variableDeclarationAst {
	variableSymbol* variable;
	astNode initalizer;
} variableDeclarationAst;

typedef struct variableAssignmentAst {
	variableSymbol* variable;
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

typedef struct whileLoopAst {
	astNode condition;
	astNode block;
} whileLoopAst;

typedef struct forLoopAst {
	variableSymbol* value;
	variableSymbol* index;
	astNode range;
	astNode block;
} forLoopAst;

typedef struct jumpAst {

} jumpAst;

typedef struct scope {
	struct scope* parentScope;
	variableSymbol variables[10];
	u8 variableCount;
} scope;

typedef struct ast {
	char* text;
	u64 length;
	parser parser;
	diagnosticContainer diagnostics;
	astNode root;
	astNode nodes[1024];
	scope scopes[20];
	rangeExpressionAst ranges[1024];
	blockStatementAst blockStatements[1024];
	ifStatementAst ifStatements[1024];
	caseBranchAst caseBranches[1024];
	caseStatementAst caseStatements[1024];
	whileLoopAst whileLoops[1024];
	forLoopAst forLoops[1024];
	unaryExpressionAst unaryExpressions[1024];
	binaryExpressionAst binaryExpressions[1024];
	callExpressionAst functionCalls[1024];
	variableDeclarationAst variableDeclarations[1024];
	variableAssignmentAst variableAssignments[1024];
	int nodesIndex;
	int scopesIndex;
	int rangeIndex;
	int currentScopeIndex;
	int blockStatementsIndex;
	int ifStatementsIndex;
	int caseBranchesIndex;
	int caseStatementsIndex;
	int whileLoopIndex;
	int forLoopIndex;
	int unaryExpressionsIndex;
	int binaryExpressionsIndex;
	int functionCallIndex;
	int variableDeclarationIndex;
	int variableAssignmentIndex;
} ast;

enum astType resolve_type_from_span(ast *tree, textspan span) {
	char *text = tree->text;
	for (int i = intType; i<= charType; i++)
		if (span_compare(text, span, astTypeText[i])) return i;
	
	report_diagnostic(&tree->diagnostics, unresolvedTypeDiagnostic, span, 0, 0, 0);
	return 0;
}

int bind_tree(ast* tree);

int create_ast(const char* filename, ast* tree, bool parseOnly) {
	{
		benchmark_start();
		tree->text = read_file(filename, &tree->length);
		benchmark_end("File read");
	}

	if (!create_syntaxtree(tree->text, tree->length, &tree->parser, &tree->diagnostics)) {
		return 0;
	}

	if (parseOnly) return 1;

	return bind_tree(tree);
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
				case intType: printf ("%*s%d%s", indent, "", root->numValue, newline?"\n":"");
				case boolType: printf ("%*s%s%s", indent, "", root->boolValue?"true":"false", newline?"\n":"");
				case stringType: printf ("%*s\"%s\"%s", indent, "", root->stringValue, newline?"\n":"");
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

		printf ("%*s%s%s%s\n", indent, "", TERMMAGENTA, "print", TERMRESET);
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
		variableSymbol vs = *(variableSymbol*)root->data;

		printf ("%*s%s%s %s%s", indent, "", TERMMAGENTA, vs.name, astTypeText[vs.type], TERMRESET);
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
