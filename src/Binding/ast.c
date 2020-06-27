enum astKind {
	missingKind,
	errorKind,
	literalKind,
	unaryExpressionKind,
	binaryExpressionKind,
	rangeExpressionKind,
	callExpressionKind,
	variableDeclarationKind,
	variableAssignmentKind,
	variableReferenceKind,
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
	"variableDeclaration",
	"variableAssignment",
	"variableReferenceKind",
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
	boolType,
	stringType,
};

static const char *astTypeText[] = {
	"errorType",
	"unresolved",
	"void",
	"int",
	"bool",
	"string",
};

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
	"logicalAnd",
	"logicalOr",
};

typedef struct typedOperator {
	enum astBinaryOperator operator;
	enum astType type;
} typedOperator;

typedOperator get_binary_operator(enum syntaxKind operatorToken, enum astType left, enum astType right) {
	if (operatorToken == plusOperator && left == intType && right == intType) return (typedOperator){ addOp, intType };
	if (operatorToken == minusOperator && left == intType && right == intType) return (typedOperator){ subtractOp, intType };
	if (operatorToken == multipliationOperator && left == intType && right == intType) return (typedOperator){ multiplyOp, intType };
	if (operatorToken == divisionOperator && left == intType && right == intType) return (typedOperator){ divideOp, intType };
	if (operatorToken == modulusOperator && left == intType && right == intType) return (typedOperator){ moduloOp, intType };

	if (operatorToken == euqualsEqualsOperator && left == boolType && right == boolType) return (typedOperator) { equalOp, boolType };
	if (operatorToken == bangEqualsOperator && left == boolType && right == boolType) return (typedOperator){ inEqualOp, boolType };

	if (operatorToken == lessOperator && left == intType && right == intType) return (typedOperator){ lessOp, boolType };
	if (operatorToken == greaterOperator && left == intType && right == intType) return (typedOperator){ greaterOp, boolType };
	if (operatorToken == lessEqualsOperator && left == intType && right == intType) return (typedOperator){ lessOrEqualOp, boolType };
	if (operatorToken == greaterEqualsOperator && left == intType && right == intType) return (typedOperator){ greaterOrEqualOp, boolType };

	if (operatorToken == ampersandAmpersandOperator && left == boolType && right == boolType) return (typedOperator){ logicalAndOp, boolType };
	if (operatorToken == pipePipeOperator && left == boolType && right == boolType) return (typedOperator) { logicalOrOp, boolType };

	return (typedOperator){0};
}

enum astUnaryOperator {
	missingUnaryOp,
	logicalNegationOp,
	negationOp,
	identityOp,
};

static const char *astUnaryText[] = {
	"missingUnary",
	"logicalNegation",
	"negation",
	"identity",
};

enum astUnaryOperator get_unary_operator(enum syntaxKind operatorToken, enum astType type) {
	if (type == intType && operatorToken == plusOperator) return identityOp;
	if (type == intType && operatorToken == minusOperator) return negationOp;
	if (type == boolType && operatorToken == bangOperator) return logicalNegationOp;
	return missingUnaryOp;
}

typedef struct astNode {
	enum astKind kind;
	enum astType type;
	union {
		void* data; 
		int numValue; 
		bool boolValue; 
	};
} astNode;

typedef struct variableSymbol {
	char name[128];
	enum astType type;
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
	u16 from;
	u16 to;
} rangeExpressionAst;

typedef struct callExpressionAst {

} callExpressionAst;

typedef struct variableDeclarationAst {
	variableSymbol* variable;
	astNode initalizer;
} variableDeclarationAst;

typedef struct variableAssignmentAst {
	variableSymbol* variable;
	astNode expression;
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
	caseBranchAst* branches;
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
	whileLoopAst whileLoops[1024];
	forLoopAst forLoops[1024];
	unaryExpressionAst unaryExpressions[1024];
	binaryExpressionAst binaryExpressions[1024];
	variableDeclarationAst variableDeclarations[1024];
	variableAssignmentAst variableAssignments[1024];
	int nodesIndex;
	int scopesIndex;
	int rangeIndex;
	int currentScopeIndex;
	int blockStatementsIndex;
	int ifStatementsIndex;
	int whileLoopIndex;
	int forLoopIndex;
	int unaryExpressionsIndex;
	int binaryExpressionsIndex;
	int variableDeclarationIndex;
	int variableAssignmentIndex;
} ast;

enum astType resolve_type_from_span(ast *tree, textspan span) {
	char *text = tree->text;
	for (int i = intType; i<= stringType; i++)
		if (span_compare(text, span, astTypeText[i])) return i;
	
    report_diagnostic(&tree->diagnostics, unresolvedTypeDiagnostic, span, 0, 0, 0);
	return 0;
}

int bind_tree(ast* tree);

int create_ast(char* filename, ast* tree) {
	{
		benchmark_start();
		tree->text = read_file(filename, &tree->length);
		benchmark_end("File read");
	}

	if (!create_syntaxtree(tree->text, tree->length, &tree->parser, &tree->diagnostics)) {
		return 0;
	}

	return bind_tree(tree);
}

void print_ast_internal(char *text, astNode *root, int indent, bool verbose, bool newline);
void print_ast(char *text, astNode *root, int indent, bool verbose) { print_ast_internal(text, root, indent, verbose, true); }
void print_ast_internal(char *text, astNode *root, int indent, bool verbose, bool newline) {

	if (root->kind == literalKind) {
		if (root->type == voidType) return;
		if (verbose) {
			printf ("%*s(", indent, "");
			TERMBLUE();
			switch (root->type) {
				case intType: printf ("%d", root->numValue); break;
				case boolType: printf ("%s", root->boolValue ? "true" : "false"); break;
			}
			TERMRESET();
			printf (" :: ");
			TERMYELLOW();
			printf ("%s", astTypeText[root->type]);
			TERMRESET();
			printf (")%s", newline?"\n":"");
		} else {
			TERMCYAN();
			switch (root->type) {
				case intType: printf ("%*s%d%s", indent, "", root->numValue, newline?"\n":"");
				case boolType: printf ("%*s%s%s", indent, "", root->boolValue?"true":"false", newline?"\n":"");
			}
			TERMRESET();
		}
		return;
	}

	printf ("%*s(%s\n", indent, "", astKindText[root->kind]);

	indent += 4;

	switch(root->kind) {
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
	case whileLoopKind: {
		whileLoopAst wn = *(whileLoopAst*)root->data;
		print_ast_internal(text, &wn.condition, indent, verbose, true);
		print_ast_internal(text, &wn.block, indent, verbose, false);
		break;
	}
	case forLoopKind: {
		forLoopAst fn = *(forLoopAst*)root->data;

		TERMMAGENTA();
		printf ("%*s%s %s\n", indent, "", fn.value->name, astTypeText[fn.value->type]);
		TERMRESET();

		if (fn.index != 0) {
			TERMMAGENTA();
			printf ("%*s%s %s\n", indent, "", fn.index->name, astTypeText[fn.index->type]);
			TERMRESET();
		}

		print_ast_internal(text, &fn.range, indent, verbose, true);
		print_ast_internal(text, &fn.block, indent, verbose, false);
		break;
	}
	case rangeExpressionKind: {

		rangeExpressionAst rn = *(rangeExpressionAst*)root->data;
		TERMMAGENTA();
		printf ("%*s%d..%d", indent, "", rn.from, rn.to);
		TERMRESET();
		break;
	}
	case unaryExpressionKind: {
		unaryExpressionAst un = *(unaryExpressionAst*)root->data;

		TERMMAGENTA();
		printf ("%*s%s\n", indent, "", astUnaryText[un.operator]);
		TERMRESET();
		print_ast_internal(text, &un.operand, indent, verbose, false);
		break;
	}
	case binaryExpressionKind: {
		binaryExpressionAst un = *(binaryExpressionAst*)root->data;

		TERMMAGENTA();
		printf ("%*s%s\n", indent, "", astBinaryText[un.operator]);
		TERMRESET();
		print_ast_internal(text, &un.left, indent, verbose, true);
		print_ast_internal(text, &un.right, indent, verbose, false);
		break;
	}
	case variableDeclarationKind: {
		variableDeclarationAst vn = *(variableDeclarationAst*)root->data;

		TERMMAGENTA();
		printf ("%*s%s %s\n", indent, "", vn.variable->name, astTypeText[vn.variable->type]);
		TERMRESET();
		print_ast_internal(text, &vn.initalizer, indent, verbose, false);
		break;
	}
	case variableAssignmentKind: {
		variableAssignmentAst va = *(variableAssignmentAst*)root->data;

		TERMMAGENTA();
		printf ("%*s%s %s\n", indent, "", va.variable->name, astTypeText[va.variable->type]);
		TERMRESET();
		print_ast_internal(text, &va.expression, indent, verbose, false);
		break;
	}
	case variableReferenceKind: {
		variableSymbol vs = *(variableSymbol*)root->data;

		TERMMAGENTA();
		printf ("%*s%s %s", indent, "", vs.name, astTypeText[vs.type]);
		TERMRESET();
		break;
	}
	default: {
		TERMRED();
		printf("ERROR: Unhandled case in print_ast for kind %s", astKindText[root->kind]);
		TERMRESET();
		exit(1);
		break;
	}
	}

	printf (" )%s", newline?"\n":"");
}
