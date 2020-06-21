enum astKind {
    missingKind,
	unaryExpressionKind,
	binaryExpressionKind,
	rangeExpressionKind,
	callExpressionKind,
	variableDeclarationKind,
	variableAssignmentKind,
	blockStatementKind,
	ifStatementKind,
	caseStatementKind,
	caseBranchKind,
	whileLoopKind,
	forLoopKind,
	jumpKind,
};

enum astType {
    unresolvedType,
    voidType,
    intType,
    boolType,
    stringType,
};

enum astBinaryOperator {
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
};

enum astUnaryOperator {
    logicalNegationOp,
    negationOp,
};

typedef struct astNode {
    enum astKind kind;
    enum astType type;
	void* data; 
} astNode;

typedef struct variableSymbol {
    char name[128];
    enum astType type;
} variableSymbol;

typedef struct ast {
    parser parser;
    diagnosticContainer diagnostics;
    astNode root;
} ast;

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
    astNode elseStatement;
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

int create_ast(char* filename, ast* tree) {
	u64 length;
	char* text;
	{
		benchmark_start();
		text = read_file(filename, &length);
		benchmark_end("File read");
	}

    if (!create_syntaxtree(text, length, &tree->parser, &tree->diagnostics)) {
		print_diagnostics(&tree->diagnostics, text);
        return 0;
    }

    bool verbose = true;
	print_syntaxtree(text, &tree->parser.root, 0, verbose);

	return 1;
}