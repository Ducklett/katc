enum astKind {
    missingKind,
    literalKind,
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

static const char *astKindText[] = {
    "missingKind",
    "literalKind",
	"unaryExpressionKind",
	"binaryExpressionKind",
	"rangeExpressionKind",
	"callExpressionKind",
	"variableDeclarationKind",
	"variableAssignmentKind",
	"blockStatementKind",
	"ifStatementKind",
	"caseStatementKind",
	"caseBranchKind",
	"whileLoopKind",
	"forLoopKind",
	"jumpKind",
};

enum astType {
    unresolvedType,
    voidType,
    intType,
    boolType,
    stringType,
};

static const char *astTypeText[] = {
    "unresolvedType",
    "voidType",
    "intType",
    "boolType",
    "stringType",
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

typedef struct ast {
	u64 length;
	char* text;
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
