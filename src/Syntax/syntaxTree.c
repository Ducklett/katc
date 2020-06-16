
enum syntaxKind {
	badToken,
	errorToken,
	endOfFileToken,

	identifierToken,

	whitespaceToken,
	newlineToken,

	numberLiteral,

	plusOperator,
	minusOperator,
	multipliationOperator,
	divisionOperator,
	modulusOperator,

	equalsToken,
	colonToken,

	openParenthesisToken,
	closeParenthesisToken,
	openCurlyToken,
	closeCurlyToken,

	unaryExpression,
	binaryExpression,
	parenthesizedExpression,
	variableDeclaration,
	variableAssignment,
	blockStatement,
};

static const char *syntaxKindText[] = {
	"badToken",
	"errorToken",
	"endOfFileToken",
	"identifierToken",
	"whitespaceToken",
	"newlineToken",
	"numberLiteral",
	"plusOperator",
	"minusOperator",
	"multipliationOperator",
	"divisionOperator",
	"modulusOperator",
	"equalsToken",
	"colonToken",
	"openParenthesisToken",
	"closeParenthesisToken",
	"openCurlyToken",
	"closeCurlyToken",
	"unaryExpression",
	"binaryExpression",
	"parenthesizedExpression",
	"variableDeclaration",
	"variableAssignment",
	"blockStatement",
};


typedef struct node {
	enum syntaxKind kind;
	u32 text_start;
	u16 text_length;
	void* data; 
} node;

typedef struct unaryExpressionNode {
	node operator;
	node operand;
} unaryExpressionNode;

typedef struct binaryExpressionNode {
	node left;
	node operator;
	node right;
} binaryExpressionNode;

typedef struct parenthesizedExpressionNode {
	node openParen;
	node expression;
	node closeParen;
} parenthesizedExpressionNode;

typedef struct variableDeclarationNode {
	node identifier;
	node colon;
	node equals;
	node expression;
} variableDeclarationNode;

typedef struct variableAssignmentNode {
	node identifier;
	node equals;
	node expression;
} variableAssignmentNode;

typedef struct blockStatementNode {
	node openCurly;
	node* statements;
	u16 statementsCount;
	node closeCurly;
} blockStatementNode;

char* ast_substring(char* text, node *n) {
	char *tokenText = (char*)malloc(sizeof(char) * (n->text_length) + 1);
	tokenText[n->text_length] = '\0';
	strncpy(tokenText, text + n->text_start, sizeof(char) * n->text_length);
	return tokenText;
}

inline i8 getBinaryOperatorPrecedence(enum syntaxKind kind) {
	switch(kind) {
	case plusOperator: return 1;
	case minusOperator: return 1;
	case multipliationOperator: return 2;
	case divisionOperator: return 2;
	case modulusOperator: return 2;
	default: return -1;
	}
}

inline i8 getUnaryOperatorPrecedence(enum syntaxKind kind) {
	switch(kind) {
	case plusOperator: return 5;
	case minusOperator: return 5;
	default: return -1;
	}
}

void print_syntaxtree_internal(char *text, node *root, int indent, bool verbose, bool newline);
void print_syntaxtree(char *text, node *root, int indent, bool verbose) { print_syntaxtree_internal(text, root, indent, verbose, true); }
void print_syntaxtree_internal(char *text, node *root, int indent, bool verbose, bool newline) {

	if (root->data == 0) {
		char* tokenText = ast_substring(text, root);
		if (verbose) {
			printf ("%*s('%s' :: %s)%s", indent, "", tokenText, syntaxKindText[root->kind], newline?"\n":"");
		} else {
			printf ("%*s%s%s", indent, "", tokenText, newline?"\n":"");
		}
		free(tokenText);
		return;
	}

	printf ("%*s(%s\n", indent, "", syntaxKindText[root->kind]);

	indent += 4;

	switch(root->kind) {
	case variableDeclaration: {
		variableDeclarationNode dn = (variableDeclarationNode)*root->data;
		print_syntaxtree_internal(text, &dn.identifier, indent, verbose, true);
		print_syntaxtree_internal(text, &dn.colon, indent, verbose, true);
		print_syntaxtree_internal(text, &dn.equals, indent, verbose, true);
		print_syntaxtree_internal(text, &dn.expression, indent, verbose, false);
		break;
	}
	case variableAssignment: {
		variableAssignmentNode an = (variableAssignmentNode)*root->data;
		print_syntaxtree_internal(text, &an.identifier, indent, verbose, true);
		print_syntaxtree_internal(text, &an.equals, indent, verbose, true);
		print_syntaxtree_internal(text, &an.expression, indent, verbose, false);
		break;
	}
	case blockStatement: {
		blockStatementNode bn = (blockStatementNode)*root->data;
		print_syntaxtree_internal(text, &bn.openCurly, indent, verbose, true);
		for (int i = 0; i< bn.statementsCount; i++) {
			print_syntaxtree_internal(text, &bn.statements[i], indent, verbose, true);
		}
		print_syntaxtree_internal(text, &bn.closeCurly, indent, verbose, false);
		break;
	}
	case unaryExpression: {
		unaryExpressionNode un = (unaryExpressionNode)*root->data;
		print_syntaxtree_internal(text, &un.operator, indent, verbose, true);
		print_syntaxtree_internal(text, &un.operand, indent, verbose, false);
		break;
	}
	case binaryExpression: {
		binaryExpressionNode bn = (binaryExpressionNode)*root->data;
		print_syntaxtree_internal(text, &bn.left, indent, verbose, true);
		print_syntaxtree_internal(text, &bn.operator, indent, verbose, true);
		print_syntaxtree_internal(text, &bn.right, indent, verbose, false);
		break;
	}
	case parenthesizedExpression: {
		parenthesizedExpressionNode pn = (parenthesizedExpressionNode)*root->data;
		print_syntaxtree_internal(text, &pn.openParen, indent, verbose, true);
		print_syntaxtree_internal(text, &pn.expression, indent, verbose, true);
		print_syntaxtree_internal(text, &pn.closeParen, indent, verbose, false);
		break;
	}
	default: {
		printf("ERROR: Unhandled case in print_syntaxTree for kind %s", syntaxKindText[root->kind]);
		exit(1);
		break;
	}
	}

	printf (" )%s", newline?"\n":"");
}
