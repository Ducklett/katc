
enum syntaxKind {
	badToken,
	errorToken,
	endOfFileToken,
	whitespaceToken,
	newlineToken,

	numberLiteral,
	plusOperator,
	minusOperator,
	multipliationOperator,
	divisionOperator,
	modulusOperator,

	openParenthesisToken,
	closeParenthesisToken,
	openCurlyToken,
	closeCurlyToken,

	binaryExpression,
	parenthesizedExpression,
	blockStatement,
};

static const char *syntaxKindText[] = {
	"badToken",
	"errorToken",
	"endOfFileToken",
	"whitespaceToken",
	"newlineToken",
	"numberLiteral",
	"plusOperator",
	"minusOperator",
	"multipliationOperator",
	"divisionOperator",
	"modulusOperator",
	"openParenthesisToken",
	"closeParenthesisToken",
	"openCurlyToken",
	"closeCurlyToken",
	"binaryExpression",
	"parenthesizedExpression",
	"blockStatement",
};


typedef struct node {
	enum syntaxKind kind;
	u32 text_start;
	u16 text_length;
	void* data; 
} node;

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

inline i8 getOperatorPrecedence(enum syntaxKind kind) {
	switch(kind) {
	case plusOperator: return 1;
	case minusOperator: return 1;
	case multipliationOperator: return 2;
	case divisionOperator: return 2;
	case modulusOperator: return 2;
	default: return -1;
	}
}

void print_syntaxtree(char *text, node *root, int indent, bool newline) {

	if (root->data == 0) {
		char* tokenText = ast_substring(text, root);
		printf ("%*s('%s' :: %s)%s", indent, "", tokenText, syntaxKindText[root->kind], newline?"\n":"");
		free(tokenText);
		return;
	}

	printf ("%*s(%s\n", indent, "", syntaxKindText[root->kind]);

	indent += 4;

	switch(root->kind) {
	case blockStatement: {
		blockStatementNode bn = (blockStatementNode)*root->data;
		print_syntaxtree(text, &bn.openCurly, indent, true);
		for (int i = 0; i< bn.statementsCount; i++) {
			print_syntaxtree(text, &bn.statements[i], indent, true);
		}
		print_syntaxtree(text, &bn.closeCurly, indent, false);
		break;
	}
	case binaryExpression: {
		binaryExpressionNode bn = (binaryExpressionNode)*root->data;
		print_syntaxtree(text, &bn.left, indent, true);
		print_syntaxtree(text, &bn.operator, indent, true);
		print_syntaxtree(text, &bn.right, indent, false);
		break;
	}
	case parenthesizedExpression: {
		parenthesizedExpressionNode pn = (parenthesizedExpressionNode)*root->data;
		print_syntaxtree(text, &pn.openParen, indent, true);
		print_syntaxtree(text, &pn.expression, indent, true);
		print_syntaxtree(text, &pn.closeParen, indent, false);
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