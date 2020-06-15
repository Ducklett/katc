
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

	binaryExpression
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
	"binaryExpression",
};


typedef struct node {
	enum syntaxKind kind;
	int text_start;
	int text_length;
	void* data; 
} node;

typedef struct binaryExpressionNode {
	node left;
	node operator;
	node right;
} binaryExpressionNode;

char* ast_substring(char* text, node *n) {
	char *tokenText = (char*)malloc(sizeof(char) * (n->text_length) + 1);
	tokenText[n->text_length] = '\0';
	strncpy(tokenText, text + n->text_start, sizeof(char) * n->text_length);
	return tokenText;
}

inline int getOperatorPrecedence(enum syntaxKind kind) {
	switch(kind) {
	case plusOperator: return 1;
	case minusOperator: return 1;
	case multipliationOperator: return 2;
	case divisionOperator: return 2;
	case modulusOperator: return 2;
	default: return -1;
	}
}

void print_ast(char *text, node *root, int indent, bool newline) {

	if (root->data == 0) {
		char* tokenText = ast_substring(text, root);
		printf ("%*s(%s :: %s)%s", indent, "", tokenText, syntaxKindText[root->kind], newline?"\n":"");
		free(tokenText);
		return;
	}

	printf ("%*s(%s\n", indent, "", syntaxKindText[root->kind]);

	// binary expression
	binaryExpressionNode n = (binaryExpressionNode)*root->data;
	print_ast(text, &(n.left), indent + 4, true);
	print_ast(text, &n.operator, indent + 4, true);
	print_ast(text, &n.right, indent + 4, false);

	printf (" )");
}