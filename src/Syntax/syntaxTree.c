
enum syntaxKind {
	emptyToken,
	badToken,
	errorToken,
	endOfFileToken,

	identifierToken,

	whitespaceToken,
	newlineToken,
	semicolonToken,

	numberLiteral,
	stringLiteral,
	charLiteral,

	plusOperator,
	minusOperator,
	plusPlusOperator,
	minusMinusOperator,
	multipliationOperator,
	divisionOperator,
	modulusOperator,

	bangOperator,
	euqualsEqualsOperator,
	bangEqualsOperator,
	lessOperator,
	greaterOperator,
	lessEqualsOperator,
	greaterEqualsOperator,
	ampersandAmpersandOperator,
	pipePipeOperator,

	tildeOperator,
	lessLessOperator,
	greaterGreaterOperator,
	ampersandOperator,
	caretOperator,
	pipeOperator,

	equalsToken,
	colonToken,
	commaToken,
	dotDotToken,

	plusEqualsToken,
	minusEqualsToken,
	starEqualsToken,
	slashEqualsToken,
	percentEqualsToken,
	lessLessEqualsToken,
	greaterGreaterEqualsToken,
	ampersandEqualsToken,
	caretEqualsToken,
	pipeEqualsToken,

	openParenthesisToken,
	closeParenthesisToken,
	openCurlyToken,
	closeCurlyToken,

	trueKeyword,
	falseKeyword,
	ifKeyword,
	elseKeyword,
	caseKeyword,
	defaultKeyword,
	whileKeyword,
	forKeyword,
	inKeyword,

	unaryExpression,
	binaryExpression,
	parenthesizedExpression,
	rangeExpression,
	callExpression,
	variableDeclaration,
	variableAssignment,
	blockStatement,
	ifStatement,
	caseStatement,
	caseBranch,
	whileLoop,
	forLoop,
	singleLineComment,
	multiLineComment,
	fileStatement,
};

static const char *syntaxKindText[] = {
	"emptyToken",
	"badToken",
	"errorToken",
	"endOfFileToken",
	"identifierToken",
	"whitespaceToken",
	"newlineToken",
	";",
	"numberLiteral",
	"stringLiteral",
	"charLiteral",
	"+",
	"-",
	"++",
	"--",
	"*",
	"/",
	"%",
	"!",
	"==",
	"!=",
	"<",
	">",
	"<=",
	">=",
	"&&",
	"||",
	"~",
	"<<",
	">>",
	"&",
	"^",
	"|",
	"=",
	":",
	",",
	"..",
	"+=",
	"-=",
	"*=",
	"/=",
	"%=",
	"<<=",
	">>=",
	"&=",
	"^=",
	"|=",
	"(",
	")",
	"{",
	"}",
	"true",
	"false",
	"if",
	"else",
	"case",
	"default",
	"while",
	"for",
	"in",
	"unaryExpression",
	"binaryExpression",
	"parenthesizedExpression",
	"rangeExpression",
	"callExpression",
	"variableDeclaration",
	"variableAssignment",
	"blockStatement",
	"ifStatement",
	"caseStatement",
	"caseBranch",
	"whileLoop",
	"forLoop",
	"singleLineComment",
	"multiLineComment",
	"fileStatement",
};

typedef struct textspan {
	u32 start;
	u16 length;
} textspan;

typedef struct node {
	enum syntaxKind kind;
	textspan span;
	union {
		void* data; 
		int numValue; 
		bool boolValue; 
		char* stringValue; 
		char charValue; 
	};
} node;

typedef struct unaryExpressionNode {
	node operator;
	node operand;
	bool left;
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

typedef struct rangeExpressionNode {
	node start;
	node dotDot;
	node end;
} rangeExpressionNode;

typedef struct functionCallNode {
	node identifier;
	node openParen;
	node* arguments;
	u16 argumentCount;
	node closeParen;
} functionCallNode;


typedef struct variableDeclarationNode {
	node identifier;
	node colon;
	node type;				// optional
	node mutabilityIndicator; // = or :
	node expression;
} variableDeclarationNode;

typedef struct variableAssignmentNode {
	node identifier;
	node assignmentOperator;
	node expression;
} variableAssignmentNode;

typedef struct blockStatementNode {
	node openCurly;
	node* statements;
	u16 statementsCount;
	node closeCurly;
} blockStatementNode;

typedef struct ifStatementNode {
	node ifKeyword;
	node condition;
	node thenExpression;
	node elseKeyword;		// optional
	node elseExpression;	// if elseKeyword exists
} ifStatementNode;

typedef struct caseBranchNode {
	node condition;
	node colon;
	node thenExpression;
} caseBranchNode;

typedef struct caseStatementNode {
	node caseKeyword;
	node openCurly;
	node* branches;
	u16 branchCount;
	node closeCurly;
} caseStatementNode;

typedef struct whileLoopNode {
	node whileKeyword;
	node condition;
	node block;
} whileLoopNode;

typedef struct forLoopNode {
	node forKeyword;
	node openParen;			// optional
	node value;
	node comma;				// optional
	node key;				// if comma exists
	node inKeyword;
	node range;
	node closeParen;		// if openParen exists
	node block;
} forLoopNode;

textspan textspan_create(u32 start, u16 length) {
	textspan span = { start, length, };
	return span;
}

textspan textspan_from_bounds(node *start, node *end) {
	textspan span = {
		.start = start->span.start,
		.length = (end->span.start - start->span.start) + end->span.length,
	};
	return span;
}

char* ast_substring(char* text, textspan span) {
	char *tokenText = (char*)malloc(sizeof(char) * (span.length) + 1);
	if (tokenText == NULL) panic("memory allocation failed\n");
	tokenText[span.length] = '\0';
	strncpy(tokenText, text + span.start, sizeof(char) * span.length);
	return tokenText;
}

char* allocate_string(char *text, int length) {
	char *allocatedText = (char*)malloc(sizeof(char) * length);
	if (allocatedText == NULL) panic("memory allocation failed\n");
	strncpy(allocatedText, text, sizeof(char) * length);
	allocatedText[length] = '\0';
	return allocatedText;
}

static inline enum syntaxKind getBinaryOperatorFromAssignmentOperator(enum syntaxKind kind) {
	switch(kind) {
	case plusEqualsToken: return plusOperator;
	case minusEqualsToken: return minusOperator;
	case starEqualsToken: return multipliationOperator;
	case slashEqualsToken: return divisionOperator;
	case percentEqualsToken: return modulusOperator;
	case lessLessEqualsToken: return lessLessOperator;
	case greaterGreaterEqualsToken: return greaterGreaterOperator;
	case ampersandEqualsToken: return ampersandOperator;
	case caretEqualsToken: return caretOperator;
	case pipeEqualsToken: return pipeOperator;
	default: return 0;
	}
}
static inline bool isAssignmentOperator(enum syntaxKind kind) {
	return (
		kind == equalsToken ||
		kind == plusEqualsToken ||
		kind == minusEqualsToken ||
		kind == starEqualsToken ||
		kind == slashEqualsToken ||
		kind == percentEqualsToken ||
		kind == lessLessEqualsToken ||
		kind == greaterGreaterEqualsToken ||
		kind == ampersandEqualsToken ||
		kind == caretEqualsToken ||
		kind == pipeEqualsToken);
}

static inline i8 getBinaryOperatorPrecedence(enum syntaxKind kind) {
	switch(kind) {
	case plusOperator: return 12;
	case minusOperator: return 12;
	case multipliationOperator: return 13;
	case divisionOperator: return 13;
	case modulusOperator: return 13;
	case lessLessOperator: return 11;
	case greaterGreaterOperator: return 11;
	case greaterOperator: return 10;
	case greaterEqualsOperator: return 10;
	case lessOperator: return 10;
	case lessEqualsOperator: return 10;
	case euqualsEqualsOperator: return 9;
	case bangEqualsOperator: return 9;
	case ampersandOperator: return 8;
	case caretOperator: return 7;
	case pipeOperator: return 6;
	case ampersandAmpersandOperator: return 5;
	case pipePipeOperator: return 4;
	default: return -1;
	}
}

static inline i8 getUnaryOperatorPrecedence(enum syntaxKind kind) {
	switch(kind) {
	case bangOperator: return 14;
	case tildeOperator: return 14;
	case plusOperator: return 14;
	case minusOperator: return 14;
	case plusPlusOperator: return 14;
	case minusMinusOperator: return 14;
	default: return -1;
	}
}

void print_syntaxtree_internal(char *text, node *root, int indent, bool verbose, bool newline);
void print_syntaxtree(char *text, node *root, int indent, bool verbose) { print_syntaxtree_internal(text, root, indent, verbose, true); }
void print_syntaxtree_internal(char *text, node *root, int indent, bool verbose, bool newline) {

	if (root->data == 0 || root->kind == numberLiteral || root->kind == stringLiteral || root->kind == charLiteral || root->kind == trueKeyword || root->kind == falseKeyword) {
		char* tokenText = ast_substring(text, root->span);
		if (verbose) {
			printf ("%*s(", indent, "");
			printf ("%s'%s'%s", TERMBLUE, tokenText, TERMRESET);
			printf (" :: ");
			printf ("%s%s%s", TERMYELLOW, syntaxKindText[root->kind], TERMRESET);
			printf (")%s", newline?"\n":"");
		} else {
			printf ("%*s%s%s%s%s", indent, "", TERMCYAN, tokenText, TERMRESET, newline?"\n":"");
		}
		free(tokenText);
		return;
	}

	printf ("%*s(%s\n", indent, "", syntaxKindText[root->kind]);

	indent += 4;

	switch(root->kind) {
	case variableDeclaration: {
		variableDeclarationNode dn = *(variableDeclarationNode*)root->data;
		print_syntaxtree_internal(text, &dn.identifier, indent, verbose, true);
		print_syntaxtree_internal(text, &dn.colon, indent, verbose, true);
		if (dn.type.kind != emptyToken)	print_syntaxtree_internal(text, &dn.type, indent, verbose, true);
		print_syntaxtree_internal(text, &dn.mutabilityIndicator, indent, verbose, true);
		print_syntaxtree_internal(text, &dn.expression, indent, verbose, false);
		break;
	}
	case variableAssignment: {
		variableAssignmentNode an = *(variableAssignmentNode*)root->data;
		print_syntaxtree_internal(text, &an.identifier, indent, verbose, true);
		print_syntaxtree_internal(text, &an.assignmentOperator, indent, verbose, true);
		print_syntaxtree_internal(text, &an.expression, indent, verbose, false);
		break;
	}
	case callExpression: {
		functionCallNode fn = *(functionCallNode*)root->data;
		print_syntaxtree_internal(text, &fn.identifier, indent, verbose, true);
		print_syntaxtree_internal(text, &fn.openParen, indent, verbose, true);
		for (int i = 0; i< fn.argumentCount; i++) {
			print_syntaxtree_internal(text, &fn.arguments[i], indent, verbose, true);
		}
		print_syntaxtree_internal(text, &fn.closeParen, indent, verbose, false);
		break;
	}
	case ifStatement: {
		ifStatementNode in = *(ifStatementNode*)root->data;
		print_syntaxtree_internal(text, &in.ifKeyword, indent, verbose, true);
		print_syntaxtree_internal(text, &in.condition, indent, verbose, true);
		print_syntaxtree_internal(text, &in.thenExpression, indent, verbose, true);
		if (in.elseKeyword.kind != emptyToken) {
			print_syntaxtree_internal(text, &in.elseKeyword, indent, verbose, true);
			print_syntaxtree_internal(text, &in.elseExpression, indent, verbose, false);
		}
		break;
	}
	case caseStatement: {
		caseStatementNode cn = *(caseStatementNode*)root->data;
		print_syntaxtree_internal(text, &cn.caseKeyword, indent, verbose, true);
		print_syntaxtree_internal(text, &cn.openCurly, indent, verbose, true);
		for (int i = 0; i< cn.branchCount; i++) {
			print_syntaxtree_internal(text, &cn.branches[i], indent, verbose, true);
		}
		print_syntaxtree_internal(text, &cn.closeCurly, indent, verbose, false);
		break;
	}
	case caseBranch: {
		caseBranchNode cb = *(caseBranchNode*)root->data;
		print_syntaxtree_internal(text, &cb.condition, indent, verbose, true);
		print_syntaxtree_internal(text, &cb.colon, indent, verbose, true);
		print_syntaxtree_internal(text, &cb.thenExpression, indent, verbose, false);
		break;
	}
	case whileLoop: {
		whileLoopNode wn = *(whileLoopNode*)root->data;
		print_syntaxtree_internal(text, &wn.whileKeyword, indent, verbose, true);
		print_syntaxtree_internal(text, &wn.condition, indent, verbose, true);
		print_syntaxtree_internal(text, &wn.block, indent, verbose, false);
		break;
	}
	case forLoop: {
		forLoopNode fn = *(forLoopNode*)root->data;
		print_syntaxtree_internal(text, &fn.forKeyword, indent, verbose, true);
		if (fn.openParen.kind != emptyToken) print_syntaxtree_internal(text, &fn.openParen, indent, verbose, true);
		print_syntaxtree_internal(text, &fn.value, indent, verbose, true);
		if (fn.comma.kind != emptyToken) {
			print_syntaxtree_internal(text, &fn.comma, indent, verbose, true);
			print_syntaxtree_internal(text, &fn.key, indent, verbose, true);
		}
		print_syntaxtree_internal(text, &fn.inKeyword, indent, verbose, true);
		print_syntaxtree_internal(text, &fn.range, indent, verbose, true);
		if (fn.closeParen.kind != emptyToken) print_syntaxtree_internal(text, &fn.closeParen, indent, verbose, true);
		print_syntaxtree_internal(text, &fn.block, indent, verbose, false);
		break;
	}
	case fileStatement:
	case blockStatement: {
		blockStatementNode bn = *(blockStatementNode*)root->data;
		print_syntaxtree_internal(text, &bn.openCurly, indent, verbose, true);
		for (int i = 0; i< bn.statementsCount; i++) {
			print_syntaxtree_internal(text, &bn.statements[i], indent, verbose, true);
		}
		print_syntaxtree_internal(text, &bn.closeCurly, indent, verbose, false);
		break;
	}
	case unaryExpression: {
		unaryExpressionNode un = *(unaryExpressionNode*)root->data;
		if (un.left) print_syntaxtree_internal(text, &un.operator, indent, verbose, true);
		print_syntaxtree_internal(text, &un.operand, indent, verbose, !un.left);
		if (!un.left) print_syntaxtree_internal(text, &un.operator, indent, verbose, false);
		break;
	}
	case binaryExpression: {
		binaryExpressionNode bn = *(binaryExpressionNode*)root->data;
		print_syntaxtree_internal(text, &bn.left, indent, verbose, true);
		print_syntaxtree_internal(text, &bn.operator, indent, verbose, true);
		print_syntaxtree_internal(text, &bn.right, indent, verbose, false);
		break;
	}
	case parenthesizedExpression: {
		parenthesizedExpressionNode pn = *(parenthesizedExpressionNode*)root->data;
		print_syntaxtree_internal(text, &pn.openParen, indent, verbose, true);
		print_syntaxtree_internal(text, &pn.expression, indent, verbose, true);
		print_syntaxtree_internal(text, &pn.closeParen, indent, verbose, false);
		break;
	}
	case rangeExpression: {
		rangeExpressionNode rn = *(rangeExpressionNode*)root->data;
		print_syntaxtree_internal(text, &rn.start, indent, verbose, true);
		print_syntaxtree_internal(text, &rn.dotDot, indent, verbose, true);
		print_syntaxtree_internal(text, &rn.end, indent, verbose, false);
		break;
	}
	default: {
		fprintf(stderr, "%sERROR: Unhandled case in print_syntaxTree for kind %s%s", TERMRED, syntaxKindText[root->kind], TERMRESET);
		exit(1);
		break;
	}
	}

	printf (" )%s", newline?"\n":"");
}
