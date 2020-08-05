
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
	questionmarkToken,
	colonToken,
	commaToken,
	dotToken,
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
	switchKeyword,
	caseKeyword,
	defaultKeyword,
	whileKeyword,
	forKeyword,
	inKeyword,
	breakKeyword,
	continueKeyword,
	fnKeyword,
	namespaceKeyword,
	enumKeyword,
	structKeyword,

	unaryExpression,
	binaryExpression,
	ternaryExpression,
	parenthesizedExpression,
	rangeExpression,
	symbolReferenceExpression,
	enumReferenceExpression,
	callExpression,
	typedIdentifier,
	functionDeclaration,
	variableDeclaration,
	namespaceDeclaration,
	enumDeclaration,
	structDeclaration,
	variableAssignment,
	blockStatement,
	ifStatement,
	caseStatement,
	caseBranch,
	switchStatement,
	switchBranch,
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
	"?",
	":",
	",",
	".",
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
	"switch",
	"case",
	"default",
	"while",
	"for",
	"in",
	"break",
	"continue",
	"fn",
	"namespace",
	"enum",
	"struct",
	"unaryExpression",
	"binaryExpression",
	"ternaryExpression",
	"parenthesizedExpression",
	"rangeExpression",
	"symbolReferenceExpression",
	"enumReferenceExpression",
	"callExpression",
	"typedIdentifier",
	"functionDeclaration",
	"variableDeclaration",
	"namespaceDeclaration",
	"enumDeclaration",
	"structDeclaration",
	"variableAssignment",
	"blockStatement",
	"ifStatement",
	"caseStatement",
	"caseBranch",
	"switchStatement",
	"switchBranch",
	"whileLoop",
	"forLoop",
	"singleLineComment",
	"multiLineComment",
	"fileStatement",
};

// marks a span in the source text
// used to show the span of text that caused an error,
// and also the span of identifiers for symbol lookups
typedef struct textspan {
	u32 start;
	u16 length;
} textspan;

// the base struct for every syntax node,
// tokens produced by the lexer are also nodes
// literals will store their value in the data section
// complex syntax nodes use the `data` pointer to point 
// to another struct which holds the remaining info
typedef struct node {
	enum syntaxKind kind;
	textspan span;
	union {
		void* data; 
		i64 numValue; 
		bool boolValue; 
		char* stringValue; 
		char charValue; 
	};
} node;

// !true
// -10
// +20
// x++
// --y
// ~1234
// pre-increment/decrement operators appear on the left
// the `left` property will be set to true for these nodes
// for others it is set to false
typedef struct unaryExpressionNode {
	node operator;
	node operand;
	bool left;			
} unaryExpressionNode;

// 10 + 10
// true || false
// 'a' + 1
typedef struct binaryExpressionNode {
	node left;
	node operator;
	node right;
} binaryExpressionNode;

// a > b ? a : b
typedef struct ternaryExpressionNode {
	node condition;
	node questionmark;
	node thenExpression;
	node colon;
	node elseExpression;
} ternaryExpressionNode;

// (10)
// (a * b)
typedef struct parenthesizedExpressionNode {
	node openParen;
	node expression;
	node closeParen;
} parenthesizedExpressionNode;

// used in for loops to specify what range to iterate over
// 10..20
// 0..10
// 'a'..'z'
// x..x+4
typedef struct rangeExpressionNode {
	node start;
	node dotDot;
	node end;
} rangeExpressionNode;

// print("Hello world")
// add(10, 20)
typedef struct functionCallNode {
	node identifier;
	node openParen;
	node* arguments;
	u16 argumentCount;
	node closeParen;
} functionCallNode;

// variables can be mutable or immutable
// the `mutabilityIndicator` is a : if it is immutable
// and an = if it is mutable
// the initialization expression is optional
// when `expression` is provided, the type of the variable
// can be inferred and thus becomes optional
// a := 10
// b :int= 20
// c :: 30
// d :int: 40
// e:int		// not initialized
typedef struct variableDeclarationNode {
	node identifier;
	node colon;
	node type;
	node mutabilityIndicator;
	node expression;
} variableDeclarationNode;

// a = 10
// b = true
typedef struct variableAssignmentNode {
	node identifier;
	node assignmentOperator;
	node expression;
} variableAssignmentNode;

// {
// 	name := "Bob"
// 	print("hello %s\n", name)
// }
typedef struct blockStatementNode {
	node openCurly;
	node* statements;
	u16 statementsCount;
	node closeCurly;
} blockStatementNode;


// enum Color { Red, Green, Blue, White, Black }
typedef struct enumDeclarationNode {
	node enumKeyword;
	node identifier;
	node openCurly;
	node* enums;
	u16 enumCount;
	node closeCurly;
} enumDeclarationNode;

// the else clause of if statements is optional
// then `thenExpression` and `elseExpression` can be any statment
// including block statements

// if (x > y) print("it is")

// if (x > y) { } else { }
typedef struct ifStatementNode {
	node ifKeyword;
	node condition;
	node thenExpression;
	node elseKeyword;
	node elseExpression;
} ifStatementNode;

// a single branch in a case statement
typedef struct caseBranchNode {
	node condition;
	node colon;
	node thenExpression;
} caseBranchNode;

// provides an alternative syntax for if statement chains
// x := 10
// case {
// 	x < 10: print("small")
// 	x < 20: print("medium")
// 	default: print("large")
// }
typedef struct caseStatementNode {
	node caseKeyword;
	node openCurly;
	node* branches;
	u16 branchCount;
	node closeCurly;
} caseStatementNode;

// a single branch in a switch statement
// the `caseKeyword` can also match a default keyword
// no condition is expected in this case
typedef struct switchBranchNode {
	node caseKeyword;
	node condition;
	node colon;
	node thenExpression;
} switchBranchNode;

// x := 2
// switch x {
// 	case 1: print("one")
// 	case 2: print("two")
// 	case 3: print("three")
// 	default: print("a high number")
// }
typedef struct switchStatementNode {
	node switchKeyword;
	node targetExpression;
	node openCurly;
	node* branches;
	u16 branchCount;
	node closeCurly;
} switchStatementNode;

// i := 0
// while ++i {
// 	print("%d\n",i)
// }
typedef struct whileLoopNode {
	node whileKeyword;
	node condition;
	node block;
} whileLoopNode;

// most grammars implicitly support parenthesized conditions
// for loops need to implement them explicitly
// the paretheses *are* optional
// the `key` variale is optional,
// the `comma` is left out if the key is not provided

// for (x in 10..-10) print("%d\n", x)

// for x in 10..-10 print("%d\n", x)

// for v,i in 'a'..'z' {
// 	print("value %c, key %d \n", v, i)
// }
typedef struct forLoopNode {
	node forKeyword;
	node openParen;
	node value;
	node comma;
	node key;
	node inKeyword;
	node range;
	node closeParen;
	node block;
} forLoopNode;

// used in function declarations
// x: int
typedef struct typedIdentifierNode {
	node identifier;
	node colon;
	node type;
} typedIdentifierNode;

// fn max(x:int, y:int) -> int { return x > y ? x : y }
typedef struct functionDeclarationNode {
	node fnKeyword;
	node identifier;
	node openParen;
	node* parameters;
	u16 parameterCount;
	node closeParen;
	node body;
} functionDeclarationNode;

// namespace Foo {
// 	x:int
// 	y:int

// 	fn travel(dx:int, dy:int) { x += dx ; y += dy }
// 	fn max(x:int, y:int) -> int { return x > y ? x : y }
// }
typedef struct namespaceDeclarationNode {
	node namespaceKeyword;
	node identifier;
	node body;
} namespaceDeclarationNode;


// struct Point { x:int; y:int }
typedef struct structDeclarationNode {
	node structKeyword;
	node identifier;
	node body;
} structDeclarationNode;

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

char* ast_substring(char* text, textspan span, arena_t *arena) {
	size_t size = sizeof(char) * (span.length + 1);
	char *tokenText = arena == NULL
		? malloc(size)
		: arena_malloc(arena, size);

	strncpy(tokenText, text + span.start, sizeof(char) * span.length);
	tokenText[span.length] = '\0';
	return tokenText;
}

char* allocate_string(char *text, int length, arena_t *arena) {
	char *allocatedText = arena_malloc(arena, sizeof(char) * (length+1));
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
	case questionmarkToken: return 3;
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
		char* tokenText = ast_substring(text, root->span, NULL);
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
	case switchStatement: {
		switchStatementNode sn = *(switchStatementNode*)root->data;
		print_syntaxtree_internal(text, &sn.switchKeyword, indent, verbose, true);
		print_syntaxtree_internal(text, &sn.targetExpression, indent, verbose, true);
		print_syntaxtree_internal(text, &sn.openCurly, indent, verbose, true);
		for (int i = 0; i< sn.branchCount; i++) {
			print_syntaxtree_internal(text, &sn.branches[i], indent, verbose, true);
		}
		print_syntaxtree_internal(text, &sn.closeCurly, indent, verbose, false);
		break;
	}
	case switchBranch: {
		switchBranchNode sb = *(switchBranchNode*)root->data;
		print_syntaxtree_internal(text, &sb.caseKeyword, indent, verbose, true);
		if (sb.caseKeyword.kind != defaultKeyword)
			print_syntaxtree_internal(text, &sb.condition, indent, verbose, true);
		print_syntaxtree_internal(text, &sb.colon, indent, verbose, true);
		print_syntaxtree_internal(text, &sb.thenExpression, indent, verbose, false);
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
	case enumDeclaration: {
		enumDeclarationNode en = *(enumDeclarationNode*)root->data;
		print_syntaxtree_internal(text, &en.enumKeyword, indent, verbose, true);
		print_syntaxtree_internal(text, &en.identifier, indent, verbose, true);
		print_syntaxtree_internal(text, &en.openCurly, indent, verbose, true);
		for (int i = 0; i< en.enumCount; i++) {
			print_syntaxtree_internal(text, &en.enums[i], indent, verbose, true);
		}
		print_syntaxtree_internal(text, &en.closeCurly, indent, verbose, false);
		break;
	}
	case typedIdentifier: {
		typedIdentifierNode dn = *(typedIdentifierNode*)root->data;
		print_syntaxtree_internal(text, &dn.identifier, indent, verbose, true);
		print_syntaxtree_internal(text, &dn.colon, indent, verbose, true);
		print_syntaxtree_internal(text, &dn.type, indent, verbose, false);
		break;
	}
	case functionDeclaration: {
		functionDeclarationNode fn = *(functionDeclarationNode*)root->data;
		print_syntaxtree_internal(text, &fn.fnKeyword, indent, verbose, true);
		print_syntaxtree_internal(text, &fn.openParen, indent, verbose, true);
		for (int i = 0; i< fn.parameterCount; i++) {
			print_syntaxtree_internal(text, &fn.parameters[i], indent, verbose, true);
		}
		print_syntaxtree_internal(text, &fn.closeParen, indent, verbose, true);
		print_syntaxtree_internal(text, &fn.body, indent, verbose, false);
		break;
	}
	case namespaceDeclaration: {
		namespaceDeclarationNode nn = *(namespaceDeclarationNode*)root->data;
		print_syntaxtree_internal(text, &nn.namespaceKeyword, indent, verbose, true);
		print_syntaxtree_internal(text, &nn.identifier, indent, verbose, true);
		print_syntaxtree_internal(text, &nn.body, indent, verbose, false);
		break;
	}
	case structDeclaration: {
		structDeclarationNode *sn = (structDeclarationNode*)root->data;
		print_syntaxtree_internal(text, &sn->structKeyword, indent, verbose, true);
		print_syntaxtree_internal(text, &sn->identifier, indent, verbose, true);
		print_syntaxtree_internal(text, &sn->body, indent, verbose, false);
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
	case symbolReferenceExpression:
	case binaryExpression: {
		binaryExpressionNode bn = *(binaryExpressionNode*)root->data;
		print_syntaxtree_internal(text, &bn.left, indent, verbose, true);
		print_syntaxtree_internal(text, &bn.operator, indent, verbose, true);
		print_syntaxtree_internal(text, &bn.right, indent, verbose, false);
		break;
	}
	case ternaryExpression: {
		ternaryExpressionNode tn = *(ternaryExpressionNode*)root->data;
		print_syntaxtree_internal(text, &tn.condition, indent, verbose, true);
		print_syntaxtree_internal(text, &tn.questionmark, indent, verbose, true);
		print_syntaxtree_internal(text, &tn.thenExpression, indent, verbose, true);
		print_syntaxtree_internal(text, &tn.colon, indent, verbose, true);
		print_syntaxtree_internal(text, &tn.elseExpression, indent, verbose, false);
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
