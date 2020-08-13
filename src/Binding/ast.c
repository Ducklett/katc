struct astSymbol;
struct scope;

enum astSyntaxKind {
	missingKind,
	errorKind,
	literalKind,
	arrayLiteralKind,
	unaryExpressionKind,
	binaryExpressionKind,
	ternaryExpressionKind,
	rangeExpressionKind,
	callExpressionKind,
	arrayAccessKind,
	constructorExpressionKind,
	castExpressionKind,
	namespaceDeclarationKind,
	enumDeclarationKind,
	structDeclarationKind,
	structReferenceKind,
	functionDeclarationKind,
	variableDeclarationKind,
	variableAssignmentKind,
	variableReferenceKind,
	typeDeclarationKind,
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

static const char *astSyntaxKindText[] = {
	"missing",
	"error",
	"literal",
	"arrayLiteral",
	"unaryExpression",
	"binaryExpression",
	"ternaryExpression",
	"rangeExpression",
	"callExpression",
	"arrayAccess",
	"constructorExpressionKind",
	"castExpression",
	"namespaceDeclaration",
	"enumDeclarationKind",
	"structDeclarationKind",
	"structReferenceKind",
	"functionDeclaration",
	"variableDeclaration",
	"variableAssignment",
	"variableReference",
	"typeDeclaration",
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

enum astKind {
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
	floatType,
	boolType,
	stringType,
	charType,
	enumType,
	structType,
	arrayType,
};

static const char *astKindText[] = {
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
	"float",
	"bool",
	"string",
	"char",
	"enum",
	"struct",
	"array",
};

bool isIntType(enum astKind t) {
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

typedef struct arrayTypeInfo;

typedef struct astType {
	enum astKind kind;
	union {
		struct astSymbol *declaration;
		struct arrayTypeInfo *arrayInfo;
	};
} astType;

typedef struct arrayTypeInfo {
	struct astType ofType;	
	u16 capacity;
} arrayTypeInfo;

#define CAST_IDENTITY 0
#define CAST_IMPLICIT 1
#define CAST_EXPLICIT 2
#define CAST_ILLEGAL  3
u8 getCastInformation(astType from, astType to) {
	switch (from.kind) {
		case structType:
			if (from.kind == to.kind && from.declaration == to.declaration) return CAST_IDENTITY;
			return CAST_ILLEGAL;
		case enumType:
			if (to.kind == arrayType) return CAST_ILLEGAL;
			if (from.kind == to.kind) {
				if (from.declaration != to.declaration) return CAST_EXPLICIT;
				return CAST_IDENTITY;
			}
			return CAST_EXPLICIT;
		case arrayType:
			if (from.kind == to.kind ) {
				bool capacityMatches = from.arrayInfo->capacity == to.arrayInfo->capacity || to.arrayInfo->capacity == 0;
				if (capacityMatches && getCastInformation(from.arrayInfo->ofType, to.arrayInfo->ofType) == CAST_IDENTITY) return CAST_IDENTITY;
			}
			return CAST_ILLEGAL;

		case voidType:
			if (to.kind == voidType) return CAST_IDENTITY;
			return CAST_ILLEGAL;
		case intType:
		case u8Type:
		case u16Type:
		case u32Type:
		case u64Type:
		case i8Type:
		case i16Type:
		case i32Type:
		case i64Type:
		case floatType:
		case boolType:
		case stringType:
		case charType:
			if (from.kind == to.kind) return CAST_IDENTITY;
			if (to.kind == stringType || (from.kind == stringType && to.kind != boolType)) return CAST_ILLEGAL;
			if (to.kind == arrayType) return CAST_ILLEGAL;

			if (from.kind == intType && isIntType(to.kind)) return CAST_IMPLICIT;

			return CAST_EXPLICIT;
		
		default:
			printf("unhandled type %s in getCastInformation\n", astKindText[from.kind]);
			exit(1);
	}

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
	astType type;
} typedOperator;

static inline astType primitive_type_from_kind(enum astKind kind) {
	return (astType){ kind, .declaration = NULL };
}

typedOperator get_binary_operator(enum syntaxKind operatorToken, astType left, astType right) {
	if (operatorToken == euqualsEqualsOperator && left.kind == enumType && right.kind == enumType && left.declaration == right.declaration) return (typedOperator) { equalOp, primitive_type_from_kind(boolType) };
	if (operatorToken == bangEqualsOperator    && left.kind == enumType && right.kind == enumType && left.declaration == right.declaration) return (typedOperator){ inEqualOp, primitive_type_from_kind(boolType) };

	bool bothFloat = left.kind == floatType && right.kind == floatType;
	bool oneFloat = left.kind == floatType || right.kind == floatType;
	if (bothFloat || (oneFloat && isIntType(left.kind == floatType ? right.kind : left.kind)) ) {
		if (operatorToken == plusOperator         ) return (typedOperator){ addOp, floatType };
		if (operatorToken == minusOperator        ) return (typedOperator){ subtractOp, floatType };
		if (operatorToken == multipliationOperator) return (typedOperator){ multiplyOp, floatType };
		if (operatorToken == divisionOperator     ) return (typedOperator){ divideOp, floatType };

		if (operatorToken == euqualsEqualsOperator) return (typedOperator) { equalOp, primitive_type_from_kind(boolType) };
		if (operatorToken == bangEqualsOperator   ) return (typedOperator){ inEqualOp, primitive_type_from_kind(boolType) };
		if (operatorToken == lessOperator         ) return (typedOperator){ lessOp, primitive_type_from_kind(boolType) };
		if (operatorToken == greaterOperator      ) return (typedOperator){ greaterOp, primitive_type_from_kind(boolType) };
		if (operatorToken == lessEqualsOperator   ) return (typedOperator){ lessOrEqualOp, primitive_type_from_kind(boolType) };
		if (operatorToken == greaterEqualsOperator) return (typedOperator){ greaterOrEqualOp, primitive_type_from_kind(boolType) };
	}

	if (operatorToken == plusOperator          && left.kind == charType && isIntType(right.kind)) return (typedOperator){ addOp, primitive_type_from_kind(charType) };
	if (operatorToken == minusOperator         && left.kind == charType && isIntType(right.kind)) return (typedOperator){ subtractOp, primitive_type_from_kind(charType) };
	if (operatorToken == euqualsEqualsOperator && left.kind == charType && right.kind == charType) return (typedOperator) { equalOp, primitive_type_from_kind(boolType) };
	if (operatorToken == bangEqualsOperator    && left.kind == charType && right.kind == charType) return (typedOperator){ inEqualOp, primitive_type_from_kind(boolType) };
	if (operatorToken == lessOperator          && left.kind == charType && right.kind == charType) return (typedOperator){ lessOp, primitive_type_from_kind(boolType) };
	if (operatorToken == greaterOperator       && left.kind == charType && right.kind == charType) return (typedOperator){ greaterOp, primitive_type_from_kind(boolType) };
	if (operatorToken == lessEqualsOperator    && left.kind == charType && right.kind == charType) return (typedOperator){ lessOrEqualOp, primitive_type_from_kind(boolType) };
	if (operatorToken == greaterEqualsOperator && left.kind == charType && right.kind == charType) return (typedOperator){ greaterOrEqualOp, primitive_type_from_kind(boolType) };

	if (operatorToken == plusOperator          && isIntType(left.kind) && isIntType(right.kind)) return (typedOperator){ addOp, left };
	if (operatorToken == minusOperator         && isIntType(left.kind) && isIntType(right.kind)) return (typedOperator){ subtractOp, left };
	if (operatorToken == multipliationOperator && isIntType(left.kind) && isIntType(right.kind)) return (typedOperator){ multiplyOp, left };
	if (operatorToken == divisionOperator      && isIntType(left.kind) && isIntType(right.kind)) return (typedOperator){ divideOp, left };
	if (operatorToken == modulusOperator       && isIntType(left.kind) && isIntType(right.kind)) return (typedOperator){ moduloOp, left };

	if (operatorToken == euqualsEqualsOperator && isIntType(left.kind) && right.kind == left.kind) return (typedOperator) { equalOp, primitive_type_from_kind(boolType) };
	if (operatorToken == bangEqualsOperator    && isIntType(left.kind) && right.kind == left.kind) return (typedOperator){ inEqualOp, primitive_type_from_kind(boolType) };

	if (operatorToken == euqualsEqualsOperator && left.kind == boolType && right.kind == boolType) return (typedOperator) { equalOp, primitive_type_from_kind(boolType) };
	if (operatorToken == bangEqualsOperator    && left.kind == boolType && right.kind == boolType) return (typedOperator){ inEqualOp, primitive_type_from_kind(boolType) };

	if (operatorToken == lessOperator          && isIntType(left.kind) && right.kind == left.kind) return (typedOperator){ lessOp, primitive_type_from_kind(boolType) };
	if (operatorToken == greaterOperator       && isIntType(left.kind) && right.kind == left.kind) return (typedOperator){ greaterOp, primitive_type_from_kind(boolType) };
	if (operatorToken == lessEqualsOperator    && isIntType(left.kind) && right.kind == left.kind) return (typedOperator){ lessOrEqualOp, primitive_type_from_kind(boolType) };
	if (operatorToken == greaterEqualsOperator && isIntType(left.kind) && right.kind == left.kind) return (typedOperator){ greaterOrEqualOp, primitive_type_from_kind(boolType) };

	if (operatorToken == lessLessOperator       && isIntType(left.kind) && isIntType(right.kind)) return (typedOperator){ shiftLeftOp, left };
	if (operatorToken == greaterGreaterOperator && isIntType(left.kind) && isIntType(right.kind)) return (typedOperator){ shiftRightOp, left };

	if (operatorToken == ampersandOperator && isIntType(left.kind) && right.kind == left.kind) return (typedOperator){ bitwiseAndOp, left };
	if (operatorToken == caretOperator     && isIntType(left.kind) && right.kind == left.kind) return (typedOperator){ bitwiseXorOp, left };
	if (operatorToken == pipeOperator      && isIntType(left.kind) && right.kind == left.kind) return (typedOperator){ bitwiseOrOp, left };

	if (operatorToken == ampersandOperator && left.kind == boolType && right.kind == boolType) return (typedOperator){ bitwiseAndOp, primitive_type_from_kind(boolType) };
	if (operatorToken == caretOperator && left.kind == boolType && right.kind == boolType) return (typedOperator){ bitwiseXorOp, primitive_type_from_kind(boolType) };
	if (operatorToken == pipeOperator && left.kind == boolType && right.kind == boolType) return (typedOperator){ bitwiseOrOp, primitive_type_from_kind(boolType) };

	if (operatorToken == ampersandAmpersandOperator && left.kind == boolType && right.kind == boolType) return (typedOperator){ logicalAndOp, primitive_type_from_kind(boolType) };
	if (operatorToken == pipePipeOperator && left.kind == boolType && right.kind == boolType) return (typedOperator) { logicalOrOp, primitive_type_from_kind(boolType) };

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

enum astUnaryOperator get_unary_operator(enum syntaxKind operatorToken, enum astKind type, bool left) {
	if (type == charType) {
		if (operatorToken == plusPlusOperator && left) return preIncrementOp;
		if (operatorToken == plusPlusOperator && !left) return postIncrementOp;
		if (operatorToken == minusMinusOperator && left) return preDecrementOp;
		if (operatorToken == minusMinusOperator && !left) return postDecrementOp;
	} else if (isIntType(type)) {
		if (operatorToken == plusPlusOperator && left) return preIncrementOp;
		if (operatorToken == plusPlusOperator && !left) return postIncrementOp;
		if (operatorToken == minusMinusOperator && left) return preDecrementOp;
		if (operatorToken == minusMinusOperator && !left) return postDecrementOp;
		if (operatorToken == tildeOperator && left) return bitwiseNegationOp;

		if (operatorToken == plusOperator && left) return identityOp;
		if (operatorToken == minusOperator && left) return negationOp;
	} else if (type == boolType) {

		if (operatorToken == bangOperator && left) return logicalNegationOp;
	} else if (type == floatType) {
		if (operatorToken == plusPlusOperator && left) return preIncrementOp;
		if (operatorToken == plusPlusOperator && !left) return postIncrementOp;
		if (operatorToken == minusMinusOperator && left) return preDecrementOp;
		if (operatorToken == minusMinusOperator && !left) return postDecrementOp;

		if (operatorToken == plusOperator && left) return identityOp;
		if (operatorToken == minusOperator && left) return negationOp;
	}

	return missingUnaryOp;
}

typedef struct astNode {
	enum astSyntaxKind kind;
	astType type;
	union {
		void* data; 
		int numValue; 
		float floatValue; 
		bool boolValue; 
		char* stringValue; 
		char charValue; 
		struct astNode* arrayValues; 
	};
} astNode;

#define VARIABLE_MUTABLE 1
#define VARIABLE_INITIALIZED 2
#define VARIABLE_VALUE_KNOWN 4
#define VARIABLE_GLOBAL 8

#define SYMBOL_VARIABLE 1
#define SYMBOL_FUNCTION 2
#define SYMBOL_NAMESPACE 3
#define SYMBOL_ENUM 4
#define SYMBOL_ENUM_MEMBER 5
#define SYMBOL_STRUCT 6
#define SYMBOL_TYPEDEF 7
#define SYMBOL_ANY 0xFF

typedef struct functionSymbolData {
	struct astSymbol **parameters;
	u16 parameterCount;
	astType returnType;
	astNode body;
} functionSymbolData;

typedef struct astSymbol {
	char* name;
	struct astSymbol *parentNamespace;
	astType type;
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

typedef struct structReferenceAst {
	astNode left;
	astNode right;
} structReferenceAst;

typedef struct ternaryExpressionAst {
	astNode condition;
	astNode thenExpression;
	astNode elseExpression;
} ternaryExpressionAst;

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

typedef struct arrayAccessAst {
	astNode left;
	astNode index;
} arrayAccessAst;

typedef struct variableDeclarationAst {
	astSymbol* variable;
	astNode initalizer;
} variableDeclarationAst;

typedef struct variableAssignmentAst {
	astNode variable;
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

typedef struct structAst {
	astSymbol* structSymbol;
	astNode block;
} structAst;

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

enum astKind resolve_primitive_type_from_span(ast *tree, textspan span) {
	char *text = tree->text;
	for (int i = intType; i<= charType; i++)
		if (span_compare(text, span, astKindText[i])) return i;
	
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

	if (s->parentNamespace != NULL &&  s->parentNamespace->symbolKind != SYMBOL_STRUCT && (s->symbolKind != SYMBOL_VARIABLE || s->flags & VARIABLE_GLOBAL)) {
		printfSymbolReference_internal(f, s->parentNamespace, separator, false);
	}
	fprintf(f, "%s%s", s->name, entry ? "" : separator);
}
void print_ast_internal(char *text, astNode *root, int indent, bool verbose, bool newline);
void print_ast(char *text, astNode *root, int indent, bool verbose) { print_ast_internal(text, root, indent, verbose, true); }
void print_ast_internal(char *text, astNode *root, int indent, bool verbose, bool newline) {

	if (root->kind == literalKind) {
		if (root->type.kind == voidType) return;
		if (verbose) {
			printf ("%*s(", indent, "");
			printf(TERMBLUE);
			switch (root->type.kind) {
				case u8Type: case u16Type: case u32Type: case u64Type: case i8Type: case i16Type: case i32Type: case i64Type:
				case intType: printf ("%d", root->numValue); break;
				case floatType: printf ("%f", root->floatValue); break;
				case boolType: printf ("%s", root->boolValue ? "true" : "false"); break;
				case stringType: printf ("\"%s\"", root->stringValue); break;
				case charType: printf ("'%c'", root->charValue); break;
				case enumType: printfSymbolReference(stdout, root->type.declaration->namespaceScope->symbols[root->numValue], "."); break;
				case arrayType: printf("%s[]",astKindText[root->type.arrayInfo->ofType.kind]); break;
				case structType: printf("{}"); break;
				default:
					fprintf(stderr, "%sUnhandled type '%s' in print_ast%s", TERMRED, astKindText[root->type.kind], TERMRESET);
					exit(1);
			}
			printf ("%s :: %s%s%s)%s", TERMRESET, TERMYELLOW, astKindText[root->type.kind], TERMRESET, newline?"\n":"");
		} else {
			printf(TERMCYAN);
			switch (root->type.kind) {
				case u8Type: case u16Type: case u32Type: case u64Type: case i8Type: case i16Type: case i32Type: case i64Type:
				case intType: printf ("%*s%d%s", indent, "", root->numValue, newline?"\n":""); break;
				case floatType: printf ("%*s%f%s", indent, "", root->floatValue, newline?"\n":""); break;
				case boolType: printf ("%*s%s%s", indent, "", root->boolValue?"true":"false", newline?"\n":""); break;
				case stringType: printf ("%*s\"%s\"%s", indent, "", root->stringValue, newline?"\n":""); break;
				case charType: printf ("%*s%c%s", indent, "", root->charValue, newline?"\n":""); break;
				default:
					fprintf(stderr, "%sUnhandled type '%s' in print_ast%s", TERMRED, astKindText[root->type.kind], TERMRESET);
					exit(1);
			}
			printf(TERMRESET);
		}
		return;
	}

	printf ("%*s(%s\n", indent, "", astSyntaxKindText[root->kind]);

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
	case arrayLiteralKind: {
		astNode* nodes = root->arrayValues;
		u16 capacity = root->type.arrayInfo->capacity;

		for(int i=0;i<capacity;i++) {
			print_ast_internal(text, &nodes[i], indent, verbose, i!=capacity-1);
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

		printf ("%*s%s%s %s%s\n", indent, "", TERMMAGENTA, fn.value->name, astKindText[fn.value->type.kind], TERMRESET);

		if (fn.index != 0) {
			printf ("%*s%s%s %s%s\n", indent, "", TERMMAGENTA, fn.index->name, astKindText[fn.index->type.kind], TERMRESET);
		}

		print_ast_internal(text, &fn.range, indent, verbose, true);
		print_ast_internal(text, &fn.block, indent, verbose, false);
		break;
	}
	case rangeExpressionKind: {
		rangeExpressionAst *rn = (rangeExpressionAst*)root->data;
		if (root->type.kind != charType) printf ("%*s%s%d..%d%s", indent, "", TERMMAGENTA, rn->fromInt, rn->toInt, TERMRESET);
		else printf ("%*s%s'%c'..'%c'%s", indent, "", TERMMAGENTA, rn->fromChar, rn->toChar, TERMRESET);
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
	case structReferenceKind: {
		structReferenceAst un = *(structReferenceAst*)root->data;

		print_ast_internal(text, &un.left, indent, verbose, true);
		print_ast_internal(text, &un.right, indent, verbose, false);
		break;
	}
	case ternaryExpressionKind: {
		ternaryExpressionAst *tn = (ternaryExpressionAst*)root->data;

		print_ast_internal(text, &tn->condition, indent, verbose, true);
		print_ast_internal(text, &tn->thenExpression, indent, verbose, true);
		print_ast_internal(text, &tn->elseExpression, indent, verbose, false);
		break;
	}
	case constructorExpressionKind:
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
	case arrayAccessKind: {
		arrayAccessAst an = *(arrayAccessAst*)root->data;

		print_ast_internal(text, &an.left, indent, verbose, true);
		print_ast_internal(text, &an.index, indent, verbose, false);
		break;
	}
	case castExpressionKind: {
		astNode en = *(astNode*)root->data;

		printf ("%*s%scast<%s>%s\n", indent, "", TERMMAGENTA, astKindText[root->type.kind], TERMRESET);
		print_ast_internal(text, &en, indent, verbose, false);
		break;
	}
	case namespaceDeclarationKind: {
		namespaceAst *ns = (namespaceAst*)root->data;

		printf ("%*s%s", indent, "", TERMMAGENTA);
		printfSymbolReference(stdout, ns->namespace, ".");
		printf ("%s\n", TERMRESET);

		print_ast_internal(text, &ns->block, indent, verbose, false);
		break;
	}
	case structDeclarationKind: {
		structAst *ns = (structAst*)root->data;

		printf ("%*s%s", indent, "", TERMMAGENTA);
		printfSymbolReference(stdout, ns->structSymbol, ".");
		printf ("%s\n", TERMRESET);

		print_ast_internal(text, &ns->block, indent, verbose, false);
		break;
	}
	case functionDeclarationKind: {
		astSymbol vn = *(astSymbol*)root->data;
		functionSymbolData fd = *vn.functionData;

		printf("%s", TERMMAGENTA);
		printf ("%*s%s ", indent, "", astKindText[vn.type.kind]);
		printfSymbolReference(stdout, &vn, ".");
		printf ("(");
		for (int i=0;i<fd.parameterCount;i++) {
			printf (" %s: %s", fd.parameters[i]->name, astKindText[fd.parameters[i]->type.kind]);
		}
		printf(")%s\n", TERMRESET);
		print_ast_internal(text, &vn.functionData->body, indent, verbose, false);
		break;
	}
	case enumDeclarationKind: {
		astSymbol *vn = (astSymbol*)root->data;

		printf("%*s%s", indent, "", TERMMAGENTA);
		printf ("enum ");
		printfSymbolReference(stdout, vn, ".");
		printf (" {");
		for (int i=0;i<sb_count(vn->namespaceScope->symbols);i++) {
			printf (" %s,", vn->namespaceScope->symbols[i]->name);
		}
		printf("}%s", TERMRESET);
		break;
	}
	case variableDeclarationKind: {
		variableDeclarationAst vn = *(variableDeclarationAst*)root->data;

		printf ("%*s%s%s %s (%s)%s\n", indent, "", TERMMAGENTA, vn.variable->name, astKindText[vn.variable->type.kind], (vn.variable->flags & VARIABLE_MUTABLE) ? "mutable" : "constant", TERMRESET);
		if (vn.initalizer.kind != 0) print_ast_internal(text, &vn.initalizer, indent, verbose, false);
		else printf ("%*s%s%s%s", indent, "", TERMMAGENTA, "<uninitialized>", TERMRESET);
		break;
	}
	case variableAssignmentKind: {
		variableAssignmentAst va = *(variableAssignmentAst*)root->data;

		print_ast_internal(text, &va.variable, indent, verbose, true);
		print_ast_internal(text, &va.expression, indent, verbose, false);
		break;
	}
	case typeDeclarationKind:
	case variableReferenceKind: {
		astSymbol *vs = (astSymbol*)root->data;

		printf ("%*s%s ", indent, "", TERMMAGENTA);
		printfSymbolReference(stdout, vs, ".");
		printf (" %s%s", astKindText[vs->type.kind], TERMRESET);
		break;
	}
	default: {
		fprintf(stderr, "%sERROR: Unhandled case in print_ast for kind %s%s", TERMRED, astSyntaxKindText[root->kind], TERMRESET);
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
		switch (root->type.kind) {
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

			case enumType: 
				fprintf (fp, "Label%p[label=\"", root);
				printfSymbolReference(stdout, root->type.declaration->namespaceScope->symbols[root->numValue], ".");
				fprintf (fp, "\"]\n");
				break;
			default:
				fprintf(stderr, "%sUnhandled type '%s' in print_ast_graph%s", TERMRED, astKindText[root->type.kind], TERMRESET);
				exit(1);
		}
		return;
	}

	fprintf (fp, "Label%p[shape=diamond label=\"%s\\n", root, astSyntaxKindText[root->kind]);
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

		ENDLABEL
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

		fprintf (fp, "%s %s", fn->value->name, astKindText[fn->value->type.kind]);

		if (fn->index != 0) {
			fprintf (fp, "\\n%s %s", fn->index->name, astKindText[fn->index->type.kind]);
		}
		ENDLABEL
		print_ast_graph_internal(text, &fn->range, fp, false);
		print_ast_graph_internal(text, &fn->block, fp, false);
		fprintf (fp, "Label%p -> Label%p\n", root, &fn->range);
		fprintf (fp, "Label%p -> Label%p\n", root, &fn->block);
		break;
	}
	case rangeExpressionKind: {
		rangeExpressionAst *rn = (rangeExpressionAst*)root->data;
		if (root->type.kind != charType) printf ("%d..%d", rn->fromInt, rn->toInt);
		else printf ("'%c'..'%c'", rn->fromChar, rn->toChar);
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
	case ternaryExpressionKind: {
		ternaryExpressionAst *tn = (ternaryExpressionAst*)root->data;

		ENDLABEL
		fprintf (fp, "Label%p -> Label%p\n", root, &tn->condition);
		fprintf (fp, "Label%p -> Label%p\n", root, &tn->thenExpression);
		fprintf (fp, "Label%p -> Label%p\n", root, &tn->elseExpression);
		print_ast_graph_internal(text, &tn->condition, fp, false);
		print_ast_graph_internal(text, &tn->thenExpression, fp, false);
		print_ast_graph_internal(text, &tn->elseExpression, fp, false);
		break;
	}
	case constructorExpressionKind:
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


		fprintf(fp, "cast<%s>", astKindText[root->type.kind]);
		ENDLABEL
		fprintf (fp, "Label%p -> Label%p\n", root, en);
		print_ast_graph_internal(text, en, fp, false);
		break;
	}
	case namespaceDeclarationKind: {
		namespaceAst *ns = (namespaceAst*)root->data;

		printfSymbolReference(fp, ns->namespace, ".");
		ENDLABEL
		fprintf (fp, "Label%p -> Label%p\n", root, &ns->block);
		print_ast_graph_internal(text, &ns->block, fp, false);
		break;
	}
	case functionDeclarationKind: {
		astSymbol *vn = (astSymbol*)root->data;
		functionSymbolData *fd = vn->functionData;

		fprintf (fp, "%s ", astKindText[vn->type.kind]);

		printfSymbolReference(fp, vn, ".");
		fprintf (fp, "(");
		for (int i=0;i<fd->parameterCount;i++) {
			printf (" %s: %s", fd->parameters[i]->name, astKindText[fd->parameters[i]->type.kind]);
		}
		fprintf(fp, ")");
		ENDLABEL
		print_ast_graph_internal(text, &fd->body, fp, false);
		fprintf (fp, "Label%p -> Label%p\n", root, &fd->body);
		break;
	}
	case enumDeclarationKind: {
		astSymbol *vn = (astSymbol*)root->data;

		fprintf (fp, "enum ");
		printfSymbolReference(stdout, vn, ".");
		fprintf(fp, " {");
		for (int i=0;i<sb_count(vn->namespaceScope->symbols);i++) {
			fprintf(fp, " %s,", vn->namespaceScope->symbols[i]->name);
		}
		fprintf(fp, "}");
		ENDLABEL
		break;
	}
	case variableDeclarationKind: {
		variableDeclarationAst *vn = (variableDeclarationAst*)root->data;

		printf ("%s %s (%s)", vn->variable->name, astKindText[vn->variable->type.kind], (vn->variable->flags & VARIABLE_MUTABLE) ? "mutable" : "constant");
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

		ENDLABEL
		print_ast_graph_internal(text, &va->variable, fp, false);
		print_ast_graph_internal(text, &va->expression, fp, false);
		fprintf (fp, "Label%p -> Label%p\n", root, &va->expression);
		break;
	}
	case typeDeclarationKind:
	case variableReferenceKind: {
		astSymbol *vs = (astSymbol*)root->data;
		printfSymbolReference(fp, vs, ".");
		fprintf(fp, "%s", astKindText[vs->type.kind]);
		ENDLABEL
		break;
	}
	default: {
		fprintf(stderr, "%sERROR: Unhandled case in print_ast for kind %s%s", TERMRED, astSyntaxKindText[root->kind], TERMRESET);
		exit(1);
		break;
	}
	}

	if (isRoot) fprintf(fp, "}\n");
}