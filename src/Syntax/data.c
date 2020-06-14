
enum syntaxKind
{
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

enum diagnosticKind
{
	badTokenDiagnostic,
	unexpectedTokenDiagnostic,
};

static const char *diagnosticText[] = {
	"bad token '%c' (%d,%d)\n",
	"unexpected token of kind '%s', expected '%s' (%d,%d)\n",
};

typedef struct diagnostic
{
	int kind;
	int start;
	int length;
	int param1;
	int param2;
	int param3;
} diagnostic;

typedef struct diagnosticContainer
{
	int index;
	diagnostic diagnostics[10];
} diagnosticContainer;

void report_diagnostic(diagnosticContainer *d, enum diagnosticKind kind, int start, int length, int param1, int param2, int param3)
{
	if (d->index >= 10)
		return;

	diagnostic dia = {
		.kind = kind,
		.start = start,
		.length = length,
		.param1 = param1,
		.param2 = param2,
		.param3 = param3,
	};
	d->diagnostics[d->index++] = dia;
}

typedef struct node
{
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