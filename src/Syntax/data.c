
enum syntaxKind
{
	badToken,
	endOfFileToken,
	whitespaceToken,
	newlineToken,

	numberLiteral,
	plusOperator,
	minusOperator,
	multipliationOperator,
	divisionOperator,
	modulusOperator,
};

static const char *syntaxKindText[] = {
	"badToken",
	"endOfFileToken",
	"whitespaceToken",
	"newlineToken",
	"numberLiteral",
	"plusOperator",
	"minusOperator",
	"multipliationOperator",
	"divisionOperator",
	"modulusOperator",
};

enum diagnosticKind
{
	badTokenDiagnostic,
};

static const char *diagnosticText[] = {
	"bad token '%c' (%d,%d)\n",
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

typedef struct token
{
	enum syntaxKind kind;
	int text_start;
	int text_length;
} token;
