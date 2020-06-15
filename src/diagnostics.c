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

enum diagnosticKind
{
	badTokenDiagnostic,
	unexpectedTokenDiagnostic,
};

static const char *diagnosticText[] = {
	"bad token '%c' (%d,%d)\n",
	"unexpected token of kind '%s', expected '%s' (%d,%d)\n",
};

void report_diagnostic(diagnosticContainer *d, enum diagnosticKind kind, int start, int length, int param1, int param2, int param3)
{
	if (d->index >= 10)
		return;

	diagnostic dia = { kind, start, length, param1, param2, param3, };
	d->diagnostics[d->index++] = dia;
}

void print_diagnostics(diagnosticContainer *diagnostics, char* sourceText) {
	for (int i = 0; i < diagnostics->index; i++)
	{
		diagnostic d = diagnostics->diagnostics[i];
		switch (d.kind) {
			case badTokenDiagnostic: printf(diagnosticText[d.kind], sourceText[d.start], d.start, d.length); break;
			case unexpectedTokenDiagnostic: printf(diagnosticText[d.kind], syntaxKindText[d.param1], syntaxKindText[d.param2], d.start, d.length); break;
		}
	}
}