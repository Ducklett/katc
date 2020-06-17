typedef struct diagnostic {
	u8 kind;
	textspan span;
	u32 param1;
	u32 param2;
	u32 param3;
} diagnostic;

typedef struct diagnosticContainer {
	u8 index;
	diagnostic diagnostics[10];
} diagnosticContainer;

enum diagnosticKind {
	badTokenDiagnostic,
	unexpectedTokenDiagnostic,
};

static const char *diagnosticText[] = {
	"bad token '%c' (%d,%d)\n",
	"unexpected token of kind '%s', expected '%s' (%d,%d)\n",
};

void report_diagnostic(diagnosticContainer *d, enum diagnosticKind kind, textspan span, u32 param1, u32 param2, u32 param3) {
	if (d->index >= 10) return;

	diagnostic dia = { kind, span, param1, param2, param3, };
	d->diagnostics[d->index++] = dia;
}

void print_diagnostics(diagnosticContainer *diagnostics, char* sourceText) {
	for (int i = 0; i < diagnostics->index; i++) {
		diagnostic d = diagnostics->diagnostics[i];
		TERMRED();
		switch (d.kind) {
		case badTokenDiagnostic:
			printf(diagnosticText[d.kind], sourceText[d.span.start], d.span.start, d.span.length); break;
		case unexpectedTokenDiagnostic:
			printf(diagnosticText[d.kind], syntaxKindText[d.param1], syntaxKindText[d.param2], d.span.start, d.span.length); break;
		}
		TERMRESET();
	}
}
