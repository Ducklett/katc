static const char *diagnosticText[] = {
	"Unexpected character '%c', expected '%c' (%d,%d)\n",
	"bad token '%c' (%d,%d)\n",
	"unexpected token of kind '%s', expected '%s' (%d,%d)\n",
	"undefined unary operator '%s' for value of type '%s' (%d,%d)\n",
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
		case unexpectedCharacterDiagnostic: {
			char param1 = d.param1;
			if (param1 == '\r' || param1 == '\n' || param1 == '\0') param1 = ' ';
			printf(diagnosticText[d.kind], param1, d.param2, d.span.start, d.span.length); break;
		}
		case badTokenDiagnostic:
			printf(diagnosticText[d.kind], sourceText[d.span.start], d.span.start, d.span.length); break;
		case unexpectedTokenDiagnostic:
			printf(diagnosticText[d.kind], syntaxKindText[d.param1], syntaxKindText[d.param2], d.span.start, d.span.length); break;
		case undefinedUnaryOperatorDiagnostic:
			printf(diagnosticText[d.kind], syntaxKindText[d.param1], astTypeText[d.param2], d.span.start, d.span.length); break;
		}
		TERMRESET();

		int lineStart;
		for (lineStart = d.span.start; sourceText[lineStart] != '\n' && lineStart > 0; lineStart--) {}
		for (int i=lineStart;i<d.span.start;i++) printf("%c", sourceText[i]);
		TERMRED();
		for (int i=0;i<d.span.length;i++) printf("%c", sourceText[d.span.start + i]);
		TERMRESET();
		for (int i=(d.span.start + d.span.length); sourceText[i] != '\n' && sourceText[i] != '\0';i++) printf("%c", sourceText[i]);
		printf("\n");
	}
}
